;;; ethersync.el ---Etherpad easysync protocol -*- coding: utf-8; lexical-binding: t -*-

;; Copyright 2020 FoAM
;;
;; Author: nik gaffney <nik@fo.am>
;; Created: 2020-12-12
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (request "0.3") (let-alist "0.0"))
;; Keywords: comm, etherpad, collaborative editing
;; URL: https://github.com/zzkt/ethermacs

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Etherpad is a highly customizable Open Source online editor providing
;; collaborative editing in really real-time.
;;
;; The easysync protocol is used for communication of edits, changesets
;; and metadata between etherpad server and clients. It uses websockets
;; for the transport layer
;;
;;  details -> https://etherpad.org/doc/v1.8.5/#index_http_api

;; current issues 2020-12-15 00:52:32
;;  - on ws per buffer. buffer local. shared.
;;  - incorrect newline counts when sending changesets
;;  - problems w. deleting text and/or changing buffer size & change-hooks
;;  - see "Additional Constraints" of easysync
;;  - potential race conditions sending changesets on buffer changes
;;  - doesn't apply attributes or changes from the apool
;;  - general lack of error checking

;;; Code:

(require 'websocket)
(require 'let-alist)
(require 'calc-bin)
(require 'parsec)
(require '0xc)
(require 's)

;; debug details
(setq websocket-debug t)

;; local and buffer local variables
(defvar-local ep--pre-buffer-length 0)
(defvar-local ep--new-buffer-length 0)
(defvar-local ep--change-length 0)
(defvar-local ep--change-string "")

(defvar-local ep--local-rev 0)
(defvar-local ep--current-pad "")
(defvar-local ep--local-author "") ;; e.g. "a.touCZaixjPgKDSiN"

(defvar-local ep--hearbeat-timer nil)
(defvar-local ep--current-socket nil)

;; enable/disable keep alive message to the server
(defun ethersync-heartbeat-start ()
  "Maintain connection to server with periodic pings."
  (message "heartbeat started: %s" (current-buffer))
  (setq ep--hearbeat-timer
        (run-with-timer 5 15 #'wss-send "2")))

(defun ethersync-heartbeat-stop ()
  "Stop sending keep-alive messages."
  (cancel-timer ep--hearbeat-timer))

;; buffering
(defvar *etherpad-buffer* (generate-new-buffer "*etherpad*"))

(defun ethersync-current-socket (&optional socket)
  "Return currently active socket or set SOCKET as current."
  (message "socket in buffer: %s" (current-buffer))
  (if socket
      (setq ep--current-socket socket)
      ep--current-socket))

;; setters
(defun ethersync--set-local-rev (n)
  "Set the local revision."
  (message "current rev: %s" ep--local-rev)
  (setq ep--local-rev n)
  (message "updated rev: %s" ep--local-rev)
  (with-current-buffer *etherpad-buffer*
    (rename-buffer (format "etherpad:%s:%s"
                           ep--current-pad
                           ep--local-rev))))

;; see also -> inhibit-modification-hooks

(defun ethersync--add-change-hooks ()
  (interactive)
  (message "setting up buffer change hooks")
  (with-current-buffer *etherpad-buffer*
    (add-hook 'before-change-functions
              'ethersync--before-buffer-changes nil t)
    ;; ordering is important...
    (add-hook 'after-change-functions
              'ethersync--after-buffer-changes 22 t)
    (add-hook 'after-change-functions
              'ethersync--send-changes 23 t)))

(defun ethersync--remove-change-hooks ()
  (interactive)
  (message "removing buffer change hooks")
  (with-current-buffer *etherpad-buffer*
    (remove-hook 'before-change-functions
                 'ethersync--before-buffer-changes t)
    (remove-hook 'after-change-functions
                 'ethersync--after-buffer-changes t)
    (remove-hook 'after-change-functions
                 'ethersync--send-changes t)))


(defun ethersync--before-buffer-changes (begin end)
  ;; (message "before -> b:%s e:%s" begin end)
  (setq-local ep--pre-buffer-length (length (buffer-string))
              ep--change-length (- end begin)))

(defun ethersync--after-buffer-changes (begin end pre)
  ;; (message "after -> b:%s e:%s p:%s" begin end pre)
  (setq-local ep--new-buffer-length (length (buffer-string))
              ep--change-string (buffer-substring begin end)
              ep--change-length (- end begin)))

  ;; (message "eabc: pre: %s post: %s changed: %s chars to: %s at: %s(%s)"
  ;;          ep--pre-buffer-length
  ;;          ep--new-buffer-length
  ;;          ep--change-length
  ;;          ep--change-string
  ;;          (point)
  ;;          (n-36 (point))))


;; emacs -> etherpad changes

(defun ethersync--send-changes (_b _e _p)
"Create and encode a changeset."
  (let* ((b0 ep--pre-buffer-length)
         (b1 ep--new-buffer-length)
         (ops (if (< b0 b1)
                  (format "+%s" (- b1 b0))
                  (format "-%s" (- b0 b1))))
         (changeset
           (ethersync--encode-changeset
            b0
            (- b1 b0)
            ops
            ep--change-string)))
    (message "changeset: %s" changeset)
    (ethersync--send-user-changes changeset)))


(defun ethersync--encode-changeset (length change-size ops chars)
 "Create a changeset from some buffer activty."
  (message "encoding: o:%s (%s) cs:%s op:%s ch:%s" length (n-36 length) change-size ops chars)
  (let* ((change (cond ((= 0 change-size) "=0")
                      ((> 0 change-size)
                       (format "<%s" (n-36 (abs change-size))))
                      ((< 0 change-size)
                       (format ">%s" (n-36 change-size)))))
         (newline-count (s-count-matches "\n" (buffer-substring
                                               (point-min)
                                               (point))))
         (offset (- (point) (length chars) 1))
         ;; offset is distance from point-min to point w.out inserted chars and w. newlines
         (pos-op (if (< 0 newline-count)
                     (format "|%s=%s=%s"
                             ;; 2 steps reqd. newline insert, then from beginning of line?
                             (n-36 newline-count) (n-36 offset)
                             (n-36 (caar
                                    (reverse
                                     (s-matched-positions-all
                                      "\n" (buffer-substring
                                            (point-min) (point)))))))
                     (format "=%s"  (n-36 offset)))))
    (format "Z:%s%s%s%s$%s" (n-36 length) change pos-op ops chars)))


(defun ethersync--send-user-changes (cs)
  "Send a 'USER_CHANGES' message with changeset CS."
  (let* ((author ep--local-author)
         (rev ep--local-rev)
         (changeset cs)
         (payload (format "42[\"message\",{\"type\":\"COLLABROOM\",\"component\":\"pad\",\"data\":{\"type\":\"USER_CHANGES\",\"baseRev\":%s,\"changeset\":\"%s\",\"apool\":{\"numToAttrib\":{},\"nextNum\":1}}}]" rev changeset author)))

    (message "send this -> %s" payload)
    (wss-send payload)
    ))

;; parsec info https://github.com/cute-jumper/parsec.el

(defun ethersync-parse-changeset (cs)
"Parse a changeset CS.

  :N : Source text has length N (must be first op)
  >N : Final text is N (positive) characters longer than source text (must be second op)
  <N : Final text is N (positive) characters shorter than source text (must be second op)
  >0 : Final text is same length as source text
  +N : Insert N characters from the bank, none of them newlines
  -N : Skip over (delete) N characters from the source text, none of them newlines
  =N : Keep N characters from the source text, none of them newlines
|L+N : Insert N characters from the source text, containing L newlines.  The last
         character inserted MUST be a newline, but not the (new) document's final newline.
|L-N : Delete N characters from the source text, containing L newlines. The last
         character inserted MUST be a newline, but not the (old) document's final newline.
|L=N : Keep N characters from the source text, containing L newlines.  The last character
         kept MUST be a newline, and the final newline of the document is allowed.
  *I : Apply attribute I from the pool to the following +, =, |+, or |= command.
         In other words, any number of * ops can come before a +, =, or | but not
         between a | and the corresponding + or =.
         If +, text is inserted having this attribute.  If =, text is kept but with
         the attribute applied as an attribute addition or removal.
         Consecutive attributes must be sorted lexically by (key,value) with key
         and value taken as strings.  It's illegal to have duplicate keys
         for (key,value) pairs that apply to the same text.  It's illegal to
         have an empty value for a key in the case of an insertion (+), the
         pair should just be omitted."

  (let* ((changes
           (parsec-with-input cs
                               ;; a letter Z (the "magic character" and format version identifier)
                               (parsec-str "Z")
                               (parsec-collect*
                                ;; source text length
                                (parsec-re ":[0-9a-z]+")
                                ;; change in text length
                                (parsec-re "[>=<][0-9a-z]+")
                                ;; insertion & deletion operations
                                (parsec-many
                                 (parsec-or
                                  (parsec-re "|[0-9a-z]+[+-=][0-9a-z]+")
                                  (parsec-re "[><+-=*][0-9a-z]+")))
                                ;; separator
                                (parsec-str "$")
                                ;; a string of characters used by insertion operations (the "char bank")
                                (parsec-many-s
                                 (parsec-any-ch))))))

    (let* ((old-length
             (0xc-string-to-number (substring (car changes) 1) 36))
           (change-sign
             (if (s-equals? ">" (substring (nth 1 changes) 0 1)) 1 -1))
           (change-size
             (0xc-string-to-number (substring (nth 1 changes) 1) 36))
           (new-length
             (+ old-length (* change-sign change-size)))
           (ops
             (nth 2 changes))
           (chars
             (car (last changes))))
      ;;(message "old length: %s new length: %s ops: %s chars: %s" old-length new-length ops chars)
      (list old-length ops chars)
   )))


;; radix reductions

(defun s-36 (string)
  "Convert a base-36 number STRING to decimal."
    (0xc-string-to-number string 36))

(defun n-36 (number)
  "Convert a decimal NUMBER to base-36 as string."
  (let ((calc-number-radix 36))
    (downcase
     (math-format-radix number))))


;; operations -> buffer changes

(defun ethersync-apply-ops (ops chars)
  "Apply a series of insert/delete operations.
Numeric offsets are calculated from the beginning of the buffer."
  (with-current-buffer *etherpad-buffer*
    (save-mark-and-excursion
     (goto-char (point-min))
     (let ((char-bank chars))
       (mapcar
        (lambda (s)
          (let* ((o1 (s-left 1 s))
                 (p1 (substring s 1)))
            (message "op: %s val: %s" o1 p1)
            (pcase o1
                   ("+" (ethersync-insert (s-left (s-36 p1) char-bank))
                        (setq char-bank (s-right (s-36 p1) char-bank)))
                   ("-" (ethersync-delete (s-36 p1)))
                   ("=" (ethersync-keep (s-36 p1)))
                   ("|" (let* ((p2 (s-split "[+=-]" (s-36 p1)))
                               (l1 (s-36 (car p2)))
                               (n1 (s-36 (cadr p2))))
                          ;; doesn't insert or delete newlines correctly (yet)
                          (message "ops: l1:%s n1:%s" l1 n1)
                          (pcase p1
                                 ((pred (s-matches? "+"))
                                  (ethersync-insert char-bank)
                                  (setq char-bank (s-right n1 char-bank)))
                                 ((pred (s-matches? "-")) (ethersync-delete n1))
                                 ((pred (s-matches? "=")) (ethersync-keep n1)))))
                   ("*" t)
                   (_ nil)
                   )))
        ops)))))


;; character operations for remote->local sync
;; which should not trigger change hooks

(defun ethersync-insert (chars)
  "Insert CHARS into the source text."
  (let ((inhibit-modification-hooks t))
    (insert chars)))

(defun ethersync-delete (n)
  "Delete (skip over) N chars from the source text."
  (let ((inhibit-modification-hooks t))
    (delete-char n)))

(defun ethersync-keep (n)
  "Keep N chars from the source text."
  (let ((inhibit-modification-hooks t))
    (forward-char n)))

;; start with current pad text

(defun ethersync-init-text (chars)
  "Seeds a buffer with CHARS from a remote pad"
  (with-current-buffer *etherpad-buffer*
    (let ((inhibit-modification-hooks t))
      (erase-buffer)
      (goto-char (point-min))
      (insert chars))))

(defun ethersync-try-changeset (cs)
  (let* ((changes
           (ethersync-parse-changeset cs))
         (len (nth 0 changes))
         (ops (nth 1 changes))
         (chars (nth 2 changes)))
    (ethersync--check-length len)
    (ethersync-apply-ops ops chars)))

(defun ethersync--check-length (size)
  "Check the changeset and buffer SIZE are consistent."
  (when (not (= size (length (buffer-string))))
    (message "changeset and buffer length are inconsistent.")))

;; various stanzas

(defun ethersync--request-client-ready (padId)
  "Ethersync: send CLIENT_READY for PADID."
 (format "42[\"message\",{\"component\":\"pad\",\"type\":\"CLIENT_READY\",\"padId\":\"%s\",\"token\":\"%s\",\"protocolVersion\":2}]" padId *session-token*))

(defun ethersync--request-get-comments (padId)
  "Ethersync: request comments on PADID."
  (format "42/comment,0[\"getComments\",{\"padId\":\"%s\"}]" padId))

(defun ethersync--request-get-comment-replies (padId)
  "Ethersync: request comment replies on PADID."
  (format "42/comment,1[\"getCommentReplies\",{\"padId\":\"%s\"}]" padId))


;; sending via websockets

(defalias 'wss-send #'ethersync-wss-send)

(defun ethersync-wss-send (msg)
  "Send MSG to a websocket."
  (when (stringp msg)
    (websocket-send-text
     (ethersync-current-socket) msg)))


;; parsing & dispatch of incoming frames

(defun ethersync-parse-wsframe (_websocket frame)
 "Parse & dispatch incoming FRAME.
Parsing occurs with-current-buffer for constancy with buffer-local variables
use let bindings for multiple connections."
  ;(message "parsing: %s" frame)
  (with-current-buffer *etherpad-buffer*
  (let* ((fr0 (websocket-frame-text frame))
         (frp (parsec-with-input
               fr0
               (parsec-collect* (parsec-re "[0-9]+")
                                (parsec-many-s (parsec-any-ch))))))
    (message "frame: %s" (length fr0))
    (pcase (car frp)
           ("0" (ethersync--parse-0 frp))
           ("2" (ethersync--parse-2 frp))
           ("3" (message "3: keep-alive"))
           ("42" (ethersync--parse-42 frp))
          ))))

;; parse various incoming message types

(defun ethersync--parse-0 (p0)
  "Parse messages beginning with 0 from P0.
set sid, upgrades, pingInterval and pingTimeout for session."
  (when (listp p0)
    (pcase (length p0)
           (0 nil)
           (1 (car p0))
           (_ (let* ((p1 (json-parse-string (nth 1 p0) :object-type 'alist))
                     (sid (alist-get 'sid p1)))
                (message "sid %s" sid)
                )))))


(defun ethersync--parse-2 (p0)
  "Parse messages beginning with 2 from P0.
set revisions etc."
  (when (listp p0)
    (pcase (length p0)
           (0 nil)
           (1 (car p0))
           (_ (let* ((p1 (json-parse-string (nth 1 p0) :object-type 'alist)))
                (let-alist (aref p1 1)
                           (pcase .type
                                  ("COLLABROOM"
                                   (pcase .data.type
                                          ("ACCEPT_COMMIT"
                                           (message "accepted changes: rev:%s and %s (by %s)"
                                                    .data.newRev)
                                           (ethersync--set-local-rev .data.newRev)))))))))))


(defun ethersync--parse-42 (p0)
  "Parse messages beginning with 42 from P0.
most of the COLLABROOM and update stuff..."
  (when (listp p0)
    (pcase (length p0)
           (0 nil)
           (1 (car p0))
           (_ (let* ((p1 (json-parse-string (nth 1 p0) :object-type 'alist)))
                (let-alist (aref p1 1)
                           (pcase .type
                                  ("COLLABROOM"
                                   (pcase .data.type

                                          ("USER_NEWINFO"
                                           (message "42: new user %s (color %s)"
                                                    .data.userInfo.userId
                                                    .data.userInfo.colorId))

                                          ("NEW_CHANGES"
                                           (message "42: new_changes rev:%s  changeset:%s (by %s)"
                                                    .data.newRev
                                                    .data.changeset
                                                    .data.author)
                                           (ethersync--set-local-rev .data.newRev)
                                           (ethersync-try-changeset .data.changeset))

                                          ("USER_CHANGES"
                                           (message "42: user_changes rev:%s  changeset:%s (by %s)"
                                                    .data.baseRev
                                                    .data.changeset
                                                    .data.apool.author))

                                          ("ACCEPT_COMMIT"
                                           (message "42: accept-commit rev:%s" .data.newRev)
                                           (ethersync--set-local-rev .data.newRev)
                                           )))

                                  ("CLIENT_READY"
                                   (message "42: ready -> %s and %s" .padId .token))

                                  ("CLIENT_VARS"
                                    (message "42: client_vars (%s) rev:%s -> %s"
                                             .data.padId
                                             .data.collab_client_vars.rev
                                             .data.collab_client_vars.initialAttributedText.text)
                                    (ethersync--set-local-rev
                                     .data.collab_client_vars.rev)
                                    (ethersync-init-text
                                     .data.collab_client_vars.initialAttributedText.text)))

                           (pcase .disconnect
                                  ("badChangeset"
                                   (message "42: disconnect (%s)" .disconnect)))
                                     ))))))


(provide 'ethersync)
;;; ethersync.el ends here
