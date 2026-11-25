;;; etherpad-server-tests.el --- Live server tests for etherpad.el -*- lexical-binding: t; -*-

;; Copyright © 2026 FoAM
;; Author: nik gaffney <nik@fo.am>
;; SPDX-License-Identifier: EUPL-1.2

;;; Commentary:

;; ERT tests that run against a live Etherpad server.
;; The pad used for testing can be configured via ETHERMACS_TEST_PAD_URL
;;
;; Most tests are read-only (fetch text, check changesets and export).
;; `etherpad-server--edit-add-insert-delete' sends edits and should restore
;; the pad to its original text before disconnecting.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etherpad)


(defconst etherpad-server--websocket-available-p (fboundp 'websocket-open))

(defvar etherpad-server-test-pad-url
  (or (getenv "ETHERMACS_TEST_PAD_URL")
      "https://example.org/p/test")
  "URL of the Etherpad pad to use for testing.")


;;; Helpers

(defun etherpad-server--wait-for (pred timeout)
  "Wait up to TIMEOUT seconds for PRED to return non-nil."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (< (float-time) deadline) (not (funcall pred)))
      (accept-process-output nil 0.1)
      (sit-for 0.05))
    (funcall pred)))

(defun etherpad-server--teardown (buf)
  "Disconnect and kill the pad buffer BUF, if any."
  (when (and buf (buffer-live-p buf))
    (with-current-buffer buf
      (ignore-errors (etherpad-disconnect)))
    (ignore-errors (kill-buffer buf))))

(defun etherpad-server--connect (url)
  "Connect to pad URL over WebSocket and wait for CLIENT_VARS."
  (condition-case nil
      (let* ((pad-id (cdr (etherpad--parse-pad-url url)))
             (buf-name (format "*etherpad:%s*" pad-id)))
        (when-let* ((old (get-buffer buf-name)))
          (etherpad-server--teardown old))
        (let ((etherpad-author-name "etherpad-tests"))
          (etherpad-connect url))
        (let ((buf (get-buffer buf-name)))
          (when buf
            (etherpad-server--wait-for
             (lambda ()
               (with-current-buffer buf
                 (and (boundp 'etherpad--base-text) etherpad--base-text)))
             30)
            (and (with-current-buffer buf
                   (and (boundp 'etherpad--base-text) etherpad--base-text))
                 buf))))
    (error nil)))

(defmacro etherpad-server--with-pad (var &rest body)
  "Bind VAR to a connected pad buffer, run BODY, then clean up."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless etherpad-server--websocket-available-p)
     (let ((,var (etherpad-server--connect etherpad-server-test-pad-url)))
       (unwind-protect
           (progn
             (unless ,var (ert-skip "Etherpad server unreachable"))
             ,@body)
         (etherpad-server--teardown ,var)))))

(defun etherpad-server--wait-synced (buf expected &optional timeout)
  "Wait (optioanlly TIMEOUT) until base text of BUF equals EXPECTED."
  (etherpad-server--wait-for
   (lambda ()
     (with-current-buffer buf
       (and (not etherpad--connecting)
            (not etherpad--awaiting-ack)
            (not etherpad--inflight-changeset)
            (equal etherpad--base-text expected))))
   (or timeout 15)))

(defun etherpad-server--canonical-changeset-p (cs)
  "Return non-nil if changeset CS is in canonical form.

Checks;
- no zero-length ops
- no adjacent ops with identical opcode and attributes
- insert ops that span newlines end on a newline boundary
- A round trip through serialize/parse leaves it unchanged."
  (let ((ops (alist-get 'ops cs))
        (bank (or (alist-get 'char-bank cs) ""))
        (bank-pos 0)
        (prev nil))
    (and (listp ops)
         (cl-loop
          for op in ops
          for ok =
          (let ((res (and (integerp (nth 1 op))
                          (> (nth 1 op) 0)
                          (or (null prev)
                              (not (and (equal (nth 0 op) (nth 0 prev))
                                        (equal (nth 3 op) (nth 3 prev))))))))
            (when (equal (nth 0 op) "insert")
              (let ((text (substring bank bank-pos (+ bank-pos (nth 1 op)))))
                (setq bank-pos (+ bank-pos (nth 1 op)))
                (setq res (and res
                               (or (not (string-match-p "\n" text))
                                   (string-suffix-p "\n" text))))))
            (setq prev op)
            res)
          always ok)
         (equal cs
                (etherpad--parse-changeset
                 (etherpad--serialize-changeset
                  (alist-get 'old-len cs)
                  (alist-get 'net-delta cs)
                  (alist-get 'ops cs)
                  (alist-get 'char-bank cs)))))))


;;; Connection

(ert-deftest etherpad-server--connect-loads-text ()
  (etherpad-server--with-pad buf
    (with-current-buffer buf
      (should (stringp etherpad--base-text))
      (should (not (string-empty-p etherpad--base-text)))
      (should (string-suffix-p "\n" etherpad--base-text))
      (should (equal etherpad--base-text
                     (etherpad--buffer-text-normalized))))))


;;; Editing

(ert-deftest etherpad-server--edit-add-insert-delete ()
  "Add, insert and delete text, verifying each edit syncs to the server."
  (etherpad-server--with-pad buf
    (with-current-buffer buf
      (let* ((original etherpad--base-text)
             (added "APPENDED-LINE\n")
             (inserted "PREFIX-"))
        ;; ADD: append a line at the end
        (goto-char (point-max))
        (insert added)
        (should (etherpad-server--wait-synced buf (concat original added)))
        (should (equal etherpad--base-text (concat original added)))
        (should (string-suffix-p added etherpad--base-text))
        ;; INSERT: add text at the start
        (goto-char (point-min))
        (insert inserted)
        (should (etherpad-server--wait-synced
                 buf (concat inserted original added)))
        (should (string-prefix-p inserted etherpad--base-text))
        ;; DELETE: remove the prefix again
        (goto-char (point-min))
        (delete-region (point-min) (+ (point-min) (length inserted)))
        (should (etherpad-server--wait-synced buf (concat original added)))
        (should-not (string-prefix-p inserted etherpad--base-text))
        ;; Restore the pad to its original text
        (goto-char (point-max))
        (delete-region (- (point-max) (length added)) (point-max))
        (should (etherpad-server--wait-synced buf original))
        (should (equal etherpad--base-text original))))))


;;; Changesets

(ert-deftest etherpad-server--changeset-round-trip ()
  (etherpad-server--with-pad buf
    (let* ((base (with-current-buffer buf etherpad--base-text))
           (edited (concat base "appended by tests\n"))
           (cs (etherpad--make-changeset base edited)))
      (let ((wire (etherpad--serialize-changeset
                   (alist-get 'old-len cs)
                   (alist-get 'net-delta cs)
                   (alist-get 'ops cs)
                   (alist-get 'char-bank cs))))
        (should (equal cs (etherpad--parse-changeset wire)))
        (should (equal edited
                       (etherpad--apply-changeset-to-string cs base)))))))

(ert-deftest etherpad-server--changeset-canonical-form ()
  (etherpad-server--with-pad buf
    (let* ((base (with-current-buffer buf etherpad--base-text))
           ;; canonical multiline splitting?
           (edited (concat "prefix\n" base "middle\n" base)))
      (dolist (target (list edited (concat base "trailing\n")))
        (should (etherpad-server--canonical-changeset-p
                 (etherpad--make-changeset base target)))))))


;;; Export

(ert-deftest etherpad-server--export-txt ()
  (etherpad-server--with-pad buf
    (with-current-buffer buf
      (let ((file (make-temp-file "etherpad-export" nil ".txt")))
        (unwind-protect
            (progn
              (etherpad-export file "txt")
              (should (equal etherpad--base-text
                             (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string)))))
          (ignore-errors (delete-file file)))))))

(provide 'etherpad-server-tests)
;;; etherpad-server-tests.el ends here
