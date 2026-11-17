;;; etherpad.el --- Collaborative editing via Etherpad -*- coding: utf-8; lexical-binding: t -*-

;; Copyright © 2020 FoAM
;; Author: nik gaffney <nik@fo.am>
;; Created: 2020-08-08
;; Version: 1.0.2
;; Package-Requires: ((emacs "29.1") (websocket "1.12"))
;; Keywords: comm, etherpad, collaborative editing
;; URL: https://codeberg.org/zzkt/ethermacs

;; This file is not part of GNU Emacs.
;; SPDX-License-Identifier: EUPL-1.2

;;; Commentary:

;; Etherpad is a highly customizable Open Source online editor providing
;; collaborative editing in really real-time.
;;
;; This package enables collaborative real-time editing of Etherpad pads
;; from the comfort of an Emacs buffer
;;
;;   M-x etherpad-connect RET

;;; Code:

(require 'url-handlers)
(require 'url-parse)
(require 'websocket)
(require 'subr-x)
(require 'cl-lib)
(require 'json)

;;; Customization

(defgroup etherpad nil
  "Collaborative editing via Etherpad."
  :group 'external)

(defcustom etherpad-default-server "https://example.org"
  "Default Etherpad server URL."
  :type 'string :group 'etherpad)

(defcustom etherpad-author-name nil
  "Display name to use when editing.
Set to nil to prompt at connect time."
  :type '(choice (string :tag "Name")
                 (const :tag "Prompt each time" nil))
  :group 'etherpad)

(defcustom etherpad-debug nil
  "When non-nil, log protocol and changeset details to *Messages*."
  :type 'boolean :group 'etherpad)

(defcustom etherpad-debug-cs nil
  "When non-nil, log changeset strings sent to the server."
  :type 'boolean :group 'etherpad)

(defcustom etherpad-author-colors t
  "When non-nil, identify author by colour."
  :type 'boolean :group 'etherpad)

(defcustom etherpad-reconnect-delay 2
  "Time (seconds) to wait before reconnecting after a disconnect.
Repeated failures use exponential backoff up to `etherpad-max-reconnect-delay'.
Set to nil to disable auto-reconnect."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'etherpad)

(defcustom etherpad-max-reconnect-delay 60
  "Maximum delay (seconds) between auto-reconnect attempts."
  :type 'number :group 'etherpad)

(defcustom etherpad-connect-timeout 20
  "Time (seconds) to wait for CLIENT_VARS after opening a connection.
If the handshake stalls, the connection will be closed and retried.
Set to nil to disable."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'etherpad)

(defcustom etherpad-ack-timeout 10
  "Time (seconds) to wait for ACCEPT_COMMIT after sending a changeset.
If no acknowledgement arrives, the connection is closed and the buffer
resynced. Set to nil to disable."
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Seconds"))
  :group 'etherpad)

(defcustom etherpad-idle-batch-ms 100
  "Idle time (milliseconds) before sending a changeset."
  :type 'number :group 'etherpad)

(defcustom etherpad-max-batch-ms 2000
  "Maximum wait time (milliseconds) before sending a queued changeset."
  :type 'number :group 'etherpad)

(defcustom etherpad-fixed-commit-window nil
  "Batch outbound changesets with a fixed cadence.
This is a workaround for an upstream Etherpad bug triggered by a series
of rapid NEW_CHANGES. See https://github.com/ether/etherpad/pull/7773"
  :type 'boolean :group 'etherpad)

(defcustom etherpad-commit-window-time 1800
  "Fixed outbound commit window time (milliseconds)."
  :type 'number :group 'etherpad)

;; TODO: prefer theme colours
(defcustom etherpad-user-color-palette
  '("goldenrod" "DodgerBlue" "chocolate" "forest green" "magenta"
    "dark orange" "purple" "dark cyan" "tomato" "MediumVioletRed"
    "RoyalBlue" "DarkGoldenrod" "SeaGreen" "DeepPink" "SandyBrown"
    "MediumSlateBlue" "Coral" "DarkKhaki" "CadetBlue" "IndianRed")
  "Color palette for author labels (X11 names)."
  :type '(repeat string) :group 'etherpad)


;;; Buffer-local state

(defvar-local etherpad--ws nil
  "WebSocket connection to the Etherpad server.")

(defvar-local etherpad--author-id nil
  "Assigned author ID.")

(defvar-local etherpad--author-name nil
  "Display name.")

(defvar-local etherpad--pad-id nil
  "Pad ID.")

(defvar-local etherpad--server-url nil
  "Server base URL.")

(defvar-local etherpad--rev nil
  "Current known revision number.")

(defvar-local etherpad--base-text nil
  "Text of the last acknowledged revision (used to build changesets).")

(defvar-local etherpad--apool nil
  "Attribute pool from the server (numToAttrib alist).")

(defvar-local etherpad--author-attrib-num nil
  "Attribute number of local author ID in the pool.")

(defvar-local etherpad--token nil
  "Client token for CLIENT_READY (t.<random>).")

(defvar-local etherpad--awaiting-ack nil
  "Flag for ACCEPT_COMMIT ack (Non-nil while waiting).")

(defvar-local etherpad--inflight-changeset nil
  "Changeset sent and awaiting ACCEPT_COMMIT.
NOTE: The changeset can still be transformed against interleaved remote changes,
and ACCEPT_COMMIT can change `etherpad--base-text' to the latest server text.")

(defvar-local etherpad--resync-preserve nil
  "When non-nil, the next CLIENT_VARS keeps local buffer text.")

(defvar-local etherpad--untagged-local-edits nil
  "Non-nil when local edits were made before an author ID was known.")

(defvar-local etherpad--users nil
  "Alist of (author-id . display-name).")

(defvar-local etherpad--user-colors nil
  "Alist of (author-id . color-name).")

(defvar-local etherpad--user-color-index 0
  "Index into `etherpad-user-color-palette' for the next new user.")

(defvar-local etherpad--color-palette nil
  "Server color palette from CLIENT_VARS (vector of hex strings).")

(defvar-local etherpad--server-colors nil
  "Alist of (author-id . hex-color) assigned by the server.")

(defvar-local etherpad--connecting nil
  "Non-nil during the initial connection handshake.")


;;; Times, timing and timers

;; The abundance of timers and locals may be useful for debugging
;; and/or working around connection and/or synchronisation issues.
;;
;; see: https://github.com/ether/etherpad/pull/7773

(defvar-local etherpad--send-timer nil
  "Idle timer for sending pending changesets.")

(defvar-local etherpad--max-batch-timer nil
  "Timer to batch changes during continuous typing.")

(defvar-local etherpad--commit-window-timer nil
  "Timer for the fixed-cadence outbound commit window.")

(defvar-local etherpad--commit-window-opened-at nil
  "Time when the current fixed commit window opened.")

(defvar-local etherpad--reject-retry-count 0
  "Exponential backoff counter for rejected USER_CHANGES.")

(defvar-local etherpad--stuck-reconnect-timer nil
  "Timer for a scheduled reconnect if changes are behind the server.")

(defvar-local etherpad--reconnect-timer nil
  "Timer for the next automatic reconnect attempt.")

(defvar-local etherpad--reconnect-attempts 0
  "Number of consecutive failed reconnect attempts.")

(defvar-local etherpad--ack-timer nil
  "Timer that runs when an ACCEPT_COMMIT does not arrive in time.")

(defvar-local etherpad--connect-timer nil
  "Timer that runs when a CLIENT_VARS handshake stalls.")

(defun etherpad--clear-timer (timer-var)
  "Cancel and clear timer stored in TIMER-VAR."
  (when (timerp (symbol-value timer-var))
    (cancel-timer (symbol-value timer-var)))
  (set timer-var nil))

(defun etherpad--run-buffer-timer (timer-var delay fn &optional idle)
  "Call FN after DELAY seconds, optionally with an IDLE timer.
The timer is stored in TIMER-VAR and cleared before FN runs.
FN is skipped if the buffer is closed before timer elapses."
  (etherpad--clear-timer timer-var)
  (let* ((buf (current-buffer))
         (callback
          (lambda ()
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (set timer-var nil)
                (funcall fn))))))
    (set timer-var
         (if idle
             (run-with-idle-timer delay nil callback)
           (run-with-timer delay nil callback)))))

(defun etherpad--start-ack-timer ()
  "Start an ACCEPT_COMMIT timeout for in-flight changeset."
  (etherpad--cancel-ack-timer)
  (when (and etherpad-ack-timeout
             (> etherpad-ack-timeout 0))
    (etherpad--run-buffer-timer
     'etherpad--ack-timer etherpad-ack-timeout
     (lambda ()
       (when (and etherpad--awaiting-ack (etherpad--live-p))
         (message "etherpad: no ACK after %ss; resyncing"
                  etherpad-ack-timeout)
         (etherpad--clear-inflight)
         (setq etherpad--resync-preserve t)
         (etherpad--close-socket etherpad--ws))))))

(defun etherpad--start-connect-timer ()
  "Start the CLIENT_VARS handshake timeout."
  (when (and etherpad-connect-timeout
             (> etherpad-connect-timeout 0))
    (etherpad--run-buffer-timer
     'etherpad--connect-timer etherpad-connect-timeout
     (lambda ()
       (when (and etherpad--ws etherpad--connecting)
         (message "etherpad: handshake stalled for %ss; reconnecting"
                  etherpad-connect-timeout)
         (setq etherpad--connecting nil
               etherpad--resync-preserve t)
         (etherpad--stop-connecting-animation)
         (etherpad--close-socket etherpad--ws))))))

(defun etherpad--cancel-ack-timer ()
  "Cancel the pending ACCEPT_COMMIT timeout."
  (etherpad--clear-timer 'etherpad--ack-timer))

(defun etherpad--cancel-connect-timer ()
  "Cancel the pending CLIENT_VARS handshake timeout."
  (etherpad--clear-timer 'etherpad--connect-timer))

(defun etherpad--cancel-reconnect-timer ()
  "Cancel a scheduled automatic reconnect."
  (etherpad--clear-timer 'etherpad--reconnect-timer))

(defun etherpad--cancel-timers ()
  "Cancel all timers, clear any state."
  (mapc #'etherpad--clear-timer
        '(etherpad--send-timer
          etherpad--max-batch-timer
          etherpad--commit-window-timer
          etherpad--stuck-reconnect-timer
          etherpad--reconnect-timer
          etherpad--ack-timer
          etherpad--connect-timer))
  (setq etherpad--commit-window-opened-at nil)
  (etherpad--stop-connecting-animation))

;;; Connection status

(defvar-local etherpad--user-disconnected nil
  "Non-nil if disconnect was user initiated (no auto-reconnect).")

(defvar-local etherpad--connecting-animation-timer nil
  "Timer for the connecting animation.")

(defvar-local etherpad--connecting-frame 0
  "Current frame of the connecting animation.")

(defconst etherpad--connecting-spinner
  ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Connecting animation frames.")

(defun etherpad--live-p ()
  "True when a WebSocket connection is open."
  (and etherpad--ws (websocket-openp etherpad--ws)))

(defun etherpad--connection-status ()
  "Return a short status string for the current pad."
  (cond
   (etherpad--connecting "connecting")
   ((etherpad--live-p) "connected")
   ((and etherpad--server-url etherpad--pad-id) "disconnected")
   (t "not connected")))

(defun etherpad--connection-status-label ()
  "Return the connection status formatted for the header or mode line."
  (format "[%s]" (etherpad--connection-status)))

(defun etherpad--set-status-header-line (&optional suffix)
  "Set the header line to the pad status, followed by SUFFIX."
  (setq-local header-line-format
              (format " %s  %s%s"
                      (or etherpad--pad-id "?")
                      (etherpad--connection-status-label)
                      (or suffix ""))))

(defun etherpad--close-socket (ws)
  "Close WS, ignoring errors."
  (condition-case nil
      (websocket-close ws)
    (error nil)))

(defun etherpad--in-buffer-later (buf delay fn)
  "Call FN with BUF current after DELAY seconds, if BUF is still live."
  (run-with-timer delay nil
                  (lambda ()
                    (when (buffer-live-p buf)
                      (with-current-buffer buf (funcall fn))))))

(defun etherpad--clear-inflight ()
  "Clear the changeset currently awaiting acknowledgement."
  (setq etherpad--awaiting-ack nil
        etherpad--inflight-changeset nil))

;;; Utils

(defun etherpad--base36-digit-p (c)
  "True if C is a base-36 digit (0-9 or a-z)."
  (or (<= ?0 c ?9) (<= ?a c ?z)))

(defun etherpad--from-base36 (str)
  "Convert lowercase base-36 string STR to an integer."
  (cl-loop with acc = 0
           for c across str
           do (setq acc (+ (* acc 36)
                           (cond ((<= ?0 c ?9) (- c ?0))
                                 ((<= ?a c ?z) (+ 10 (- c ?a)))
                                 (t 0))))
           finally return acc))

(defun etherpad--to-base36 (n)
  "Convert non-negative integer N to a lowercase base-36 string."
  (if (<= n 0)
      "0"
    (cl-loop with digits = "0123456789abcdefghijklmnopqrstuvwxyz"
             for m = n then (/ m 36)
             while (> m 0)
             collect (aref digits (mod m 36)) into acc
             finally return (apply #'string (nreverse acc)))))

(defun etherpad--ensure-trailing-newline (s)
  "Append a newline to string S if it doesn't already end with one."
  (cond ((string-empty-p s) "\n")
        ((string-suffix-p "\n" s) s)
        (t (concat s "\n"))))

(defun etherpad--count-newlines (s &optional start end)
  "Count newline characters in string S, optionally within START..END."
  (cl-count ?\n s :start (or start 0) :end end))


;;; URL parsing

(defun etherpad--parse-pad-url (url)
  "Parse URL into a (SERVER-BASE . PAD-ID) pair."
  (let* ((parsed (url-generic-parse-url url))
         (scheme (url-type parsed))
         (host (url-host parsed))
         (port (url-port parsed))
         (path (url-filename parsed))
         (clean-path (if (string-match "[?#]" path)
                         (substring path 0 (match-beginning 0))
                       path)))
    (unless (member scheme '("http" "https"))
      (error "URL must be http or https"))
    (unless (string-prefix-p "/p/" clean-path)
      (error "URL path must be /p/<pad-id>"))
    (let ((pad-id (car (split-string (substring clean-path 3) "/+" t))))
      (unless (and pad-id (not (string-empty-p pad-id)))
        (error "URL path must be /p/<pad-id>"))
      (cons (if (= port (if (equal scheme "https") 443 80))
                (format "%s://%s" scheme host)
              (format "%s://%s:%d" scheme host port))
            pad-id))))


;;; Connection animation

(defun etherpad--start-connecting-animation ()
  "Start a connecting animation in the header line."
  (etherpad--stop-connecting-animation)
  (let ((buf (current-buffer)))
    (setq etherpad--connecting-animation-timer
          (run-with-timer
           0.0 0.12
           (lambda ()
             (when (and (buffer-live-p buf)
                        (buffer-local-value 'etherpad--connecting buf))
               (with-current-buffer buf
                 (setq-local
                  header-line-format
                  (propertize
                   (format " %s  %s %s"
                           (or etherpad--pad-id "?")
                           (etherpad--connection-status-label)
                           (aref etherpad--connecting-spinner
                                 (mod etherpad--connecting-frame
                                      (length etherpad--connecting-spinner))))
                   'face 'header-line-highlight))
                 (cl-incf etherpad--connecting-frame)
                 (force-mode-line-update))))))))

(defun etherpad--stop-connecting-animation ()
  "Stop the connecting animation and reset frame counter."
  (when etherpad--connecting-animation-timer
    (cancel-timer etherpad--connecting-animation-timer)
    (setq etherpad--connecting-animation-timer nil))
  (setq etherpad--connecting-frame 0))


;;; Wire protocol

(defun etherpad--ws-url (base)
  "Build the Socket.IO WebSocket URL from BASE."
  (concat (if (string-prefix-p "https://" base)
              (concat "wss://" (substring base 8))
            (concat "ws://" (substring base 7)))
          "/socket.io/?EIO=4&transport=websocket"))

(defun etherpad--send-eio-text (text)
  "Send raw TEXT via WebSocket.
Signal an error if the connection is not open (caller can resync)."
  (unless (etherpad--live-p)
    (error "etherpad: WebSocket is not connected"))
  (websocket-send-text etherpad--ws text))

(defun etherpad--send-sio (event payload)
  "Send a Socket.IO EVENT with PAYLOAD."
  (let ((json-array-type 'vector))
    (etherpad--send-eio-text
     (concat "42" (json-encode (vector event payload))))))

(defun etherpad--schedule-reconnect ()
  "Schedule an automatic reconnect with exponential backoff.
Does nothing if auto-reconnect is disabled, or a reconnect is pending."
  (unless (or (not etherpad-reconnect-delay)
              etherpad--user-disconnected
              (not etherpad--server-url)
              (not etherpad--pad-id)
              (not (buffer-live-p (current-buffer)))
              (timerp etherpad--reconnect-timer))
    (let* ((attempts etherpad--reconnect-attempts)
           (delay (min (float etherpad-max-reconnect-delay)
                       (* (float etherpad-reconnect-delay)
                          (expt 2 attempts))))
           (url (format "%s/p/%s" etherpad--server-url etherpad--pad-id))
           (name etherpad--author-name))
      (setq etherpad--reconnect-attempts (1+ attempts))
      (etherpad--set-status-header-line
       (format "  reconnecting in %.1fs" delay))
      (message "etherpad: reconnecting in %.1fs (attempt %d)..."
               delay (1+ attempts))
      (etherpad--run-buffer-timer
       'etherpad--reconnect-timer delay
       (lambda ()
         (unless etherpad--user-disconnected
           (setq etherpad--resync-preserve t)
           (let ((etherpad-author-name (or name etherpad-author-name)))
             (etherpad-connect url))))))))

(defun etherpad--close-and-resync ()
  "Close the current WebSocket, preserving local text for resync."
  (setq etherpad--resync-preserve t)
  (if (etherpad--live-p)
      (etherpad--close-socket etherpad--ws)
    (setq etherpad--connecting nil)
    (etherpad--stop-connecting-animation)
    (etherpad--cancel-connect-timer)
    (etherpad--cancel-ack-timer)
    (etherpad--clear-inflight)
    (etherpad--schedule-reconnect)
    (unless (timerp etherpad--reconnect-timer)
      (etherpad--set-status-header-line))))

(defun etherpad--on-open (ws)
  "Handle opening WebSocket WS (wait for OPEN frame)."
  (when (eq ws etherpad--ws)
    (message "etherpad: WebSocket connected")))

(defun etherpad--on-error (ws kind err)
  "Handle a KIND of error ERR reported on connection WS."
  (message "etherpad: WebSocket %s error: %S" kind err)
  (when (eq ws etherpad--ws)
    (setq etherpad--resync-preserve t)
    (etherpad--close-socket ws)))

(defun etherpad--on-close (ws)
  "Handle closing WebSocket WS, reconnect with backoff."
  (when (eq ws etherpad--ws)
    (message "etherpad: WebSocket disconnected")
    (etherpad--cancel-timers)
    (setq etherpad--ws nil
          etherpad--connecting nil)
    (etherpad--clear-inflight)
    (cond (etherpad--user-disconnected
           (setq etherpad--user-disconnected nil)
           (etherpad--set-status-header-line))
          (t
           (setq etherpad--resync-preserve t)
           (etherpad--schedule-reconnect)
           (unless (timerp etherpad--reconnect-timer)
             (etherpad--set-status-header-line))))))

(defun etherpad--handle-eio-packet (packet)
  "Dispatch a complete Engine.IO PACKET."
  (when (and (stringp packet) (> (length packet) 0))
    (pcase (aref packet 0)
      (?0 (condition-case err
              (etherpad--on-eio-open
               (json-read-from-string (substring packet 1)))
            (error (message "etherpad: bad OPEN frame: %S" err)
                   (etherpad--close-and-resync))))
      (?1 (message "etherpad: server requested disconnect")
          (etherpad--close-and-resync))
      (?2 (condition-case err
              (etherpad--send-eio-text "3")
            (error (message "etherpad: pong failed: %S" err)
                   (etherpad--close-and-resync))))
      (?3 nil)
      (?4
       (let ((rest (substring packet 1)))
         (when (> (length rest) 0)
           (pcase (aref rest 0)
             (?0
              (message "etherpad: SIO CONNECT ack")
              (etherpad--in-buffer-later
               (current-buffer) 0.2
               #'etherpad--send-client-ready))
             (?1
              (message "etherpad: SIO disconnect")
              (etherpad--close-and-resync))
             (?2
              (condition-case err
                  (etherpad--handle-sio-frame (substring rest 1))
                (error (message "etherpad: SIO event error: %S" err)
                       (etherpad--resync-preserving-buffer))))
             (?3 nil)
             (?4
              (message "etherpad: SIO connect error: %s"
                       (substring rest 1))
              (etherpad--close-and-resync))
             (c (message "etherpad: unknown SIO type: %c" c))))))
      (c (message "etherpad: unknown EIO packet: %c" c)))))

(defun etherpad--on-message (ws frame)
  "Process incoming WebSocket FRAME on connection WS."
  (when (eq ws etherpad--ws)
    (condition-case err
        (let ((text (websocket-frame-text frame)))
          (when (stringp text)
            (dolist (packet (split-string text "\x1e"))
              (unless (string-empty-p packet)
                (etherpad--handle-eio-packet packet)))))
      (error
       (message "etherpad: protocol handler error: %S" err)
       (etherpad--resync-preserving-buffer)))))

(defun etherpad--on-eio-open (_data)
  "Handle the Engine.IO OPEN frame (open a Socket.IO connection)."
  (message "etherpad: Engine.IO open")
  (etherpad--send-eio-text "40"))

(defun etherpad--handle-sio-frame (json-str)
  "Handle a Socket.IO EVENT frame JSON-STR in the form [event, payload]."
  (let ((arr (condition-case err
                 (let ((json-array-type 'vector)
                       (json-object-type 'alist)
                       (json-key-type 'symbol))
                   (json-read-from-string json-str))
               (error (message "etherpad: SIO frame parse error: %S" err)
                      nil))))
    (cond ((not (vectorp arr))
           (when arr (message "etherpad: ignoring malformed SIO frame")))
          ((< (length arr) 2)
           (message "etherpad: ignoring short SIO frame"))
          ((equal (aref arr 0) "message")
           (etherpad--handle-message (aref arr 1)))
          (t
           (message "etherpad: unhandled event: %s" (aref arr 0))))))

(defun etherpad--handle-message (msg)
  "Handle an Etherpad protocol message MSG (JSON alist)."
  (if (not (listp msg))
      (message "etherpad: ignoring malformed protocol message: %S" msg)
    (pcase (alist-get 'type msg)
      ("CLIENT_VARS" (etherpad--handle-client-vars
                      (alist-get 'data msg)))
      ("COLLABROOM" (etherpad--handle-collabroom
                     (alist-get 'data msg)))
      (_ (cond ((alist-get 'disconnect msg)
                (message "etherpad: server disconnect: %s"
                         (alist-get 'disconnect msg))
                (setq etherpad--user-disconnected t)
                (etherpad--close-and-resync))
               ((alist-get 'accessStatus msg)
                (message "etherpad: access denied: %s"
                         (alist-get 'accessStatus msg))
                (setq etherpad--user-disconnected t
                      etherpad--token nil)
                (etherpad--close-and-resync))
               ((message "etherpad: unknown message type: %s"
                         (alist-get 'type msg))))))))


;;; Etherpad (EasySync) protocol

;; CLIENT_READY

(defun etherpad--send-client-ready ()
  "Send CLIENT_READY handshake."
  (when (etherpad--live-p)
    (setq etherpad--connecting t)
    (unless etherpad--token
      (setq etherpad--token
            (concat "t."
                    (md5 (format "%s%s%s"
                                 (emacs-pid) (time-to-seconds) (random))))))
    (etherpad--start-connecting-animation)
    (etherpad--send-sio
     "message"
     `((component . "pad")
       (type . "CLIENT_READY")
       (padId . ,etherpad--pad-id)
       (sessionID . nil)
       (token . ,etherpad--token)
       (protocolVersion . 2)
       ;; Current version gets name from `userInfo', previous versions
       ;; used `userName'. Send both for compatibility (?)
       (userInfo . ((name . ,(or etherpad--author-name ""))))
       (userName . ,(or etherpad--author-name ""))))
    (message "etherpad: CLIENT_READY sent")))

;; CLIENT_VARS

(defun etherpad--handle-client-vars (data)
  "Handle CLIENT_VARS DATA and use server snapshot as base."
  (when etherpad-debug
    (message "etherpad-debug: CLIENT_VARS userId=%S keys=%S"
             (alist-get 'userId data)
             (and (listp data) (mapcar #'car data))))
  (etherpad--cancel-timers)
  (condition-case err
      (let* ((collab (alist-get 'collab_client_vars data))
             (attributed (alist-get 'initialAttributedText collab))
             (initial-raw (or (and (listp attributed)
                                   (alist-get 'text attributed))
                              (alist-get 'initialText collab)))
             (attribs (and (listp attributed)
                           (alist-get 'attribs attributed)))
             (user-id (alist-get 'userId data))
             (server-name (alist-get 'userName data))
             (rev (alist-get 'rev collab))
             (preserve etherpad--resync-preserve)
             (old-base (and preserve etherpad--base-text)))
        (unless (and (listp collab)
                     (stringp initial-raw)
                     (stringp user-id))
          (error "Incomplete CLIENT_VARS"))
        (let ((initial (etherpad--ensure-trailing-newline initial-raw)))
          (when (and (stringp server-name)
                     (not (string-empty-p server-name)))
            (setq etherpad--author-name server-name))
          (setq etherpad--author-id user-id
                etherpad--rev (if (numberp rev) rev 0)
                etherpad--base-text initial
                etherpad--apool (alist-get 'apool collab)
                etherpad--users nil
                etherpad--color-palette (alist-get 'colorPalette data)
                etherpad--server-colors
                (etherpad--historical-author-colors data collab)
                etherpad--connecting nil
                etherpad--awaiting-ack nil
                etherpad--inflight-changeset nil
                etherpad--reject-retry-count 0
                etherpad--reconnect-attempts 0
                etherpad--resync-preserve nil)
          (unless preserve
            (setq etherpad--user-colors nil
                  etherpad--user-color-index 0
                  etherpad--untagged-local-edits nil))
          (etherpad--stop-connecting-animation)
          (etherpad--set-local-user-color data user-id)
          (when preserve
            (etherpad--tag-local-diff old-base)
            (etherpad--refresh-author-colors))
          (setq etherpad--author-attrib-num (etherpad--find-author-attrib-num))
          (unless preserve
            (with-silent-modifications
              (erase-buffer)
              (insert initial)
              (goto-char (point-min)))
            (when (and (stringp attribs)
                       (> (length attribs) 0))
              (etherpad--apply-initial-attribs attribs))
            (set-buffer-modified-p nil))
          (etherpad--update-header-line)
          (message "etherpad: connected as %s rev=%d (%d chars) attrib=%s"
                   etherpad--author-id etherpad--rev (length initial)
                   (or etherpad--author-attrib-num "nil"))
          (etherpad--try-send-pending)))
    (error
     (message "etherpad: CLIENT_VARS error: %S; resyncing" err)
     (setq etherpad--connecting nil)
     (etherpad--close-and-resync))))

(defun etherpad--find-author-attrib-num ()
  "Return the pool number for local author, or nil if absent."
  (when etherpad--apool
    (cl-loop for (key . val) in (alist-get 'numToAttrib etherpad--apool)
             when (and (vectorp val) (= (length val) 2)
                       (equal (aref val 0) "author")
                       (equal (aref val 1) etherpad--author-id))
             return (etherpad--from-base36 (symbol-name key)))))

(defun etherpad--max-attrib-num ()
  "Return highest attribute number present in the pool, or 0."
  (or (cl-loop for (key . _) in
               (and etherpad--apool (alist-get 'numToAttrib etherpad--apool))
               maximize (etherpad--from-base36 (symbol-name key)))
      0))

(defun etherpad--apool-attrib-num-used-p (num)
  "Return non-nil if attribute number NUM is present in `etherpad--apool'."
  (and etherpad--apool
       (alist-get (intern (etherpad--to-base36 num))
                  (alist-get 'numToAttrib etherpad--apool))))

(defun etherpad--resolve-attrib-num ()
  "Return current pool number of local author (or allocate one).
Should never reuse a cached number that the server pool has already assigned
to a different attribute."
  (or (etherpad--find-author-attrib-num)
      (and (integerp etherpad--author-attrib-num)
           (not (etherpad--apool-attrib-num-used-p etherpad--author-attrib-num))
           etherpad--author-attrib-num)
      (setq etherpad--author-attrib-num
            (let ((num (max 1 (or (and etherpad--apool
                                       (alist-get 'nextNum etherpad--apool))
                                  (1+ (etherpad--max-attrib-num))))))
              (while (etherpad--apool-attrib-num-used-p num)
                (cl-incf num))
              num))))

(defun etherpad--apool-with-author ()
  "Return the attribute pool (and ensure local author is present)."
  (if (etherpad--find-author-attrib-num)
      etherpad--apool
    (let* ((num (etherpad--resolve-attrib-num))
           (key (intern (etherpad--to-base36 num)))
           (entries (and etherpad--apool
                         (alist-get 'numToAttrib etherpad--apool))))
      `((numToAttrib
         . ,(cons (cons key (vector "author" (or etherpad--author-id "")))
                  (cl-remove key entries :key #'car)))
        (nextNum . ,(max (1+ num)
                         (or (and etherpad--apool
                                  (alist-get 'nextNum etherpad--apool))
                             1)))))))

;; COLLABROOM

(defun etherpad--handle-collabroom (data)
  "Dispatch a COLLABROOM DATA message by type."
  (if (not (listp data))
      (message "etherpad: ignoring malformed COLLABROOM data")
    (pcase (alist-get 'type data)
      ("NEW_CHANGES" (etherpad--handle-new-changes data))
      ("ACCEPT_COMMIT" (etherpad--handle-accept-commit data))
      ("REJECT_CHANGES" (etherpad--handle-reject-changes data))
      ("USER_NEWINFO" (etherpad--handle-user-newinfo data))
      ("USER_LEAVE" (etherpad--handle-user-leave data)))))

(defun etherpad--finalize-inflight (new-rev)
  "Advance base text past the in-flight change NEW-REV.
Return non-nil if the local base was advanced, or nil for an invalid state."
  (etherpad--cancel-ack-timer)
  (when (and (numberp new-rev) (> new-rev (or etherpad--rev 0)))
    (setq etherpad--rev new-rev))
  (let ((ok t))
    (when etherpad--inflight-changeset
      (let ((base (etherpad--apply-changeset-to-string
                   etherpad--inflight-changeset etherpad--base-text)))
        (if (stringp base)
            (setq etherpad--base-text base)
          (setq ok nil))))
    (etherpad--clear-inflight)
    (setq etherpad--reject-retry-count 0)
    ok))

(defun etherpad--handle-new-changes (data)
  "Merge the incoming changeset in DATA into the local buffer.
Remote changes are transformed against any unacknowledged local edits."
  (condition-case err
      (let* ((wire (alist-get 'changeset data))
             (cs (etherpad--parse-changeset wire))
             (new-rev (alist-get 'newRev data))
             (author (alist-get 'author data)))
        (let ((pool (alist-get 'apool data)))
          (when (and (listp pool) (alist-get 'numToAttrib pool))
            (setq etherpad--apool pool)))
        (cond
         ((and wire (not cs))
          (message "etherpad: malformed NEW_CHANGES changeset; resyncing")
          (etherpad--resync-preserving-buffer))
         (cs
          (cond
           ((and (numberp new-rev) (<= new-rev (or etherpad--rev 0)))
            (when etherpad-debug
              (message "etherpad-debug: skip stale NEW_CHANGES new-rev=%d rev=%d"
                       new-rev etherpad--rev)))
           ((and (numberp new-rev)
                 (> new-rev (1+ (or etherpad--rev 0))))
            (message "etherpad: revision gap %s -> %d; resyncing"
                     (or etherpad--rev "?") new-rev)
            (etherpad--resync-preserving-buffer))
           ((and (equal author etherpad--author-id)
                 etherpad--awaiting-ack)
            (if (and (numberp new-rev)
                     (= new-rev (1+ (or etherpad--rev 0))))
                (if (etherpad--finalize-inflight new-rev)
                    (etherpad--try-send-pending)
                  (message "etherpad: in-flight finalize failed; resyncing")
                  (etherpad--resync-preserving-buffer))
              (message "etherpad: unexpected own-change revision; resyncing")
              (etherpad--resync-preserving-buffer)))
           ((progn
              (when (and (stringp author)
                         (not (equal author etherpad--author-id)))
                (etherpad--ensure-user-color author))
              (etherpad--apply-remote-change cs new-rev))
            (message "etherpad: applied %s (rev %d)"
                     (etherpad--find-author-name author) (or etherpad--rev 0))
            (etherpad--try-send-pending))
           (t
            (message
             "etherpad: NEW_CHANGES out of sync (old-len=%S base-len=%d). resyncing"
             (alist-get 'old-len cs) (length (or etherpad--base-text "")))
            (etherpad--resync-preserving-buffer))))
         (t
          (when etherpad-debug
            (message "etherpad-debug: NEW_CHANGES without changeset"))))
        (etherpad--update-header-line))
    (error
     (message "etherpad: NEW_CHANGES error: %S; resyncing" err)
     (etherpad--resync-preserving-buffer))))

(defun etherpad--resync-preserving-buffer ()
  "Reconnect, use server snapshot as base while keeping local text.
Any unacknowledged local edits are kept and re-sent using the new base."
  (unless (or etherpad--user-disconnected
              (timerp etherpad--stuck-reconnect-timer))
    (etherpad--cancel-ack-timer)
    (etherpad--cancel-connect-timer)
    (setq etherpad--resync-preserve t)
    (etherpad--run-buffer-timer
     'etherpad--stuck-reconnect-timer 1.0
     (lambda ()
       (unless etherpad--user-disconnected
         (setq etherpad--resync-preserve t)
         (etherpad--reconnect))))))

;; ACCEPT_COMMIT

(defun etherpad--handle-accept-commit (data)
  "Handle ACCEPT_COMMIT DATA and flush any pending changes."
  (let ((new-rev (alist-get 'newRev data))
        (rev (or etherpad--rev 0)))
    (cond
     ((not (numberp new-rev))
      (if etherpad--awaiting-ack
          (progn
            (message "etherpad: ACCEPT_COMMIT without newRev; resyncing")
            (etherpad--cancel-ack-timer)
            (etherpad--clear-inflight)
            (etherpad--resync-preserving-buffer))
        (when etherpad-debug
          (message "etherpad-debug: ignoring stale ACCEPT_COMMIT"))))
     ((<= new-rev rev)
      (when etherpad-debug
        (message "etherpad-debug: ignoring duplicate ACCEPT_COMMIT rev=%d (current %d)"
                 new-rev rev)))
     ((= new-rev (1+ rev))
      (if (or etherpad--awaiting-ack etherpad--inflight-changeset)
          (if (etherpad--finalize-inflight new-rev)
              (progn
                (etherpad--update-header-line)
                (etherpad--try-send-pending))
            (message "etherpad: unusable ACCEPT_COMMIT; resyncing")
            (etherpad--resync-preserving-buffer))
        (message "etherpad: ACCEPT_COMMIT for unknown change; resyncing")
        (etherpad--resync-preserving-buffer)))
     (t
      (message "etherpad: ACCEPT_COMMIT revision gap %d -> %d; resyncing"
               rev new-rev)
      (etherpad--cancel-ack-timer)
      (etherpad--clear-inflight)
      (etherpad--resync-preserving-buffer)))))

;; REJECT_CHANGES

(defun etherpad--handle-reject-changes (data)
  "Handle REJECT_CHANGES DATA for local USER_CHANGES."
  (etherpad--cancel-ack-timer)
  (if (not (or etherpad--awaiting-ack
               etherpad--inflight-changeset))
      (when etherpad-debug
        (message "etherpad-debug: ignoring stale REJECT_CHANGES"))
    (let ((count etherpad--reject-retry-count))
      (message "etherpad: USER_CHANGES rejected: %s (retry #%d)"
               (or (alist-get 'reason data) "unknown") (1+ count))
      (etherpad--clear-inflight)
      (if (>= count 5)
          (progn
            (setq etherpad--reject-retry-count 0)
            (etherpad--resync-preserving-buffer))
        (setq etherpad--reject-retry-count (1+ count))
        (etherpad--in-buffer-later (current-buffer)
                                   (min (* (expt 2 count) 0.1) 1.0)
                                   #'etherpad--try-send-pending)))))

(defun etherpad--find-author-name (author-id)
  "Return the display name for AUTHOR-ID, or fall back to a short label."
  (or (alist-get author-id etherpad--users nil nil #'equal)
      (and (stringp author-id)
           (let ((stripped (string-remove-prefix "a." author-id)))
             (concat "user:" (substring stripped 0 (min 8 (length stripped))))))
      author-id
      "unknown"))

(defun etherpad--palette-color (palette index)
  "Return the color at INDEX in PALETTE, or nil.
PALETTE may be a vector (as decoded from JSON) or a list."
  (when (and (integerp index) (>= index 0))
    (cond ((vectorp palette)
           (and (< index (length palette)) (aref palette index)))
          ((listp palette)
           (nth index palette)))))

(defun etherpad--historical-author-colors (data collab)
  "Return (AUTHOR-ID . COLOR) pairs from CLIENT_VARS DATA and COLLAB."
  (let ((palette (alist-get 'colorPalette data))
        (history (alist-get 'historicalAuthorData collab)))
    (when (listp history)
      (cl-loop for (key . info) in history
               for id = (if (symbolp key) (symbol-name key) key)
               for color = (and (listp info)
                                (etherpad--palette-color
                                 palette (alist-get 'colorId info)))
               when (and (stringp id) (stringp color))
               collect (cons id color)))))

(defun etherpad--ensure-user-color (author-id &optional color-id)
  "Return AUTHOR-ID's colour, using server COLOR-ID when available."
  (let ((server-color
         (or (etherpad--palette-color etherpad--color-palette color-id)
             (alist-get author-id etherpad--server-colors nil nil #'equal)))
        (existing (alist-get author-id etherpad--user-colors nil nil #'equal)))
    (cond (server-color
           (unless (equal existing server-color)
             (setq etherpad--user-colors
                   (cons (cons author-id server-color)
                         (assoc-delete-all author-id etherpad--user-colors))))
           server-color)
          (existing existing)
          (t
           (let ((color (nth (mod etherpad--user-color-index
                                  (length etherpad-user-color-palette))
                             etherpad-user-color-palette)))
             (cl-incf etherpad--user-color-index)
             (push (cons author-id color) etherpad--user-colors)
             color)))))

(defun etherpad--set-local-user-color (data user-id)
  "Record the server-assigned colour for USER-ID from CLIENT_VARS DATA."
  (let* ((palette (alist-get 'colorPalette data))
         (color (etherpad--palette-color palette (alist-get 'userColor data))))
    (when (and (stringp user-id) (stringp color))
      (setq etherpad--user-colors
            (cons (cons user-id color)
                  (assoc-delete-all user-id etherpad--user-colors))
            etherpad--server-colors
            (cons (cons user-id color)
                  (assoc-delete-all user-id etherpad--server-colors))))
    (when (stringp user-id)
      (etherpad--ensure-user-color user-id))))

(defun etherpad--local-user ()
  "Return the local user as an (ID . NAME) pair, or nil."
  (when (and (stringp etherpad--author-id)
             (not (string-empty-p etherpad--author-id)))
    (cons etherpad--author-id (or etherpad--author-name "anonymous"))))

(defun etherpad--all-users ()
  "Return local and remote users as an alist of (ID . NAME)."
  (let ((local (etherpad--local-user)))
    (if (and local (not (assoc (car local) etherpad--users)))
        (cons local etherpad--users)
      etherpad--users)))

;; USER_NEWINFO

(defun etherpad--handle-user-newinfo (data)
  "Handle USER_NEWINFO DATA."
  (let ((info (and (listp data) (alist-get 'userInfo data))))
    (when (listp info)
      (when-let* ((id (alist-get 'userId info))
                  ((stringp id)))
        (let ((name (alist-get 'name info))
              (old-color (alist-get id etherpad--user-colors nil nil #'equal)))
          (setq etherpad--users (assoc-delete-all id etherpad--users))
          (push (cons id (if (stringp name) name "anonymous"))
                etherpad--users)
          (let ((color (etherpad--ensure-user-color
                        id (alist-get 'colorId info))))
            (unless (equal old-color color)
              (etherpad--refresh-author-colors))))
        (etherpad--update-header-line)))))

;; USER_LEAVE

(defun etherpad--handle-user-leave (data)
  "Handle USER_LEAVE DATA."
  (let ((info (and (listp data) (alist-get 'userInfo data))))
    (when (listp info)
      (when-let* ((id (alist-get 'userId info))
                  ((stringp id)))
        (setq etherpad--users (assoc-delete-all id etherpad--users))
        (etherpad--update-header-line)))))

;; Header line

(defun etherpad--user-count-label (&optional users)
  "Return a short mode/header line label for USERS or all connected users."
  (let ((count (length (if users users (etherpad--all-users)))))
    (format "[%d user%s]" count (if (= count 1) "" "s"))))

(defun etherpad--update-header-line ()
  "Update `header-line-format' with pad, revision, and user info."
  (let* ((all-users (etherpad--all-users))
         (users (mapconcat
                 (lambda (user)
                   (let ((color (alist-get (car user) etherpad--user-colors
                                           nil nil #'equal)))
                     (propertize (cdr user)
                                 'face (and color (list :foreground color)))))
                 all-users " ")))
    (setq-local header-line-format
                (format " %s  %s %s  %s %s%s"
                        (propertize (or etherpad--pad-id "?")
                                    'face 'header-line-highlight)
                        (propertize (etherpad--connection-status-label)
                                    'face 'font-lock-comment-face)
                        (propertize (concat "rev="
                                            (number-to-string
                                             (or etherpad--rev 0)))
                                    'face 'font-lock-comment-face)
                        (propertize (etherpad--user-count-label all-users)
                                    'face 'header-line-highlight)
                        (propertize (if all-users "| " "")
                                    'face 'header-line-highlight)
                        users))))


;; Changeset wire format
;;  -> Z:<old-len><sign><delta><ops>$<char-bank>

(defun etherpad--scan-base36 (s pos &optional limit)
  "Scan base-36 digits in S starting at POS.
Don't scan at or beyond an optional LIMIT.
Return (NUM . NEW-POS), where NUM is 0 if no digits found."
  (let* ((s-len (length s))
         (end pos)
         (len (min (or limit s-len) s-len)))
    (while (and (< end len)
                (etherpad--base36-digit-p (aref s end)))
      (setq end (1+ end)))
    (cons (etherpad--from-base36 (substring s pos end)) end)))

(defun etherpad--parse-ops-string (s pos end)
  "Parse ops from S between POS and END, stopping early at `$'.
Ignores truncated attributes/opcodes to avoid reading past END."
  (let ((ops nil))
    (while (and (< pos end) (/= (aref s pos) ?$))
      (let (attribs (lines 0))
        (while (and (< pos end) (= (aref s pos) ?*))
          (let ((scan (etherpad--scan-base36 s (1+ pos) end)))
            (if (<= (cdr scan) (1+ pos))
                (setq pos end)
              (push (car scan) attribs)
              (setq pos (cdr scan)))))
        (when (and (< pos end) (= (aref s pos) ?|))
          (let ((scan (etherpad--scan-base36 s (1+ pos) end)))
            (if (<= (cdr scan) (1+ pos))
                (setq pos end)
              (setq lines (car scan) pos (cdr scan)))))
        (if (>= pos end)
            (setq pos end)
          (let* ((opcode (aref s pos))
                 (kind (pcase opcode
                         (?+ "insert")
                         (?- "delete")
                         (?= "keep")))
                 (scan (etherpad--scan-base36 s (1+ pos) end)))
            (if (and kind (> (cdr scan) (1+ pos)))
                (progn
                  (push (list kind (car scan) lines (nreverse attribs)) ops)
                  (setq pos (cdr scan)))
              (setq pos end))))))
    (nreverse ops)))

(defun etherpad--parse-changeset (wire)
  "Parse changeset WIRE into an alist of old-len, net-delta, ops, char-bank.
Return nil if WIRE is incomplete, lacks the `$' separator, or has an
invalid changeset header."
  (when (and (stringp wire) (string-prefix-p "Z:" wire))
    (condition-case nil
        (let* ((wire-len (length wire))
               (scanned (etherpad--scan-base36 wire 2))
               (old-len (car scanned))
               (pos (cdr scanned)))
          (when (and (> pos 2) (< pos wire-len))
            (let* ((sign (aref wire pos))
                   (scanned (etherpad--scan-base36 wire (1+ pos)))
                   (delta (car scanned))
                   (dollar (string-match-p "\\$" wire (cdr scanned))))
              (when (and dollar (memq sign '(?> ?<)))
                (list (cons 'old-len old-len)
                      (cons 'net-delta (if (eq sign ?>) delta (- delta)))
                      (cons 'ops (etherpad--parse-ops-string
                                  wire (cdr scanned) dollar))
                      (cons 'char-bank (substring wire (1+ dollar))))))))
      (error nil))))

(defun etherpad--serialize-changeset (old-len net-delta ops char-bank)
  "Serialize OLD-LEN, NET-DELTA, OPS, and CHAR-BANK to Etherpad wire format."
  (format "Z:%s%s%s%s$%s"
          (etherpad--to-base36 old-len)
          (if (< net-delta 0) "<" ">")
          (etherpad--to-base36 (abs net-delta))
          (etherpad--serialize-ops ops)
          char-bank))

(defun etherpad--serialize-ops (ops)
  "Serialize OPS to a wire ops string."
  (mapconcat
   (lambda (op)
     (let ((opcode (nth 0 op))
           (count (nth 1 op))
           (lines (nth 2 op))
           (attribs (nth 3 op)))
       (concat
        (mapconcat (lambda (a)
                     (concat "*" (etherpad--to-base36 a)))
                   attribs "")
        (if (> lines 0)
            (concat "|" (etherpad--to-base36 lines))
          "")
        (pcase opcode
          ("insert" "+")
          ("delete" "-")
          ("keep" "=")
          (_ "+"))
        (etherpad--to-base36 count))))
   ops ""))


;;; Apply changesets

(defun etherpad--apply-cs-core (text ops char-bank &optional attrib-resolver)
  "Apply OPS with CHAR-BANK to TEXT, and return the result string.
The optional ATTRIB-RESOLVER function receives an op's attribute indices and
returns a plist (or nil) for inserted text."
  (with-temp-buffer
    (let ((text-len (length text))
          (bank-len (length char-bank))
          (src 0)
          (bank 0))
      (dolist (op ops)
        (let ((opcode (nth 0 op))
              (count (nth 1 op))
              (attribs (nth 3 op)))
          (pcase opcode
            ("keep"
             (let ((end (min (+ src count) text-len)))
               (insert (substring text src end))
               (setq src end)))
            ("delete"
             (cl-incf src count))
            ("insert"
             (let ((end (min (+ bank count) bank-len))
                   (start (point)))
               (insert (substring char-bank bank end))
               (when attrib-resolver
                 (when-let* ((props (funcall attrib-resolver attribs)))
                   (add-text-properties start (point) props)))
               (setq bank end))))))
      (when (< src text-len)
        (insert (substring text src)))
      (buffer-substring (point-min) (point-max)))))

(defun etherpad--resolve-attrib-props (attribs)
  "Resolve pool indices ATTRIBS to text properties.
Always sets `etherpad-author' and sets `face' when colors are enabled."
  (when (and etherpad--apool attribs)
    (let ((nta (alist-get 'numToAttrib etherpad--apool))
          (author-id nil))
      (cl-loop for n in attribs
               for entry = (alist-get (intern (etherpad--to-base36 n)) nta)
               when (and (vectorp entry) (= (length entry) 2)
                         (equal (aref entry 0) "author"))
               do (setq author-id (aref entry 1)))
      (when author-id
        (let ((props (list 'etherpad-author author-id)))
          (when etherpad-author-colors
            (when-let* ((color (alist-get author-id etherpad--user-colors
                                          nil nil #'equal)))
              (setq props (append props
                                  (list 'face (list :foreground color))))))
          props)))))

(defun etherpad--apply-initial-attribs (attribs)
  "Parse ATTRIBS and apply author text properties to the buffer.
ATTRIBS is an ops string from initialAttributedText."
  (condition-case err
      (with-silent-modifications
        (let ((ops (etherpad--parse-ops-string
                    attribs 0 (length attribs))))
          (if (null ops)
              (message "etherpad: attribs parse error: %S" attribs)
            (let ((pos 1))
              (dolist (op ops)
                (pcase (nth 0 op)
                  ("insert"
                   (let ((end (+ pos (nth 1 op))))
                     (when-let* ((resolved (etherpad--resolve-attrib-props
                                            (nth 3 op))))
                       (add-text-properties pos end resolved)
                       (when-let* ((author (plist-get resolved
                                                      'etherpad-author)))
                         (etherpad--ensure-user-color author)))
                     (setq pos end)))
                  ("keep"
                   (cl-incf pos (nth 1 op)))
                  ("delete" nil)))
              (etherpad--refresh-author-colors)))))
    (error (message "etherpad: failed to apply initial attribs: %s"
                    (error-message-string err)))))

(defun etherpad--refresh-author-colors ()
  "Apply or remove author face properties according to `etherpad-author-colors'.
Only text carrying an `etherpad-author' property is touched, so faces
installed by other modes (font-lock, etc.) are left intact."
  (when (buffer-live-p (current-buffer))
    (with-silent-modifications
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((author (get-text-property (point) 'etherpad-author))
                 (next (next-single-property-change (point) 'etherpad-author
                                                    nil (point-max))))
            (when author
              (let ((color (and etherpad-author-colors
                                (alist-get author etherpad--user-colors
                                           nil nil #'equal))))
                (if color
                    (add-text-properties (point) next
                                         (list 'face (list :foreground color)))
                  (remove-text-properties (point) next '(face nil)))))
            (goto-char next)))))))

(defun etherpad--buffer-text ()
  "Return buffer contents without text properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(defun etherpad--buffer-text-normalized ()
  "Return buffer contents, ensuring a final newline."
  (etherpad--ensure-trailing-newline (etherpad--buffer-text)))

(defun etherpad--apply-remote-change (cs rev)
  "Merge remote changeset CS with revision REV into local state.
Return non-nil on success, nil if CS is out of sync (caller should resync)."
  (let* ((cs-old-len (alist-get 'old-len cs))
         (raw-buf-text (etherpad--buffer-text)))
    (if (and (not etherpad--awaiting-ack)
             (null etherpad--inflight-changeset)
             (equal cs-old-len (length raw-buf-text))
             (equal raw-buf-text etherpad--base-text))
        ;; If buffer and base text are identical CS can be applied in place
        (condition-case err
            (progn
              (etherpad--apply-cs-in-buffer
               (alist-get 'ops cs)
               (or (alist-get 'char-bank cs) ""))
              (let ((new-text (etherpad--buffer-text)))
                (unless (string-suffix-p "\n" new-text)
                  (with-silent-modifications
                    (save-excursion
                      (goto-char (point-max))
                      (insert "\n")))
                  (setq new-text (etherpad--buffer-text)))
                (setq etherpad--base-text new-text))
              (when (numberp rev)
                (setq etherpad--rev rev))
              t)
          (error
           (message "etherpad: applying remote change failed: %S" err)
           nil))
      ;; Local edits and/or in-flight changeset that needs merging
      (let* ((submitted (and etherpad--awaiting-ack
                             etherpad--inflight-changeset))
             (submitted-proj (if submitted
                                 (etherpad--apply-changeset-to-string
                                  submitted etherpad--base-text)
                               etherpad--base-text))
             (submitted-proj-len (and submitted-proj
                                      (length submitted-proj)))
             (submitted-valid (consp submitted))
             (base-len (length (or etherpad--base-text "")))
             (buf-text (etherpad--ensure-trailing-newline raw-buf-text))
             (submitted-at-base (and submitted-valid (= cs-old-len base-len)))
             (submitted-at-proj (and submitted-valid submitted-proj-len
                                     (= cs-old-len submitted-proj-len)))
             (user (and (stringp submitted-proj)
                        buf-text
                        (not (equal submitted-proj buf-text))
                        (etherpad--make-changeset submitted-proj buf-text)))
             ;; CS is computed against base-text B or the projection S(B)
             ;; oldLen can be used to check if server already has the changes
             (cs-for-buffer
              (cond
               (submitted-at-base (etherpad--follow submitted cs t))
               (submitted-at-proj cs)
               ((and (not submitted) (= cs-old-len base-len)) cs)))
             (post (if (and cs-for-buffer user)
                       (etherpad--follow user cs-for-buffer nil)
                     cs-for-buffer)))
        (unless (equal raw-buf-text buf-text)
          (with-silent-modifications
            (save-excursion
              (goto-char (point-max))
              (insert "\n"))))
        (if (not post)
            (progn
              (when (and submitted (not submitted-proj))
                (message "etherpad: cannot project in-flight changeset. resyncing"))
              (etherpad--clear-inflight)
              nil)
          (let ((applied
                 (condition-case err
                     (progn
                       (etherpad--apply-cs-in-buffer
                        (alist-get 'ops post)
                        (or (alist-get 'char-bank post) ""))
                       t)
                   (error (message
                           "etherpad: merge of remote change failed: %S" err)
                          nil))))
            (when applied
              (cond
               (submitted-at-proj
                (setq etherpad--base-text
                      (etherpad--apply-changeset-to-string
                       cs submitted-proj))
                (etherpad--cancel-ack-timer)
                (etherpad--clear-inflight))
               (submitted-at-base
                (setq etherpad--base-text
                      (etherpad--apply-changeset-to-string
                       cs etherpad--base-text)
                      etherpad--inflight-changeset
                      (etherpad--follow cs submitted nil)))
               ((not submitted)
                (setq etherpad--base-text
                      (etherpad--apply-changeset-to-string
                       cs etherpad--base-text))))
              (when (numberp rev)
                (setq etherpad--rev rev)))
            applied))))))

(defun etherpad--apply-changeset-to-string (cs text)
  "Apply changeset CS to string TEXT (and ensure trailing newline)."
  (when (and cs (listp cs) (stringp text))
    (let ((old-len (alist-get 'old-len cs)))
      (unless (or (not old-len) (equal old-len (length text)))
        (message
         "etherpad: WARN applying CS to string old-len=%d but text has %d chars"
         old-len (length text))))
    (etherpad--ensure-trailing-newline
     (etherpad--apply-cs-core text (alist-get 'ops cs)
                              (or (alist-get 'char-bank cs) "")))))

(defun etherpad--apply-cs-in-buffer (ops char-bank)
  "Apply OPS and CHAR-BANK to the current buffer in place.
Modifications are silent, any edits use `delete-region'/`insert', so buffer
state should stay consistent."
  (with-silent-modifications
    (save-excursion
      (goto-char (point-min))
      (let ((bank 0)
            (bank-len (length char-bank)))
        (dolist (op ops)
          (pcase (nth 0 op)
            ("keep" (goto-char (min (+ (point) (nth 1 op)) (point-max))))
            ("delete"
             (let ((end (min (+ (point) (nth 1 op)) (point-max))))
               (when (> end (point)) (delete-region (point) end))))
            ("insert"
             (let* ((end (min (+ bank (nth 1 op)) bank-len))
                    (start (point)))
               (insert (substring char-bank bank end))
               (when-let* ((props (etherpad--resolve-attrib-props (nth 3 op))))
                 (add-text-properties start (point) props))
               (setq bank end)))))))))


;;; Operational transform

(defun etherpad--copy-ops (changeset)
  "Return mutable copies of CHANGESET's ops."
  (vconcat (mapcar (lambda (op)
                     (vector (nth 0 op) (nth 1 op) (nth 2 op) (nth 3 op)))
                   (alist-get 'ops changeset))))

(defun etherpad--follow (cs1 cs2 reverse)
  "Transpose changeset CS2 so it can be applied after CS1.

Ref. Etherpad `Changeset.follow (cs1, cs2, reverseInsertOrder, pool)'
CS1 and CS2 are computed against the same source text and the result is
computed against CS1 output.

Concurrent inserts at the same position are ordered the same way as Etherpad,
an insert whose first character is a newline goes after one that is not,
otherwise REVERSE decides (nil keeps CS1 insert first)."
  (let* ((ops1 (etherpad--copy-ops cs1))
         (ops2 (etherpad--copy-ops cs2))
         (bank1 (or (alist-get 'char-bank cs1) ""))
         (bank2 (or (alist-get 'char-bank cs2) ""))
         (bank1-len (length bank1))
         (bank2-len (length bank2))
         (n1 (length ops1))
         (n2 (length ops2))
         (i1 0) (i2 0) (cpos1 0) (cpos2 0)
         (op1 (and (> n1 0) (aref ops1 0)))
         (op2 (and (> n2 0) (aref ops2 0)))
         (out nil) (oldpos 0) (newlen 0) (done nil))
    (cl-labels
        ((advance ()
           (while (and op1 (equal (aref op1 0) ""))
             (cl-incf i1)
             (setq op1 (and (< i1 n1) (aref ops1 i1))))
           (while (and op2 (equal (aref op2 0) ""))
             (cl-incf i2)
             (setq op2 (and (< i2 n2) (aref ops2 i2))))
           (unless op1 (setq op1 (vector "" 0 0 nil)))
           (unless op2 (setq op2 (vector "" 0 0 nil))))
         (emit (code count attribs)
           (when (and code (not (equal code "")) (> count 0))
             (pcase code
               ("keep" (cl-incf oldpos count) (cl-incf newlen count))
               ("delete" (cl-incf oldpos count))
               ("insert" (cl-incf newlen count)))
             (let ((last (car out)))
               (if (and last (equal (aref last 0) code)
                        (equal (aref last 3) attribs))
                   (setf (aref last 1) (+ (aref last 1) count))
                 (push (vector code count 0 attribs) out)))))
         (peek (bank pos len)
           (and (< pos len) (aref bank pos)))
         (consume (op count lines)
           (cl-decf (aref op 1) count)
           (cl-decf (aref op 2) lines)
           (when (= (aref op 1) 0)
             (aset op 0 ""))))
      (while (not done)
        (advance)
        (let ((c1 (aref op1 0))
              (c2 (aref op2 0)))
          (cond ((and (equal c1 "") (equal c2 ""))
                 (setq done t))
                ((or (equal c1 "insert") (equal c2 "insert"))
                 (let ((which
                        (cond ((not (equal c2 "insert")) 1)
                              ((not (equal c1 "insert")) 2)
                              (t
                               (let ((f1 (peek bank1 cpos1 bank1-len))
                                     (f2 (peek bank2 cpos2 bank2-len)))
                                 (cond ((and (eq f1 ?\n) (not (eq f2 ?\n))) 2)
                                       ((and (not (eq f1 ?\n)) (eq f2 ?\n)) 1)
                                       (reverse 2)
                                       (t 1)))))))
                   (if (= which 1)
                       (progn
                         (cl-incf cpos1 (aref op1 1))
                         (emit "keep" (aref op1 1) nil)
                         (aset op1 0 ""))
                     (cl-incf cpos2 (aref op2 1))
                     (emit "insert" (aref op2 1) (aref op2 3))
                     (aset op2 0 ""))))
                ((equal c1 "delete")
                 (cond ((equal c2 "")
                        (aset op1 0 ""))
                       ((<= (aref op1 1) (aref op2 1))
                        (consume op2 (aref op1 1) (aref op1 2))
                        (aset op1 0 ""))
                       (t
                        (consume op1 (aref op2 1) (aref op2 2))
                        (aset op2 0 ""))))
                ((equal c2 "delete")
                 (cond ((equal c1 "")
                        (emit "delete" (aref op2 1) (aref op2 3))
                        (aset op2 0 ""))
                       ((<= (aref op2 1) (aref op1 1))
                        (emit "delete" (aref op2 1) (aref op2 3))
                        (consume op1 (aref op2 1) (aref op2 2))
                        (aset op2 0 ""))
                       (t
                        (emit "delete" (aref op1 1) nil)
                        (consume op2 (aref op1 1) (aref op1 2))
                        (aset op1 0 ""))))
                ((equal c1 "")
                 (emit (aref op2 0) (aref op2 1) (aref op2 3))
                 (aset op2 0 ""))
                ((equal c2 "")
                 (aset op1 0 ""))
                (t
                 (let ((n (min (aref op1 1) (aref op2 1))))
                   (emit "keep" n nil)
                   (if (<= (aref op1 1) (aref op2 1))
                       (progn
                         (consume op2 (aref op1 1) (aref op1 2))
                         (aset op1 0 ""))
                     (consume op1 (aref op2 1) (aref op2 2))
                     (aset op2 0 "")))))))
      (let* ((oldlen-out (+ (alist-get 'old-len cs1)
                            (alist-get 'net-delta cs1)))
             (final-newlen (+ newlen
                              (- oldlen-out oldpos))))
        ;; assemble return alist
        `((old-len . ,oldlen-out)
          (net-delta . ,(- final-newlen oldlen-out))
          (ops . ,(nreverse (mapcar (lambda (v) (append v nil)) out)))
          (char-bank . ,bank2))))))


;;; Changeset generation

(defun etherpad--canonical-multiline (opcode text attribs)
  "Return canonical OPCODE ops covering TEXT with ATTRIBS.
An op that spans newlines must end at a line boundary, if a seqeunce contains
a newline but does not end with one, split at the last newline."
  (let* ((len (length text))
         (last-nl (cl-position ?\n text :from-end t)))
    (if (and last-nl (< (1+ last-nl) len))
        (list (list opcode (1+ last-nl)
                    (etherpad--count-newlines text 0 (1+ last-nl)) attribs)
              (list opcode (- len (1+ last-nl)) 0 attribs))
      (list (list opcode len (etherpad--count-newlines text) attribs)))))

(defun etherpad--make-changeset (old-text new-text &optional author-num)
  "Build a changeset alist transforming OLD-TEXT into NEW-TEXT.
If AUTHOR-NUM is given, insert ops will use it for the author attribute."
  (let* ((old-len (length old-text))
         (new-len (length new-text))
         (limit (min old-len new-len))
         (prefix 0)
         (suffix 0))
    (while (and (< prefix limit)
                (= (aref old-text prefix)
                   (aref new-text prefix)))
      (cl-incf prefix))
    (while (and (< (+ prefix suffix) old-len)
                (< (+ prefix suffix) new-len)
                (= (aref old-text (- old-len suffix 1))
                   (aref new-text (- new-len suffix 1))))
      (cl-incf suffix))
    (let* ((deleted (substring old-text prefix (- old-len suffix)))
           (inserted (substring new-text prefix (- new-len suffix)))
           (del-len (length deleted))
           (ins-len (length inserted))
           (ops nil))
      (when (> prefix 0)
        (dolist (op (etherpad--canonical-multiline
                     "keep" (substring old-text 0 prefix) nil))
          (push op ops)))
      (when (> del-len 0)
        (dolist (op (etherpad--canonical-multiline "delete" deleted nil))
          (push op ops)))
      (when (> ins-len 0)
        (dolist (op (etherpad--canonical-multiline
                     "insert" inserted (and author-num (list author-num))))
          (push op ops)))
      (list (cons 'old-len old-len)
            (cons 'net-delta (- new-len old-len))
            (cons 'ops (nreverse ops))
            (cons 'char-bank inserted)))))

(defun etherpad--compute-pending-changeset ()
  "Return diff from base text to local buffer, or nil if unchanged."
  (let ((new-text (etherpad--buffer-text-normalized)))
    (and etherpad--base-text
         (not (string= etherpad--base-text new-text))
         (etherpad--make-changeset etherpad--base-text new-text
                                   (etherpad--resolve-attrib-num)))))


;;; Local edit tracking

(defun etherpad--tag-local-diff (old-text)
  "Tag buffer text differing from OLD-TEXT with the local author.
If OLD-TEXT is nil, the whole buffer is tagged only if local edits were made
before the author ID was available."
  (when (stringp etherpad--author-id)
    (if (not (stringp old-text))
        (when etherpad--untagged-local-edits
          (etherpad--tag-local-change (point-min) (point-max)))
      (let ((new-text (etherpad--buffer-text))
            (pos 1))
        (unless (equal old-text new-text)
          (dolist (op (alist-get 'ops
                                 (etherpad--make-changeset old-text new-text)))
            (pcase (nth 0 op)
              ("keep" (cl-incf pos (nth 1 op)))
              ("delete" nil)
              ("insert"
               (let ((end (+ pos (nth 1 op))))
                 (etherpad--tag-local-change pos end)
                 (setq pos end))))))))
    (setq etherpad--untagged-local-edits nil)))

(defun etherpad--tag-local-change (beg end)
  "Tag local text between BEG and END with the local author."
  (when (and (stringp etherpad--author-id) (< beg end))
    (let ((color (and etherpad-author-colors
                      (alist-get etherpad--author-id etherpad--user-colors
                                 nil nil #'equal))))
      (with-silent-modifications
        (add-text-properties
         beg end
         (if color
             (list 'etherpad-author etherpad--author-id
                   'face (list :foreground color))
           (list 'etherpad-author etherpad--author-id)))))))

(defun etherpad--after-change (beg end _pre-len)
  "Tag the region from BEG to END and schedule a send after a local edit."
  (unless (stringp etherpad--author-id)
    (setq etherpad--untagged-local-edits t))
  (etherpad--tag-local-change beg end)
  (when (and etherpad-fixed-commit-window
             (not etherpad--connecting)
             (etherpad--live-p))
    (unless etherpad--commit-window-opened-at
      (setq etherpad--commit-window-opened-at (float-time))))
  (unless (or etherpad--connecting (not (etherpad--live-p))
              etherpad--awaiting-ack)
    (etherpad--schedule-send)))


;;; Send scheduling

(defun etherpad--commit-window-due-p ()
  "Return non-nil if the fixed commit window has elapsed."
  (and etherpad--commit-window-opened-at
       (>= (float-time)
           (+ etherpad--commit-window-opened-at
              (/ etherpad-commit-window-time 1000.0)))))

(defun etherpad--schedule-fixed-commit-window ()
  "Schedule next fixed-cadence outbound commit."
  (unless (or etherpad--awaiting-ack
              (timerp etherpad--commit-window-timer))
    (unless etherpad--commit-window-opened-at
      (setq etherpad--commit-window-opened-at (float-time)))
    (let* ((window (/ etherpad-commit-window-time 1000.0))
           (due (+ etherpad--commit-window-opened-at window)))
      (etherpad--run-buffer-timer
       'etherpad--commit-window-timer
       (max 0.0 (- due (float-time)))
       #'etherpad--try-send-pending))))

(defun etherpad--schedule-send ()
  "Schedule a send of any pending changes."
  (if etherpad-fixed-commit-window
      (etherpad--schedule-fixed-commit-window)
    (etherpad--run-buffer-timer
     'etherpad--send-timer (/ etherpad-idle-batch-ms 1000.0)
     #'etherpad--try-send-pending t)
    (unless etherpad--max-batch-timer
      (etherpad--run-buffer-timer
       'etherpad--max-batch-timer (/ etherpad-max-batch-ms 1000.0)
       #'etherpad--try-send-pending))))

(defun etherpad--try-send-pending ()
  "Compute and send any pending changeset unless awaiting an ack.
If `etherpad-fixed-commit-window' is enabled, wait until window has elapsed."
  (etherpad--clear-timer 'etherpad--send-timer)
  (when (and (not etherpad--awaiting-ack) (etherpad--live-p))
    (etherpad--clear-timer 'etherpad--max-batch-timer)
    (let ((cs (etherpad--compute-pending-changeset)))
      (cond ((and cs etherpad-fixed-commit-window
                  (not (etherpad--commit-window-due-p)))
             (etherpad--schedule-fixed-commit-window))
            (cs (setq etherpad--awaiting-ack t
                      etherpad--inflight-changeset cs)
                (condition-case err
                    (progn
                      (etherpad--send-changeset-wire
                       (etherpad--serialize-changeset
                        (alist-get 'old-len cs)
                        (alist-get 'net-delta cs)
                        (alist-get 'ops cs)
                        (or (alist-get 'char-bank cs) "")))
                      (when etherpad-fixed-commit-window
                        (setq etherpad--commit-window-opened-at nil))
                      (when (and etherpad--awaiting-ack
                                 (etherpad--live-p))
                        (etherpad--start-ack-timer)))
                  (error
                   (etherpad--cancel-ack-timer)
                   (etherpad--clear-inflight)
                   (message "etherpad: send error: %S" err)
                   (when (etherpad--live-p)
                     (when etherpad-fixed-commit-window
                       (setq etherpad--commit-window-opened-at nil))
                     (etherpad--schedule-send)))))
            (etherpad-fixed-commit-window
             (setq etherpad--commit-window-opened-at nil))))))

(defun etherpad--send-changeset-wire (wire)
  "Send serialized changeset WIRE as USER_CHANGES."
  (when etherpad-debug-cs
    (message "etherpad-debug-cs: CS=%S rev=%s"
             wire (or etherpad--rev "?")))
  (when etherpad-debug
    (message "etherpad-debug: SEND CS wire=%S baseRev=%s baseLen=%d bufLen=%d"
             wire (or etherpad--rev "?") (length (or etherpad--base-text ""))
             (buffer-size)))
  (etherpad--send-sio
   "message"
   `((component . "pad")
     (type . "COLLABROOM")
     (data . ((type . "USER_CHANGES")
              (baseRev . ,(or etherpad--rev 0))
              (changeset . ,wire)
              (apool . ,(etherpad--apool-with-author)))))))


;;; Connection lifecycle

(defconst etherpad--token-regexp
  "^\\(?:#HttpOnly_\\)?[^\t]*\t[^\t]*\t[^\t]*\t[^\t]*\t[0-9]+\ttoken\t\\([^\t\r\n]+\\)$")

(defun etherpad--fetch-token (server-url pad-id)
  "Return author token for SERVER-URL and PAD-ID, or nil.
Fetches the pad using curl and extracts the `token' cookie."
  (let ((cookie-file (make-temp-file "etherpad-cookies")))
    (unwind-protect
        (let ((status (condition-case nil
                          (call-process
                           "curl" nil nil nil "-s"
                           "--connect-timeout" "3" "--max-time" "5"
                           "-c" cookie-file
                           (format "%s/p/%s"
                                   (string-trim-right server-url "/") pad-id))
                        (error nil))))
          (when (and (integerp status) (zerop status)
                     (file-exists-p cookie-file))
            (with-temp-buffer
              (insert-file-contents cookie-file)
              (goto-char (point-min))
              (when (re-search-forward etherpad--token-regexp nil t)
                (string-trim (match-string 1))))))
      (when (file-exists-p cookie-file)
        (delete-file cookie-file)))))

;;;###autoload
(defun etherpad-connect (pad-url)
  "Connect to the Etherpad pad at PAD-URL.
Prompts for an author name if `etherpad-author-name' is nil."
  (interactive (list (read-string "Etherpad URL: "
                                  etherpad-default-server nil nil t)))
  (let* ((parsed (etherpad--parse-pad-url pad-url))
         (server (car parsed))
         (pad-id (cdr parsed))
         (buf (get-buffer-create (format "*etherpad:%s*" pad-id)))
         (name (or etherpad-author-name
                   (buffer-local-value 'etherpad--author-name buf)
                   (read-string "Author name: " (user-login-name) nil nil t))))
    (switch-to-buffer buf)
    (etherpad-mode)
    (etherpad--cancel-timers)
    (when (or etherpad--ws (etherpad--live-p))
      (etherpad-disconnect))
    (setq etherpad--server-url server
          etherpad--pad-id pad-id
          etherpad--author-name name
          etherpad--user-disconnected nil
          etherpad--connecting t)
    (message "etherpad: connecting to %s..." pad-url)
    (unless etherpad--token
      (condition-case nil
          (when-let* ((fetched (etherpad--fetch-token server pad-id)))
            (setq etherpad--token fetched))
        (error nil)))
    (condition-case err
        (let ((buf (current-buffer)))
          (setq etherpad--ws
                (websocket-open
                 (etherpad--ws-url server)
                 :on-open (lambda (ws)
                            (with-current-buffer buf
                              (etherpad--on-open ws)))
                 :on-message (lambda (ws frame)
                               (with-current-buffer buf
                                 (etherpad--on-message ws frame)))
                 :on-close (lambda (ws)
                             (with-current-buffer buf
                               (etherpad--on-close ws)))
                 :on-error (lambda (ws kind error)
                             (with-current-buffer buf
                               (etherpad--on-error ws kind error)))))
          (etherpad--start-connect-timer))
      (error
       (setq etherpad--ws nil
             etherpad--connecting nil)
       (etherpad--stop-connecting-animation)
       (message "etherpad: WebSocket failed: %S" err)
       (etherpad--schedule-reconnect)))
    (add-hook 'after-change-functions #'etherpad--after-change nil t)
    (message "etherpad: WebSocket connecting...")))


;;; Major mode

(defvar etherpad-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'etherpad-connect)
    (keymap-set map "C-c C-r" #'etherpad-reconnect)
    (keymap-set map "C-c C-l" #'etherpad-disconnect)
    (keymap-set map "C-c C-u" #'etherpad-list-users)
    (keymap-set map "C-c C-w" #'etherpad-who-here)
    (keymap-set map "C-c C-e" #'etherpad-export)
    (keymap-set map "C-c C-s" #'etherpad-check-sync)
    (keymap-set map "C-c C-f" #'etherpad-toggle-author-colors)
    map)
  "Keymap for `etherpad-mode'.")

;;;###autoload
(define-derived-mode etherpad-mode text-mode "Etherpad"
  "Major mode for collaborative editing via Etherpad.

Commands:
  \\[etherpad-connect]     Connect to a pad (url)
  \\[etherpad-reconnect]   Reconnect the current pad
  \\[etherpad-disconnect]  Disconnect
  \\[etherpad-list-users]  List connected users
  \\[etherpad-export]      Export the pad (etherpad/txt/html/pdf)"
  :group 'etherpad
  (setq-local header-line-format " etherpad: [not connected]")
  (setq-local mode-line-process '(:eval (etherpad--mode-line-status)))
  (setq-local require-final-newline t)
  (add-hook 'kill-buffer-hook #'etherpad-disconnect nil t))

(defun etherpad--mode-line-status ()
  "Return mode-line string with connection status and users."
  (concat " " (etherpad--connection-status-label)
          (when (etherpad--live-p)
            (concat " " (etherpad--user-count-label)))))


;;; Commands

(defun etherpad--user-line (id name &optional echo-style)
  "Format a user ID/NAME line.
If ECHO-STYLE is non-nil, return a compact line suitable for the minibuffer,
otherwise include author colours and a trailing newline."
  (let ((color (alist-get id etherpad--user-colors nil nil #'equal)))
    (if echo-style
        (format "%s%s (%s)%s"
                name
                (if (equal id etherpad--author-id) " (you)" "") id
                (if color (format " [%s]" color) ""))
      (propertize (format "  %s%s  (%s)\n" name
                          (if (equal id etherpad--author-id) " ← you" "") id)
                  'face (and color (list :foreground color))))))

(defun etherpad--users-content (users)
  "Return formatted buffer contents for USERS display."
  (let ((url (and etherpad--server-url etherpad--pad-id
                  (format "%s/p/%s"
                          (string-trim-right etherpad--server-url "/")
                          etherpad--pad-id))))
    (format "Connected users on %s\n\n%s\nrev=%d  total: %d\n"
            (or url "?")
            (mapconcat (lambda (user)
                         (etherpad--user-line (car user) (cdr user)))
                       users "")
            (or etherpad--rev 0)
            (length users))))

(defun etherpad--display-users (&optional buffer)
  "Display connected users.
If BUFFER is true, show users in a buffer, otherwise use the minibuffer."
  (let ((users (and (etherpad--live-p)
                    (etherpad--all-users))))
    (cond ((not users)
           (message "No users connected"))
          (buffer
           (let ((content (etherpad--users-content users))
                 (ub (get-buffer-create "*etherpad-users*")))
             (with-current-buffer ub
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert content)
                 (goto-char (point-min)))
               (setq buffer-read-only t))
             (display-buffer ub)))
          (t
           (message "Connected users:\n%s"
                    (mapconcat (lambda (user)
                                 (etherpad--user-line (car user) (cdr user) t))
                               users "\n"))))))

;;;###autoload
(defun etherpad-who-here ()
  "Show currently connected users in the `*etherpad-users*' buffer."
  (interactive)
  (etherpad--display-users t))

(defun etherpad-list-users ()
  "List connected users in the minibuffer."
  (interactive)
  (etherpad--display-users))


;;; Sync and connection commands

(defun etherpad-check-sync ()
  "Report local sync state in the minibuffer."
  (interactive)
  (let ((ok t))
    (when (and etherpad--awaiting-ack
               (not (etherpad--live-p)))
      (message "etherpad: sync awaiting ack but WebSocket is closed")
      (etherpad--cancel-ack-timer)
      (etherpad--clear-inflight)
      (setq ok nil))
    (if ok
        (message "etherpad: sync OK  rev=%d  base-len=%d  users=%d  ack=%s"
                 (or etherpad--rev -1) (length (or etherpad--base-text ""))
                 (length (etherpad--all-users))
                 (if etherpad--awaiting-ack "waiting" "clear"))
      (message "etherpad: sync problems detected (see *Messages*)"))))

(defun etherpad-disconnect ()
  "Disconnect from the current Etherpad pad."
  (interactive)
  (setq etherpad--user-disconnected t)
  (etherpad--cancel-timers)
  (let ((ws etherpad--ws))
    (setq etherpad--ws nil)
    (etherpad--close-socket ws))
  (remove-hook 'after-change-functions #'etherpad--after-change t)
  (setq etherpad--connecting nil
        etherpad--author-id nil
        etherpad--rev nil
        etherpad--users nil
        etherpad--base-text nil
        etherpad--apool nil
        etherpad--author-attrib-num nil
        etherpad--reject-retry-count 0
        etherpad--resync-preserve nil
        etherpad--untagged-local-edits nil
        etherpad--reconnect-attempts 0)
  (etherpad--clear-inflight)
  (etherpad--set-status-header-line)
  (message "etherpad: disconnected"))

;;;###autoload
(defun etherpad-reconnect ()
  "Reconnect to the current Etherpad pad."
  (interactive)
  (unless (and etherpad--server-url etherpad--pad-id)
    (user-error "No Etherpad pad to reconnect"))
  (etherpad--reconnect))

(defun etherpad--reconnect ()
  "Disconnect the current pad and reconnect with a short delay."
  (etherpad--clear-timer 'etherpad--stuck-reconnect-timer)
  (when (and etherpad--server-url
             etherpad--pad-id
             (buffer-live-p (current-buffer)))
    (let ((pad-url (format "%s/p/%s" etherpad--server-url etherpad--pad-id))
          (name etherpad--author-name))
      (etherpad-disconnect)
      (setq etherpad--user-disconnected nil
            etherpad--resync-preserve t)
      (message "etherpad: reconnecting in 1s...")
      (etherpad--run-buffer-timer
       'etherpad--reconnect-timer 1.0
       (lambda ()
         (unless etherpad--user-disconnected
           (let ((etherpad-author-name name))
             (etherpad-connect pad-url))))))))


;;; Export

(defcustom etherpad-export-formats '("etherpad" "txt" "html" "pdf")
  "Export formats supported by the Etherpad API (can be extended with plugins)."
  :type '(repeat string) :group 'etherpad)

(defcustom etherpad-export-default-format "txt"
  "Default format for `etherpad-export'."
  :type 'string :group 'etherpad)

(defun etherpad--export-url (export-format)
  "Return the export URL for the current pad in EXPORT-FORMAT."
  (unless (and etherpad--server-url etherpad--pad-id)
    (user-error "No Etherpad pad to export"))
  (unless (member export-format etherpad-export-formats)
    (user-error "Unsupported export format: %s" export-format))
  (format "%s/p/%s/export/%s"
          (string-trim-right etherpad--server-url "/")
          etherpad--pad-id
          export-format))

(defun etherpad-export (filename &optional export-format)
  "Export current pad to FILENAME optionally in EXPORT-FORMAT.
Available formats depend on the server, customize `etherpad-export-formats'."
  (interactive
   (let* ((export-format (completing-read
                          "Export format: " etherpad-export-formats
                          nil t nil nil etherpad-export-default-format))
          (filename (read-file-name
                     (format "Export %s to: " export-format) nil
                     (expand-file-name
                      (concat (or etherpad--pad-id "etherpad")
                              "." export-format))
                     nil)))
     (list filename export-format)))
  (let ((export-format (or export-format
                           etherpad-export-default-format)))
    (condition-case err
        (url-copy-file (etherpad--export-url export-format) filename t)
      (error
       (user-error "Etherpad export failed: %s" (error-message-string err))))
    (message "etherpad: exported %s to %s"
             export-format (expand-file-name filename))))


;;; Author colours

(defun etherpad-toggle-author-colors ()
  "Toggle author-based text colouring in the Etherpad buffer."
  (interactive)
  (setq etherpad-author-colors (not etherpad-author-colors))
  (etherpad--refresh-author-colors)
  (etherpad--update-header-line)
  (message "etherpad: author colors %s"
           (if etherpad-author-colors "enabled" "disabled")))


(provide 'etherpad)
;;; etherpad.el ends here
