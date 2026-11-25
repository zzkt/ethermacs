;;; etherpad-tests.el --- Tests for etherpad.el -*- lexical-binding: t; -*-

;; Copyright © 2026 FoAM
;; Author: nik gaffney <nik@fo.am>
;; SPDX-License-Identifier: EUPL-1.2

;;; Commentary:

;; ERT test suite for etherpad
;; Doesn't require a network connection or live Etherpad server

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'etherpad)

;;; Text helpers

(ert-deftest etherpad--ensure-trailing-newline ()
  (should (equal "\n" (etherpad--ensure-trailing-newline "")))
  (should (equal "foo\n" (etherpad--ensure-trailing-newline "foo")))
  (should (equal "foo\n" (etherpad--ensure-trailing-newline "foo\n"))))

(ert-deftest etherpad--count-newlines ()
  (should (equal 0 (etherpad--count-newlines "abc")))
  (should (equal 2 (etherpad--count-newlines "a\nb\nc")))
  (should (equal 3 (etherpad--count-newlines "a\nb\nc\n")))
  (should (equal 1 (etherpad--count-newlines "a\nb\nc" 1 3))))


;;; URL helpers

(ert-deftest etherpad--parse-pad-url-https ()
  (should (equal (cons "https://pad.example.org" "test")
                 (etherpad--parse-pad-url "https://pad.example.org/p/test"))))

(ert-deftest etherpad--parse-pad-url-custom-port ()
  (should (equal (cons "http://example.org:8080" "foo")
                 (etherpad--parse-pad-url "http://example.org:8080/p/foo"))))

(ert-deftest etherpad--parse-pad-url-strips-query-and-fragment ()
  (should (equal (cons "https://pad.example.org" "test")
                 (etherpad--parse-pad-url
                  "https://pad.example.org/p/test?x=1#frag"))))

(ert-deftest etherpad--parse-pad-url-rejects-non-http ()
  (should-error (etherpad--parse-pad-url "ftp://example.org/p/test")
                :type 'error))

(ert-deftest etherpad--ws-url-https ()
  (should (equal
           "wss://pad.example.org/socket.io/?EIO=4&transport=websocket"
           (etherpad--ws-url "https://pad.example.org"))))

(ert-deftest etherpad--ws-url-http ()
  (should (equal
           "ws://example.org/socket.io/?EIO=4&transport=websocket"
           (etherpad--ws-url "http://example.org"))))


;;; Colours

(ert-deftest etherpad--palette-color-list ()
  (should (equal "a" (etherpad--palette-color '("a" "b") 0)))
  (should (equal "b" (etherpad--palette-color '("a" "b") 1)))
  (should (equal nil (etherpad--palette-color '("a" "b") 2))))

(ert-deftest etherpad--palette-color-vector ()
  (should (equal "x" (etherpad--palette-color ["x" "y"] 0)))
  (should (equal "y" (etherpad--palette-color ["x" "y"] 1))))

(ert-deftest etherpad--palette-color-invalid ()
  (should (equal nil (etherpad--palette-color nil 0)))
  (should (equal nil (etherpad--palette-color '("a") -1))))


;;; Export

(ert-deftest etherpad--export-url ()
  (with-temp-buffer
    (setq-local etherpad--server-url "https://pad.example.org/")
    (setq-local etherpad--pad-id "test")
    (should (equal "https://pad.example.org/p/test/export/txt"
                   (etherpad--export-url "txt")))))

(ert-deftest etherpad--export-url-missing-pad ()
  (with-temp-buffer
    (setq-local etherpad--server-url nil)
    (setq-local etherpad--pad-id "test")
    (should-error (etherpad--export-url "txt") :type 'user-error)))

(ert-deftest etherpad--export-url-bad-format ()
  (with-temp-buffer
    (setq-local etherpad--server-url "https://pad.example.org")
    (setq-local etherpad--pad-id "test")
    (should-error (etherpad--export-url "nope") :type 'user-error)))


;;; Connection status

(ert-deftest etherpad--connection-status ()
  (with-temp-buffer
    (setq-local etherpad--server-url "https://pad.example.org")
    (setq-local etherpad--pad-id "test")
    (should (equal "disconnected" (etherpad--connection-status)))
    (should (equal "[disconnected]" (etherpad--connection-status-label)))
    (setq-local etherpad--connecting t)
    (should (equal "connecting" (etherpad--connection-status)))
    (setq-local etherpad--connecting nil)
    (cl-letf (((symbol-function 'etherpad--live-p) (lambda () t)))
      (should (equal "connected" (etherpad--connection-status))))
    (setq-local etherpad--server-url nil)
    (should (equal "not connected" (etherpad--connection-status)))))


;;; Users and colours

(ert-deftest etherpad--find-author-name ()
  (let ((etherpad--users '(("a.1" . "Alice"))))
    (should (equal "Alice" (etherpad--find-author-name "a.1"))))
  (should (equal "user:12345678" (etherpad--find-author-name "a.1234567890")))
  (should (equal "user:short" (etherpad--find-author-name "short")))
  (should (equal "unknown" (etherpad--find-author-name nil))))

(ert-deftest etherpad--historical-author-colors ()
  (let ((data '((colorPalette . ["#111" "#222"])))
        (collab '((historicalAuthorData
                   . (("a.1" . ((colorId . 0)))
                      (a.2 . ((colorId . 1)))
                      ("a.3" . ((colorId . 9))))))))
    (should (equal '(("a.1" . "#111") ("a.2" . "#222"))
                   (etherpad--historical-author-colors data collab)))))

(ert-deftest etherpad--users ()
  (with-temp-buffer
    (setq-local etherpad--author-id "a.me")
    (setq-local etherpad--author-name "Me")
    (setq-local etherpad--users '(("a.x" . "X")))
    (should (equal '("a.me" . "Me") (etherpad--local-user)))
    (should (equal '(("a.me" . "Me") ("a.x" . "X")) (etherpad--all-users)))
    (should (equal "[2 users]" (etherpad--user-count-label)))
    (setq-local etherpad--author-name nil)
    (should (equal '("a.me" . "anonymous") (etherpad--local-user)))
    (setq-local etherpad--users '(("a.me" . "Me")))
    (should (equal '(("a.me" . "Me")) (etherpad--all-users)))
    (should (equal "[1 user]" (etherpad--user-count-label)))
    (setq-local etherpad--author-id "")
    (should-not (etherpad--local-user))
    (setq-local etherpad--users nil)
    (should (equal "[0 users]" (etherpad--user-count-label)))))

(provide 'etherpad-tests)
;;; etherpad-tests.el ends here
