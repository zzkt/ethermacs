;;; etherpad-changeset-tests.el --- Changeset tests for etherpad.el -*- lexical-binding: t; -*-

;; Copyright © 2026 FoAM
;; Author: nik gaffney <nik@fo.am>
;; SPDX-License-Identifier: EUPL-1.2

;;; Commentary:

;; ERT tests for the Etherpad/Easysync changeset functions

;;; Code:

(require 'ert)
(require 'etherpad)

;;; Base-36

(ert-deftest etherpad--base36-digit-p-t ()
  (should (etherpad--base36-digit-p ?0))
  (should (etherpad--base36-digit-p ?9))
  (should (etherpad--base36-digit-p ?a))
  (should (etherpad--base36-digit-p ?z)))

(ert-deftest etherpad--base36-digit-p-nil ()
  (should-not (etherpad--base36-digit-p ?A))
  (should-not (etherpad--base36-digit-p ?Z))
  (should-not (etherpad--base36-digit-p ?-))
  (should-not (etherpad--base36-digit-p ?\s)))

(ert-deftest etherpad--from-base36 ()
  (should (equal 0 (etherpad--from-base36 "0")))
  (should (equal 9 (etherpad--from-base36 "9")))
  (should (equal 10 (etherpad--from-base36 "a")))
  (should (equal 35 (etherpad--from-base36 "z")))
  (should (equal 36 (etherpad--from-base36 "10")))
  (should (equal 71 (etherpad--from-base36 "1z")))
  (should (equal 1295 (etherpad--from-base36 "zz"))))

(ert-deftest etherpad--to-base36 ()
  (should (equal "0" (etherpad--to-base36 0)))
  (should (equal "0" (etherpad--to-base36 -1)))
  (should (equal "9" (etherpad--to-base36 9)))
  (should (equal "a" (etherpad--to-base36 10)))
  (should (equal "z" (etherpad--to-base36 35)))
  (should (equal "10" (etherpad--to-base36 36)))
  (should (equal "1z" (etherpad--to-base36 71)))
  (should (equal "zz" (etherpad--to-base36 1295))))

(ert-deftest etherpad--base36-round-trip ()
  (dolist (n '(0 1 9 10 35 36 71 100 1295 46656))
    (should (equal n (etherpad--from-base36 (etherpad--to-base36 n))))))

(ert-deftest etherpad--scan-base36 ()
  (should (equal (cons 0 1) (etherpad--scan-base36 "0" 0)))
  (should (equal (cons 1295 2) (etherpad--scan-base36 "zz" 0)))
  (should (equal (cons 71 2) (etherpad--scan-base36 "1z+" 0)))
  (should (equal (cons 0 0) (etherpad--scan-base36 "+" 0)))
  (should (equal (cons 1295 2) (etherpad--scan-base36 "zzzz" 0 2))))


;;; Changesets

(ert-deftest etherpad--serialize-changeset-empty ()
  (should (equal "Z:0>0$" (etherpad--serialize-changeset 0 0 nil ""))))

(ert-deftest etherpad--make-changeset-noop ()
  (should (equal '((old-len . 5) (net-delta . 0)
                   (ops ("keep" 5 0 nil)) (char-bank . ""))
                 (etherpad--make-changeset "hello" "hello"))))

(ert-deftest etherpad--make-changeset-insert ()
  (should (equal '((old-len . 5) (net-delta . 6)
                   (ops ("keep" 5 0 nil) ("insert" 6 0 nil))
                   (char-bank . " world"))
                 (etherpad--make-changeset "hello" "hello world"))))

(ert-deftest etherpad--make-changeset-delete ()
  (should (equal '((old-len . 5) (net-delta . -1)
                   (ops ("keep" 4 0 nil) ("delete" 1 0 nil))
                   (char-bank . ""))
                 (etherpad--make-changeset "hello" "hell"))))

(ert-deftest etherpad--make-changeset-replace ()
  (should (equal '((old-len . 5) (net-delta . 0)
                   (ops ("keep" 1 0 nil) ("delete" 3 0 nil) ("insert" 3 0 nil))
                   (char-bank . "ipp"))
                 (etherpad--make-changeset "hello" "hippo"))))

(ert-deftest etherpad--make-changeset-insert-at-start ()
  (should (equal '((old-len . 3) (net-delta . 1)
                   (ops ("insert" 1 0 nil)) (char-bank . "z"))
                 (etherpad--make-changeset "abc" "zabc"))))

(ert-deftest etherpad--apply-changeset-to-string ()
  (let ((cs (etherpad--make-changeset "hello" "hello world")))
    (should (equal "hello world\n"
                   (etherpad--apply-changeset-to-string cs "hello")))))

(ert-deftest etherpad--apply-changeset-to-string-noop ()
  (let ((cs (etherpad--make-changeset "hello" "hello")))
    (should (equal "hello\n"
                   (etherpad--apply-changeset-to-string cs "hello")))))

(ert-deftest etherpad--parse-changeset ()
  (should (equal
           '((old-len . 5) (net-delta . 6)
             (ops ("keep" 5 0 nil) ("insert" 6 0 nil)) (char-bank . " world"))
           (etherpad--parse-changeset "Z:5>6=5+6$ world"))))

(ert-deftest etherpad--parse-changeset-invalid ()
  (should-not (etherpad--parse-changeset "garbage"))
  (should-not (etherpad--parse-changeset "Z:"))
  (should-not (etherpad--parse-changeset "5>6=5+6$ world")))

(ert-deftest etherpad--changeset-serialize-parse-round-trip ()
  (let ((cs (etherpad--make-changeset "hello" "hello world")))
    (should (equal cs
                   (etherpad--parse-changeset
                    (etherpad--serialize-changeset
                     (alist-get 'old-len cs)
                     (alist-get 'net-delta cs)
                     (alist-get 'ops cs)
                     (alist-get 'char-bank cs)))))))

(ert-deftest etherpad--follow-converges ()
  "Concurrent changesets converge when transposed with `etherpad--follow'."
  (let ((base "abc\n")
        (cs1 (etherpad--make-changeset "abc\n" "Xabc\n"))
        (cs2 (etherpad--make-changeset "abc\n" "Yabc\n")))
    (let* ((t1 (etherpad--apply-changeset-to-string cs1 base))
           (t2 (etherpad--apply-changeset-to-string cs2 base))
           (cs2-after-cs1 (etherpad--follow cs1 cs2 t))
           (cs1-after-cs2 (etherpad--follow cs2 cs1 nil))
           (one-way (etherpad--apply-changeset-to-string cs2-after-cs1 t1))
           (other-way (etherpad--apply-changeset-to-string cs1-after-cs2 t2)))
      (should (equal one-way other-way)))))

(ert-deftest etherpad--canonical-multiline-splits ()
  (should (equal '(("insert" 2 1 nil) ("insert" 1 0 nil))
                 (etherpad--canonical-multiline "insert" "a\nb" nil))))

(ert-deftest etherpad--canonical-multiline-whole-line ()
  (should (equal '(("insert" 2 1 nil))
                 (etherpad--canonical-multiline "insert" "a\n" nil))))

(ert-deftest etherpad--canonical-multiline-delete ()
  (should (equal '(("delete" 2 1 nil) ("delete" 1 0 nil))
                 (etherpad--canonical-multiline "delete" "a\nb" nil)))
  (should (equal '(("delete" 4 2 nil))
                 (etherpad--canonical-multiline "delete" "a\nb\n" nil))))

(ert-deftest etherpad--parse-ops-string ()
  (should (equal '(("keep" 3 0 nil) ("insert" 2 0 nil) ("delete" 1 0 nil))
                 (etherpad--parse-ops-string "=3+2-1" 0 6)))
  (should (equal '(("insert" 3 0 (0)))
                 (etherpad--parse-ops-string "*0+3" 0 4)))
  (should (equal '(("keep" 3 1 (0 2)))
                 (etherpad--parse-ops-string "*0*2|1=3" 0 8)))
  (should (equal '(("keep" 2 0 nil))
                 (etherpad--parse-ops-string "=2$garbage" 0 11))))

(ert-deftest etherpad--serialize-ops ()
  (should (equal "=3+2-1"
                 (etherpad--serialize-ops
                  '(("keep" 3 0 nil) ("insert" 2 0 nil) ("delete" 1 0 nil)))))
  (should (equal "*0+3" (etherpad--serialize-ops '(("insert" 3 0 (0))))))
  (should (equal "*0*2|1=3" (etherpad--serialize-ops '(("keep" 3 1 (0 2))))))

  (let* ((ops '(("keep" 3 1 (0 2)) ("insert" 2 0 (1)) ("delete" 1 0 nil)))
         (wire (etherpad--serialize-ops ops)))
    (should (equal ops (etherpad--parse-ops-string wire 0 (length wire))))))

(ert-deftest etherpad--changeset-wire ()
  (should (equal "Z:5<2=3-2$"
                 (etherpad--serialize-changeset
                  5 -2 '(("keep" 3 0 nil) ("delete" 2 0 nil)) "")))
  (should (equal '((old-len . 5) (net-delta . -2)
                   (ops ("keep" 3 0 nil) ("delete" 2 0 nil))
                   (char-bank . ""))
                 (etherpad--parse-changeset "Z:5<2=3-2$")))
  (should (equal '((old-len . 5) (net-delta . 1)
                   (ops ("keep" 3 0 (0)) ("insert" 1 0 (1)))
                   (char-bank . "x"))
                 (etherpad--parse-changeset "Z:5>1*0=3*1+1$x"))))

(ert-deftest etherpad--apply-cs-core ()
  (should (equal "heppo"
                 (etherpad--apply-cs-core
                  "hello"
                  '(("keep" 2 0 nil) ("delete" 2 0 nil) ("insert" 2 0 nil))
                  "pp")))
  (should (equal "hello world"
                 (etherpad--apply-cs-core
                  "hello" '(("keep" 5 0 nil) ("insert" 6 0 nil)) " world")))

  (let (seen)
    (let ((res (etherpad--apply-cs-core
                "" '(("insert" 2 0 (0))) "ab"
                (lambda (attribs) (setq seen attribs) '(face bold)))))
      (should (equal "ab" res))
      (should (equal '(0) seen))
      (should (eq 'bold (get-text-property 0 'face res))))))

(ert-deftest etherpad--resolve-attrib-props ()
  (with-temp-buffer
    (setq-local etherpad--apool
                (list (cons 'numToAttrib
                            (list (cons (intern "0")
                                        (vector "author" "a.me"))))))
    (setq-local etherpad--user-colors '(("a.me" . "#123456")))

    (let ((etherpad-author-colors nil))
      (should (equal '(etherpad-author "a.me")
                     (etherpad--resolve-attrib-props '(0)))))
    (let ((etherpad-author-colors t))
      (should (equal '(etherpad-author "a.me" face (:foreground "#123456"))
                     (etherpad--resolve-attrib-props '(0)))))
    (should-not (etherpad--resolve-attrib-props nil))
    (should-not (etherpad--resolve-attrib-props '(5)))))

(ert-deftest etherpad--apool ()
  (with-temp-buffer
    (setq-local etherpad--author-id "a.me")
    (setq-local etherpad--apool
                (list (cons 'numToAttrib
                            (list (cons (intern "0") (vector "author" "a.me"))
                                  (cons (intern "3")
                                        (vector "author" "a.other"))))))
    (should (equal 0 (etherpad--find-author-attrib-num)))
    (should (equal 3 (etherpad--max-attrib-num)))
    (should (etherpad--apool-attrib-num-used-p 0))
    (should-not (etherpad--apool-attrib-num-used-p 5))
    (should (equal 0 (etherpad--resolve-attrib-num)))
    (should (equal etherpad--apool (etherpad--apool-with-author))))

  (with-temp-buffer
    (setq-local etherpad--author-id "a.new")
    (setq-local etherpad--apool
                (list (cons 'numToAttrib
                            (list (cons (intern "0")
                                        (vector "author" "a.other"))))
                      (cons 'nextNum 1)))
    (let ((pool (etherpad--apool-with-author)))
      (should (equal (vector "author" "a.new")
                     (alist-get (intern "1") (alist-get 'numToAttrib pool))))
      (should (equal 2 (alist-get 'nextNum pool)))
      (should (equal 1 etherpad--author-attrib-num)))))

(ert-deftest etherpad--make-changeset-author ()
  (should (equal '((old-len . 5) (net-delta . 6)
                   (ops ("keep" 5 0 nil) ("insert" 6 0 (1)))
                   (char-bank . " world"))
                 (etherpad--make-changeset "hello" "hello world" 1)))
  (should (equal '((old-len . 0) (net-delta . 3)
                   (ops ("insert" 2 1 nil) ("insert" 1 0 nil))
                   (char-bank . "a\nb"))
                 (etherpad--make-changeset "" "a\nb"))))

(ert-deftest etherpad--copy-ops ()
  (let* ((ops '(("keep" 2 0 nil) ("insert" 1 0 (0))))
         (copy (etherpad--copy-ops (list (cons 'ops ops)))))
    (should (equal [["keep" 2 0 nil] ["insert" 1 0 (0)]] copy))
    (aset (aref copy 0) 1 99)
    (should (equal '("keep" 2 0 nil) (car ops)))))

(ert-deftest etherpad--compute-pending-changeset ()
  (let ((base "line one\nline two\n"))
    (with-temp-buffer
      (setq-local etherpad--base-text base)
      (insert base)
      (goto-char (point-max))
      (insert "line three\n")
      (let ((cs (etherpad--compute-pending-changeset)))
        (should cs)
        (should (equal "line one\nline two\nline three\n"
                       (etherpad--apply-changeset-to-string cs base)))))))

(ert-deftest etherpad--tag-local-change ()
  (with-temp-buffer
    (setq-local etherpad--author-id "a.me")
    (insert "hello")
    (let ((etherpad-author-colors nil))
      (etherpad--tag-local-change 1 3)
      (should (equal "a.me" (get-text-property 1 'etherpad-author)))
      (should-not (get-text-property 1 'face))))
  (with-temp-buffer
    (setq-local etherpad--author-id "a.me")
    (setq-local etherpad--user-colors '(("a.me" . "#123456")))
    (insert "hello")
    (let ((etherpad-author-colors t))
      (etherpad--tag-local-change 1 3)
      (should (equal '(:foreground "#123456")
                     (get-text-property 1 'face))))))

(ert-deftest etherpad--tag-local-diff ()
  (with-temp-buffer
    (setq-local etherpad--author-id "a.me")
    (let ((etherpad-author-colors nil))
      (insert "hello world")
      (etherpad--tag-local-diff "hello")
      (should (equal "a.me" (get-text-property 7 'etherpad-author)))
      (should-not (get-text-property 1 'etherpad-author)))))

(provide 'etherpad-changeset-tests)
;;; etherpad-changeset-tests.el ends here
