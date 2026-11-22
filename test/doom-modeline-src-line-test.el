;;; doom-modeline-src-line-test.el --- Tests for the Org Babel src-line display -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  Tests for the "src:N" part of the `buffer-position' segment: the line of
;;  point relative to the current Org Babel source block.
;;
;;  Run with:
;;    emacs -Q --batch -L . -l test/doom-modeline-src-line-test.el \
;;      -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

(defun doom-modeline-src-line-test--rendered ()
  "Return a printable representation of the `buffer-position' segment."
  (format "%S" (doom-modeline-segment--buffer-position)))

(defmacro doom-modeline-src-line-test--with-block (&rest body)
  "Evaluate BODY in an Org buffer with a two-line source block.
Point is placed on the first line of code."
  (declare (indent 0))
  `(with-temp-buffer
     (org-mode)
     (insert "#+begin_src emacs-lisp\n(message \"one\")\n(message \"two\")\n#+end_src\n")
     (goto-char (point-min))
     (forward-line 1)
     (setq doom-modeline--src-line-cache nil)
     (let ((doom-modeline-enable-buffer-position t)
           (doom-modeline-enable-src-line t))
       ,@body)))

(ert-deftest doom-modeline-src-line/within-block ()
  "Report the one-based line of point within the source block."
  (doom-modeline-src-line-test--with-block
    (should (= (doom-modeline--src-line) 1))
    (forward-line 1)
    (should (= (doom-modeline--src-line) 2))
    (should (string-match-p "src:2"
                            (doom-modeline-src-line-test--rendered)))))

(ert-deftest doom-modeline-src-line/outside-block ()
  "No line is reported when point is outside any source block."
  (doom-modeline-src-line-test--with-block
    (goto-char (point-min))
    (should-not (doom-modeline--src-line))
    (should-not (string-match-p
                 "src:"
                 (doom-modeline-src-line-test--rendered)))
    (goto-char (point-max))
    (should-not (doom-modeline--src-line))))

(ert-deftest doom-modeline-src-line/outside-org-mode ()
  "Non-Org buffers never produce a src line."
  (with-temp-buffer
    (text-mode)
    (insert "#+begin_src emacs-lisp\n(message \"one\")\n#+end_src\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((doom-modeline-enable-buffer-position t)
          (doom-modeline-enable-src-line t))
      (should-not (doom-modeline--src-line))
      (should-not (string-match-p
                   "src:"
                   (doom-modeline-src-line-test--rendered))))))

(ert-deftest doom-modeline-src-line/disabled-by-options ()
  "Both the position and src-line options can suppress the display."
  (doom-modeline-src-line-test--with-block
    (let ((doom-modeline-enable-src-line nil))
      (should-not (string-match-p
                   "src:"
                   (doom-modeline-src-line-test--rendered))))
    (let ((doom-modeline-enable-buffer-position nil))
      (should-not (doom-modeline-segment--buffer-position)))))

(ert-deftest doom-modeline-src-line/cached-until-change ()
  "The expensive block lookup runs only when point or text changes."
  (doom-modeline-src-line-test--with-block
    (forward-line 1)
    (let ((calls 0)
          (orig (symbol-function 'org-babel-where-is-src-block-head)))
      (cl-letf (((symbol-function 'org-babel-where-is-src-block-head)
                 (lambda (&rest args)
                   (setq calls (1+ calls))
                   (apply orig args))))
        (should (= (doom-modeline--src-line) 2))
        (should (= (doom-modeline--src-line) 2))
        (should (= calls 1))
        ;; Moving point invalidates the cached line.
        (forward-line -1)
        (should (= (doom-modeline--src-line) 1))
        (should (= calls 2))
        ;; Editing the buffer invalidates it as well.
        (goto-char (point-min))
        (forward-line 1)
        (insert "new\n")
        (forward-line 1)
        (should (= (doom-modeline--src-line) 3))
        (should (= calls 3))))))

(ert-deftest doom-modeline-src-line/preserves-match-data ()
  "Looking up the block does not clobber the caller's match data."
  (doom-modeline-src-line-test--with-block
    (string-match "b" "abc")
    (let ((data (match-data)))
      (doom-modeline--src-line)
      (should (equal (match-data) data)))))

(ert-deftest doom-modeline-src-line/ignores-org-errors ()
  "A failing Org Babel lookup must not break redisplay."
  (doom-modeline-src-line-test--with-block
    (cl-letf (((symbol-function 'org-babel-where-is-src-block-head)
               (lambda (&rest _) (error "malformed block"))))
      (should-not (doom-modeline--src-line))
      (should-not (string-match-p
                   "src:"
                   (doom-modeline-src-line-test--rendered))))))

(provide 'doom-modeline-src-line-test)
;;; doom-modeline-src-line-test.el ends here
