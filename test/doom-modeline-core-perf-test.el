;;; doom-modeline-core-perf-test.el --- Tests for core redisplay caching -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  Regression tests for the pixel-width cache in `doom-modeline-core.el'.
;;  Measuring the mode-line right-hand side is expensive and happens on every
;;  redisplay, so the measured string is cached across consecutive calls.
;;  The contract under test: the expensive measurement runs only when the
;;  measured input (frame, string content, or text properties) changes.
;;
;;  Run with:
;;    emacs -Q --batch -L . -l test/doom-modeline-core-perf-test.el \
;;      -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'cl-lib)
(require 'doom-modeline-core)

(defmacro doom-modeline-core-perf--with-pixel-spy (counter-var &rest body)
  "Run BODY with `string-pixel-width' replaced by a counting spy.
COUNTER-VAR is the symbol of a caller-local counter variable; the spy
increments it and returns a deterministic width derived from the content."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'string-pixel-width)
              (lambda (s)
                (setq ,counter-var (1+ ,counter-var))
                (length s))))
     ,@body))

(ert-deftest doom-modeline-string-pixel-width/cached-until-input-changes ()
  "The expensive measurement runs once until the string changes."
  (let ((calls 0)
        (doom-modeline--pixel-width-cache nil)
        (s " Project:foo mode:emacs-lisp Ln:1,Col:1 42% UTF-8 1/100 "))
    (doom-modeline-core-perf--with-pixel-spy calls
      (should (= (doom-modeline-string-pixel-width s) (length s)))
      ;; Second call with identical input: served from cache.
      (should (= (doom-modeline-string-pixel-width s) (length s)))
      (should (= calls 1))
      ;; Changed content forces exactly one fresh measurement.
      (let ((s2 (concat s "master")))
        (should (= (doom-modeline-string-pixel-width s2) (length s2)))
        (should (= calls 2))
        (should (= (doom-modeline-string-pixel-width s2) (length s2)))
        (should (= calls 2))))))

(ert-deftest doom-modeline-string-pixel-width/face-change-invalidates ()
  "A change in text properties (face) invalidates the cache.
`string-pixel-width' accounts for faces in graphical displays, so two
strings with equal characters but different properties must not share a
cached width."
  (let ((calls 0)
        (doom-modeline--pixel-width-cache nil)
        (plain "master")
        (bold (propertize "master" 'face 'bold))
        (red (propertize "master" 'face '(:foreground "red"))))
    (doom-modeline-core-perf--with-pixel-spy calls
      (doom-modeline-string-pixel-width plain)
      (doom-modeline-string-pixel-width plain)
      (should (= calls 1))
      ;; Same characters, different face: must remeasure.
      (doom-modeline-string-pixel-width bold)
      (should (= calls 2))
      (doom-modeline-string-pixel-width bold)
      (should (= calls 2))
      (doom-modeline-string-pixel-width red)
      (should (= calls 3)))))

(ert-deftest doom-modeline-string-pixel-width/frame-change-invalidates ()
  "Switching frames invalidates the cached width.
Pixel width depends on the frame's font, so a cache entry from another frame
must not be reused even when the string is identical."
  (let ((calls 0)
        (doom-modeline--pixel-width-cache nil)
        (s "master")
        (fake-frame (make-symbol "fake-dm-frame")))
    (doom-modeline-core-perf--with-pixel-spy calls
      (doom-modeline-string-pixel-width s)
      (doom-modeline-string-pixel-width s)
      (should (= calls 1))
      (cl-letf (((symbol-function 'selected-frame) (lambda () fake-frame)))
        (doom-modeline-string-pixel-width s)
        (should (= calls 2))
        (doom-modeline-string-pixel-width s)
        (should (= calls 2))))))

(ert-deftest doom-modeline-string-pixel-width/single-entry-bounded ()
  "The cache holds at most one entry even when the string changes often.
The mode-line clock may change the string every second; keeping more than one
entry would grow the cache without bound."
  (let ((calls 0)
        (doom-modeline--pixel-width-cache nil))
    (doom-modeline-core-perf--with-pixel-spy calls
      (dotimes (i 50)
        (doom-modeline-string-pixel-width (format "tick-%d" i)))
      (should (= calls 50))
      ;; Cache is a single (KEY . WIDTH) cons, KEY is (FRAME . STRING).
      (should (consp doom-modeline--pixel-width-cache))
      (let ((key (car doom-modeline--pixel-width-cache)))
        (should (eq (car key) (selected-frame)))
        (should (equal (cdr key) "tick-49"))))))

(ert-deftest doom-modeline-string-pixel-width/fallback-path-cached ()
  "The no-`string-pixel-width' fallback is cached too.
On builds without the built-in measurement, the manual estimate is equally
expensive and must not run on every redisplay."
  (let ((doom-modeline--pixel-width-cache nil)
        (s "master")
        (calls 0))
    (cl-letf* ((fboundp-orig (symbol-function 'fboundp))
               ((symbol-function 'fboundp)
                (lambda (sym)
                  (if (eq sym 'string-pixel-width) nil
                    (funcall fboundp-orig sym))))
               ((symbol-function 'window-font-width)
                (lambda (&rest _) (setq calls (1+ calls)) 10)))
      (doom-modeline-string-pixel-width s)
      (doom-modeline-string-pixel-width s)
      (should (= calls 1)))))

(ert-deftest doom-modeline-def-modeline/rhs-evaluated-once ()
  "The RHS of a modeline is rendered once and returned as a string.
Regression: the old implementation measured the RHS with `format-mode-line'
and then returned the un-evaluated forms as the third element of the mode-line
format, so Emacs evaluated every RHS segment a second time at display time.
The third element must be the pre-rendered string, not a (:eval ...) form."
  (let ((doom-modeline--modelines nil)
        (doom-modeline--pixel-width-cache nil)
        (rendered (propertize "X" 'face 'doom-modeline-info 'help-echo "probe")))
    (unwind-protect
        (progn
          ;; In batch mode `format-mode-line' does not evaluate :eval forms, so
          ;; stub it to return the string a real redisplay would produce.
          (cl-letf (((symbol-function 'format-mode-line)
                     (lambda (&rest _) rendered))
                    ((symbol-function 'doom-modeline-string-pixel-width)
                     (lambda (_s) 10)))
            (add-to-list 'doom-modeline--fn-alist '(rhs-count-probe . ignore))
            (doom-modeline-def-modeline 'test-rhs-once nil '(rhs-count-probe))
            (let ((fmt (funcall
                        (intern "doom-modeline-format--test-rhs-once"))))
              ;; The third element is the rendered string, not a form list.
              (should (stringp (nth 2 fmt)))
              (should (equal (nth 2 fmt) rendered))
              ;; Text properties survive, unchanged by width measurement.
              (should (eq 'doom-modeline-info
                          (get-text-property 0 'face (nth 2 fmt))))
              (should (string= "probe"
                               (get-text-property 0 'help-echo (nth 2 fmt)))))))
      (setq doom-modeline--modelines nil))))

(ert-deftest doom-modeline-def-modeline/measure-uses-copy ()
  "Width measurement must not mutate the string returned as the RHS.
`add-face-text-property' mutates in place; measuring a copy keeps the
returned RHS free of the appended `mode-line' face, which would otherwise
override segment faces carrying their own colors."
  (let ((measured-input nil)
        (doom-modeline--pixel-width-cache nil))
    (cl-letf (((symbol-function 'format-mode-line)
               (lambda (&rest _) (propertize "X" 'face 'doom-modeline-info)))
              ((symbol-function 'doom-modeline-string-pixel-width)
               (lambda (s) (setq measured-input s) 10)))
      (add-to-list 'doom-modeline--fn-alist '(rhs-copy-probe . ignore))
      (doom-modeline-def-modeline 'test-rhs-copy nil '(rhs-copy-probe))
      (let ((fmt (funcall (intern "doom-modeline-format--test-rhs-copy"))))
        ;; The measured string carries the appended mode-line face (the copy).
        (should (memq 'mode-line (get-text-property 0 'face measured-input)))
        ;; The returned RHS does not.
        (should (eq 'doom-modeline-info
                    (get-text-property 0 'face (nth 2 fmt))))))))

(provide 'doom-modeline-core-perf-test)
;;; doom-modeline-core-perf-test.el ends here
