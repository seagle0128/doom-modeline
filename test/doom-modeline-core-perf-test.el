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

(ert-deftest doom-modeline-icon/cached-by-args-and-options ()
  "The icon render runs once per distinct (args, options) combination."
  (let ((doom-modeline--icon-cache (make-hash-table :test #'equal))
        (doom-modeline--icon-cache-order nil)
        (doom-modeline-icon t)
        (doom-modeline-unicode-fallback nil)
        (renders 0))
    (cl-letf (((symbol-function 'doom-modeline-propertize-icon)
               (lambda (_icon _face)
                 (setq renders (1+ renders))
                 (propertize "I" 'face 'doom-modeline-info))))
      ;; First call renders; second identical call is served from cache.
      (doom-modeline-icon 'mdicon "nf-md-email" "e" "@" :face 'doom-modeline-info)
      (should (= renders 1))
      (doom-modeline-icon 'mdicon "nf-md-email" "e" "@" :face 'doom-modeline-info)
      (should (= renders 1))
      ;; A different face is a different key.
      (doom-modeline-icon 'mdicon "nf-md-email" "e" "@" :face 'doom-modeline-warning)
      (should (= renders 2))
      ;; A different icon-name is a different key.
      (doom-modeline-icon 'mdicon "nf-md-account" "e" "@" :face 'doom-modeline-info)
      (should (= renders 3))
      ;; A different nerd-icons option is a different key.
      (let ((nerd-icons-scale-factor 1.5))
        (doom-modeline-icon 'mdicon "nf-md-email" "e" "@" :face 'doom-modeline-info))
      (should (= renders 4)))))

(ert-deftest doom-modeline-icon/cache-returns-isolated-copy ()
  "Mutating a returned icon must not corrupt the cached value."
  (let ((doom-modeline--icon-cache (make-hash-table :test #'equal))
        (doom-modeline--icon-cache-order nil)
        (doom-modeline-icon nil)
        (doom-modeline-unicode-fallback nil))
    (let ((a (doom-modeline-icon 'mdicon nil nil "XY" :face 'doom-modeline-info))
          (b (doom-modeline-icon 'mdicon nil nil "XY" :face 'doom-modeline-info)))
      ;; Distinct string objects ...
      (should-not (eq a b))
      ;; ... with equal content.
      (should (equal a b))
      ;; Mutating A's text properties leaves B untouched.
      (add-face-text-property 0 (length a) 'mode-line t a)
      (should-not (memq 'mode-line (get-text-property 0 'face b))))))


(ert-deftest doom-modeline-icon/cache-evicts-oldest-at-capacity ()
  "The icon cache evicts the oldest entry once it reaches the bound.
The high hit rate of an `equal' hash cache must be preserved: a single-slot
cache is not acceptable because one redisplay renders many distinct icons."
  (let ((doom-modeline--icon-cache (make-hash-table :test #'equal))
        (doom-modeline--icon-cache-order nil)
        (doom-modeline-icon-cache-max 3)
        (doom-modeline-icon nil)
        (doom-modeline-unicode-fallback nil)
        (renders 0))
    (cl-letf (((symbol-function 'doom-modeline-propertize-text)
               (lambda (text _face)
                 (setq renders (1+ renders))
                 (propertize text 'face 'doom-modeline-info))))
      ;; Fill the cache to capacity with three distinct icons.
      (doom-modeline-icon 'mdicon nil nil "A")
      (doom-modeline-icon 'mdicon nil nil "B")
      (doom-modeline-icon 'mdicon nil nil "C")
      (should (= renders 3))
      (should (= (hash-table-count doom-modeline--icon-cache) 3))
      (should (= (length doom-modeline--icon-cache-order) 3))
      ;; A fourth distinct icon evicts the oldest (A).
      (doom-modeline-icon 'mdicon nil nil "D")
      (should (= renders 4))
      (should (= (hash-table-count doom-modeline--icon-cache) 3))
      (should (= (length doom-modeline--icon-cache-order) 3))
      ;; Re-requesting an evicted icon re-renders; a retained one hits cache.
      (doom-modeline-icon 'mdicon nil nil "A")
      (should (= renders 5))
      (doom-modeline-icon 'mdicon nil nil "C")
      (should (= renders 5))
      ;; Re-requesting a present key does not grow the order list.
      (doom-modeline-icon 'mdicon nil nil "D")
      (should (= renders 5))
      (should (= (length doom-modeline--icon-cache-order) 3)))))

(ert-deftest doom-modeline-icon/dynamic-text-does-not-grow-cache ()
  "Dynamic TEXT arguments cannot grow the cache without bound.
Regression: the cache key includes TEXT and the table had no capacity bound,
so passing varying text could leak entries indefinitely."
  (let ((doom-modeline--icon-cache (make-hash-table :test #'equal))
        (doom-modeline--icon-cache-order nil)
        (doom-modeline-icon nil)
        (doom-modeline-unicode-fallback nil))
    (dotimes (i 1000)
      (doom-modeline-icon 'faicon "nf-fa-terminal" "$" (number-to-string i) :face 'x))
    (should (<= (hash-table-count doom-modeline--icon-cache)
                doom-modeline-icon-cache-max))
    (should (<= (length doom-modeline--icon-cache-order)
                doom-modeline-icon-cache-max))))

(defmacro doom-modeline-core-perf--with-file-buffer (name file &rest body)
  "Run BODY in a buffer named NAME visiting FILE with a primed root cache."
  (declare (indent 2))
  `(let ((doom-modeline--buffer-file-name-format-cache nil))
     (with-temp-buffer
       (rename-buffer ,name t)
       (setq buffer-file-name ,file)
       (setq buffer-file-truename ,file)
       (setq-local doom-modeline--project-root "/tmp/project/")
       ,@body)))

(ert-deftest doom-modeline-buffer-file-name/cached-formatting-chain ()
  "The whole formatting chain runs once until an input changes.
`shrink-path' probes directories on the redisplay path; a second call with
identical inputs must not probe again, and editing the buffer must not
invalidate a path-only rendering."
  (let ((doom-modeline-buffer-file-name-style 'truncate-upto-project)
        (shrink-calls 0)
        (shrink-after-cold nil)
        (truename-calls 0))
    (doom-modeline-core-perf--with-file-buffer
     " *dm-file-name-cache*" "/tmp/project/src/dm.el"
     (setq-local default-directory "/tmp/project/src/")
     (cl-letf* (((symbol-function 'shrink-path--dirs-internal)
                 (lambda (&rest _)
                   (setq shrink-calls (1+ shrink-calls))
                   "/p"))
                ((symbol-function 'file-truename)
                 (lambda (f)
                   (setq truename-calls (1+ truename-calls))
                   f)))
       (let ((first (doom-modeline-buffer-file-name))
             (second (doom-modeline-buffer-file-name)))
         (should (equal first second))
         ;; `truncate-upto-project' shrinks the root parent only.
         (should (= shrink-calls 1))
         (setq shrink-after-cold shrink-calls)
         (doom-modeline-buffer-file-name)
         (should (= shrink-calls shrink-after-cold))
         ;; `buffer-file-truename' is set, so no fallback resolution at all.
         (should (= truename-calls 0))
         ;; Text edits do not change any path input.
         (insert ";; edit\n")
         (doom-modeline-buffer-file-name)
         (should (= shrink-calls shrink-after-cold)))))))

(ert-deftest doom-modeline-buffer-file-name/invalidates-on-input-change ()
  "Every input the rendering reads invalidates the cache."
  (let ((doom-modeline-buffer-file-name-style 'file-name)
        (doom-modeline-buffer-file-name-function #'identity)
        (calls 0))
    (doom-modeline-core-perf--with-file-buffer
     " *dm-file-name-invalid*" "/tmp/project/src/dm.el"
     (cl-letf (((symbol-function 'doom-modeline--format-buffer-file-name)
                (lambda () (setq calls (1+ calls)) "/tmp/project/src/dm.el")))
       (doom-modeline-buffer-file-name)
       (doom-modeline-buffer-file-name)
       (should (= calls 1))
       ;; Style changes take effect immediately.
       (setq doom-modeline-buffer-file-name-style 'buffer-name)
       (doom-modeline-buffer-file-name)
       (should (= calls 2))
       ;; A renamed buffer is a different rendering.
       (rename-buffer " *dm-file-name-renamed*" t)
       (doom-modeline-buffer-file-name)
       (should (= calls 3))
       ;; Visiting another file invalidates.
       (setq buffer-file-name "/tmp/project/src/other.el")
       (doom-modeline-buffer-file-name)
       (should (= calls 4))
       ;; A changed project root invalidates.
       (setq-local doom-modeline--project-root "/tmp/other/")
       (doom-modeline-buffer-file-name)
       (should (= calls 5))
       ;; A custom name function invalidates even with the same raw file.
       (setq doom-modeline-buffer-file-name-function
             (lambda (_) "/tmp/project/src/rewritten.el"))
       (doom-modeline-buffer-file-name)
       (should (= calls 6))))))

(ert-deftest doom-modeline-buffer-file-name/cache-is-buffer-local ()
  "Two buffers never share a cached rendering."
  (let ((doom-modeline-buffer-file-name-style 'file-name))
    (let ((a (generate-new-buffer " *dm-name-a*"))
          (b (generate-new-buffer " *dm-name-b*")))
      (unwind-protect
          (progn
            (with-current-buffer a
              (setq buffer-file-name "/tmp/project/a.el"
                    buffer-file-truename "/tmp/project/a.el")
              (setq-local doom-modeline--project-root "/tmp/project/")
              (doom-modeline-buffer-file-name))
            (with-current-buffer b
              (setq buffer-file-name "/tmp/project/b.el"
                    buffer-file-truename "/tmp/project/b.el")
              (setq-local doom-modeline--project-root "/tmp/project/")
              (doom-modeline-buffer-file-name))
            (let ((ca (buffer-local-value 'doom-modeline--buffer-file-name-format-cache a))
                  (cb (buffer-local-value 'doom-modeline--buffer-file-name-format-cache b)))
              (should (consp ca))
              (should (consp cb))
              (should-not (eq ca cb))
              (should (string= "a.el" (substring-no-properties (cdr ca))))
              (should (string= "b.el" (substring-no-properties (cdr cb))))))
        (kill-buffer a)
        (kill-buffer b)))))

(ert-deftest doom-modeline-buffer-file-name/cache-returns-isolated-copy ()
  "Mutating a returned string cannot corrupt later cache hits."
  (let ((doom-modeline-buffer-file-name-style 'file-name))
    (doom-modeline-core-perf--with-file-buffer
     " *dm-file-name-copy*" "/tmp/project/src/dm.el"
     (let ((a (doom-modeline-buffer-file-name))
           (b (doom-modeline-buffer-file-name)))
       (should-not (eq a b))
       (should (equal a b))
       (add-face-text-property 0 (length a) 'mode-line nil a)
       (let ((faces (get-text-property 0 'face b)))
         (should-not (memq 'mode-line (if (listp faces) faces (list faces)))))
       (let* ((c (doom-modeline-buffer-file-name))
              (faces (get-text-property 0 'face c)))
         (should-not (memq 'mode-line (if (listp faces) faces (list faces)))))))))

(provide 'doom-modeline-core-perf-test)
;;; doom-modeline-core-perf-test.el ends here
