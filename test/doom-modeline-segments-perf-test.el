;;; doom-modeline-segments-perf-test.el --- Tests for P0 segment caching -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  Regression tests for the caching of segments whose data is expensive to
;;  compute (`word-count', the total-line count of `buffer-position', and the
;;  buffer state icon).  The contract under test: the expensive computation
;;  runs at most once per buffer change, and redisplay only reads the cache.
;;
;;  Run with:
;;    emacs -Q --batch -L . -l test/doom-modeline-segments-perf-test.el \
;;      -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'cl-lib)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

(ert-deftest doom-modeline-word-count/cached-until-change ()
  "The full-buffer `count-words' scan runs once until the buffer changes."
  (let ((doom-modeline-enable-word-count t)
        (doom-modeline-continuous-word-count-modes '(text-mode))
        (calls 0))
    (with-temp-buffer
      (text-mode)
      (insert "one two three four five")
      (setq doom-modeline--word-count nil)
      (cl-letf* ((orig (symbol-function 'count-words))
                 ((symbol-function 'count-words)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        ;; Two redisplays with no intervening change: one scan.
        (doom-modeline-segment--word-count)
        (doom-modeline-segment--word-count)
        (should (= calls 1))
        (should (= doom-modeline--word-count 5))
        ;; An edit invalidates the cache; the next render recomputes once.
        (insert " six")
        (doom-modeline-segment--word-count)
        (should (= calls 2))
        (should (= doom-modeline--word-count 6))))))

(ert-deftest doom-modeline-word-count/zero-is-cached ()
  "A zero word count is a valid cached value, not a permanent miss."
  (let ((doom-modeline-enable-word-count t)
        (doom-modeline-continuous-word-count-modes '(text-mode))
        (calls 0))
    (with-temp-buffer
      (text-mode)
      (setq doom-modeline--word-count nil)
      (cl-letf* ((orig (symbol-function 'count-words))
                 ((symbol-function 'count-words)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        (doom-modeline-segment--word-count)
        (doom-modeline-segment--word-count)
        (should (= calls 1))
        (should (= doom-modeline--word-count 0))))))

(ert-deftest doom-modeline-total-lines/cached-until-change ()
  "The full-buffer `line-number-at-pos' scan runs once until a change."
  (let ((doom-modeline-enable-buffer-position t)
        (doom-modeline-total-line-number t)
        (calls 0))
    (with-temp-buffer
      (insert "a\nb\nc\n")
      (setq doom-modeline--total-lines nil)
      (cl-letf* ((orig (symbol-function 'line-number-at-pos))
                 ((symbol-function 'line-number-at-pos)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        (doom-modeline-segment--buffer-position)
        (doom-modeline-segment--buffer-position)
        (should (= calls 1))
        (should (= doom-modeline--total-lines 4))
        (insert "d\n")
        (doom-modeline-segment--buffer-position)
        (should (= calls 2))
        (should (= doom-modeline--total-lines 5))))))

(ert-deftest doom-modeline-state-icon/reads-cache-on-redisplay ()
  "The buffer state icon is computed lazily and reused across redisplays."
  (with-temp-buffer
    (setq-local buffer-read-only t)
    (setq doom-modeline--buffer-state-icon-cache nil)
    (doom-modeline--buffer-state-icon)
    (let ((cached doom-modeline--buffer-state-icon-cache))
      (should cached)
      ;; A redisplay must not recompute: the cached object is reused as-is.
      (doom-modeline--buffer-state-icon)
      (should (eq cached doom-modeline--buffer-state-icon-cache)))))

(ert-deftest doom-modeline-state-icon/update-refreshes-cache ()
  "The update function writes the cache for the current buffer."
  (with-temp-buffer
    (setq doom-modeline--buffer-state-icon-cache nil)
    (doom-modeline-update-buffer-file-state-icon)
    (should doom-modeline--buffer-state-icon-cache)))

(provide 'doom-modeline-segments-perf-test)
;;; doom-modeline-segments-perf-test.el ends here
