;;; check-segment.el --- Benchmark the check segment -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;;  Measures how long the `check' segment takes, both on the redisplay
;;  path and end to end, for the Flymake and Flycheck backends.
;;
;;  Usage:
;;
;;    emacs -Q --batch -L . -l bench/check-segment.el
;;
;;  Environment variables:
;;
;;    DM_DIR         directory to load doom-modeline from (defaults to
;;                   the repository this file lives in)
;;    DM_N           number of diagnostics to generate (default 2000;
;;                   0 runs without any diagnostic)
;;    DM_ITERATIONS  iterations per measurement (default 5000)
;;
;;  To compare two revisions, byte-compile each into its own directory
;;  and run the script twice with DM_DIR pointing at each of them:
;;
;;    cp doom-modeline*.el /tmp/after/ && (cd /tmp/after && \
;;      emacs -Q --batch -L . -f batch-byte-compile doom-modeline-*.el)
;;    DM_DIR=/tmp/after emacs -Q --batch -l bench/check-segment.el
;;
;;  Flycheck is not a dependency of this repository, so its run stubs
;;  `flycheck-count-errors' with an O(n) equivalent.  The same stub is
;;  used on both sides of a before/after comparison.
;;
;;  Each measurement warms up on a tenth of the iterations and then
;;  runs with GC deferred, so the numbers are steady-state costs.

;;; Code:

(require 'cl-lib)
(require 'package)

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (root (expand-file-name ".." here)))
  (setq package-user-dir (expand-file-name "~/.emacs.d/elpa"))
  (package-initialize)
  ;; Prefer the requested sources over any installed doom-modeline.
  (setq load-path
        (cl-remove-if (lambda (dir) (string-match-p "/doom-modeline-[0-9]" dir))
                      load-path))
  (add-to-list 'load-path (or (getenv "DM_DIR") root)))

(require 'flymake)
(require 'nerd-icons)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

(defvar flycheck-mode)
(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)
(defvar flycheck-mode-menu-map)

(declare-function make-flymake--backend-state "ext:flymake")

(setq flycheck-mode-menu-map (make-sparse-keymap))

(defvar bench-diagnostics
  (string-to-number (or (getenv "DM_N") "2000"))
  "Number of diagnostics each backend reports.")

(defvar bench-iterations
  (string-to-number (or (getenv "DM_ITERATIONS") "5000"))
  "Iterations per measurement.")

;;; Helpers

(defun bench--time (label thunk &optional n)
  "Print how long THUNK takes on average over N iterations."
  (let* ((n (or n bench-iterations))
         (warm (max 1 (/ n 10)))
         (i 0))
    (while (< i warm)
      (funcall thunk)
      (setq i (1+ i)))
    (let ((gc-cons-threshold most-positive-fixnum)
          (start (float-time)))
      (setq i 0)
      (while (< i n)
        (funcall thunk)
        (setq i (1+ i)))
      (princ (format "  %-44s %9.2f us\n" label
                     (/ (* 1000000 (- (float-time) start)) n))))))

(defun bench--flymake-state (n)
  "Return a Flymake state with N diagnostics."
  (let ((state (if (fboundp 'flymake--make-backend-state)
                   (flymake--make-backend-state)
                 (make-flymake--backend-state)))
        (states (make-hash-table :test #'eq)))
    (oset state running t)
    (oset state reported-p t)
    (oset state diags
          (cl-loop for j below n
                   collect (flymake-make-diagnostic
                            (current-buffer) 1 2
                            (if (= 0 (mod j 3)) :error :warning)
                            "msg")))
    (puthash 'backend state states)
    states))

(defun bench--flymake-report ()
  "Simulate a Flymake report and update the segment."
  (when (fboundp 'doom-modeline--flymake-invalidate)
    (doom-modeline--flymake-invalidate))
  (doom-modeline-update-flymake))

(defun bench--flycheck-count-errors (errors)
  "Count the fake ERRORS the way `flycheck-count-errors' would."
  (let ((counts (make-hash-table :test #'eq)))
    (dolist (level errors)
      (puthash level (1+ (gethash level counts 0)) counts))
    (let (result)
      (maphash (lambda (key value) (push (cons key value) result)) counts)
      result)))

(defun bench--flycheck-errors (n)
  "Return N fake Flycheck errors."
  (cl-loop for j below n
           collect (if (= 0 (mod j 3)) :error :warning)))

(defun bench--redisplay ()
  "Evaluate the check segment the way a redisplay does."
  (doom-modeline-segment--check))

(defun bench--inactive-redisplay ()
  "Evaluate the check segment for an inactive window."
  (let ((doom-modeline-current-window nil))
    (doom-modeline-segment--check)))

;;; Runs

(defun bench--flymake ()
  "Benchmark the check segment fed by Flymake."
  (with-temp-buffer
    (setq-local flymake-mode t)
    (setq-local flymake--state (bench--flymake-state bench-diagnostics))
    (dolist (format '(simple full))
      (setq-local doom-modeline-check format)
      (bench--flymake-report)
      (princ (format "\nFlymake, %s format\n" format))
      (bench--time "redisplay, active window" #'bench--redisplay)
      (bench--time "redisplay, inactive window" #'bench--inactive-redisplay)
      (bench--time "one report (update + redisplay)"
                   (lambda ()
                     (bench--flymake-report)
                     (bench--redisplay))))))

(defun bench--flycheck ()
  "Benchmark the check segment fed by Flycheck."
  (cl-letf (((symbol-function 'flycheck-count-errors)
             #'bench--flycheck-count-errors)
            ((symbol-function 'flycheck-error-level-compilation-level)
             (lambda (level) (pcase level (:error 2) (:warning 1) (_ 0)))))
    (with-temp-buffer
      (setq-local flycheck-mode t)
      (setq-local flycheck-last-status-change 'finished)
      (setq-local flycheck-current-errors (bench--flycheck-errors bench-diagnostics))
      (dolist (format '(simple full))
        (setq-local doom-modeline-check format)
        (doom-modeline-update-flycheck)
        (princ (format "\nFlycheck, %s format\n" format))
        (bench--time "redisplay, active window" #'bench--redisplay)
        (bench--time "redisplay, inactive window" #'bench--inactive-redisplay)
        (bench--time "window switch (update + redisplay)"
                     (lambda ()
                       (doom-modeline-update-flycheck)
                       (bench--redisplay)))
        (bench--time "one report (update + redisplay)"
                     (lambda ()
                       (setq-local flycheck-current-errors
                                   (bench--flycheck-errors bench-diagnostics))
                       (doom-modeline-update-flycheck 'finished)
                       (bench--redisplay)))))))

;;; Entry point

(princ (format "doom-modeline check segment benchmark\n"))
(princ (format "  Emacs %s\n" emacs-version))
(princ (format "  sources: %s\n"
               (or (symbol-file 'doom-modeline-update-flymake 'defun)
                   "unknown")))
(princ (format "  diagnostics: %d, iterations: %d\n"
               bench-diagnostics bench-iterations))

(bench--flymake)
(bench--flycheck)

(provide 'check-segment)
;;; check-segment.el ends here
