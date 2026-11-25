;;; segments-perf.el --- Benchmark the perf-sensitive segments -*- lexical-binding: t; -*-

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
;;  Measures the redisplay-path cost of every segment whose data acquisition
;;  was optimised (the P0/P1 work): word count, total line number, file-state
;;  icon, default directory, remote host and selection info.
;;
;;  For each segment two scenarios are measured:
;;
;;    hot   - consecutive redisplay calls, i.e. the cache is warm.
;;    cold  - caches are invalidated before every call, forcing a re-scan.
;;
;;  A revision without caching (the "before" side) shows roughly equal hot
;;  and cold numbers; a cached revision ("after") shows a much cheaper hot
;;  path while cold stays comparable to before.
;;
;;  Usage:
;;
;;    emacs -Q --batch -L . -l bench/segments-perf.el
;;
;;  To compare two revisions, point DM_DIR at each source tree and redirect:
;;
;;    DM_DIR=/path/to/before DM_FORMAT=tsv emacs -Q --batch \
;;      -l bench/segments-perf.el > before.tsv
;;    DM_DIR=/path/to/after  DM_FORMAT=tsv emacs -Q --batch \
;;      -l bench/segments-perf.el > after.tsv
;;
;;  Environment variables:
;;
;;    DM_DIR          directory to load doom-modeline from (default: repo root)
;;    DM_ITERATIONS   hot-path iterations per measurement (default 2000)
;;    DM_LINES        synthetic buffer size in lines (default 5000)
;;    DM_FORMAT       'text' (default) or 'tsv'
;;    DM_SMOKE        '1' also runs a smoke pass over every segment (default off)
;;
;;  Each measurement warms up on a tenth of the iterations and runs with GC
;;  deferred, so the numbers are steady-state redisplay costs.

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

(require 'doom-modeline-core)
(require 'doom-modeline-segments)

(defvar dm-perf-iterations
  (string-to-number (or (getenv "DM_ITERATIONS") "2000"))
  "Iterations per hot-path measurement.")

(defvar dm-perf-lines
  (string-to-number (or (getenv "DM_LINES") "5000"))
  "Number of lines in the synthetic benchmark buffer.")

(defvar dm-perf-format (or (getenv "DM_FORMAT") "text")
  "Output format: \"text\" or \"tsv\".")

(defvar dm-perf-cold-iterations
  (max 100 (/ dm-perf-iterations 10))
  "Iterations per cold-path measurement (a full re-scan each time).")

(defvar dm-perf--label-width 26)

;;; Helpers

(defun dm-perf--emit (segment scenario us)
  "Print one measurement for SEGMENT/SCENARIO taking US microseconds."
  (if (equal dm-perf-format "tsv")
      (princ (format "%s\t%s\t%.3f\n" segment scenario us))
    (princ (format "  %s%s %s %10.2f us\n"
                   segment
                   (make-string (max 1 (- dm-perf--label-width (length segment))) ?\s)
                   (concat scenario
                           (make-string (max 1 (- 7 (length scenario))) ?\s))
                   us))))

(defun dm-perf--time (thunk &optional n)
  "Return microseconds per call of THUNK over N iterations."
  (let* ((n (or n dm-perf-iterations))
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
      (/ (* 1000000.0 (- (float-time) start)) n))))

(defun dm-perf--invalidate ()
  "Invalidate every perf cache introduced by the P0/P1 work.
On a revision without these caches this is a harmless no-op, which keeps
the before/after measurement setup identical."
  (dolist (v '(doom-modeline--word-count
               doom-modeline--total-lines
               doom-modeline--buffer-state-icon-cache
               doom-modeline--default-directory-cache
               doom-modeline--selection-cache
               doom-modeline--selection-word-count))
    (when (and (boundp v) (local-variable-p v))
      (set (make-local-variable v) nil)))
  (when (boundp 'doom-modeline--workspace-keymap-cache)
    (setq doom-modeline--workspace-keymap-cache nil)))

(defun dm-perf--measure (segment thunk)
  "Measure SEGMENT via THUNK in the hot and cold scenarios."
  (dm-perf--emit segment "hot" (dm-perf--time thunk dm-perf-iterations))
  (dm-perf--emit segment "cold"
                 (dm-perf--time (lambda ()
                                  (dm-perf--invalidate)
                                  (funcall thunk))
                                dm-perf-cold-iterations)))

(defun dm-perf--make-buffer ()
  "Return a synthetic buffer with `dm-perf-lines' lines of prose."
  (let ((buf (generate-new-buffer " *dm-perf*")))
    (with-current-buffer buf
      (text-mode)
      (dotimes (_ dm-perf-lines)
        (insert "the quick brown fox jumps over the lazy dog\n"))
      (goto-char (point-min)))
    buf))

(defun dm-perf--active ()
  "Non-nil when the mode line is considered active.
The benchmark forces this to t so segments do not short-circuit in batch."
  t)

;;; Scenarios

(defun dm-perf--word-count ()
  "Benchmark the word-count segment."
  (with-temp-buffer
    (text-mode)
    (setq-local doom-modeline-enable-word-count t)
    (setq-local doom-modeline-continuous-word-count-modes '(text-mode))
    (dotimes (_ dm-perf-lines)
      (insert "one two three four five six seven\n"))
    (cl-letf (((symbol-function 'doom-modeline--active) #'dm-perf--active))
      (dm-perf--measure "word-count"
                        #'doom-modeline-segment--word-count))))

(defun dm-perf--buffer-position ()
  "Benchmark the buffer-position segment (total line number part)."
  (with-temp-buffer
    (text-mode)
    (setq-local doom-modeline-total-line-number t)
    (dotimes (_ dm-perf-lines)
      (insert "line of text for counting\n"))
    (cl-letf (((symbol-function 'doom-modeline--active) #'dm-perf--active))
      (dm-perf--measure "buffer-position"
                        #'doom-modeline-segment--buffer-position))))

(defun dm-perf--state-icon ()
  "Benchmark the file-state icon (P0 pseudo-cache fix)."
  (with-temp-buffer
    (setq-local doom-modeline-buffer-state-icon t)
    (setq-local doom-modeline-icon t)
    (cl-letf (((symbol-function 'doom-modeline--active) #'dm-perf--active))
      ;; The helper is the hot spot shared by buffer-info and friends.
      (dm-perf--measure "state-icon"
                        #'doom-modeline--buffer-state-icon))))

(defun dm-perf--default-directory ()
  "Benchmark the default-directory segments with a TRAMP-like path."
  (with-temp-buffer
    (setq-local default-directory "/ssh:user@example.com:/very/long/remote/path/to/project/")
    (setq-local doom-modeline-buffer-default-directory-icon nil)
    (cl-letf (((symbol-function 'doom-modeline--active) #'dm-perf--active))
      (dm-perf--measure "default-directory"
                        #'doom-modeline-segment--buffer-default-directory)
      (dm-perf--measure "default-directory-simple"
                        #'doom-modeline-segment--buffer-default-directory-simple))))

(defun dm-perf--remote-host ()
  "Benchmark the remote-host segment with a TRAMP-like path."
  (with-temp-buffer
    (setq-local default-directory "/ssh:user@example.com:/remote/path/")
    (setq-local doom-modeline-remote-host t)
    (cl-letf (((symbol-function 'doom-modeline--active) #'dm-perf--active))
      (dm-perf--measure "remote-host"
                        #'doom-modeline-segment--remote-host))))

(defun dm-perf--selection-info ()
  "Benchmark the selection-info segment over a large active region."
  (with-temp-buffer
    (text-mode)
    (setq-local doom-modeline-selection-info t)
    (setq-local doom-modeline-enable-word-count t)
    (dotimes (_ dm-perf-lines)
      (insert "one two three four five six seven\n"))
    (goto-char (point-min))
    (set-mark (point))
    (goto-char (point-max))
    (setq mark-active t)
    (cl-letf (((symbol-function 'doom-modeline--active) #'dm-perf--active))
      (dm-perf--measure "selection-info"
                        #'doom-modeline-segment--selection-info))))

(defun dm-perf--workspace-name ()
  "Benchmark the workspace-name segment with two tabs."
  (when (and (fboundp 'tab-bar-mode)
             (>= (length (frame-parameter nil 'tabs)) 2))
    (with-temp-buffer
      (setq-local doom-modeline-workspace-name t)
      (cl-letf (((symbol-function 'doom-modeline--active) #'dm-perf--active)
                ((symbol-function 'doom-modeline--limited-width-p)
                 (lambda () nil)))
        (dm-perf--measure "workspace-name"
                          #'doom-modeline-segment--workspace-name)))))

(defun dm-perf--smoke ()
  "Call every defined segment once, reporting which ones are runnable.
External-state segments (irc/k8s/gnus/...) that signal an error are skipped."
  (princ (if (equal dm-perf-format "tsv")
             "segment\trunnable\n"
           "\n  segment smoke pass:\n"))
  (let (names)
    (mapatoms (lambda (sym)
                (when (and (fboundp sym)
                           (string-prefix-p "doom-modeline-segment--"
                                            (symbol-name sym)))
                  (push (symbol-name sym) names))))
    (dolist (name (sort names #'string<))
      (let* ((runnable
              (condition-case nil
                  (progn (funcall (intern name)) t)
                (error nil)))
             (short (substring name (length "doom-modeline-segment--")))
             (pad (make-string (max 1 (- dm-perf--label-width
                                        (length short))) ?\s)))
        (if (equal dm-perf-format "tsv")
            (princ (format "%s\t%s\n" name (if runnable "yes" "no")))
          (princ (format "  %s%s %s\n"
                         short pad (if runnable "ok" "skip"))))))))

;;; Entry point

;; Local Variables:
;; no-byte-compile: t
;; End:

(princ (if (equal dm-perf-format "tsv")
           "segment\tscenario\tus\n"
         (format "doom-modeline segments benchmark\n  Emacs %s\n  sources: %s\n  iterations: %d (cold %d), lines: %d\n\n"
                 emacs-version
                 (or (locate-library "doom-modeline-segments") "unknown")
                 dm-perf-iterations dm-perf-cold-iterations dm-perf-lines)))

(dm-perf--word-count)
(dm-perf--buffer-position)
(dm-perf--state-icon)
(dm-perf--default-directory)
(dm-perf--remote-host)
(dm-perf--selection-info)
(dm-perf--workspace-name)

(when (equal (getenv "DM_SMOKE") "1")
  (dm-perf--smoke))

(provide 'segments-perf)
;;; segments-perf.el ends here
