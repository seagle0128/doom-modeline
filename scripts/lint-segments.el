;;; lint-segments.el --- Guard the mode-line redisplay path -*- lexical-binding: t; -*-

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
;;  Static guard for the mode-line redisplay path.  A segment function runs
;;  on every redisplay, so expensive scans and external queries must live in
;;  a cached helper (`doom-modeline-update-*' plus a `doom-modeline--*'
;;  cache) and never directly in the segment body.
;;
;;  This script parses the source with the Lisp reader (not regexps) and
;;  reports every `doom-modeline-def-segment' whose BODY directly calls one
;;  of the functions in `lint-dangerous-calls'.  Helper definitions are not
;;  inspected -- that is exactly where the expensive work is allowed to live.
;;
;;  Usage:
;;
;;    emacs -Q --batch -l scripts/lint-segments.el -- doom-modeline-segments.el
;;    emacs -Q --batch -l scripts/lint-segments.el -- FILE [FILE ...]
;;
;;  Environment:
;;
;;    LINT_FORMAT  'text' (default) or 'json'
;;
;;  Exit status is 0 when clean, 1 when any violation is found, 2 on a
;;  parse/read error.  The CI job fails on a non-zero status.

;;; Code:

(require 'cl-lib)
(require 'json)

(defvar lint-dangerous-calls
  '(count-words count-lines line-number-at-pos how-many
    file-exists-p file-truename file-size file-remote-p abbreviate-file-name
    shell-command-to-string call-process process-lines start-process make-process
    project-current projectile-project-root)
  "Functions that must not be called directly from a segment body.
They perform whole-buffer scans, file I/O, subprocesses or project lookups
and would therefore run on every redisplay.  Wrap them in a cached helper
instead.")

(defvar lint-exempt-segments
  '(check lsp)
  "Segments intentionally excluded from this guard.")

(defvar lint-format (or (getenv "LINT_FORMAT") "text")
  "Output format: \"text\" or \"json\".")

(defvar lint--violations nil
  "Accumulated violations as plists :file :segment :line :symbol.")

(defun lint--guarded-by-cache-p (form)
  "Non-nil when FORM is the lazy-init guard `(unless CACHE ...)' or
`(when (not CACHE) ...)' around a cached computation.
Such a call only runs on cache miss and is therefore acceptable."
  (and (consp form)
       (memq (car form) '(unless when if))
       (let ((test (cadr form)))
         (or (and (symbolp test)
                  (string-prefix-p "doom-modeline--" (symbol-name test)))
             (and (consp test)
                  (eq (car test) 'not)
                  (symbolp (cadr test))
                  (string-prefix-p "doom-modeline--"
                                   (symbol-name (cadr test))))))))

(defun lint--called-symbols (form)
  "Return the dangerous symbols called anywhere in FORM.
Only the car of a cons is considered a call position.  Quoted data that
merely mentions a symbol is not a call and is ignored.  Calls inside a
lazy-init form guarded by a `doom-modeline--' cache are allowed."
  (let (found)
    (cl-labels
        ((walk (x quoted guarded)
           (cond
            ((consp x)
             (let ((head (car x)))
               ;; (quote X) / (function X) do not evaluate X.
               (cond
                ((memq head '(quote function))
                 nil)
                ((and (symbolp head)
                      (memq head lint-dangerous-calls)
                      (not quoted)
                      (not guarded))
                 (push head found)))
               (walk head quoted guarded)
               ;; Guard applies to the whole form once we are inside a
               ;; lazy-init conditional; recurse into cdr safely (it may be
               ;; a dotted pair such as (beg . end)).
               (let ((g (or guarded (lint--guarded-by-cache-p x))))
                 (walk (cdr x) quoted g))))
            (t nil))))
      (walk form nil nil))
    (delete-dups found)))

(defun lint--check-file (file)
  "Parse FILE and record violations in `lint--violations'."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (skip-chars-forward " \t\n\r\f")
        (if (eobp)
            (setq done t)
          (let* ((start-line (line-number-at-pos))
                 (form (condition-case err
                           (read (current-buffer))
                         (end-of-file (setq done t) nil)
                         (error (princ (format "LINT-ERROR %s:%d %s\n"
                                               file start-line
                                               (error-message-string err)))
                                (setq done t)
                                nil))))
            (when (and form
                       (consp form)
                       (eq (car form) 'doom-modeline-def-segment))
              (let ((name (cadr form)))
                (unless (memq name lint-exempt-segments)
                  (dolist (sym (lint--called-symbols (cddr form)))
                    (push (list :file file :segment name
                                :line start-line :symbol sym)
                          lint--violations)))))))))))

(defun lint--report ()
  "Print the accumulated violations and set the process exit status."
  (setq lint--violations (nreverse lint--violations))
  (if (equal lint-format "json")
      (princ (json-encode
              (list (cons :violations
                          (mapcar (lambda (v)
                                    (list (cons :file (plist-get v :file))
                                          (cons :segment (plist-get v :segment))
                                          (cons :line (plist-get v :line))
                                          (cons :symbol (plist-get v :symbol))))
                                  lint--violations)))))
    (progn
      (princ (format "segment redisplay-path lint: %d violation(s)\n"
                     (length lint--violations)))
      (dolist (v lint--violations)
        (princ (format "  %s:%d  %s  ->  %s\n"
                       (plist-get v :file)
                       (plist-get v :line)
                       (plist-get v :segment)
                       (plist-get v :symbol))))))
  (when lint--violations
    (kill-emacs 1)))

;;; Entry point

(let* ((args (cl-remove-if (lambda (a) (string= a "--"))
                           command-line-args-left))
       (files (or args
                  (list (expand-file-name "../doom-modeline-segments.el"
                                          (file-name-directory
                                           (or load-file-name buffer-file-name)))))))
  (dolist (file files)
    (lint--check-file file))
  (lint--report))

(provide 'lint-segments)
;;; lint-segments.el ends here
