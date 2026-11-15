;;; doom-modeline-flymake-test.el --- Tests for the Flymake check segment -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2023 Vincent Zhang

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
;;  Tests for the Flymake check segment.
;;

;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

;;; Helpers

(defun doom-modeline-test--diag (type)
  "Create a Flymake diagnostic of TYPE in the current buffer."
  (flymake-make-diagnostic (current-buffer) 1 2 type "dummy"))

(defun doom-modeline-test--backend-state (diags)
  "Return a reporting Flymake backend state holding DIAGS.

The constructor has been renamed across Emacs versions, so probe
for both of them."
  (let ((state (cond ((fboundp 'flymake--make-backend-state)
                      (flymake--make-backend-state))
                     ((fboundp 'make-flymake--backend-state)
                      (make-flymake--backend-state))
                     (t (ert-skip "Unsupported Flymake version")))))
    (oset state running t)
    (oset state reported-p t)
    (oset state diags diags)
    state))

(defun doom-modeline-test--call-with-diags (diags fn)
  "Call FN with a Flymake backend state holding DIAGS."
  (let ((states (make-hash-table :test #'eq)))
    (puthash 'backend (doom-modeline-test--backend-state diags) states)
    (with-temp-buffer
      (setq-local flymake-mode t)
      (setq-local flymake--state states)
      (funcall fn))))

;;; Tests

(ert-deftest doom-modeline-flymake/counts-by-severity ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error)
         (doom-modeline-test--diag :warning)
         (doom-modeline-test--diag :note)
         (doom-modeline-test--diag (intern "untyped")))
   (lambda ()
     (should (equal (doom-modeline--flymake-count-errors)
                    '((note . 1) (warning . 1) (error . 2)))))))

(ert-deftest doom-modeline-flymake/count-errors-looks-up-severity-once-per-type ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error)
         (doom-modeline-test--diag :warning)
         (doom-modeline-test--diag :error)
         (doom-modeline-test--diag :warning)
         (doom-modeline-test--diag :note))
   (lambda ()
     (let ((calls 0))
       (cl-letf (((symbol-function 'flymake--lookup-type-property)
                  (lambda (&rest _) (cl-incf calls) 3)))
         (doom-modeline--flymake-count-errors)
         (should (= calls 3)))))))

(ert-deftest doom-modeline-flymake/reuse-counts-until-invalidated ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error))
   (lambda ()
     (doom-modeline-update-flymake)
     (let ((calls 0))
       (cl-letf (((symbol-function 'doom-modeline--flymake-count-errors)
                  (lambda ()
                    (cl-incf calls)
                    '((note . 0) (warning . 0) (error . 1)))))
         (doom-modeline-update-flymake)
         (should (= calls 0)))))))

(ert-deftest doom-modeline-flymake/reuses-rendered-string ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error))
   (lambda ()
     (doom-modeline-update-flymake)
     (let ((seg doom-modeline--flymake))
       (doom-modeline-update-flymake)
       (should (eq seg doom-modeline--flymake))))))

(ert-deftest doom-modeline-flymake/invalidate-forces-recount ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error))
   (lambda ()
     (doom-modeline-update-flymake)
     (let ((calls 0))
       (cl-letf (((symbol-function 'doom-modeline--flymake-count-errors)
                  (lambda ()
                    (cl-incf calls)
                    '((note . 0) (warning . 0) (error . 1)))))
         (doom-modeline--flymake-invalidate)
         (doom-modeline-update-flymake)
         (should (= calls 1)))))))

(ert-deftest doom-modeline-flymake/report-invalidates ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error))
   (lambda ()
     (doom-modeline-update-flymake)
     (let ((calls 0))
       (cl-letf (((symbol-function 'doom-modeline--flymake-count-errors)
                  (lambda ()
                    (cl-incf calls)
                    '((note . 0) (warning . 0) (error . 1)))))
         (doom-modeline--flymake-report)
         (should (= calls 1)))))))

(ert-deftest doom-modeline-flymake/disable-mode-clears-segment ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error))
   (lambda ()
     (doom-modeline-update-flymake)
     (should doom-modeline--flymake)
     (setq flymake-mode nil)
     (doom-modeline-update-flymake)
     (should-not doom-modeline--flymake))))

(ert-deftest doom-modeline-flymake/rebuild-on-width-change ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error))
   (lambda ()
     (let ((doom-modeline-check 'auto)
           (doom-modeline--limited-width-p nil))
       (doom-modeline-update-flymake)
       (let ((full (substring-no-properties doom-modeline--flymake)))
         (setq doom-modeline--limited-width-p t)
         (doom-modeline-update-flymake)
         (should-not (string= full (substring-no-properties doom-modeline--flymake))))))))

(ert-deftest doom-modeline-flymake/reuse-check-icons ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error))
   (lambda ()
     (let ((doom-modeline-check 'simple))
       (doom-modeline-update-flymake)
       (let ((calls 0))
         (cl-letf (((symbol-function 'doom-modeline--flymake-count-errors)
                    (lambda () '((note . 0) (warning . 0) (error . 2))))
                   ((symbol-function 'doom-modeline-check-icon)
                    (lambda (&rest _) (cl-incf calls) "")))
           (doom-modeline--flymake-invalidate)
           (doom-modeline-update-flymake)
           (should (zerop calls))))))))

;;; doom-modeline-flymake-test.el ends here
