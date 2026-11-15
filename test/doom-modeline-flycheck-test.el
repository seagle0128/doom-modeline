;;; doom-modeline-flycheck-test.el --- Tests for the Flycheck check segment -*- lexical-binding: t; -*-

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
;;  Tests for the Flycheck check segment.  Flycheck is not a test
;;  dependency, so the parts the segment reads are stubbed out.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

;;; Helpers

(defun doom-modeline-test--flycheck-count-errors (errors)
  "Mimic `flycheck-count-errors' for the fake ERRORS list."
  (let (counts)
    (dolist (level '(:error :warning :info))
      (let ((n (cl-count level errors)))
        (when (> n 0)
          (push (cons level n) counts))))
    counts))

(defun doom-modeline-test--with-flycheck (errors status fn)
  "Call FN with a fake Flycheck state holding ERRORS and STATUS."
  (with-temp-buffer
    (setq-local flycheck-mode t)
    (setq-local flycheck-last-status-change status)
    (setq-local flycheck-current-errors errors)
    (setq-local flycheck-mode-menu-map (make-sparse-keymap))
    (cl-letf (((symbol-function 'flycheck-count-errors)
               #'doom-modeline-test--flycheck-count-errors)
              ((symbol-function 'flycheck-error-level-compilation-level)
               (lambda (level) (pcase level (:error 2) (:warning 1) (_ 0)))))
      (funcall fn))))

(defun doom-modeline-test--with-active (active fn)
  "Call FN with `doom-modeline--active' reporting ACTIVE."
  (cl-letf (((symbol-function 'doom-modeline--active) (lambda () active)))
    (funcall fn)))

(defun doom-modeline-test--strip-map (str)
  "Return STR without its `local-map' property."
  (when str
    (let ((copy (copy-sequence str)))
      (remove-text-properties 0 (length copy) '(local-map nil) copy)
      copy)))

(defun doom-modeline-test--flycheck-reference (status)
  "Render the Flycheck segment the way the previous implementation did."
  (let-alist (let ((info 0) (warning 0) (error 0))
               (mapc
                (lambda (item)
                  (let ((count (cdr item)))
                    (pcase (flycheck-error-level-compilation-level (car item))
                      (0 (cl-incf info count))
                      (1 (cl-incf warning count))
                      (2 (cl-incf error count)))))
                (flycheck-count-errors flycheck-current-errors))
               `((info . ,info) (warning . ,warning) (error . ,error)))
    (let* ((vsep (doom-modeline-vspc))
           (seg (cond
                 ((or (eq doom-modeline-check 'simple)
                      (and doom-modeline--limited-width-p
                           (eq doom-modeline-check 'auto)))
                  (let ((count (+ .error .warning .info)))
                    (pcase status
                      ('finished
                       (if (> count 0)
                           (let ((face (if (> .error 0)
                                           'doom-modeline-urgent
                                         'doom-modeline-warning)))
                             (concat
                              (doom-modeline-check-icon
                               "nf-md-alert_circle_outline" "⚠" "!" face)
                              vsep
                              (doom-modeline-check-text
                               (number-to-string count) face)))
                         (doom-modeline-check-icon
                          "nf-md-check_circle_outline" "✔" "*" 'doom-modeline-info)))
                      ('running
                       (concat
                        (doom-modeline-check-icon
                         "nf-md-timer_sand" "⏳" "*" 'doom-modeline-debug)
                        (when (> count 0)
                          (concat
                           vsep
                           (doom-modeline-check-text
                            (number-to-string count) 'doom-modeline-debug)))))
                      ('no-checker
                       (doom-modeline-check-icon
                        "nf-md-alert_box_outline" "⚠" "-" 'doom-modeline-debug))
                      ('errored
                       (doom-modeline-check-icon
                        "nf-md-alert_box_outline" "⚠" "!" 'doom-modeline-urgent))
                      ('interrupted
                       (doom-modeline-check-icon
                        "nf-md-pause_circle_outline" "⦷" "." 'doom-modeline-debug))
                      ('suspicious
                       (doom-modeline-check-icon
                        "nf-md-file_question_outline" "❓" "?" 'doom-modeline-debug))
                      (_ ""))))
                 ((or (eq doom-modeline-check 'full)
                      (and (not doom-modeline--limited-width-p)
                           (eq doom-modeline-check 'auto)))
                  (concat
                   (doom-modeline-check-icon
                    "nf-md-close_circle_outline" "⮾" "!" 'doom-modeline-urgent)
                   vsep
                   (doom-modeline-check-text
                    (number-to-string .error) 'doom-modeline-urgent)
                   vsep
                   (doom-modeline-check-icon
                    "nf-md-alert_outline" "⚠" "!" 'doom-modeline-warning)
                   vsep
                   (doom-modeline-check-text
                    (number-to-string .warning) 'doom-modeline-warning)
                   vsep
                   (doom-modeline-check-icon
                    "nf-md-information_outline" "🛈" "!" 'doom-modeline-info)
                   vsep
                   (doom-modeline-check-text
                    (number-to-string .info) 'doom-modeline-info)))
                 (t ""))))
      (propertize seg
                  'help-echo (concat
                              "Flycheck\n"
                              (pcase status
                                ('finished (format "error: %d, warning: %d, info: %d"
                                                   .error .warning .info))
                                ('running "Checking...")
                                ('no-checker "No Checker")
                                ('errored "Error")
                                ('interrupted "Interrupted")
                                ('suspicious "Suspicious"))
                              "\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode")
                  'mouse-face 'doom-modeline-highlight
                  'local-map (doom-modeline--check-keymap 'flycheck)))))

;;; Tests

(ert-deftest doom-modeline-flycheck/reuse-counts-until-errors-change ()
  "Diagnostics are not counted again while the error list is unchanged."
  (doom-modeline-test--with-flycheck '(:error :warning) 'finished
    (lambda ()
      (doom-modeline-update-flycheck)
      (let ((calls 0))
        (cl-letf (((symbol-function 'doom-modeline--flycheck-count-errors)
                   (lambda ()
                     (cl-incf calls)
                     '((info . 0) (warning . 1) (error . 1)))))
          (doom-modeline-update-flycheck)
          (should (= calls 0))
          (setq-local flycheck-current-errors '(:error :error))
          (doom-modeline-update-flycheck)
          (should (= calls 1)))))))

(ert-deftest doom-modeline-flycheck/reuses-rendered-string ()
  "Repeated updates with the same inputs return the same string."
  (doom-modeline-test--with-flycheck '(:error) 'finished
    (lambda ()
      (doom-modeline-update-flycheck)
      (let ((seg doom-modeline--flycheck))
        (should seg)
        (doom-modeline-update-flycheck)
        (should (eq seg doom-modeline--flycheck))))))

(ert-deftest doom-modeline-flycheck/rebuild-on-new-errors ()
  "A new error list is reflected in the segment."
  (doom-modeline-test--with-flycheck '(:error) 'finished
    (lambda ()
      (doom-modeline-update-flycheck)
      (let ((first (substring-no-properties doom-modeline--flycheck)))
        (setq-local flycheck-current-errors '(:error :error :warning))
        (doom-modeline-update-flycheck)
        (should-not (string= first
                             (substring-no-properties doom-modeline--flycheck)))
        (should (string-match-p "2" (substring-no-properties doom-modeline--flycheck)))))))

(ert-deftest doom-modeline-flycheck/rebuild-on-status-change ()
  "A status change updates the segment and its help-echo."
  (doom-modeline-test--with-flycheck '(:error) 'finished
    (lambda ()
      (doom-modeline-update-flycheck)
      (let ((finished doom-modeline--flycheck))
        (setq-local flycheck-last-status-change 'running)
        (doom-modeline-update-flycheck)
        (should-not (eq finished doom-modeline--flycheck))
        (should (string-match-p "Checking"
                                (get-text-property 0 'help-echo
                                                   doom-modeline--flycheck)))))))

(ert-deftest doom-modeline-flycheck/rebuild-on-width-change ()
  "`auto' adapts to the window width."
  (doom-modeline-test--with-flycheck '(:error) 'finished
    (lambda ()
      (let ((doom-modeline-check 'auto)
            (doom-modeline--limited-width-p nil))
        (doom-modeline-update-flycheck)
        (let ((full (substring-no-properties doom-modeline--flycheck)))
          (setq doom-modeline--limited-width-p t)
          (doom-modeline-update-flycheck)
          (should-not (string= full
                               (substring-no-properties doom-modeline--flycheck))))))))

(ert-deftest doom-modeline-flycheck/reuse-check-icons ()
  "Rendering a new report reuses the cached icons."
  (doom-modeline-test--with-flycheck '(:error) 'finished
    (lambda ()
      (let ((doom-modeline-check 'simple))
        (doom-modeline-update-flycheck)
        (let ((calls 0))
          (cl-letf (((symbol-function 'doom-modeline-check-icon)
                     (lambda (&rest _) (cl-incf calls) "")))
            (setq-local flycheck-current-errors '(:error :error))
            (doom-modeline-update-flycheck)
            (should (zerop calls))))))))

(ert-deftest doom-modeline-flycheck/reuses-menu-keymap ()
  "The mouse menu keymap is built once and shared."
  (doom-modeline-test--with-flycheck '(:error) 'finished
    (lambda ()
      (doom-modeline-update-flycheck)
      (let ((map (get-text-property 0 'local-map doom-modeline--flycheck)))
        (should (keymapp map))
        (setq-local flycheck-current-errors '(:error :error))
        (doom-modeline-update-flycheck)
        (should (eq map (get-text-property 0 'local-map doom-modeline--flycheck)))
        (should (eq map (doom-modeline--check-keymap 'flycheck)))))))

(ert-deftest doom-modeline-flycheck/disable-mode-clears-segment ()
  "Turning Flycheck off clears the cache."
  (doom-modeline-test--with-flycheck '(:error) 'finished
    (lambda ()
      (doom-modeline-update-flycheck)
      (should doom-modeline--flycheck)
      (setq flycheck-mode nil)
      (doom-modeline-update-flycheck)
      (should-not doom-modeline--flycheck)
      (should-not doom-modeline--flycheck-counts))))

(ert-deftest doom-modeline-flycheck/status-argument ()
  "A window passed by `window-state-change-functions' is ignored."
  (doom-modeline-test--with-flycheck '(:error) 'running
    (lambda ()
      (doom-modeline-update-flycheck (selected-window))
      (should (string-match-p "Checking"
                              (get-text-property 0 'help-echo
                                                 doom-modeline--flycheck)))
      (doom-modeline-update-flycheck 'finished)
      (should-not (string-match-p "Checking"
                                  (get-text-property 0 'help-echo
                                                     doom-modeline--flycheck))))))

(ert-deftest doom-modeline-flycheck/matches-reference ()
  "Cached output must match the previous implementation."
  (dolist (status '(finished running no-checker errored interrupted suspicious))
    (dolist (format '(simple full auto))
      (dolist (active '(t nil))
        (dolist (limited '(t nil))
          (doom-modeline-test--with-flycheck '(:error :warning :info) status
            (lambda ()
              (let ((doom-modeline-check format)
                    (doom-modeline--limited-width-p limited))
                (doom-modeline-test--with-active active
                  (lambda ()
                    (doom-modeline-update-flycheck)
                    (should (equal
                             (doom-modeline-test--strip-map doom-modeline--flycheck)
                             (doom-modeline-test--strip-map
                              (doom-modeline-test--flycheck-reference
                               flycheck-last-status-change))))))))))))))

;;; doom-modeline-flycheck-test.el ends here
