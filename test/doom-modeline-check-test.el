;;; doom-modeline-check-test.el --- Tests for the check segment -*- lexical-binding: t; -*-

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
;;  Tests for the check segment: the cached wrapper, the Flymake path
;;  and the Flycheck path.  Flycheck is not a test dependency, so the
;;  parts the segment reads are stubbed out.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'flymake)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

;;; Helpers

;; Flymake

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

;; Flycheck

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

;; Check segment

(defun doom-modeline-test--check-seg (text)
  "Build a check segment string holding TEXT."
  (propertize text
              'help-echo "Flymake\nerror: 1, warning: 2, note: 3"
              'mouse-face 'doom-modeline-highlight
              'local-map (make-sparse-keymap)))

(defun doom-modeline-test--with-check (seg active fn)
  "Call FN in a buffer whose check segment is SEG.
ACTIVE is what `doom-modeline--active' should report."
  (with-temp-buffer
    (setq-local flymake-mode t)
    (setq-local doom-modeline--flymake seg)
    (cl-letf (((symbol-function 'doom-modeline--active)
               (lambda () active)))
      (funcall fn))))

(defun doom-modeline-test--strip-map (str)
  "Return STR without its `local-map' property."
  (when str
    (let ((copy (copy-sequence str)))
      (remove-text-properties 0 (length copy) '(local-map nil) copy)
      copy)))

(defun doom-modeline-test--check-reference (seg)
  "Wrap SEG the way the check segment used to, as a reference.
Kept verbatim so the cached implementation can be compared against
it."
  (when seg
    (let ((sep (doom-modeline-spc))
          (vsep (doom-modeline-vspc))
          (str))
      (dolist (s (split-string seg " "))
        (setq str
              (concat str
                      (if (string-match-p "^[0-9]+$" s)
                          (concat vsep
                                  (doom-modeline-display-text s)
                                  vsep)
                        (doom-modeline-display-icon s)))))
      (concat sep
              (propertize str
                          'help-echo (get-text-property 0 'help-echo seg)
                          'mouse-face 'doom-modeline-highlight
                          'local-map (get-text-property 0 'local-map seg))
              sep))))

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

;;; Flymake tests

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

(ert-deftest doom-modeline-flymake/reuses-keymap ()
  (doom-modeline-test--call-with-diags
   (list (doom-modeline-test--diag :error))
   (lambda ()
     (doom-modeline-update-flymake)
     (let ((map (get-text-property 0 'local-map doom-modeline--flymake)))
       (should (keymapp map))
       (should (eq map (doom-modeline--check-keymap 'flymake)))
       (should (eq (lookup-key map [mode-line down-mouse-1]) flymake-menu))))))

;;; Flycheck tests

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

;;; Check segment tests

(ert-deftest doom-modeline-check/reuse-wrapped-segment ()
  "The wrapped segment is cached while its inputs do not change."
  (doom-modeline-test--with-check (doom-modeline-test--check-seg "icon 3") t
    (lambda ()
      (let ((first (doom-modeline-segment--check)))
        (should first)
        (should (eq first (doom-modeline-segment--check)))))))

(ert-deftest doom-modeline-check/rebuild-on-new-segment ()
  "A new diagnostic string must not be masked by the cache."
  (doom-modeline-test--with-check (doom-modeline-test--check-seg "icon 3") t
    (lambda ()
      (let ((first (doom-modeline-segment--check)))
        (setq-local doom-modeline--flymake (doom-modeline-test--check-seg "icon 5"))
        (let ((second (doom-modeline-segment--check)))
          (should (not (eq first second)))
          (should (string-match-p "5" (substring-no-properties second)))
          (should-not (string-match-p "3" (substring-no-properties second))))))))

(ert-deftest doom-modeline-check/rebuild-on-active-change ()
  "The active and inactive renderings are cached separately."
  (doom-modeline-test--with-check (doom-modeline-test--check-seg "icon 3") nil
    (lambda ()
      (let ((inactive (doom-modeline-segment--check)))
        (cl-letf (((symbol-function 'doom-modeline--active) (lambda () t)))
          (let ((active (doom-modeline-segment--check)))
            (should (not (equal inactive active)))
            (should (equal active
                           (doom-modeline-test--check-reference
                            doom-modeline--flymake)))))
        (cl-letf (((symbol-function 'doom-modeline--active) (lambda () nil)))
          (should (equal inactive
                         (doom-modeline-test--check-reference
                          doom-modeline--flymake))))))))

(ert-deftest doom-modeline-check/matches-reference ()
  "Cached output must be identical to the uncached implementation."
  (dolist (text '("icon 3" "icon 1 icon 2 icon 3" ""))
    (dolist (active '(t nil))
      (doom-modeline-test--with-check (doom-modeline-test--check-seg text) active
        (lambda ()
          (should (equal (doom-modeline-segment--check)
                         (doom-modeline-test--check-reference
                          doom-modeline--flymake))))))))

(ert-deftest doom-modeline-check/nil-segment-stays-nil ()
  "No diagnostics source means no segment, even after a cache hit."
  (doom-modeline-test--with-check nil t
    (lambda ()
      (should-not (doom-modeline-segment--check))
      (setq-local doom-modeline--flymake (doom-modeline-test--check-seg "icon 1"))
      (should (doom-modeline-segment--check))
      (setq-local doom-modeline--flymake nil)
      (should-not (doom-modeline-segment--check)))))

(ert-deftest doom-modeline-check/cache-is-buffer-local ()
  "Each buffer caches its own wrapped segment."
  (let ((a (doom-modeline-test--check-seg "icon 1"))
        (b (doom-modeline-test--check-seg "icon 2")))
    (doom-modeline-test--with-check a t
      (lambda ()
        (let ((rendered (doom-modeline-segment--check)))
          (doom-modeline-test--with-check b t
            (lambda ()
              (should (string-match-p
                       "2" (substring-no-properties (doom-modeline-segment--check))))))
          (should (eq rendered (doom-modeline-segment--check))))))))

;;; doom-modeline-check-test.el ends here
