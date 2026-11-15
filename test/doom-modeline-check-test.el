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
;;  Tests for the check segment.
;;

;;; Code:

(require 'cl-lib)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

;;; Helpers

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

(defun doom-modeline-test--with-check (seg active fn)
  "Call FN in a buffer whose check segment is SEG.
ACTIVE is what `doom-modeline--active' should report."
  (with-temp-buffer
    (setq-local flymake-mode t)
    (setq-local doom-modeline--flymake seg)
    (cl-letf (((symbol-function 'doom-modeline--active)
               (lambda () active)))
      (funcall fn))))

(defun doom-modeline-test--check-seg (text)
  "Build a check segment string holding TEXT."
  (propertize text
              'help-echo "Flymake\nerror: 1, warning: 2, note: 3"
              'mouse-face 'doom-modeline-highlight
              'local-map (make-sparse-keymap)))

;;; Tests

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
