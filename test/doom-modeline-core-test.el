;;; doom-modeline-core-test.el --- Unit tests for doom-modeline -*- lexical-binding: t; -*-

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
;;  Unit tests for doom-modeline.
;;

;;; Code:

(require 'cl-lib)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

;; XXX: This is a precaution for older Emacsen that may ship with
;; outdated versions of `project'.
(load "project")

;; (ert-deftest doom-modeline-icon/octicon-icon ()
;;   (let ((doom-modeline-icon t)
;;         (doom-modeline-unicode-fallback t))
;;     ;; In TUI, fallback to unicode.
;;     (should
;;      (string= (substring-no-properties
;;                (doom-modeline-icon 'octicon "nf-oct-octoface" "☻" ":)" :face 'error))
;;               (nerd-icons-octicon "nf-oct-octoface")))))

(ert-deftest doom-modeline-icon/octicon-unicode ()
  (let ((doom-modeline-icon nil)
        (doom-modeline-unicode-fallback t))
    (should
     (string= (substring-no-properties
               (doom-modeline-icon 'octicon "nf-oct-octoface" "☻" ":)" :face 'warning))
              "☻"))))

(ert-deftest doom-modeline-icon/octicon-text ()
  (let ((doom-modeline-icon nil)
        (doom-modeline-unicode-fallback nil))
    (should
     (string= (substring-no-properties
               (doom-modeline-icon 'octicon "nf-oct-octoface" "☻" ":)" :face 'success))
              ":)"))))

(ert-deftest doom-modeline-project-root/auto ()
  ;; The latest `project' requires Emacs >= 26.1
  (skip-unless (>= emacs-major-version 26))
  (let ((default-directory "/home/user/project/")
        (doom-modeline-project-detection 'auto)
        (doom-modeline--project-root nil))
    (should (string= (doom-modeline-project-root) default-directory))))

(ert-deftest doom-modeline-project-root/auto-old ()
  ;; Old versions of project.el do not have `project-root'
  (skip-unless (< emacs-major-version 26))
  (let ((default-directory "/home/user/project-current/")
        (doom-modeline-project-detection 'auto)
        (doom-modeline--project-root nil))
    (should (string= (doom-modeline-project-root) default-directory))))

(ert-deftest doom-modeline-project-root/ffip ()
  (let ((default-directory "/home/user/project-ffip/")
        (doom-modeline-project-detection 'ffip)
        (doom-modeline--project-root nil))
    (cl-flet ((ffip-get-project-root-directory () default-directory))
      (should (string= (doom-modeline-project-root) default-directory)))))

(ert-deftest doom-modeline-project-root/projectile ()
  (let ((default-directory "/home/user/projectile/")
        (doom-modeline-project-detection 'projectile)
        (doom-modeline--project-root nil))
    (cl-flet ((projectile-project-root () default-directory))
      (should (string= (doom-modeline-project-root) default-directory)))))

(ert-deftest doom-modeline-project-root/project ()
  ;; The latest `project' requires Emacs >= 26.1
  (skip-unless (>= emacs-major-version 26))
  (let ((default-directory "/home/user/project-current/")
        (doom-modeline-project-detection 'project)
        (doom-modeline--project-root nil))
    (should (string= (doom-modeline-project-root) default-directory))))

(ert-deftest doom-modeline-project-root/project-old ()
  ;; Old versions of project.el do not have `project-root'
  (skip-unless (< emacs-major-version 26))
  (let ((default-directory "/home/user/project-current/")
        (doom-modeline-project-detection 'project)
        (doom-modeline--project-root nil))
    (should (string= (doom-modeline-project-root) default-directory))))

(ert-deftest doom-modeline-project-root/default ()
  (let ((default-directory "/home/user/project/")
        (doom-modeline-project-detection nil))
    (should (string= (doom-modeline-project-root) default-directory))))

(ert-deftest doom-modeline-buffer-file-name/invalid ()
  :expected-result :failed
  (let* ((default-directory "/home/user/project/")
         (buffer-file-name "/home/user/project/relative/test.txt")
         (buffer-file-truename "/home/user/project/relative/test.txt")
         (doom-modeline--project-root default-directory)
         (doom-modeline-buffer-file-name-style 'invalid))
    (cl-flet ((doom-modeline-project-p () t)
              (doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "test.txt")))))

(ert-deftest doom-modeline-buffer-file-name/auto-in-project ()
  (let* ((default-directory "/home/user/project/")
         (buffer-file-name "/home/user/project/relative/test.txt")
         (buffer-file-truename "/home/user/project/relative/test.txt")
         (doom-modeline--project-root default-directory)
         (doom-modeline-buffer-file-name-style 'auto))
    (cl-flet ((doom-modeline-project-p () t)
              (doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "project/r/test.txt")))))

(ert-deftest doom-modeline-buffer-file-name/auto-file-name ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'auto))
    (cl-flet ((doom-modeline-project-p () nil)
              (doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                (buffer-name))))))

(ert-deftest doom-modeline-buffer-file-name/file-name ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'file-name))
    (should
     (string= (substring-no-properties (doom-modeline-buffer-file-name))
              "test.txt"))))

(ert-deftest doom-modeline-buffer-file-name/buffer-name ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'buffer-name))
    (should
     (string= (substring-no-properties (doom-modeline-buffer-file-name))
              (buffer-name)))))

(ert-deftest doom-modeline-buffer-file-name/truncate-upto-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'truncate-upto-project))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "/h/u/project/relative/test.txt")))))

(ert-deftest doom-modeline-buffer-file-name/truncate-from-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'truncate-from-project))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "/home/user/project/r/test.txt")))))

(ert-deftest doom-modeline-buffer-file-name/truncate-with-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'truncate-with-project))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "project/r/test.txt")))))

(ert-deftest doom-modeline-buffer-file-name/truncate-except-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'truncate-except-project))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "/h/u/project/r/test.txt")))))

(ert-deftest doom-modeline-buffer-file-name/truncate-upto-root ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'truncate-upto-root))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string-suffix-p "/h/u/p/relative/test.txt"
                        (substring-no-properties (doom-modeline-buffer-file-name)))))))

(ert-deftest doom-modeline-buffer-file-name/truncate-all ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'truncate-all))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string-suffix-p "/h/u/p/r/test.txt"
                        (substring-no-properties (doom-modeline-buffer-file-name)))))))

(ert-deftest doom-modeline-buffer-file-name/truncate-nil ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'truncate-nil))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "/home/user/project/relative/test.txt")))))

(ert-deftest doom-modeline-buffer-file-name/relative-to-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'relative-to-project))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string-suffix-p "relative/test.txt"
                        (substring-no-properties (doom-modeline-buffer-file-name)))))))

(ert-deftest doom-modeline-buffer-file-name/relative-from-project ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'relative-from-project))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "project/relative/test.txt")))))

(ert-deftest doom-modeline--buffer-file-name/truncate-upto-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (substring-no-properties
               (doom-modeline--buffer-file-name file-path true-file-path 'shrink))
              "/h/u/project/relative/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name/truncate-from-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (substring-no-properties
               (doom-modeline--buffer-file-name file-path true-file-path nil 'shrink))
              "/home/user/project/r/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name/truncate-with-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (substring-no-properties
               (doom-modeline--buffer-file-name file-path true-file-path 'shrink 'shrink 'hide))
              "project/r/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name/truncate-except-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (substring-no-properties
               (doom-modeline--buffer-file-name file-path true-file-path 'shrink 'shrink))
              "/h/u/project/r/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name-truncate/truncate-upto-root ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path "/home/user/project/relative/test.txt"))
    (should
     (string= (substring-no-properties
               (doom-modeline--buffer-file-name-truncate file-path true-file-path))
              "/h/u/p/relative/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name-truncate/truncate-all ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path "/home/user/project/relative/test.txt"))
    (should
     (string= (substring-no-properties
               (doom-modeline--buffer-file-name-truncate file-path true-file-path t))
              "/h/u/p/r/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name/truncate-nil ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (substring-no-properties
               (doom-modeline--buffer-file-name file-path true-file-path 'nil))
              "/home/user/project/relative/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name-relative/relative-to-project ()
  (let ((default-directory "/home/user/project/")
        (file-path nil)
        (true-file-path "/home/user/project/relative/test.txt"))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties
                 (doom-modeline--buffer-file-name-relative file-path true-file-path))
                "relative/test.txt")))))

(ert-deftest doom-modeline--buffer-file-name-relative/relative-from-project ()
  (let ((default-directory "/home/user/project/")
        (file-path nil)
        (true-file-path "/home/user/project/relative/test.txt"))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties
                 (doom-modeline--buffer-file-name-relative file-path true-file-path 'include-project))
                "project/relative/test.txt")))))

(ert-deftest doom-modeline-enable-buffer-position/test ()
  "Test that doom-modeline-enable-buffer-position is defined correctly."
  ;; Test that the variable exists and defaults to t
  (should (boundp 'doom-modeline-enable-buffer-position))
  (should doom-modeline-enable-buffer-position)

  ;; Test that it can be set to nil
  (let ((doom-modeline-enable-buffer-position nil))
    (should-not doom-modeline-enable-buffer-position)))

;;
;; Tests for doom-modeline-add-segment
;;

(ert-deftest doom-modeline--insert-segment-in-list/after ()
  "Test inserting segment after anchor."
  (should (equal '(a b x c)
                 (doom-modeline--insert-segment-in-list '(a b c) 'b 'x :after)))
  (should (equal '(a x)
                 (doom-modeline--insert-segment-in-list '(a) 'nonexistent 'x :after)))
  (should (equal '(a x)
                 (doom-modeline--insert-segment-in-list '(a) 'a 'x :after))))

(ert-deftest doom-modeline--insert-segment-in-list/before ()
  "Test inserting segment before anchor."
  (should (equal '(a x b c)
                 (doom-modeline--insert-segment-in-list '(a b c) 'b 'x :before)))
  (should (equal '(x a)
                 (doom-modeline--insert-segment-in-list '(a) 'a 'x :before)))
  (should (equal '(a x)
                 (doom-modeline--insert-segment-in-list '(a) 'nonexistent 'x :before))))

(ert-deftest doom-modeline--insert-segment-in-list/empty ()
  "Test inserting into empty list."
  (should (equal '(x)
                 (doom-modeline--insert-segment-in-list nil 'anchor 'x :after))))

(ert-deftest doom-modeline-add-segment/to-all-modelines ()
  "Test adding segment to all modelines."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        ;; Use existing segments that are already defined in the package
        ;; Define test modelines using existing segments
        (doom-modeline-def-modeline 'test-main-seg
          '(buffer-info buffer-info-simple)
          '(major-mode time))
        (doom-modeline-def-modeline 'test-minimal-seg
          '(buffer-info)
          '(major-mode))

        ;; Add segment after buffer-info in all modelines
        (doom-modeline-add-segment 'buffer-position 'buffer-info)

        ;; Verify segment was added after buffer-info in both modelines
        (should (member 'buffer-position
                       (cadr (assq 'test-main-seg doom-modeline--modelines))))
        (should (member 'buffer-position
                       (cadr (assq 'test-minimal-seg doom-modeline--modelines)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-add-segment/to-specific-modeline ()
  "Test adding segment to a specific modeline only."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        ;; Define test modelines using existing segments
        (doom-modeline-def-modeline 'test-main2-seg
          '(buffer-info buffer-info-simple)
          '(major-mode))
        (doom-modeline-def-modeline 'test-minimal2-seg
          '(buffer-info)
          '(major-mode))

        ;; Add segment only to test-main2
        (doom-modeline-add-segment 'buffer-position 'buffer-info :after 'test-main2-seg)

        ;; Verify segment was added only to test-main2
        (should (member 'buffer-position
                       (cadr (assq 'test-main2-seg doom-modeline--modelines))))
        ;; test-minimal2 should NOT have the segment
        (should-not (member 'buffer-position
                           (cadr (assq 'test-minimal2-seg doom-modeline--modelines)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-add-segment/before-position ()
  "Test adding segment before anchor."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-main3-seg
          '(buffer-info buffer-info-simple)
          '(major-mode))

        ;; Add segment before buffer-info
        (doom-modeline-add-segment 'buffer-position 'buffer-info :before)

        ;; Verify segment was added before buffer-info
        (let ((lhs (cadr (assq 'test-main3-seg doom-modeline--modelines))))
          (should (equal (car lhs) 'buffer-position))
          (should (equal (cadr lhs) 'buffer-info))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-add-segment/excluded-modelines ()
  "Test that excluded modelines are not modified."
  (setq doom-modeline-excluded-modelines '(test-minimal4-seg))
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-main4-seg
          '(buffer-info)
          '(major-mode))
        (doom-modeline-def-modeline 'test-minimal4-seg
          '(buffer-info)
          '(major-mode))

        ;; Add segment to all modelines
        (doom-modeline-add-segment 'buffer-position 'buffer-info)

        ;; test-main4 should have the segment
        (should (member 'buffer-position
                       (cadr (assq 'test-main4-seg doom-modeline--modelines))))
        ;; test-minimal4 should NOT have the segment (it's excluded)
        (should-not (member 'buffer-position
                           (cadr (assq 'test-minimal4-seg doom-modeline--modelines)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-add-segment/to-rhs ()
  "Test adding segment to RHS when anchor is in RHS."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-main5-seg
          '(buffer-info)
          '(major-mode time))

        ;; Add segment after major-mode (which is in RHS)
        (doom-modeline-add-segment 'buffer-position 'major-mode :after)

        ;; Verify segment was added to RHS after major-mode
        (let ((rhs (caddr (assq 'test-main5-seg doom-modeline--modelines))))
          (should (equal rhs '(major-mode buffer-position time)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-add-segment/to-both-lhs-and-rhs ()
  "Test adding segment when anchor exists in both LHS and RHS."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-main6-seg
          '(buffer-info time)
          '(time major-mode))

        ;; Add segment after time (which exists in both LHS and RHS)
        (doom-modeline-add-segment 'buffer-position 'time :after)

        ;; Verify segment was added to both LHS and RHS after time
        (let ((lhs (cadr (assq 'test-main6-seg doom-modeline--modelines)))
              (rhs (caddr (assq 'test-main6-seg doom-modeline--modelines))))
          (should (equal lhs '(buffer-info time buffer-position)))
          (should (equal rhs '(time buffer-position major-mode)))))
    (setq doom-modeline-excluded-modelines nil)))

;;
;; Tests for doom-modeline-remove-segment
;;

(ert-deftest doom-modeline--remove-segment-from-list ()
  "Test removing segment from list."
  (should (equal '(a b c)
                 (doom-modeline--remove-segment-from-list '(a b c d) 'd)))
  (should (equal '(b c)
                 (doom-modeline--remove-segment-from-list '(a b c) 'a)))
  (should (equal '(a c)
                 (doom-modeline--remove-segment-from-list '(a b c) 'b)))
  (should (equal '(a b c)
                 (doom-modeline--remove-segment-from-list '(a b c) 'nonexistent)))
  (should (equal nil
                 (doom-modeline--remove-segment-from-list '(a) 'a)))
  (should (equal nil
                 (doom-modeline--remove-segment-from-list nil 'a))))

(ert-deftest doom-modeline-remove-segment/from-lhs ()
  "Test removing segment from LHS."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-remove-lhs
          '(buffer-info buffer-position buffer-info-simple)
          '(major-mode))

        ;; Remove buffer-position from LHS
        (doom-modeline-remove-segment 'buffer-position)

        ;; Verify segment was removed from LHS
        (let ((lhs (cadr (assq 'test-remove-lhs doom-modeline--modelines))))
          (should (equal lhs '(buffer-info buffer-info-simple)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-remove-segment/from-rhs ()
  "Test removing segment from RHS."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-remove-rhs
          '(buffer-info)
          '(major-mode time buffer-position))

        ;; Remove buffer-position from RHS
        (doom-modeline-remove-segment 'buffer-position)

        ;; Verify segment was removed from RHS
        (let ((rhs (caddr (assq 'test-remove-rhs doom-modeline--modelines))))
          (should (equal rhs '(major-mode time)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-remove-segment/from-both-sides ()
  "Test removing segment from both LHS and RHS."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-remove-both
          '(buffer-info time)
          '(time major-mode))

        ;; Remove time from both sides
        (doom-modeline-remove-segment 'time)

        ;; Verify segment was removed from both LHS and RHS
        (let ((lhs (cadr (assq 'test-remove-both doom-modeline--modelines)))
              (rhs (caddr (assq 'test-remove-both doom-modeline--modelines))))
          (should (equal lhs '(buffer-info)))
          (should (equal rhs '(major-mode)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-remove-segment/to-specific-modeline ()
  "Test removing segment from a specific modeline only."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-remove-specific1
          '(buffer-info buffer-position)
          '(major-mode))
        (doom-modeline-def-modeline 'test-remove-specific2
          '(buffer-info buffer-position)
          '(major-mode))

        ;; Remove segment only from test-remove-specific1
        (doom-modeline-remove-segment 'buffer-position 'test-remove-specific1)

        ;; Verify segment was removed only from test-remove-specific1
        (let ((lhs1 (cadr (assq 'test-remove-specific1 doom-modeline--modelines)))
              (lhs2 (cadr (assq 'test-remove-specific2 doom-modeline--modelines))))
          (should (equal lhs1 '(buffer-info)))
          (should (equal lhs2 '(buffer-info buffer-position)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-remove-segment/excluded-modelines ()
  "Test that excluded modelines are not modified."
  (setq doom-modeline-excluded-modelines '(test-remove-excluded))
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-remove-main
          '(buffer-info buffer-position)
          '(major-mode))
        (doom-modeline-def-modeline 'test-remove-excluded
          '(buffer-info buffer-position)
          '(major-mode))

        ;; Remove segment from all modelines
        (doom-modeline-remove-segment 'buffer-position)

        ;; test-remove-main should have segment removed
        (let ((lhs-main (cadr (assq 'test-remove-main doom-modeline--modelines))))
          (should (equal lhs-main '(buffer-info))))
        ;; test-remove-excluded should NOT have segment removed (it's excluded)
        (let ((lhs-excluded (cadr (assq 'test-remove-excluded doom-modeline--modelines))))
          (should (equal lhs-excluded '(buffer-info buffer-position)))))
    (setq doom-modeline-excluded-modelines nil)))

(ert-deftest doom-modeline-remove-segment/not-found ()
  "Test removing segment that doesn't exist."
  (setq doom-modeline-excluded-modelines nil)
  (unwind-protect
      (progn
        (doom-modeline-def-modeline 'test-remove-not-found
          '(buffer-info buffer-info-simple)
          '(major-mode time))

        ;; Try to remove non-existent segment
        (doom-modeline-remove-segment 'nonexistent-segment)

        ;; Modeline should be unchanged
        (let ((lhs (cadr (assq 'test-remove-not-found doom-modeline--modelines)))
              (rhs (caddr (assq 'test-remove-not-found doom-modeline--modelines))))
          (should (equal lhs '(buffer-info buffer-info-simple)))
          (should (equal rhs '(major-mode time)))))
    (setq doom-modeline-excluded-modelines nil)))

;;; doom-modeline-core-test.el ends here
