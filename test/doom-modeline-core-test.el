;;; doom-modeline-core-test.el --- Unit tests for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Vincent Zhang

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

;; ;; XXX: This is a precaution for older Emacsen that may ship with
;; outdated versions of `project'.
(load "project")

(ert-deftest doom-modeline-icon/octicon-icon ()
  (let ((doom-modeline-icon t)
        (doom-modeline-unicode-fallback t))
    ;; In TUI, fallback to unicode.
    (should
     (string= (substring-no-properties
               (doom-modeline-icon 'octicon "octoface" "☻" ":)" 'error))
              "☻"))))

(ert-deftest doom-modeline-icon/octicon-unicode ()
  (let ((doom-modeline-icon nil)
        (doom-modeline-unicode-fallback t))
    (should
     (string= (substring-no-properties
               (doom-modeline-icon 'octicon "octoface" "☻" ":)" 'error))
              "☻"))))

(ert-deftest doom-modeline-icon/octicon-text ()
  (let ((doom-modeline-icon nil)
        (doom-modeline-unicode-fallback nil))
    (should
     (string= (substring-no-properties
               (doom-modeline-icon 'octicon "octoface" "☻" ":)" 'error))
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
                "%b")))))

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
              "%b"))))

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
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "/h/u/p/relative/test.txt")))))

(ert-deftest doom-modeline-buffer-file-name/truncate-all ()
  (let ((default-directory "/home/user/project/")
        (buffer-file-name "/home/user/project/relative/test.txt")
        (buffer-file-truename "/home/user/project/relative/test.txt")
        (doom-modeline-buffer-file-name-style 'truncate-all))
    (cl-flet ((doom-modeline-project-root () default-directory))
      (should
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "/h/u/p/r/test.txt")))))

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
       (string= (substring-no-properties (doom-modeline-buffer-file-name))
                "relative/test.txt")))))

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

;;; doom-modeline-core-test.el ends here
