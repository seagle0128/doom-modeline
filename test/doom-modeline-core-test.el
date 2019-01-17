;;; doom-modeline-core-test.el --- Unit tests for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;;  Unit tests for doom-modeline.
;;

;;; Code:

(require 'doom-modeline-core)

(ert-deftest doom-modeline--buffer-file-name/truncate-upto-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (strip-text-properties
               (doom-modeline--buffer-file-name file-path true-file-path 'shrink))
              "/h/u/project/relative/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name/truncate-from-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (strip-text-properties
               (doom-modeline--buffer-file-name file-path true-file-path nil 'shrink))
              "/home/user/project/r/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name/truncate-with-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (strip-text-properties
               (doom-modeline--buffer-file-name file-path true-file-path 'shrink 'shrink 'hide))
              "project/r/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name/truncate-except-project ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path nil))
    (should
     (string= (strip-text-properties
               (doom-modeline--buffer-file-name file-path true-file-path 'shrink 'shrink))
              "/h/u/project/r/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name-truncate/truncate-upto-root ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path "~/project/relative/test.txt"))
    (should
     (string= (strip-text-properties
               (doom-modeline--buffer-file-name-truncate file-path true-file-path))
              "~/p/relative/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name-truncate/truncate-all ()
  (let ((default-directory "/home/user/project/")
        (file-path "/home/user/project/relative/test.txt")
        (true-file-path "~/project/relative/test.txt"))
    (should
     (string= (strip-text-properties
               (doom-modeline--buffer-file-name-truncate file-path true-file-path t))
              "~/p/r/test.txt"))))

(ert-deftest doom-modeline--buffer-file-name-relative/relative-to-project ()
  (let ((default-directory "/home/user/project/")
        (file-path nil)
        (true-file-path "/home/user/project/relative/test.txt"))
    (cl-flet ((doom-modeline-project-root () "/home/user/project/"))
      (should
       (string= (strip-text-properties
                 (doom-modeline--buffer-file-name-relative file-path true-file-path))
                "relative/test.txt")))))

(ert-deftest doom-modeline--buffer-file-name-relative/relative-from-project ()
  (let ((default-directory "/home/user/project/")
        (file-path nil)
        (true-file-path "/home/user/project/relative/test.txt"))
    (cl-flet ((doom-modeline-project-root () "/home/user/project/"))
      (should
       (string= (strip-text-properties
                 (doom-modeline--buffer-file-name-relative file-path true-file-path 'include-project))
                "project/relative/test.txt")))))

;;; doom-modeline-core-test.el ends here
