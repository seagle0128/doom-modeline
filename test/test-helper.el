;;; test-helper.el --- Helpers for doom-modeline-test.el -*- lexical-binding: t; -*-

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
;;  Helpers for doom-modeline-test.el
;;
;;; Code:

(add-to-list 'load-path (expand-file-name "."))

;; @see https://github.com/rejeep/ert-runner.el/issues/49
(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace 'backtrace-to-string))

(defun ffip-get-project-root-directory ()
  default-directory)

(defun projectile-project-root ()
  default-directory)

(defun project-current ()
  `(vc . ,default-directory))

;;; test-helper.el ends here
