;;; doom-modeline-env.el --- A environment parser for doom-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2019 Justin Barclay, Vincent Zhang

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
;; Parse programming environment version.
;;

;;; Code:

(require 'subr-x)

(defun doom-modeline-env--ruby (line)
  "Parse Ruby version from LINE."
  (car (split-string
        (cadr
         (split-string line))
        "p")))

(defun doom-modeline-env--elixir (line)
  "Parse Elixir version from LINE."
  (cadr
   (split-string line)))

(defun doom-modeline-env--rustc (line)
  "Parse Rust version from LINE."
  (car
   (split-string
    (cadr
     (split-string line))
    "-")))

(defun doom-modeline-env--go (line)
  "Parse Go version from LINE."
  (cadr
   (split-string
    (cadr
     (cdr
      (split-string
       line)))
    "go")))

(defun doom-modeline-env--perl (line)
  "Parse Perl version from LINE."
  (cadr
   (split-string
    (car
     (split-string
      (cadr
       (split-string line "("))
      ")"))
    "v")))

(defun doom-modeline-env--python (line)
  "Parse Python version from LINE."
  (cadr
   (split-string line)))

(defun doom-modeline-env--get (prog args callback)
  "Start a sub process using PROG and apply the ARGS to the sub process.
Once it recieves information from STDOUT, it closes off the subprocess and
passes on the information into the CALLBACK.
Example:
  (doom-modeline-env--get
     \"ruby\"
     '(\"--version\")
     (lambda (line)
        (message (doom-modeline-parser--ruby line)))"
  (let ((proc (apply 'start-process
                     (append ;; Flaten process-args into a single list so we can handle variadic length args
                      (list "doom-modeline-env" nil prog)
                      args)))
        (parser callback))
    (set-process-filter proc
                        (lambda (_proc line)
                          (ignore-errors
                            (funcall parser line))))))

(provide 'doom-modeline-env)

;;; doom-modeline-env.el ends here
