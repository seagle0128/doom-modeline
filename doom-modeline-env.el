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
;; Parse programming environment.
;;

;;; Code:

(require 'subr-x)
(require 'doom-modeline-core)

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local doom-modeline-env-version nil
  "The version to display with major-mode in mode-line.
Example: \"2.6.0\"")
(defvar-local doom-modeline-env-command nil
  "A program that we're looking to extract version information from.
Example: \"ruby\"")
(defvar-local doom-modeline-env-command-args nil
  "A list of arguments to pass to `doom-modeline-env-command` to extract the version from.
Example: '(\"--version\") ")
(defvar-local doom-modeline-env-parser nil
  "A function that returns version number from a programs --version (or similar) command.
Example: 'doom-modeline-env--ruby")

(defun doom-modeline-update-env ()
  "Update environment info on mode-line."
  (when (and doom-modeline-version
             doom-modeline-env-command
             (executable-find doom-modeline-env-command)
             doom-modeline-env-command-args
             doom-modeline-env-parser)
    (let ((default-directory (doom-modeline-project-root)))
      (doom-modeline-env--get doom-modeline-env-command
                              doom-modeline-env-command-args
                              (lambda (prog-version)
                                (setq doom-modeline-env-version
                                      (funcall doom-modeline-env-parser prog-version)))))))

(add-hook 'find-file-hook #'doom-modeline-update-env)
(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda ()
                      (if (frame-focus-state)
                          (doom-modeline-update-env))))
    (add-hook 'focus-in-hook #'doom-modeline-update-env)))

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
                     ;; Flaten process-args into a single list so we can handle
                     ;; variadic length args
                     (append
                      (list "doom-modeline-env" nil prog)
                      args)))
        (parser callback))
    (set-process-filter proc
                        (lambda (_proc line)
                          (ignore-errors
                            (funcall parser line))))))

;;
;; Parser
;;

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


;;
;; Bootstrap
;; Versions, support Python, Ruby, Perl and Golang, etc.
;;

(add-hook 'python-mode-hook
          (lambda ()
            (cond ((and (fboundp 'pipenv-project-p) (pipenv-project-p) (executable-find "pipenv"))
                   (setq doom-modeline-env-parser 'doom-modeline-env--python)
                   (setq doom-modeline-env-command "pipenv")
                   (setq doom-modeline-env-command-args '("run" "python" "--version")))
                  ((and doom-modeline-python-executable
                        (executable-find doom-modeline-python-executable))
                   (setq doom-modeline-env-parser 'doom-modeline-env--python)
                   (setq doom-modeline-env-command doom-modeline-python-executable)
                   (setq doom-modeline-env-command-args '("--version"))))))
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "ruby")
            (setq doom-modeline-env-command-args '("--version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--ruby)))
(add-hook 'perl-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "perl")
            (setq doom-modeline-env-command-args '("--version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--perl)))
(add-hook 'go-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "go")
            (setq doom-modeline-env-command-args '("version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--go)))

(add-hook 'elixir-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "iex")
            (setq doom-modeline-env-command-args '("--version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--elixir)))
(add-hook 'rust-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "rustc")
            (setq doom-modeline-env-command-args '("--version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--rustc)))

(provide 'doom-modeline-env)

;;; doom-modeline-env.el ends here
