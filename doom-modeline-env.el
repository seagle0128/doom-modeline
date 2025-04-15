;;; doom-modeline-env.el --- A environment parser for doom-modeline -*- lexical-binding: t -*-

;; Copyright (C) 2019-2025 Vincent Zhang, Justin Barclay

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
;; Parse programming environment.
;;

;;; Code:

(require 'doom-modeline-core)
(eval-when-compile
  (require 'subr-x))


;; Externals
(defvar python-shell-interpreter)


;; Customization

(defgroup doom-modeline-env nil
  "The environment parser for `doom-modeline'."
  :group 'doom-modeline
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/doom-modeline"))

(defcustom doom-modeline-env-load-string doom-modeline-ellipsis
  "What to display as the version while a new one is being loaded."
  :type 'string
  :group 'doom-modeline-env)

(defcustom doom-modeline-before-update-env-hook nil
  "Hooks that run before the modeline version string is updated."
  :type 'hook
  :group 'doom-modeline-env)

(defcustom doom-modeline-after-update-env-hook nil
  "Hooks that run after the modeline version string is updated."
  :type 'hook
  :group 'doom-modeline-env)


;; Variables

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local doom-modeline-env--version nil
  "The version to display with major-mode in mode-line.
Example: \"2.6.0\"")

(defvar-local doom-modeline-env--command nil
  "A program that we're looking to extract version information from.
Example: \"ruby\"")

(defvar-local doom-modeline-env--command-args nil
  "A list of arguments for the command to extract the version from.
Example: \\='(\"--version\")")

(defvar-local doom-modeline-env--parser nil
  "A function that returns version number from a command --version (or similar).
Example: \\='doom-modeline-env--ruby")


;; Functions & Macros

(defun doom-modeline-update-env ()
  "Update environment info on mode-line."
  (when (and doom-modeline-env-version
             doom-modeline-env--command
             (executable-find doom-modeline-env--command)
             doom-modeline-env--command-args
             doom-modeline-env--parser)
    (let ((default-directory (doom-modeline-project-root))
          (buffer (current-buffer)))
      (run-hooks 'doom-modeline-before-update-env-hook)
      (setq doom-modeline-env--version doom-modeline-env-load-string)
      (doom-modeline-env--get
       doom-modeline-env--command
       doom-modeline-env--command-args
       (lambda (prog-version)
         (with-current-buffer buffer
           (setq doom-modeline-env--version
                 (funcall doom-modeline-env--parser prog-version))
           (run-hooks 'doom-modeline-after-update-env-hook)))))))

(add-hook 'find-file-hook #'doom-modeline-update-env)
(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (add-function
       :after after-focus-change-function
       (lambda ()
         (if (frame-focus-state)
             (doom-modeline-update-env))))
    (add-hook 'focus-in-hook #'doom-modeline-update-env)))

(defun doom-modeline-env--get (prog args callback)
  "Start a sub process using PROG and apply the ARGS to the sub process.
Once it receives information from STDOUT, it closes off the subprocess and
passes on the information into the CALLBACK.
Example:
  (doom-modeline-env--get
     \"ruby\"
     \\='(\"--version\")
     (lambda (line)
        (message (doom-modeline-parser--ruby line)))"
  (when-let* ((proc (ignore-errors
                      (apply 'start-process
                             ;; Flaten process-args into a single list so we can handle
                             ;; variadic length args
                             (append
                              (list "doom-modeline-env" nil prog)
                              args))))
              (parser callback))
    (set-process-filter proc
                        (lambda (_proc line)
                          (ignore-errors
                            (funcall parser line))))))

(cl-defmacro doom-modeline-def-env (name &key hooks command parser)
  "Define a handler for updating & displaying a version string for a language.

NAME is an unquoted symbol representing the handler's unique ID.
HOOKS is a list of hook symbols where this handler should be triggered.
COMMAND should be a function that returns a shell command and its arguments (as
  a list). It is run on HOOKS. It takes no arguments.
PARSER should be a function for parsing COMMAND's output line-by-line, to
  extract the version string."
  (declare (indent defun))
  (unless (and hooks command parser)
    (error "'%s' env is missing either :hooks, :command or :parser" name))
  (let ((parse-fn  (intern (format "doom-modeline-env--%s-parse" name)))
        (action-fn (intern (format "doom-modeline-env--%s-args"  name)))
        (setup-fn  (intern (format "doom-modeline-env-setup-%s"  name)))
        (update-fn (intern (format "doom-modeline-env-update-%s" name)))
        (enable-var  (intern (format "doom-modeline-env-enable-%s" name)))
        (command-var (intern (format "doom-modeline-env-%s-command" name)))
        (parser-var  (intern (format "doom-modeline-env-%s-parser-fn" name)))
        (exe-var     (intern (format "doom-modeline-env-%s-executable" name))))
    (macroexp-progn
     `((defcustom ,enable-var t
         ,(format "Whether to display the version string for %s buffers." name)
         :type 'boolean
         :group 'doom-modeline-env)
       (defvar ,command-var ',action-fn
         ,(concat "A function that returns the shell command and arguments (as a list) to\n"
                  "produce a version string."))
       (defvar ,parser-var ',parse-fn
         ,(format "The function to parse each line of `%s'\'s output." command-var))
       (defcustom ,exe-var nil
         ,(format (concat "What executable to use for the version indicator in %s buffers.\n\n"
                          "If nil, the default binary for this language is used.")
                  name)
         :type 'string
         :group 'doom-modeline-env)
       (defalias ',parse-fn ,parser
         (format "The line parser for %s buffers.\n\nUsed by `%s'."
                 ',name ',update-fn))
       (defalias ',action-fn ,command
         (format "The command resolver for %s buffers.\n\nUsed by `%s'."
                 ',name ',update-fn))
       (defalias ',setup-fn
         (lambda ()
           (if enable-local-variables
               (add-hook 'hack-local-variables-hook #',update-fn nil t)
             (,update-fn)))
         (format "Prepares the modeline to later display the %s version string."
                 ',name))
       (defalias ',update-fn
         (lambda ()
           (when ,enable-var
             (when-let* ((command-list (funcall ,command-var))
                         (exe (executable-find (car command-list))))
               (setq doom-modeline-env--command exe
                     doom-modeline-env--command-args (cdr command-list)
                     doom-modeline-env--parser ,parser-var)
               (doom-modeline-update-env))))
         (format "Updates the %s version string in the modeline." ',name))
       (let ((hooks ',(eval hooks)))
         (dolist (hook (if (listp hooks) hooks (list hooks)))
           (add-hook hook #',setup-fn)))))))


;; Bootstrap
;; Versions, support Python, Ruby, Perl and Golang, etc.

;;;###autoload (autoload 'doom-modeline-env-setup-python "doom-modeline-env")
(doom-modeline-def-env python
  :hooks   '(python-mode-hook python-ts-mode-hook)
  :command (lambda () (cond ((and (executable-find "pipenv")
                                  (locate-dominating-file default-directory "Pipfile"))
                        (list "pipenv" "run"
                              (or doom-modeline-env-python-executable
                                  python-shell-interpreter
                                  "python")
                              "--version"))
                       ((executable-find "pyenv") (list "pyenv" "version-name"))
                       ((and (executable-find "direnv")
                                  (locate-dominating-file default-directory ".envrc"))
                             (list "bash"
                                   "-c"
                                   ;; Direnv unfortunately writes crao on stderr
                                   ;; so we need to pipe that to /dev/null
                                   (format "direnv exec %s %s --version 2>/dev/null"
                                           (file-truename default-directory)
                                           (or doom-modeline-env-python-executable
                                               python-shell-interpreter
                                               "python"))))
                       ((executable-find "uv") (list "uv" "run" "--no-sync" "python" "--version"))
                       ((list (or doom-modeline-env-python-executable
                                  python-shell-interpreter
                                  "python")
                              "--version"))))
  :parser  (lambda (line) (let ((version (split-string line)))
                       (if (length> version 1)
                           (cadr version)
                         (car version)))))

;;;###autoload (autoload 'doom-modeline-env-setup-ruby "doom-modeline-env")
(doom-modeline-def-env ruby
  :hooks   '(ruby-mode-hook ruby-ts-mode-hook enh-ruby-mode-hook)
  :command (lambda () (list (or doom-modeline-env-ruby-executable "ruby") "--version"))
  :parser  (lambda (line)
             (car (split-string
                   (cadr
                    (split-string line))
                   "p"))))

;;;###autoload (autoload 'doom-modeline-env-setup-perl "doom-modeline-env")
(doom-modeline-def-env perl
  :hooks   'perl-mode-hook
  :command (lambda () (list (or doom-modeline-env-perl-executable "perl") "--version"))
  :parser  (lambda (line)
             (cadr
              (split-string
               (car
                (split-string
                 (cadr
                  (split-string line "("))
                 ")"))
               "v"))))

;;;###autoload (autoload 'doom-modeline-env-setup-go "doom-modeline-env")
(doom-modeline-def-env go
  :hooks   '(go-mode-hook go-ts-mode-hook)
  :command (lambda () (list (or doom-modeline-env-go-executable "go") "version"))
  :parser  (lambda (line)
             (cadr
              (split-string
               (cadr
                (cdr
                 (split-string line)))
               "go"))))

;;;###autoload (autoload 'doom-modeline-env-setup-elixir "doom-modeline-env")
(doom-modeline-def-env elixir
  :hooks   '(elixir-mode-hook elixir-ts-mode-hook)
  :command (lambda () (list (or doom-modeline-env-elixir-executable "elixir") "--version"))
  :parser  (lambda (line) (cadr (split-string line))))

;;;###autoload (autoload 'doom-modeline-env-setup-rust "doom-modeline-env")
(doom-modeline-def-env rust
  :hooks   '(rust-mode-hook rust-ts-mode-hook)
  :command (lambda () (list (or doom-modeline-env-rust-executable "rustc") "--version"))
  :parser  (lambda (line)
             (car
              (split-string
               (cadr
                (split-string line))
               "-"))))

(provide 'doom-modeline-env)

;;; doom-modeline-env.el ends here
