;;; doom-modeline-env-test.el --- Unit tests for doom-modeline-env -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Vincent Zhang, Justin Barclay

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
;;  Unit tests for doom-modeline-env.
;;

;;; Code:

(require 'cl-lib)
(require 'doom-modeline-env)

(ert-deftest doom-modeline-env--ruby/parse-ruby-version-string ()
  (should
   (string= (doom-modeline-env--ruby-parse "ruby 2.3.3p222 (2016-11-21 revision 56859) [x86_64-darwin16]")
            "2.3.3")))

(ert-deftest doom-modeline-env--elixir/parse-elixir-version-string ()
  (should
   (string= (doom-modeline-env--elixir-parse
             "Erlang/OTP 26 [erts-14.2.5] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit]\n\nElixir 1.18.2 (compiled with Erlang/OTP 26)")
            "1.18.2/OTP26")))

(ert-deftest doom-modeline-env--rustc/parse-rustc-version-string ()
  (should
   (string= (doom-modeline-env--rust-parse "rustc 1.32.0-nightly (14997d56a 2018-12-05)")
            "1.32.0")))

(ert-deftest doom-modeline-env--go/parse-go-version-string ()
  (should
   (string= (doom-modeline-env--go-parse "go version go1.11.4 darwin/amd64")
            "1.11.4")))

(ert-deftest doom-modeline-env--perl/parse-perl-version-string ()
  (should
   (string= (doom-modeline-env--perl-parse
             "This is perl 5, version 18, subversion 2 (v5.18.2) built for darwin-thread-multi-2level
(with 2 registered patches, see perl -V for more detail)

Copyright 1987-2013, Larry Wall

Perl may be copied only under the terms of either the Artistic License or the
GNU General Public License, which may be found in the Perl 5 source kit.

Complete documentation for Perl, including FAQ lists, should be found on
this system using \"man perl\" or \"perldoc perl\".  If you have access to the
Internet, point your browser at http://www.perl.org/, the Perl Home Page.")
            "5.18.2")))

(ert-deftest doom-modeline-env--python/parse-python-version-string ()
  (should
   (string= (doom-modeline-env--python-parse "Python 2.7.15")
            "2.7.15")))

;;; P0: version query must short-circuit on repeated focus/find-file triggers

(ert-deftest doom-modeline-env/short-circuits-after-first-query ()
  "Version query forks the subprocess at most once per buffer.
Repeated calls from `find-file-hook' and focus changes while the query is
in flight (loading state) or after it resolved must not fork again."
  (let ((doom-modeline-env-version t)
        (doom-modeline-env--command "python")
        (doom-modeline-env--command-args '("--version"))
        (doom-modeline-env--parser #'identity)
        (doom-modeline-env--version nil)
        (spawns 0)
        (before 0)
        (after 0))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd) "/usr/bin/python"))
              ((symbol-function 'doom-modeline-project-root)
               (lambda () default-directory))
              ((symbol-function 'start-process)
               (lambda (&rest _)
                 (setq spawns (1+ spawns))
                 nil))
              ((symbol-function 'run-hooks)
               (lambda (hook &rest _)
                 (pcase hook
                   ('doom-modeline-before-update-env-hook
                    (setq before (1+ before)))
                   ('doom-modeline-after-update-env-hook
                    (setq after (1+ after)))))))
      ;; 1st trigger: version nil -> fork, set loading state, run before-hook.
      (doom-modeline-update-env)
      (should (= spawns 1))
      (should (= before 1))
      (should (equal doom-modeline-env--version doom-modeline-env-load-string))
      ;; Repeated triggers while loading -> no fork, no before-hook,
      ;; loading state preserved (must not be reset to a new loading string).
      (doom-modeline-update-env)
      (doom-modeline-update-env)
      (should (= spawns 1))
      (should (= before 1))
      (should (equal doom-modeline-env--version doom-modeline-env-load-string))
      ;; Version resolved -> still no fork on further focus/find-file.
      (setq doom-modeline-env--version "3.11.0")
      (doom-modeline-update-env)
      (should (= spawns 1))
      (should (= before 1))
      (should (equal doom-modeline-env--version "3.11.0")))))

(ert-deftest doom-modeline-env/allows-retry-when-query-fails ()
  "If the parser yields nil, version returns to nil and a retry is allowed.
This preserves the original behavior where a failed resolution could be
re-attempted on a later trigger."
  (let ((doom-modeline-env-version t)
        (doom-modeline-env--command "python")
        (doom-modeline-env--command-args '("--version"))
        (doom-modeline-env--parser (lambda (_line) nil))
        (doom-modeline-env--version nil)
        (spawns 0)
        (callback nil))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (_cmd) "/usr/bin/python"))
              ((symbol-function 'doom-modeline-project-root)
               (lambda () default-directory))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq spawns (1+ spawns)) nil))
              ((symbol-function 'doom-modeline-env--get)
               (lambda (_cmd _args cb) (setq callback cb))))
      (doom-modeline-update-env)
      (should (= spawns 0))                ; --get mocked, so no real spawn
      (should (equal doom-modeline-env--version doom-modeline-env-load-string))
      ;; Simulate callback delivering unparseable output -> version back to nil.
      (funcall callback "unparseable")
      (should (null doom-modeline-env--version))
      ;; Now a retry forks again.
      (doom-modeline-update-env)
      (should (equal doom-modeline-env--version doom-modeline-env-load-string)))))

(ert-deftest doom-modeline-env/no-fork-when-feature-disabled ()
  "No subprocess is forked when `doom-modeline-env-version' is nil."
  (let ((doom-modeline-env-version nil)
        (doom-modeline-env--command "python")
        (doom-modeline-env--version nil)
        (spawns 0))
    (cl-letf (((symbol-function 'start-process)
               (lambda (&rest _) (setq spawns (1+ spawns)) nil)))
      (doom-modeline-update-env)
      (should (= spawns 0)))))

;;; P0 follow-up: invalidate cached version when the resolved command changes

(ert-deftest doom-modeline-env/invalidates-version-on-command-change ()
  "A changed resolved command (venv/pyenv switch) invalidates the version.
`doom-modeline-env-update-python' re-resolves the command on
`hack-local-variables-hook'; if the executable or args differ from the
cached ones, the version must be cleared so a fresh query is forked."
  (let ((doom-modeline-env-version t)
        (doom-modeline-env--version "3.10.0")
        (doom-modeline-env--command "/usr/bin/python3.10")
        (doom-modeline-env--command-args '("--version"))
        (doom-modeline-env--parser #'identity)
        (spawns 0)
        (cmd "/usr/bin/python3.10"))
    (cl-letf (((symbol-value 'doom-modeline-env-python-command)
               (lambda () (list cmd "--version")))
              ((symbol-function 'executable-find)
               (lambda (_cmd) cmd))
              ((symbol-function 'doom-modeline-project-root)
               (lambda () default-directory))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq spawns (1+ spawns)) nil)))
      ;; Same command -> version kept, no re-fork (focus/find-file path).
      (doom-modeline-env-update-python)
      (should (equal doom-modeline-env--version "3.10.0"))
      (should (= spawns 0))
      ;; Executable changed -> version invalidated, re-fork with loading state.
      (setq cmd "/opt/venv/bin/python")
      (doom-modeline-env-update-python)
      (should (= spawns 1))
      (should (equal doom-modeline-env--version doom-modeline-env-load-string))
      (should (equal doom-modeline-env--command "/opt/venv/bin/python")))))

(ert-deftest doom-modeline-env/invalidates-version-on-args-change ()
  "Changed command args also invalidate the cached version."
  (let ((doom-modeline-env-version t)
        (doom-modeline-env--version "3.10.0")
        (doom-modeline-env--command "/usr/bin/python")
        (doom-modeline-env--command-args '("--version"))
        (doom-modeline-env--parser #'identity)
        (spawns 0)
        (args '("--version")))
    (cl-letf (((symbol-value 'doom-modeline-env-python-command)
               (lambda () (cons "/usr/bin/python" args)))
              ((symbol-function 'executable-find)
               (lambda (_cmd) "/usr/bin/python"))
              ((symbol-function 'doom-modeline-project-root)
               (lambda () default-directory))
              ((symbol-function 'start-process)
               (lambda (&rest _) (setq spawns (1+ spawns)) nil)))
      (setq args '("-V"))             ; args changed, exe identical
      (doom-modeline-env-update-python)
      (should (= spawns 1))
      (should (equal doom-modeline-env--command-args '("-V")))
      (should (equal doom-modeline-env--version doom-modeline-env-load-string)))))

;;; doom-modeline-env-test.el ends here
