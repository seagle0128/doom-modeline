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

(ert-deftest doom-modeline-icon/octicon-icon ()
  (let ((doom-modeline-icon t)
        (doom-modeline-unicode-fallback t))
    ;; In TUI, fallback to unicode.
    (should
     (string= (substring-no-properties
               (doom-modeline-icon 'octicon "nf-oct-smiley" "☻" ":)" :face 'error))
              ""))))

(ert-deftest doom-modeline-icon/octicon-unicode ()
  (let ((doom-modeline-icon nil)
        (doom-modeline-unicode-fallback t))
    (should
     (string= (substring-no-properties
               (doom-modeline-icon 'octicon "nf-oct-smiley" "☻" ":)" :face 'warning))
              "☻"))))

(ert-deftest doom-modeline-icon/octicon-text ()
  (let ((doom-modeline-icon nil)
        (doom-modeline-unicode-fallback nil))
    (should
     (string= (substring-no-properties
               (doom-modeline-icon 'octicon "nf-oct-smiley" "☻" ":)" :face 'success))
              ":)"))))

;; The latest `project' requires Emacs >= 26.1
;; Old versions of project.el do not have `project-root'
(when (>= emacs-major-version 26)
  (ert-deftest doom-modeline-project-root/auto ()
    (let ((default-directory "~/project/")
          (doom-modeline-project-detection 'auto)
          (doom-modeline--project-root nil))
      (should (string= (doom-modeline-project-root) default-directory))))

  (ert-deftest doom-modeline-project-root/auto-old ()
    (let ((default-directory "~/project-current/")
          (doom-modeline-project-detection 'auto)
          (doom-modeline--project-root nil))
      (should (string= (doom-modeline-project-root) default-directory))))

  (ert-deftest doom-modeline-project-root/project ()
    (let ((default-directory "~/project-current/")
          (doom-modeline-project-detection 'project)
          (doom-modeline--project-root nil))
      (should (string= (doom-modeline-project-root) default-directory))))

  (ert-deftest doom-modeline-project-root/project-old ()
    (let ((default-directory "~/project-current/")
          (doom-modeline-project-detection 'project)
          (doom-modeline--project-root nil))
      (should (string= (doom-modeline-project-root) default-directory))))
  )

;; The latest `projectile' requires Emacs >= 28.1
(when (>= emacs-major-version 28)
  (ert-deftest doom-modeline-project-root/projectile ()
    (let ((default-directory "~/projectile/")
          (doom-modeline-project-detection 'projectile)
          (doom-modeline--project-root nil))
      (cl-flet ((projectile-project-root () default-directory))
        (should (string= (doom-modeline-project-root) default-directory)))))
  )

(ert-deftest doom-modeline-project-root/ffip ()
  (let ((default-directory "~/project-ffip/")
        (doom-modeline-project-detection 'ffip)
        (doom-modeline--project-root nil))
    (cl-flet ((ffip-get-project-root-directory () default-directory))
      (should (string= (doom-modeline-project-root) default-directory)))))

;;; P1: changing project-detection must not revert visited buffers
(ert-deftest doom-modeline-project-detection/watcher-invalidates-without-revert ()
  "Changing `doom-modeline-project-detection' clears the per-buffer cache
but never calls `revert-buffer'.  Reverting would re-read every visited file
from disk and re-run major mode, which is unnecessary for a mode-line value."
  (let ((reverts 0)
        (orig-detection doom-modeline-project-detection)
        (bufs nil))
    (unwind-protect
        (progn
          ;; Two file-visiting buffers with populated caches.  Create them
          ;; without `find-file' to avoid heavy major-mode hooks; setting
          ;; `buffer-file-name' is enough for the watcher's old code path.
          (dolist (name '("/tmp/dm-proj-a.txt" "/tmp/dm-proj-b.txt"))
            (let ((buf (generate-new-buffer " *dm-proj-test*")))
              (push buf bufs)
              (with-current-buffer buf
                (setq buffer-file-name name)
                (setq doom-modeline--project-root "/fake/root"
                      doom-modeline--project-root-abbrev-cache
                      '("/fake/root" . "/fake/root")))))
          (cl-letf (((symbol-function 'revert-buffer)
                     (lambda (&rest _) (setq reverts (1+ reverts)))))
            ;; Trigger the variable watcher via a real set.
            (setq doom-modeline-project-detection 'project)
            ;; No buffer may be reverted, even though both visit files.
            (should (= reverts 0))
            ;; Every buffer's cache is invalidated so it recomputes lazily.
            (dolist (buf bufs)
              (with-current-buffer buf
                (should (null doom-modeline--project-root))
                (should (null doom-modeline--project-root-abbrev-cache))))))
      ;; Cleanup: restore detection (fires the watcher again, harmless) and
      ;; kill the temporary buffers.
      (setq doom-modeline-project-detection orig-detection)
      (dolist (buf bufs)
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(ert-deftest doom-modeline-project-root/default ()
  (let ((default-directory "~/project/")
        (doom-modeline-project-detection nil))
    (should (string= (doom-modeline-project-root) default-directory))))

;;; P1: abbreviate-file-name must not run on every redisplay.
(ert-deftest doom-modeline-project-root/abbrev-cached-until-input-changes ()
  "Cache the abbreviated root until the path or any abbreviation input changes."
  (let* ((default-directory "/tmp/project/")
         (doom-modeline-project-detection nil)
         (doom-modeline--project-root nil)
         (doom-modeline--project-root-abbrev-cache nil)
         (directory-abbrev-alist nil)
         (abbreviated-home-dir nil)
         (file-name-handler-alist (list (cons "\\`foo" 'ignore)))
         (calls 0))
    (cl-letf (((symbol-function 'abbreviate-file-name)
               (lambda (file)
                 (setq calls (1+ calls))
                 (concat "<" file ">"))))
      ;; Warm hit: the same path is abbreviated only once.
      (should (string= (doom-modeline-project-root) "</tmp/project/>"))
      (should (string= (doom-modeline-project-root) "</tmp/project/>"))
      (should (= calls 1))
      ;; No project root: default-directory is part of the key.
      (setq default-directory "/tmp/other/")
      (should (string= (doom-modeline-project-root) "</tmp/other/>"))
      (should (= calls 2))
      ;; A project root replaces default-directory in the key.
      (setq doom-modeline--project-root "/tmp/proj2/")
      (should (string= (doom-modeline-project-root) "</tmp/proj2/>"))
      (should (= calls 3))
      ;; directory-abbrev-alist changes invalidate the cache.
      (setq directory-abbrev-alist '(("^/tmp" . "/t")))
      (should (string= (doom-modeline-project-root) "</tmp/proj2/>"))
      (should (= calls 4))
      ;; abbreviated-home-dir changes invalidate the cache.
      (setq abbreviated-home-dir "\\`/Users/[^/]+")
      (should (string= (doom-modeline-project-root) "</tmp/proj2/>"))
      (should (= calls 5))
      ;; Replacing the handler table (as TRAMP does) invalidates the cache.
      (setq file-name-handler-alist (list (cons "\\`bar" 'ignore)))
      (should (string= (doom-modeline-project-root) "</tmp/proj2/>"))
      (should (= calls 6)))))

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

;;
;; Tests for `doom-modeline--format-buffer-file-truename' truename caching (P1')
;;

(ert-deftest doom-modeline-format-truename/uses-cache-no-io ()
  "With the default name function, no `file-truename' call is made."
  (let ((calls 0)
        (doom-modeline-buffer-file-name-function #'identity)
        (doom-modeline-buffer-file-truename-function #'identity))
    (find-file "/etc/hosts")
    (unwind-protect
        (cl-letf (((symbol-function 'file-truename)
                   (lambda (&rest _) (setq calls (1+ calls)) "SHOULD-NOT-BE-USED")))
          (should (equal (doom-modeline--format-buffer-file-truename "/etc/hosts")
                         (file-local-name buffer-file-truename)))
          (should (= calls 0)))
      (kill-buffer))))

(ert-deftest doom-modeline-format-truename/fallback-when-no-cache ()
  "Without a cached truename, fall back to a real `file-truename' call."
  (let ((calls 0)
        (doom-modeline-buffer-file-name-function #'identity)
        (doom-modeline-buffer-file-truename-function #'identity)
        (real (symbol-function 'file-truename)))
    (with-temp-buffer
      (setq buffer-file-name "/etc/hosts")
      ;; No find-file: buffer-file-truename is nil for this buffer.
      (should (null buffer-file-truename))
      (cl-letf (((symbol-function 'file-truename)
                 (lambda (&rest args) (setq calls (1+ calls)) (apply real args))))
        (let ((got (doom-modeline--format-buffer-file-truename "/etc/hosts")))
          ;; `file-truename' recurses internally, so count > 0 (not exactly 1).
          (should (> calls 0))
          (should (equal got (file-local-name (funcall real "/etc/hosts")))))))))

(ert-deftest doom-modeline-format-truename/custom-name-function-falls-back ()
  "A custom name function disables the cache to preserve semantics.
When B-F-N may be rewritten, resolve it directly as the original code did."
  (let ((calls 0)
        (doom-modeline-buffer-file-name-function (lambda (n) (concat n "X")))
        (doom-modeline-buffer-file-truename-function #'identity)
        (real (symbol-function 'file-truename)))
    (find-file "/etc/hosts")
    (unwind-protect
        (cl-letf (((symbol-function 'file-truename)
                   (lambda (&rest args) (setq calls (1+ calls)) (apply real args))))
          (doom-modeline--format-buffer-file-truename "/etc/hosts")
          (should (> calls 0)))
      (kill-buffer))))

(ert-deftest doom-modeline-format-truename/indirect-buffer-uses-base ()
  "An indirect buffer resolves truename from its base buffer."
  (let ((doom-modeline-buffer-file-name-function #'identity)
        (doom-modeline-buffer-file-truename-function #'identity))
    (find-file "/etc/hosts")
    (unwind-protect
        (let ((ind (clone-indirect-buffer " *dm-p1-ind*" nil)))
          (unwind-protect
              (with-current-buffer ind
                (should (null buffer-file-truename))
                (should (equal (doom-modeline--format-buffer-file-truename "/etc/hosts")
                               (file-local-name
                                (buffer-local-value 'buffer-file-truename
                                                    (buffer-base-buffer))))))
            (kill-buffer ind)))
      (kill-buffer))))

(ert-deftest doom-modeline-format-truename/truename-function-applied ()
  "The custom truename function still post-processes the result."
  (let ((doom-modeline-buffer-file-name-function #'identity)
        (doom-modeline-buffer-file-truename-function (lambda (s) (concat "<" s ">"))))
    (find-file "/etc/hosts")
    (unwind-protect
        (should (equal (doom-modeline--format-buffer-file-truename "/etc/hosts")
                       (concat "<" (file-local-name buffer-file-truename) ">")))
      (kill-buffer))))

(ert-deftest doom-modeline-enable-buffer-position/test ()
  "Test that `doom-modeline-enable-buffer-position' is defined correctly."
  ;; Test that the variable exists and defaults to t
  (should (boundp 'doom-modeline-enable-buffer-position))
  (should doom-modeline-enable-buffer-position)

  ;; Test that it can be set to nil
  (let ((doom-modeline-enable-buffer-position nil))
    (should-not doom-modeline-enable-buffer-position)))

;;
;; Tests for doom-modeline-def-segment
;;

(ert-deftest doom-modeline-def-segment/function-segment ()
  "Test defining a function-based segment."
  (let ((doom-modeline--fn-alist nil)
        (doom-modeline--var-alist nil))
    (unwind-protect
        (progn
          ;; Define a function segment
          (doom-modeline-def-segment test-func-seg
            "Test segment that returns a string."
            "test-value")

          ;; Should be added to fn-alist
          (should (assoc 'test-func-seg doom-modeline--fn-alist))
          ;; Should create a function
          (should (fboundp (cdr (assoc 'test-func-seg doom-modeline--fn-alist))))
          ;; Function should return the value
          (should (string= "test-value"
                           (funcall (cdr (assoc 'test-func-seg doom-modeline--fn-alist))))))
      (setq doom-modeline--fn-alist nil
            doom-modeline--var-alist nil))))

(ert-deftest doom-modeline-def-segment/variable-segment ()
  "Test defining a variable reference segment."
  (let ((doom-modeline--fn-alist nil)
        (doom-modeline--var-alist nil))
    (unwind-protect
        (progn
          ;; Define a variable segment (just a symbol, no body)
          (doom-modeline-def-segment test-var-seg buffer-info)

          ;; Should be added to var-alist, not fn-alist
          (should-not (assoc 'test-var-seg doom-modeline--fn-alist))
          (should (assoc 'test-var-seg doom-modeline--var-alist))
          (should (eq (cdr (assoc 'test-var-seg doom-modeline--var-alist)) 'buffer-info)))
      (setq doom-modeline--fn-alist nil
            doom-modeline--var-alist nil))))

(ert-deftest doom-modeline-def-segment/with-docstring ()
  "Test defining a segment with docstring."
  (let ((doom-modeline--fn-alist nil)
        (doom-modeline--var-alist nil))
    (unwind-protect
        (progn
          (doom-modeline-def-segment test-doc-seg
            "This is the docstring."
            "value")

          ;; Check the function has the docstring
          (let ((fn (cdr (assoc 'test-doc-seg doom-modeline--fn-alist))))
            (should (string= (documentation fn) "This is the docstring."))))
      (setq doom-modeline--fn-alist nil
            doom-modeline--var-alist nil))))

;;
;; Tests for doom-modeline-def-modeline
;;

(ert-deftest doom-modeline-def-modeline/basic ()
  "Test defining a basic modeline."
  (let ((doom-modeline--modelines nil))
    (unwind-protect
        (progn
          (doom-modeline-def-modeline 'test-basic-modeline
            '(buffer-info)
            '(major-mode))

          ;; Should be registered in modelines
          (should (assoc 'test-basic-modeline doom-modeline--modelines))
          ;; Should have correct lhs and rhs
          (let ((def (assoc 'test-basic-modeline doom-modeline--modelines)))
            (should (equal (cadr def) '(buffer-info)))
            (should (equal (caddr def) '(major-mode)))))
      (setq doom-modeline--modelines nil))))

(ert-deftest doom-modeline-def-modeline/lhs-only ()
  "Test defining a modeline with only LHS."
  (let ((doom-modeline--modelines nil))
    (unwind-protect
        (progn
          (doom-modeline-def-modeline 'test-lhs-only
            '(buffer-info))

          (let ((def (assoc 'test-lhs-only doom-modeline--modelines)))
            (should (equal (cadr def) '(buffer-info)))
            (should (equal (caddr def) nil))))
      (setq doom-modeline--modelines nil))))

(ert-deftest doom-modeline-def-modeline/empty-rhs ()
  "Test defining a modeline with empty RHS."
  (let ((doom-modeline--modelines nil))
    (unwind-protect
        (progn
          (doom-modeline-def-modeline 'test-empty-rhs
            '(buffer-info)
            nil)

          (let ((def (assoc 'test-empty-rhs doom-modeline--modelines)))
            (should (equal (cadr def) '(buffer-info)))
            (should (equal (caddr def) nil))))
      (setq doom-modeline--modelines nil))))

(ert-deftest doom-modeline-def-modeline/update-existing ()
  "Test updating an existing modeline definition."
  (let ((doom-modeline--modelines nil))
    (unwind-protect
        (progn
          ;; Define initial modeline
          (doom-modeline-def-modeline 'test-update
            '(buffer-info)
            '(major-mode))

          ;; Update it
          (doom-modeline-def-modeline 'test-update
            '(buffer-info buffer-position)
            '(major-mode time))

          ;; Should be updated, not duplicated
          (let ((count 0))
            (dolist (def doom-modeline--modelines)
              (when (eq (car def) 'test-update)
                (cl-incf count)))
            (should (= count 1)))

          ;; Should have new values
          (let ((def (assoc 'test-update doom-modeline--modelines)))
            (should (equal (cadr def) '(buffer-info buffer-position)))
            (should (equal (caddr def) '(major-mode time)))))
      (setq doom-modeline--modelines nil))))

(ert-deftest doom-modeline-def-modeline/multiple-segments ()
  "Test defining a modeline with multiple segments."
  (let ((doom-modeline--modelines nil))
    (unwind-protect
        (progn
          (doom-modeline-def-modeline 'test-multi
            '(bar buffer-info buffer-info-simple matches)
            '(major-mode time buffer-encoding))

          (let ((def (assoc 'test-multi doom-modeline--modelines)))
            (should (equal (cadr def) '(bar buffer-info buffer-info-simple matches)))
            (should (equal (caddr def) '(major-mode time buffer-encoding)))))
      (setq doom-modeline--modelines nil))))

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

(ert-deftest doom-modeline-buffer-encoding/undecided-eol ()
  (with-temp-buffer
    (setq buffer-file-coding-system 'utf-8)
    (let ((doom-modeline-buffer-encoding t))
      (should (vectorp (coding-system-eol-type buffer-file-coding-system)))
      (should (equal (substring-no-properties
                      (doom-modeline-segment--buffer-encoding))
                     " UTF-8 ")))))

(ert-deftest doom-modeline-buffer-encoding/undecided-eol-default-coding ()
  (with-temp-buffer
    (setq buffer-file-coding-system 'utf-8)
    (let ((doom-modeline-buffer-encoding 'nondefault)
          (doom-modeline-default-eol-type 0)
          (doom-modeline-default-coding-system 'utf-8))
      (should-not (doom-modeline-segment--buffer-encoding)))))

(ert-deftest doom-modeline-buffer-encoding/undecided-eol-nondefault-coding ()
  (with-temp-buffer
    (setq buffer-file-coding-system 'utf-8)
    (let ((doom-modeline-buffer-encoding 'nondefault)
          (doom-modeline-default-eol-type 0)
          (doom-modeline-default-coding-system 'iso-latin-1))
      (should (equal (substring-no-properties
                      (doom-modeline-segment--buffer-encoding))
                     " UTF-8 ")))))

(ert-deftest doom-modeline-buffer-encoding/known-eol-labels-and-properties ()
  (with-temp-buffer
    (let ((doom-modeline-buffer-encoding t))
      (dolist (case '((utf-8-unix " LF UTF-8 ")
                      (utf-8-dos " CRLF UTF-8 ")
                      (utf-8-mac " CR UTF-8 ")))
        (setq buffer-file-coding-system (car case))
        (let* ((text (doom-modeline-segment--buffer-encoding))
               (encoding-start (- (length text) 6)))
          (should (equal (substring-no-properties text) (cadr case)))
          (should (get-text-property 1 'face text))
          (should (eq (lookup-key (get-text-property 1 'local-map text)
                                  [mode-line mouse-1])
                      #'mode-line-change-eol))
          (should (eq (get-text-property encoding-start 'local-map text)
                      mode-line-coding-system-map)))))))

(ert-deftest doom-modeline-buffer-encoding/nondefault-field-combinations ()
  (with-temp-buffer
    (let ((doom-modeline-buffer-encoding 'nondefault)
          (doom-modeline-default-eol-type 0)
          (doom-modeline-default-coding-system 'utf-8))
      (setq buffer-file-coding-system 'utf-8-unix)
      (should-not (doom-modeline-segment--buffer-encoding))
      (setq buffer-file-coding-system 'utf-8-dos)
      (should (equal (substring-no-properties
                      (doom-modeline-segment--buffer-encoding))
                     " CRLF "))
      (let ((doom-modeline-default-coding-system 'iso-latin-1))
        (setq buffer-file-coding-system 'utf-8-unix)
        (should (equal (substring-no-properties
                        (doom-modeline-segment--buffer-encoding))
                       " UTF-8 "))
        (setq buffer-file-coding-system 'utf-8-dos)
        (should (equal (substring-no-properties
                        (doom-modeline-segment--buffer-encoding))
                       " CRLF UTF-8 "))))))

(ert-deftest doom-modeline-buffer-encoding/disabled ()
  (with-temp-buffer
    (setq buffer-file-coding-system 'utf-8)
    (let ((doom-modeline-buffer-encoding nil))
      (should-not (doom-modeline-segment--buffer-encoding)))))

;;;
;;; Eglot LSP state (issue #846)
;;;

(ert-deftest doom-modeline-eglot-face/state-transitions ()
  "The eglot face follows the server state without re-running the hook."
  (let ((managed t)
        (pending 2)
        (last-error nil)
        (server '(mock-server)))
    ;; `cl-letf' cannot bind a place whose function is void, and neither
    ;; eglot nor jsonrpc is necessarily loaded in the test session.
    (dolist (fn '(eglot-managed-p eglot-current-server eglot-project-nickname
                  jsonrpc-last-error))
      (unless (fboundp fn)
        (fset fn #'ignore)))
    (cl-letf (((symbol-function 'eglot-managed-p) (lambda () managed))
              ((symbol-function 'eglot-current-server) (lambda () server))
              ((symbol-function 'eglot-project-nickname) (lambda (_) "mock"))
              ((symbol-function 'jsonrpc-last-error) (lambda (_) last-error))
              ((symbol-function 'doom-modeline--eglot-pending-count)
               (lambda (_) pending)))
      ;; Requests are in flight when `eglot-managed-mode-hook' runs.
      (should (eq (doom-modeline--eglot-face) 'doom-modeline-lsp-warning))
      ;; They finish, but no hook runs: the segment must pick another face.
      (setq pending 0)
      (should (eq (doom-modeline--eglot-face) 'doom-modeline-lsp-success))
      (setq last-error '(jsonrpc-error))
      (should (eq (doom-modeline--eglot-face) 'doom-modeline-lsp-error))
      (setq managed nil)
      (should (eq (doom-modeline--eglot-face) 'doom-modeline-lsp-warning)))))

;; `doom-modeline--eglot-icon' reads these eglot variables dynamically.  The
;; bare `defvar' declarations in `doom-modeline-segments' are optimized away
;; when that file is byte-compiled, so declare them here as well: otherwise
;; the `let' below is a lexical binding and the icon builder gets a
;; `void-variable' error under `lexical-binding'.
(defvar eglot-menu-string)
(defvar eglot-menu)
(defvar eglot-server-menu)

(ert-deftest doom-modeline-update-eglot/caches-all-faces ()
  "Updating eglot caches an icon variant for every server state."
  (let ((eglot-menu-string "E")
        (eglot-menu #'ignore)
        (eglot-server-menu #'ignore))
    (dolist (fn '(eglot-managed-p eglot-current-server eglot-project-nickname
                  eglot--server-info eglot--major-modes))
      (unless (fboundp fn)
        (fset fn #'ignore)))
    ;; Stub the icon helper with its terminal fallback, which is what adds
    ;; FACE to the string; this keeps the test independent of icon fonts.
    (cl-letf (((symbol-function 'doom-modeline-lsp-icon)
               (lambda (text face) (propertize text 'face face)))
              ((symbol-function 'eglot-managed-p) (lambda () t))
              ((symbol-function 'eglot-current-server) (lambda () 'mock-server))
              ((symbol-function 'eglot-project-nickname) (lambda (_) "mock"))
              ((symbol-function 'eglot--server-info)
               (lambda (_) '(:name "mock-server")))
              ((symbol-function 'eglot--major-modes) (lambda (_) "mock-mode")))
      (with-temp-buffer
        (doom-modeline-update-eglot)
        (dolist (face '(doom-modeline-lsp-error
                        doom-modeline-lsp-warning
                        doom-modeline-lsp-success))
          (should
           (eq (get-text-property 0 'face (plist-get doom-modeline--eglot face))
               face)))))))

;;; doom-modeline-core-test.el ends here
