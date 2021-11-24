;;; doom-modeline-core.el --- The core libraries for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Vincent Zhang

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
;; The core libraries for doom-modeline.
;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'all-the-icons)
(require 'shrink-path)


;;
;; Compatibility
;;

(eval-and-compile
  (when (< emacs-major-version 26)
    ;; Define `if-let*' and `when-let*' variants for 25 users.
    (unless (fboundp 'if-let*) (defalias 'if-let* #'if-let))
    (unless (fboundp 'when-let*) (defalias 'when-let* #'when-let))))

;; Don’t compact font caches during GC.
(when (eq system-type 'windows-nt)
  (setq inhibit-compacting-font-caches t))

;;`file-local-name' is introduced in 25.2.2.
(unless (fboundp 'file-local-name)
  (defun file-local-name (file)
    "Return the local name component of FILE.
It returns a file name which can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
    (or (file-remote-p file 'localname) file)))

;; Set correct font width for `all-the-icons' for appropriate mode-line width.
;; @see https://emacs.stackexchange.com/questions/14420/how-can-i-fix-incorrect-character-width
(defun doom-modeline--set-char-widths (alist)
  "Set correct widths of icons characters in ALIST."
  (while (char-table-parent char-width-table)
    (setq char-width-table (char-table-parent char-width-table)))
  (dolist (pair alist)
    (let ((width (car pair))
          (chars (cdr pair))
          (table (make-char-table nil)))
      (dolist (char chars)
        (set-char-table-range table char width))
      (optimize-char-table table)
      (set-char-table-parent table char-width-table)
      (setq char-width-table table))))

(defconst doom-modeline-rhs-icons-alist
  '((2 . (;; VCS
          ?\xf0ac                      ; git-compare
          ?\xf023                      ; git-merge
          ?\xf03f                      ; arrow-down
          ?\xf02d                      ; alert
          ?\xf020                      ; git-branch

          ;; Checker
          ?\xe611                      ; do_not_disturb_alt
          ?\xe5ca                      ; check
          ?\xe192                      ; access_time
          ?\xe624                      ; sim_card_alert
          ?\xe034                      ; pause
          ?\xe645                      ; priority_high

          ;; Minor modes
          ?\xf02f                      ; gear

          ;; Persp
          ?\xe2c7                      ; folder

          ;; Preview
          ?\xe8a0                      ; pageview

          ;; REPL
          ?\xf155                      ; dollar-sign

          ;; LSP
          ?\xf135                      ; rocket

          ;; GitHub
          ?\xf09b                      ; github

          ;; Debug
          ?\xf188                      ; bug

          ;; Mail
          ?\xe0be                      ; email

          ;; IRC
          ?\xe0c9                      ; message

          ;; Battery
          ?\xe939                      ; battery-charging
          ?\xf244                      ; battery-empty
          ?\xf240                      ; battery-full
          ?\xf242                      ; battery-half
          ?\xf243                      ; battery-quarter
          ?\xf241                      ; battery-three-quarters
          ))))

(defun doom-modeline-set-char-widths (&rest _)
  "Set char widths for the unicode icons."
  (doom-modeline--set-char-widths doom-modeline-rhs-icons-alist))

(if (and (daemonp)
         (not (frame-parameter nil 'client)))
    (add-hook 'after-make-frame-functions #'doom-modeline-set-char-widths)
  (and (display-graphic-p) (doom-modeline-set-char-widths)))


;;
;; Customization
;;

(defgroup doom-modeline nil
  "A minimal and modern mode-line."
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/doom-modeline"))

(defcustom doom-modeline-height 25
  "How tall the mode-line should be. It's only respected in GUI.
If the actual char height is larger, it respects the actual char height.
If `doom-modeline-height' is <= 0 the modeline will have default height."
  :type 'integer
  :group 'doom-modeline)

(defcustom doom-modeline-bar-width 4
  "How wide the mode-line bar should be. It's only respected in GUI."
  :type 'integer
  :set (lambda (sym val)
         (set sym (if (> val 0) val 1)))
  :group 'doom-modeline)

(defcustom doom-modeline-hud nil
  "Whether to use hud instead of default bar. It's only respected in GUI."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-hud-min-height 2
  "Minimum height in pixels of the \"thumb\" of the hud.
Only respected in GUI."
  :type 'integer
  :set (lambda (sym val)
         (set sym (if (> val 1) val 1)))
  :group 'doom-modeline)

(defcustom doom-modeline-window-width-limit fill-column
  "The limit of the window width.

If `window-width' is smaller than the limit, some information won't be
displayed."
  :type '(choice integer
                 (const :tag "Disable" nil))
  :group 'doom-modeline)

(defcustom doom-modeline-project-detection 'auto
  "How to detect the project root.

nil means to use `default-directory'.

The project management packages have some issues on detecting project root.
e.g. `projectile' doesn't handle symlink folders well, while `project' is
unable to handle sub-projects.
Specify another one if you encounter the issue."
  :type '(choice (const :tag "Auto-detect" auto)
                 (const :tag "Find File in Project" ffip)
                 (const :tag "Projectile" projectile)
                 (const :tag "Built-in Project" project)
                 (const :tag "Disable" nil))
  :group 'doom-modeline)

(defcustom doom-modeline-buffer-file-name-style 'auto
  "Determines the style used by `doom-modeline-buffer-file-name'.

Given ~/Projects/FOSS/emacs/lisp/comint.el
  auto => emacs/lisp/comint.el (in a project) or comint.el
  truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  truncate-with-project => emacs/l/comint.el
  truncate-except-project => ~/P/F/emacs/l/comint.el
  truncate-upto-root => ~/P/F/e/lisp/comint.el
  truncate-all => ~/P/F/e/l/comint.el
  truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
  relative-from-project => emacs/lisp/comint.el
  relative-to-project => lisp/comint.el
  file-name => comint.el
  buffer-name => comint.el<2> (uniquify buffer name)"
  :type '(choice (const auto)
                 (const truncate-upto-project)
                 (const truncate-upto-project)
                 (const truncate-from-project)
                 (const truncate-with-project)
                 (const truncate-except-project)
                 (const truncate-upto-root)
                 (const truncate-all)
                 (const truncate-nil)
                 (const relative-from-project)
                 (const relative-to-project)
                 (const file-name)
                 (const buffer-name))
  :group'doom-modeline)

(defcustom doom-modeline-icon t
  "Whether display the icons in the mode-line.

While using the server mode in GUI, should set the value explicitly."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-major-mode-icon t
  "Whether display the icon for `major-mode'.

It respects `doom-modeline-icon'."
  :type 'boolean
  :group'doom-modeline)

(defcustom doom-modeline-major-mode-color-icon t
  "Whether display the colorful icon for `major-mode'.

It respects `all-the-icons-color-icons'."
  :type 'boolean
  :group'doom-modeline)

(defcustom doom-modeline-buffer-state-icon t
  "Whether display the icon for the buffer state.

It respects `doom-modeline-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-buffer-modification-icon t
  "Whether display the modification icon for the buffer.

It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-unicode-fallback nil
  "Whether to use unicode as a fallback (instead of ASCII) when not using icons."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-minor-modes nil
  "Whether display the minor modes in the mode-line."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline segment."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-continuous-word-count-modes
  '(markdown-mode gfm-mode org-mode)
  "Major modes in which to display word count continuously.

It respects `doom-modeline-enable-word-count'."
  :type '(repeat (symbol :tag "Major-Mode") )
  :group 'doom-modeline)

(defcustom doom-modeline-buffer-encoding t
  "Whether display the buffer encoding."
  :type '(choice (const :tag "Always" t)
                 (const :tag "When non-default" nondefault)
                 (const :tag "Never" nil))
  :group 'doom-modeline)

(defcustom doom-modeline-default-coding-system 'utf-8
  "Default coding system for `doom-modeline-buffer-encoding' `nondefault'."
  :type 'coding-system
  :group 'doom-modeline)

(defcustom doom-modeline-default-eol-type 0
  "Default EOL type for `doom-modeline-buffer-encoding' `nondefault'."
  :type '(choice (const :tag "Unix-style LF" 0)
                 (const :tag "DOS-style CRLF" 1)
                 (const :tag "Mac-style CR" 2))
  :group 'doom-modeline)

(defcustom doom-modeline-indent-info nil
  "Whether display the indentation information."
  :type 'boolean
  :group 'doom-modeline)

;; It is based upon `editorconfig-indentation-alist' but is used to read indentation levels instead
;; of setting them. (https://github.com/editorconfig/editorconfig-emacs)
(defcustom doom-modeline-indent-alist
  '((apache-mode apache-indent-level)
    (awk-mode c-basic-offset)
    (bpftrace-mode c-basic-offset)
    (c++-mode c-basic-offset)
    (c-mode c-basic-offset)
    (cmake-mode cmake-tab-width)
    (coffee-mode coffee-tab-width)
    (cperl-mode cperl-indent-level)
    (crystal-mode crystal-indent-level)
    (csharp-mode c-basic-offset)
    (css-mode css-indent-offset)
    (d-mode c-basic-offset)
    (emacs-lisp-mode lisp-indent-offset)
    (enh-ruby-mode enh-ruby-indent-level)
    (erlang-mode erlang-indent-level)
    (ess-mode ess-indent-offset)
    (f90-mode f90-associate-indent
              f90-continuation-indent
              f90-critical-indent
              f90-do-indent
              f90-if-indent
              f90-program-indent
              f90-type-indent)
    (feature-mode feature-indent-offset
                  feature-indent-level)
    (fsharp-mode fsharp-continuation-offset
                 fsharp-indent-level
                 fsharp-indent-offset)
    (groovy-mode groovy-indent-offset)
    (haskell-mode haskell-indent-spaces
                  haskell-indent-offset
                  haskell-indentation-layout-offset
                  haskell-indentation-left-offset
                  haskell-indentation-starter-offset
                  haskell-indentation-where-post-offset
                  haskell-indentation-where-pre-offset
                  shm-indent-spaces)
    (haxor-mode haxor-tab-width)
    (idl-mode c-basic-offset)
    (jade-mode jade-tab-width)
    (java-mode c-basic-offset)
    (js-mode js-indent-level)
    (js-jsx-mode js-indent-level
                 sgml-basic-offset)
    (js2-mode js2-basic-offset)
    (js2-jsx-mode js2-basic-offset
                  sgml-basic-offset)
    (js3-mode js3-indent-level)
    (json-mode js-indent-level)
    (julia-mode julia-indent-offset)
    (kotlin-mode kotlin-tab-width)
    (latex-mode tex-indent-basic)
    (lisp-mode lisp-indent-offset)
    (livescript-mode livescript-tab-width)
    (lua-mode lua-indent-level)
    (matlab-mode matlab-indent-level)
    (mips-mode mips-tab-width)
    (mustache-mode mustache-basic-offset)
    (nasm-mode nasm-basic-offset)
    (nginx-mode nginx-indent-level)
    (nxml-mode nxml-child-indent)
    (objc-mode c-basic-offset)
    (octave-mode octave-block-offset)
    (perl-mode perl-indent-level)
    (php-mode c-basic-offset)
    (pike-mode c-basic-offset)
    (ps-mode ps-mode-tab)
    (pug-mode pug-tab-width)
    (puppet-mode puppet-indent-level)
    (python-mode python-indent-offset)
    (ruby-mode ruby-indent-level)
    (rust-mode rust-indent-offset)
    (rustic-mode rustic-indent-offset)
    (scala-mode scala-indent:step)
    (scss-mode css-indent-offset)
    (sgml-mode sgml-basic-offset)
    (sh-mode sh-basic-offset
             sh-indentation)
    (slim-mode slim-indent-offset)
    (sml-mode sml-indent-level)
    (tcl-mode tcl-indent-level
              tcl-continued-indent-level)
    (terra-mode terra-indent-level)
    (typescript-mode typescript-indent-level)
    (verilog-mode verilog-indent-level
                  verilog-indent-level-behavioral
                  verilog-indent-level-declaration
                  verilog-indent-level-module
                  verilog-cexp-indent
                  verilog-case-indent)
    (web-mode web-mode-attr-indent-offset
              web-mode-attr-value-indent-offset
              web-mode-code-indent-offset
              web-mode-css-indent-offset
              web-mode-markup-indent-offset
              web-mode-sql-indent-offset
              web-mode-block-padding
              web-mode-script-padding
              web-mode-style-padding)
    (yaml-mode yaml-indent-offset))
  "Indentation retrieving variables matched to major modes used
  when `doom-modeline-indent-info' is non-nil. When multiple
  variables are specified for a mode, they will be tried resolved
  in the given order."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'doom-modeline)

(defcustom doom-modeline-checker-simple-format t
  "If non-nil, only display one number for checker information if applicable."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-number-limit 99
  "The maximum number displayed for notifications."
  :type 'integer
  :group 'doom-modeline)

(defcustom doom-modeline-vcs-max-length 12
  "The maximum displayed length of the branch name of version control."
  :type 'integer
  :group 'doom-modeline)

(defcustom doom-modeline-workspace-name t
  "Whether display the workspace name.

Non-nil to display in the mode-line."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-persp-name t
  "Whether display the perspective name.

Non-nil to display in the mode-line."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-display-default-persp-name nil
  "If non nil the default perspective name is displayed in the mode-line."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-persp-icon t
  "If non nil the perspective name is displayed alongside a folder icon."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-repl t
  "Whether display the `repl' state.

Non-nil to display in the mode-line."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-lsp t
  "Whether display the `lsp' state.

Non-nil to display in the mode-line."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-github nil
  "Whether display the GitHub notifications.

It requires `ghub' and `async' packages."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-github-interval 1800 ; (* 30 60)
  "The interval of checking GitHub."
  :type 'integer
  :group 'doom-modeline)

(defcustom doom-modeline-env-version t
  "Whether display the environment version."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-modal-icon t
  "Whether display the modal state icon.

Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-mu4e nil
  "Whether display the mu4e notifications.

It requires `mu4e-alert' package."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-gnus nil
  "Whether to display notifications from gnus.

It requires `gnus' to be setup"
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-gnus-timer 2
  "The wait time in minutes before gnus fetches mail.

If nil, don't set up a hook."
  :type 'integer
  :group 'doom-modeline)

(defcustom doom-modeline-gnus-excluded-groups nil
  "A list of groups to be excluded from the unread count.
Groups' names list in `gnus-newsrc-alist'`"
  :type '(repeat string)
  :group 'doom-modeline)

(defcustom doom-modeline-irc t
  "Whether display the irc notifications.

It requires `circe' or `erc' package."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-irc-buffers nil
  "Whether display the unread irc buffers."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-irc-stylize 'identity
  "Function to stylize the irc buffer names."
  :type 'function
  :group 'doom-modeline)


;;
;; Faces
;;

(defgroup doom-modeline-faces nil
  "The faces of `doom-modeline'."
  :group 'doom-modeline
  :group 'faces
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/doom-modeline"))

(defface doom-modeline-spc-face
  '((t (:inherit mode-line)))
  "Face used for the white space."
  :group 'doom-modeline-faces)

(defface doom-modeline-vspc-face
  '((t (:inherit variable-pitch)))
  "Face used for the variable white space."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-minor-mode
  '((t (:inherit font-lock-doc-face :slant normal)))
  "Face used for the minor-modes segment in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-project-parent-dir
  '((t (:inherit (font-lock-comment-face bold))))
  "Face used for the project parent directory of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-project-dir
  '((t (:inherit (font-lock-string-face bold))))
  "Face used for the project directory of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-project-root-dir
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the project part of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `anzu', `evil-substitute' and`iedit', etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-host
  '((t (:inherit italic)))
  "Face for remote hosts in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-input-method
  '((t (:inherit (mode-line-emphasis bold))))
  "Face for input method in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-input-method-alt
  '((t (:inherit (font-lock-doc-face bold) :slant normal)))
  "Alternative face for input method in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-debug
  '((t (:inherit (font-lock-doc-face bold) :slant normal)))
  "Face for debug-level messages in the mode-line. Used by vcs, checker, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-info
  '((t (:inherit (success bold))))
  "Face for info-level messages in the mode-line. Used by vcs, checker, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-warning
  '((t (:inherit (warning bold))))
  "Face for warnings in the mode-line. Used by vcs, checker, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-urgent
  '((t (:inherit (error bold))))
  "Face for errors in the mode-line. Used by vcs, checker, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-notification
  '((t (:inherit doom-modeline-warning)))
  "Face for notifications in the mode-line. Used by GitHub, mu4e,
etc. (also see the face `doom-modeline-unread-number')."
  :group 'doom-modeline-faces)

(defface doom-modeline-unread-number
  '((t (:slant italic :weight normal)))
  "Face for unread number in the mode-line. Used by GitHub, mu4e, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-bar
  '((t (:inherit highlight)))
  "The face used for the left-most bar in the mode-line of an active window."
  :group 'doom-modeline-faces)

(defface doom-modeline-bar-inactive
  `((t (:background ,(face-foreground 'mode-line-inactive))))
  "The face used for the left-most bar in the mode-line of an inactive window."
  :group 'doom-modeline-faces)

(defface doom-modeline-debug-visual
  `((((class color) (background light))
     (:background ,(face-foreground 'all-the-icons-orange)))
    (((class color) (background dark))
     (:background ,(face-foreground 'all-the-icons-dorange))))
  "Face to use for the mode-line while debugging."
  :group 'doom-modeline)

(defface doom-modeline-evil-emacs-state
  '((t (:inherit (font-lock-builtin-face bold))))
  "Face for the Emacs state tag in evil state indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-insert-state
  '((t (:inherit (font-lock-keyword-face bold))))
  "Face for the insert state tag in evil state indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-motion-state
  '((t :inherit (font-lock-doc-face bold) :slant normal))
  "Face for the motion state tag in evil state indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-normal-state
  '((t (:inherit doom-modeline-info)))
  "Face for the normal state tag in evil state indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-operator-state
  '((t (:inherit doom-modeline-buffer-file)))
  "Face for the operator state tag in evil state indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-visual-state
  '((t (:inherit doom-modeline-warning)))
  "Face for the visual state tag in evil state indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-replace-state
  '((t (:inherit doom-modeline-urgent)))
  "Face for the replace state tag in evil state indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-persp-name
  '((t (:inherit (font-lock-comment-face italic))))
  "Face for the persp name."
  :group 'doom-modeline-faces)

(defface doom-modeline-persp-buffer-not-in-persp
  '((t (:inherit (font-lock-doc-face bold italic))))
  "Face for the buffers which are not in the persp."
  :group 'doom-modeline-faces)

(defface doom-modeline-repl-success
  '((t (:inherit success :weight normal)))
  "Face for REPL success state."
  :group 'doom-modeline-faces)

(defface doom-modeline-repl-warning
  '((t (:inherit warning :weight normal)))
  "Face for REPL warning state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-success
  '((t (:inherit success :weight normal)))
  "Face for LSP success state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-warning
  '((t (:inherit warning :weight normal)))
  "Face for LSP warning state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-error
  '((t (:inherit error :weight normal)))
  "Face for LSP error state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-running
  '((t (:inherit compilation-mode-line-run :weight normal :slant normal)))
  "Face for LSP running state."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-charging
  '((t (:inherit success :weight normal)))
  "Face for battery charging status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-full
  '((t (:inherit success :weight normal)))
  "Face for battery full status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-normal
  '((t (:inherit mode-line :weight normal)))
  "Face for battery normal status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-warning
  '((t (:inherit warning :weight normal)))
  "Face for battery warning status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-critical
  '((t (:inherit error :weight normal)))
  "Face for battery critical status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-error
  '((t (:inherit error :weight normal)))
  "Face for battery error status."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-timemachine
  '((t (:inherit doom-modeline-buffer-file :slant italic)))
  "Face for timemachine status."
  :group 'doom-modeline-faces)


;;
;; Externals
;;

(declare-function face-remap-remove-relative 'face-remap)
(declare-function ffip-get-project-root-directory 'find-file-in-project)
(declare-function project-roots 'project)
(declare-function projectile-project-root 'projectile)


;;
;; Core helpers
;;

;; FIXME #183: Force to calculate mode-line height
;; @see https://github.com/seagle0128/doom-modeline/issues/183
;; @see https://github.com/seagle0128/doom-modeline/issues/483
(defun doom-modeline-redisplay (&rest _)
  "Call `redisplay' to trigger mode-line height calculations.

Certain functions, including e.g. `fit-window-to-buffer', base
their size calculations on values which are incorrect if the
mode-line has a height different from that of the `default' face
and certain other calculations have not yet taken place for the
window in question.

These calculations can be triggered by calling `redisplay'
explicitly at the appropriate time and this functions purpose
is to make it easier to do so.

This function is like `redisplay' with non-nil FORCE argument,
but it will only trigger a redisplay when there is a non nil
`mode-line-format' and the height of the mode-line is different
from that of the `default' face. This function is intended to be
used as an advice to window creation functions."
  (when (and (bound-and-true-p doom-modeline-mode)
             mode-line-format
             (/= (frame-char-height) (window-mode-line-height)))
    (redisplay t)))
(unless (>= emacs-major-version 29)
  (advice-add 'split-window :after #'doom-modeline-redisplay))

;; Keep `doom-modeline-current-window' up-to-date
(defun doom-modeline--get-current-window (&optional frame)
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent frame))
      (frame-selected-window (frame-parent frame))
    (frame-selected-window frame)))

(defvar doom-modeline-current-window (doom-modeline--get-current-window))

(defun doom-modeline--active ()
  "Whether is an active window."
  (unless (and (bound-and-true-p mini-frame-frame)
               (and (frame-live-p mini-frame-frame)
                    (frame-visible-p mini-frame-frame)))
    (and doom-modeline-current-window
         (eq (doom-modeline--get-current-window) doom-modeline-current-window))))

(defun doom-modeline-set-selected-window (&rest _)
  "Set `doom-modeline-current-window' appropriately."
  (let ((win (doom-modeline--get-current-window)))
    (setq doom-modeline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun doom-modeline-unset-selected-window ()
  "Unset `doom-modeline-current-window' appropriately."
  (setq doom-modeline-current-window nil))

(add-hook 'pre-redisplay-functions #'doom-modeline-set-selected-window)

;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar doom-modeline-remap-face-cookie nil)
(defun doom-modeline-focus ()
  "Focus mode-line."
  (when doom-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative doom-modeline-remap-face-cookie)))
(defun doom-modeline-unfocus ()
  "Unfocus mode-line."
  (setq doom-modeline-remap-face-cookie
        (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defun doom-modeline-focus-change (&rest _)
          (if (frame-focus-state)
              (doom-modeline-focus)
            (doom-modeline-unfocus)))
        (advice-add #'handle-switch-frame :after #'doom-modeline-focus-change)
        (add-function :after after-focus-change-function #'doom-modeline-focus-change))
    (progn
      (add-hook 'focus-in-hook #'doom-modeline-focus)
      (add-hook 'focus-out-hook #'doom-modeline-unfocus))))


;;
;; Core
;;

(defvar doom-modeline-fn-alist ())
(defvar doom-modeline-var-alist ())

(defmacro doom-modeline-def-segment (name &rest body)
  "Defines a modeline segment NAME with BODY and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "doom-modeline-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "%s modeline segment" name))))
    (cond ((and (symbolp (car body))
                (not (cdr body)))
           (add-to-list 'doom-modeline-var-alist (cons name (car body)))
           `(add-to-list 'doom-modeline-var-alist (cons ',name ',(car body))))
          (t
           (add-to-list 'doom-modeline-fn-alist (cons name sym))
           `(progn
              (fset ',sym (lambda () ,docstring ,@body))
              (add-to-list 'doom-modeline-fn-alist (cons ',name ',sym))
              ,(unless (bound-and-true-p byte-compile-current-file)
                 `(let (byte-compile-warnings)
                    (byte-compile #',sym))))))))

(defun doom-modeline--prepare-segments (segments)
  "Prepare mode-line `SEGMENTS'."
  (let (forms it)
    (dolist (seg segments)
      (cond ((stringp seg)
             (push seg forms))
            ((symbolp seg)
             (cond ((setq it (cdr (assq seg doom-modeline-fn-alist)))
                    (push (list :eval (list it)) forms))
                   ((setq it (cdr (assq seg doom-modeline-var-alist)))
                    (push it forms))
                   ((error "%s is not a defined segment" seg))))
            ((error "%s is not a valid segment" seg))))
    (nreverse forms)))

(defvar doom-modeline--font-width-cache nil)
(defun doom-modeline--font-width ()
  "Cache the font width."
  (if (display-graphic-p)
      (let ((attributes (face-all-attributes 'mode-line)))
        (or (cdr (assoc attributes doom-modeline--font-width-cache))
            (let ((width (window-font-width nil 'mode-line)))
              (push (cons attributes width) doom-modeline--font-width-cache)
              width)))
    1))

;; Refresh the font width after setting frame parameters
;; to ensure the font width is correct.
(defun doom-modeline-refresh-font-width-cache (&rest _)
  "Refresh the font width cache."
  (setq doom-modeline--font-width-cache nil)
  (doom-modeline--font-width))
(add-hook 'window-setup-hook #'doom-modeline-refresh-font-width-cache)
(add-hook 'after-make-frame-functions #'doom-modeline-refresh-font-width-cache)
(add-hook 'after-setting-font-hook #'doom-modeline-refresh-font-width-cache)
(add-hook 'server-after-make-frame-hook #'doom-modeline-refresh-font-width-cache)

(defun doom-modeline-def-modeline (name lhs &optional rhs)
  "Defines a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `doom-modeline' for retrieval).
LHS and RHS are lists of symbols of modeline segments defined with
`doom-modeline-def-segment'.

Example:
  (doom-modeline-def-modeline 'minimal
    '(bar matches \" \" buffer-info)
    '(media-info major-mode))
  (doom-modeline-set-modeline 'minimal t)"
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
        (lhs-forms (doom-modeline--prepare-segments lhs))
        (rhs-forms (doom-modeline--prepare-segments rhs)))
    (defalias sym
      (lambda ()
        (list lhs-forms
              (propertize
               " "
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
               'display `((space
                           :align-to
                           (- (+ right right-fringe right-margin)
                              ,(* (let ((width (doom-modeline--font-width)))
                                    (or (and (= width 1) 1)
                                        (/ width (frame-char-width) 1.0)))
                                  (string-width
                                   (format-mode-line (cons "" rhs-forms))))))))
              rhs-forms))
      (concat "Modeline:\n"
              (format "  %s\n  %s"
                      (prin1-to-string lhs)
                      (prin1-to-string rhs))))))
(put 'doom-modeline-def-modeline 'lisp-indent-function 'defun)

(defun doom-modeline (key)
  "Return a mode-line configuration associated with KEY (a symbol).
Throws an error if it doesn't exist."
  (let ((fn (intern-soft (format "doom-modeline-format--%s" key))))
    (when (functionp fn)
      `(:eval (,fn)))))

(defun doom-modeline-set-modeline (key &optional default)
  "Set the modeline format. Does nothing if the modeline KEY doesn't exist.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (when-let ((modeline (doom-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          (list "%e" modeline))))


;;
;; Helpers
;;

(defsubst doom-modeline-spc ()
  "Text style with whitespace."
  (propertize " " 'face (if (doom-modeline--active)
                            'doom-modeline-spc-face
                          '(:inherit mode-line-inactive))))

(defsubst doom-modeline-wspc ()
  "Text style with wide whitespace."
  (propertize "  " 'face (if (doom-modeline--active)
                            'doom-modeline-spc-face
                          '(:inherit mode-line-inactive))))

(defsubst doom-modeline-vspc ()
  "Text style with icons in mode-line."
  (propertize " " 'face (if (doom-modeline--active)
                            'doom-modeline-vspc-face
                          '(:inherit (doom-modeline-vspc-face mode-line-inactive)))))

(defun doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (let ((height (face-attribute 'mode-line :height)))
    ;; WORKAROUND: Fix tall issue of 27 on Linux
    ;; @see https://github.com/seagle0128/doom-modeline/issues/271
    (round
     (* (if (or (<= doom-modeline-height 0)
                (and (>= emacs-major-version 27)
                     (not (eq system-type 'darwin))))
            1.0
          (if doom-modeline-icon 1.68 1.25))
        (cond ((integerp height) (/ height 10))
              ((floatp height) (* height (frame-char-height)))
              (t (frame-char-height)))))))

(defun doom-modeline--original-value (sym)
  "Return the original value for SYM, if any.

If SYM has an original value, return it in a list. Return nil
otherwise."
  (let* ((orig-val-expr (get sym 'standard-value)))
    (when (consp orig-val-expr)
      (ignore-errors
        (list
         (eval (car orig-val-expr)))))))

(defun doom-modeline-add-variable-watcher (symbol watch-function)
  "Cause WATCH-FUNCTION to be called when SYMBOL is set if possible.

See docs of `add-variable-watcher'."
  (when (fboundp 'add-variable-watcher)
    (add-variable-watcher symbol watch-function)))

(defun doom-modeline-propertize-icon (icon &optional face)
  "Propertize the ICON with the specified FACE.

The face should be the first attribute, or the font family may be overridden.
So convert the face \":family XXX :height XXX :inherit XXX\" to
\":inherit XXX :family XXX :height XXX\".
See https://github.com/seagle0128/doom-modeline/issues/301."
  (if (and doom-modeline-icon (display-graphic-p))
      (when-let ((props (get-text-property 0 'face icon)))
        (cl-destructuring-bind (&key family height inherit &allow-other-keys) props
          (propertize icon 'face `(:inherit ,(or face inherit props)
                                   :family  ,family
                                   :height  ,height))))
    (propertize icon 'face face)))

(defun doom-modeline-icon (icon-set icon-name unicode text &rest args)
  "Display icon of ICON-NAME with ARGS in mode-line.

ICON-SET includes `octicon', `faicon', `material', `alltheicons' and `fileicon',
etc.
UNICODE is the unicode char fallback. TEXT is the ASCII char fallback.
ARGS is same as `all-the-icons-octicon' and others."
  (let ((face (or (plist-get args :face) 'mode-line)))
    (or
     ;; Icons
     (when (and (display-graphic-p)
                doom-modeline-icon
                icon-name
                (not (string-empty-p icon-name)))
       (when-let* ((func (all-the-icons--function-name icon-set))
                   (icon (and (fboundp func) (apply func icon-name args))))
         (doom-modeline-propertize-icon icon face)))
     ;; Unicode fallback
     (and doom-modeline-unicode-fallback
          unicode
          (not (string-empty-p unicode))
          (char-displayable-p (string-to-char unicode))
          (propertize unicode 'face face))
     ;; ASCII text
     (and text (propertize text 'face face))
     "")))

(defun doom-modeline--create-bar-image (face width height)
  "Create the bar image.
Use FACE1 for the bar, FACE2 for the background.
WIDTH and HEIGHT are the image size in pixels."
  (when (and (display-graphic-p)
             (image-type-available-p 'pbm))
    (propertize
     " " 'display
     (let ((color (or (face-background face nil t) "None")))
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" width height)
                  (make-string (* width height) ?1)
                  "\n")
          'pbm t :foreground color :ascent 'center))))))

(defun doom-modeline--create-hud-image
    (face1 face2 width height top-margin bottom-margin)
  "Create the hud image.
Use FACE1 for the bar, FACE2 for the background.
WIDTH and HEIGHT are the image size in pixels.
TOP-MARGIN and BOTTOM-MARGIN are the size of the margin above and below the bar,
respectively."
  (when (and (display-graphic-p)
             (image-type-available-p 'pbm))
    (let ((min-height (min height doom-modeline-hud-min-height)))
      (unless (> (- height top-margin bottom-margin) min-height)
        (let ((margin (- height min-height)))
          (setq top-margin (/ (* margin top-margin) (+ top-margin bottom-margin))
                bottom-margin (- margin top-margin)))))
    (propertize
     " " 'display
     (let ((color1 (or (face-background face1 nil t) "None"))
           (color2 (or (face-background face2 nil t) "None")))
       (create-image
        (concat
         (format "P1\n%i %i\n" width height)
         (make-string (* top-margin width) ?0)
         (make-string (* (- height top-margin bottom-margin) width) ?1)
         (make-string (* bottom-margin width) ?0)
         "\n")
        'pbm t :foreground color1 :background color2 :ascent 'center)))))

;; Check whether `window-total-width' is smaller than the limit
(defvar-local doom-modeline--limited-width-p nil)
(defun doom-modeline-window-size-change-function (&rest _)
  "Function for `window-size-change-functions'."
  (setq doom-modeline--limited-width-p
        (and (numberp doom-modeline-window-width-limit)
             (<= (+ (window-total-width)
                    (or scroll-bar-width 0)
                    (or left-fringe-width 0)
                    (or right-fringe-width 0)
                    (or left-margin-width 0)
                    (or right-margin-width 0))
                 doom-modeline-window-width-limit))))

(add-hook 'window-size-change-functions #'doom-modeline-window-size-change-function)
(add-hook 'buffer-list-update-hook #'doom-modeline-window-size-change-function)

(defvar-local doom-modeline--project-root nil)
(defun doom-modeline--project-root ()
  "Get the path to the root of your project.
Return nil if no project was found."
  (or doom-modeline--project-root
      (setq doom-modeline--project-root
            (pcase (if (eq doom-modeline-project-detection 'auto)
                       (cond
                        ((fboundp 'ffip-get-project-root-directory)  'ffip)
                        ((fboundp 'projectile-project-root) 'projectile)
                        ((fboundp 'project-current)  'project)
                        (t 'default))
                     doom-modeline-project-detection)
              ('ffip
               (let ((inhibit-message t))
                 (ffip-get-project-root-directory)))
              ('projectile
               (projectile-project-root))
              ('project
               (when-let ((project (project-current)))
                 (expand-file-name (cdr project))))))))

(defun doom-modeline-project-p ()
  "Check if the file is in a project."
  (doom-modeline--project-root))

(defun doom-modeline-project-root ()
  "Get the path to the root of your project.
Return `default-directory' if no project was found."
  (or (doom-modeline--project-root) default-directory))

;; HACK: fix invalid-regexp "Trailing backslash" while handling $HOME on Windows
(defun doom-modeline-shrink-path--dirs-internal (full-path &optional truncate-all)
  "Return fish-style truncated string based on FULL-PATH.
Optional parameter TRUNCATE-ALL will cause the function to truncate the last
directory too."
  (let* ((home (expand-file-name "~"))
         (path (replace-regexp-in-string
                (s-concat "^" home) "~" full-path))
         (split (s-split "/" path 'omit-nulls))
         (split-len (length split))
         shrunk)
    (->> split
         (--map-indexed (if (= it-index (1- split-len))
                            (if truncate-all (shrink-path--truncate it) it)
                          (shrink-path--truncate it)))
         (s-join "/")
         (setq shrunk))
    (s-concat (unless (s-matches? (rx bos (or "~" "/")) shrunk) "/")
              shrunk
              (unless (s-ends-with? "/" shrunk) "/"))))
(advice-add #'shrink-path--dirs-internal :override #'doom-modeline-shrink-path--dirs-internal)

(defun doom-modeline-buffer-file-name ()
  "Propertized variable `buffer-file-name' based on
`doom-modeline-buffer-file-name-style'."
  (let* ((buffer-file-name (file-local-name (or (buffer-file-name (buffer-base-buffer)) "")))
         (buffer-file-truename (file-local-name
                                (or buffer-file-truename (file-truename buffer-file-name) "")))
         (file-name
          (pcase doom-modeline-buffer-file-name-style
            ('auto
             (if (doom-modeline-project-p)
                 (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil nil 'hide)
               (propertize "%b" 'face 'doom-modeline-buffer-file)))
            ('truncate-upto-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink))
            ('truncate-from-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil 'shrink))
            ('truncate-with-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shink 'hide))
            ('truncate-except-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shink))
            ('truncate-upto-root
             (doom-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename))
            ('truncate-all
             (doom-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename t))
            ('truncate-nil
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename))
            ('relative-to-project
             (doom-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename))
            ('relative-from-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil nil 'hide))
            ('file-name
             (propertize (file-name-nondirectory buffer-file-name)
                         'face 'doom-modeline-buffer-file))
            ((or 'buffer-name _)
             (propertize "%b" 'face 'doom-modeline-buffer-file)))))
    (propertize (if (string-empty-p file-name)
                    (propertize "%b" 'face 'doom-modeline-buffer-file)
                  file-name)
                'mouse-face 'mode-line-highlight
                'help-echo (concat buffer-file-truename
                                   (unless (string= (file-name-nondirectory buffer-file-truename)
                                                    (buffer-name))
                                     (concat "\n" (buffer-name)))
                                   "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
                'local-map mode-line-buffer-identification-keymap)))

(defun doom-modeline--buffer-file-name-truncate (file-path true-file-path &optional truncate-tail)
  "Propertized variable `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory true-file-path))))
    (if (null dirs)
        (propertize "%b" 'face 'doom-modeline-buffer-file)
      (let ((dirname (car dirs))
            (basename (cdr dirs)))
        (concat (propertize (concat dirname
                                    (if truncate-tail (substring basename 0 1) basename)
                                    "/")
                            'face 'doom-modeline-project-root-dir)
                (propertize (file-name-nondirectory file-path)
                            'face 'doom-modeline-buffer-file))))))

(defun doom-modeline--buffer-file-name-relative (_file-path true-file-path &optional include-project)
  "Propertized variable `buffer-file-name' showing directories relative to
project's root only."
  (let ((root (file-local-name (doom-modeline-project-root))))
    (if (null root)
        (propertize "%b" 'face 'doom-modeline-buffer-file)
      (let ((relative-dirs (file-relative-name (file-name-directory true-file-path)
                                               (if include-project (concat root "../") root))))
        (and (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face 'doom-modeline-buffer-path)
                (propertize (file-name-nondirectory true-file-path)
                            'face 'doom-modeline-buffer-file))))))

(defun doom-modeline--buffer-file-name (file-path
                                        _true-file-path
                                        &optional
                                        truncate-project-root-parent
                                        truncate-project-relative-path
                                        hide-project-root-parent)
  "Propertized variable `buffer-file-name' given by FILE-PATH.
If TRUNCATE-PROJECT-ROOT-PARENT is non-nil will be saved by truncating project
root parent down fish-shell style.

Example:
  ~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/lisp/comint.el

If TRUNCATE-PROJECT-RELATIVE-PATH is non-nil will be saved by truncating project
relative path down fish-shell style.

Example:
  ~/Projects/FOSS/emacs/lisp/comint.el => ~/Projects/FOSS/emacs/l/comint.el

If HIDE-PROJECT-ROOT-PARENT is non-nil will hide project root parent.

Example:
  ~/Projects/FOSS/emacs/lisp/comint.el => emacs/lisp/comint.el"
  (let ((project-root (file-local-name (doom-modeline-project-root))))
    (concat
     ;; Project root parent
     (unless hide-project-root-parent
       (when-let (root-path-parent
                  (file-name-directory (directory-file-name project-root)))
         (propertize
          (if (and truncate-project-root-parent
                   (not (string-empty-p root-path-parent))
                   (not (string= root-path-parent "/")))
              (shrink-path--dirs-internal root-path-parent t)
            (abbreviate-file-name root-path-parent))
          'face 'doom-modeline-project-parent-dir)))
     ;; Project directory
     (propertize
      (concat (file-name-nondirectory (directory-file-name project-root)) "/")
      'face 'doom-modeline-project-dir)
     ;; relative path
     (propertize
      (when-let (relative-path (file-relative-name
                                (or (file-name-directory file-path) "./")
                                project-root))
        (if (string= relative-path "./")
            ""
          (if truncate-project-relative-path
              (substring (shrink-path--dirs-internal relative-path t) 1)
            relative-path)))
      'face 'doom-modeline-buffer-path)
     ;; File name
     (propertize (file-name-nondirectory file-path)
                 'face 'doom-modeline-buffer-file))))

(provide 'doom-modeline-core)

;;; doom-modeline-core.el ends here
