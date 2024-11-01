;;; doom-modeline-core.el --- The core libraries for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Vincent Zhang

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
;; The core libraries for doom-modeline.
;;

;;; Code:

(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
(require 'nerd-icons)
(require 'shrink-path)


;;
;; Compatibility
;;

(unless (boundp 'mode-line-right-align-edge)
  (defcustom mode-line-right-align-edge 'window
    "Where mode-line should align to.
Internally, that function uses `:align-to' in a display property,
so aligns to the left edge of the given area.  See info node
`(elisp)Pixel Specification'.

Must be set to a symbol.  Acceptable values are:
- `window': align to extreme right of window, regardless of margins
  or fringes
- `right-fringe': align to right-fringe
- `right-margin': align to right-margin"
    :type '(choice (const right-margin)
                   (const right-fringe)
                   (const window))
    :group 'mode-line))


;;
;; Optimization
;;

;; Don’t compact font caches during GC.
(when (eq system-type 'windows-nt)
  (setq inhibit-compacting-font-caches t))


;;
;; Customization
;;

(defgroup doom-modeline nil
  "A minimal and modern mode-line."
  :group 'mode-line
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/doom-modeline"))

(defcustom doom-modeline-support-imenu nil
  "If non-nil, cause imenu to see `doom-modeline' declarations.
This is done by adjusting `lisp-imenu-generic-expression' to
include support for finding `doom-modeline-def-*' forms.

Must be set before loading `doom-modeline'."
  :type 'boolean
  :set (lambda (_sym val)
         (if val
             (add-hook 'emacs-lisp-mode-hook #'doom-modeline-add-imenu)
           (remove-hook 'emacs-lisp-mode-hook #'doom-modeline-add-imenu)))
  :group 'doom-modeline)

(defcustom doom-modeline-height (+ (frame-char-height) 4)
  "How tall the mode-line should be. It's only respected in GUI.
If the actual char height is larger, it respects the actual char height."
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

(defcustom doom-modeline-window-width-limit 85
  "The limit of the window width.

If `window-width' is smaller than the limit, some information won't be
displayed. It can be an integer or a float number. nil means no limit."
  :type '(choice integer
                 float
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
  auto => emacs/l/comint.el (in a project) or comint.el
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
  file-name-with-project => FOSS|comint.el
  buffer-name => comint.el<2> (uniquify buffer name)"
  :type '(choice (const auto)
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
                 (const file-name-with-project)
                 (const buffer-name))
  :group'doom-modeline)

(defcustom doom-modeline-buffer-file-true-name nil
  "Use `file-truename' on buffer file name.

Project detection(projectile.el) may uses `file-truename' on directory path.
Turn on this to provide right relative path for buffer file name."
  :type 'boolean
  :group'doom-modeline)

(defcustom doom-modeline-icon t
  "Whether display the icons in the mode-line.

While using the server mode in GUI, should set the value explicitly."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-major-mode-icon t
  "Whether display the icon for `major-mode'.

It respects option `doom-modeline-icon'."
  :type 'boolean
  :group'doom-modeline)

(defcustom doom-modeline-major-mode-color-icon t
  "Whether display the colorful icon for `major-mode'.

It respects option `nerd-icons-color-icons'."
  :type 'boolean
  :group'doom-modeline)

(defcustom doom-modeline-buffer-state-icon t
  "Whether display the icon for the buffer state.

It respects option `doom-modeline-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-buffer-modification-icon t
  "Whether display the modification icon for the buffer.

It respects option `doom-modeline-icon' and `doom-modeline-buffer-state-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-lsp-icon t
  "Whether display the icon of lsp client.

It respects option `doom-modeline-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-time-icon t
  "Whether display the icon of time.

It respects option `doom-modeline-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-time-live-icon t
  "Whether display the live icons of time.

It respects option `doom-modeline-icon' and option `doom-modeline-time-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-time-analogue-clock t
  "Whether to draw an analogue clock SVG as the live time icon.
It respects the option `doom-modeline-icon', option `doom-modeline-time-icon',
and option `doom-modeline-time-live-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-time-clock-minute-resolution 1
  "The clock will be updated every this many minutes, truncated.
See `doom-modeline-time-analogue-clock'."
  :type 'natnum
  :group 'doom-modeline)

(defcustom doom-modeline-time-clock-size 0.7
  "Size of the analogue clock drawn, either in pixels or as a proportional height.
An integer value is used as the diameter of clock in pixels.
A floating point value sets the diameter of the clock realtive to
`doom-modeline-height'.

Only relevant when `doom-modeline-time-analogue-clock' is non-nil, which see."
  :type 'number
  :group 'doom-modeline)

(defcustom doom-modeline-unicode-fallback nil
  "Whether to use unicode as a fallback (instead of ASCII) when not using icons."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-buffer-name t
  "Whether display the buffer name."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-highlight-modified-buffer-name t
  "Whether highlight the modified buffer name."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-column-zero-based t
  "When non-nil, mode line displays column numbers zero-based.
See `column-number-indicator-zero-based'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-percent-position '(-3 "%p")
  "Specification of \"percentage offset\" of window through buffer.
See `mode-line-percent-position'."
  :type '(radio
          (const :tag "nil:  No offset is displayed" nil)
          (const :tag "\"%o\": Proportion of \"travel\" of the window through the buffer"
            (-3 "%o"))
          (const :tag "\"%p\": Percentage offset of top of window"
            (-3 "%p"))
          (const :tag "\"%P\": Percentage offset of bottom of window"
            (-3 "%P"))
          (const :tag "\"%q\": Offsets of both top and bottom of window"
            (6 "%q")))
  :group 'doom-modeline)

(defcustom doom-modeline-position-line-format '("L%l")
  "Format used to display line numbers in the mode line.
See `mode-line-position-line-format'."
  :type '(list string)
  :group 'doom-modeline)

(defcustom doom-modeline-position-column-format '("C%c")
  "Format used to display column numbers in the mode line.
See `mode-line-position-column-format'."
  :type '(list string)
  :group 'doom-modeline)

(defcustom doom-modeline-position-column-line-format '("%l:%c")
  "Format used to display combined line/column numbers in the mode line.
See `mode-line-position-column-line-format'."
  :type '(list string)
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

(defcustom doom-modeline-total-line-number nil
  "Whether display the total line number."
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
  "Indentation retrieving variables matched to major modes.

Which is used when `doom-modeline-indent-info' is non-nil.
When multiple variables are specified for a mode, they will be tried resolved
in the given order."
  :type '(alist :key-type symbol :value-type sexp)
  :group 'doom-modeline)

(defcustom doom-modeline-vcs-icon t
  "Whether display the icon of vcs segment.

It respects option `doom-modeline-icon'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-vcs-max-length 15
  "The maximum displayed length of the branch name of version control."
  :type 'integer
  :group 'doom-modeline)

(defcustom doom-modeline-vcs-display-function #'doom-modeline-vcs-name
  "The function to display the branch name."
  :type 'function
  :group 'doom-modeline)

(defcustom doom-modeline-check-icon t
  "Whether display the icon of check segment.

It respects option `doom-modeline-icon'."
  :type 'boolean
  :group 'doom-modeline)

(define-obsolete-variable-alias
  'doom-modeline-checker-simple-format
  'doom-modeline-check-simple-format
  "4.2.0")

(defcustom doom-modeline-check-simple-format nil
  "If non-nil, only display one number for check information if applicable."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-number-limit 99
  "The maximum number displayed for notifications."
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

It requires `ghub' and `async' packages. Additionally, your GitHub personal
access token must have `notifications' permissions.

If you use `pass' to manage your secrets, you also need to add this hook:
  (add-hook \\='doom-modeline-before-github-fetch-notification-hook
	   #\\='auth-source-pass-enable)"
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

(defcustom doom-modeline-modal t
  "Whether display the modal state.

Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-modal-icon t
  "Whether display the modal state icon.

Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-modal-modern-icon t
  "Whether display the modern icons for modals."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-always-show-macro-register nil
  "When non-nil, always show the register name when recording an evil macro."
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

(defcustom doom-modeline-gnus-idle nil
  "Whether to wait an idle time to scan for news.

When t, sets `doom-modeline-gnus-timer' as an idle timer.  If a
number, Emacs must have been idle this given time, checked after
reach the defined timer, to fetch news.  The time step can be
configured in `gnus-demon-timestep'."
  :type '(choice
	  (boolean :tag "Set `doom-modeline-gnus-timer' as an idle timer")
	  (number :tag "Set a custom idle timer"))
  :group 'doom-modeline)

(defcustom doom-modeline-gnus-excluded-groups nil
  "A list of groups to be excluded from the unread count.
Groups' names list in `gnus-newsrc-alist'`"
  :type '(repeat string)
  :group 'doom-modeline)

(defcustom doom-modeline-irc t
  "Whether display the irc notifications.

It requires either `circe' , `erc' or `rcirc' package."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-irc-buffers nil
  "Whether display the unread irc buffers."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-irc-stylize #'doom-modeline-shorten-irc
  "Which function to call to stylize IRC buffer names.

Buffer names are stylized using the selected `function'.
By default buffer names are shortened, you may want to disable or call
your own function.
The function must accept `buffer-name' and return `shortened-name'."
  :type '(radio (function-item :tag "Shorten"
                               :format "%t: %v\n %h"
                               doom-modeline-shorten-irc)
                (function-item
                 :tag "Leave unchanged"
                 :format "%t: %v\n"
                 identity)
                (function
                 :tag "Other function"))
  :group 'doom-modeline)

(defcustom doom-modeline-battery t
  "Whether display the battery status.

It respects `display-battery-mode'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-time t
  "Whether display the time.

It respects `display-time-mode'."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-display-misc-in-all-mode-lines t
  "Whether display the misc segment on all mode lines.

If nil, display only if the mode line is active."
  :type 'boolean
  :group 'doom-modeline)

(defcustom doom-modeline-always-visible-segments nil
  "A list of segments that should be visible even in inactive windows."
  :type '(repeat symbol)
  :group 'doom-modeline)

(defcustom doom-modeline-buffer-file-name-function #'identity
  "The function to handle variable `buffer-file-name'."
  :type 'function
  :group 'doom-modeline)

(defcustom doom-modeline-buffer-file-truename-function #'identity
  "The function to handle `buffer-file-truename'."
  :type 'function
  :group 'doom-modeline)

(defcustom doom-modeline-k8s-show-namespace t
  "Whether to show the current Kubernetes context's default namespace."
  :type 'boolean
  :group 'doom-modeline)


;;
;; Faces
;;

(defgroup doom-modeline-faces nil
  "The faces of `doom-modeline'."
  :group 'doom-modeline
  :group 'faces
  :link '(url-link :tag "Homepage" "https://github.com/seagle0128/doom-modeline"))

(defface doom-modeline
  '((t ()))
  "Default face."
  :group 'doom-modeline-faces)

(defface doom-modeline-emphasis
  '((t (:inherit (doom-modeline mode-line-emphasis))))
  "Face used for emphasis."
  :group 'doom-modeline-faces)

(defface doom-modeline-highlight
  '((t (:inherit (doom-modeline mode-line-highlight))))
  "Face used for highlighting."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-path
  '((t (:inherit (doom-modeline-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-file
  '((t (:inherit (doom-modeline mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-modified
  '((t (:inherit (doom-modeline warning bold) :background unspecified)))
  "Face used for the \\='unsaved\\=' symbol in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-major-mode
  '((t (:inherit (doom-modeline-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-minor-mode
  '((t (:inherit (doom-modeline font-lock-doc-face) :weight normal :slant normal)))
  "Face used for the minor-modes segment in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-project-parent-dir
  '((t (:inherit (doom-modeline font-lock-comment-face bold))))
  "Face used for the project parent directory of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-project-dir
  '((t (:inherit (doom-modeline font-lock-string-face bold))))
  "Face used for the project directory of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-project-root-dir
  '((t (:inherit (doom-modeline-emphasis bold))))
  "Face used for the project part of the mode-line buffer path."
  :group 'doom-modeline-faces)

(defface doom-modeline-panel
  '((t (:inherit doom-modeline-highlight)))
  "Face for \\='X out of Y\\=' segments.
This applies to `anzu', `evil-substitute', `iedit' etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-host
  '((t (:inherit (doom-modeline italic))))
  "Face for remote hosts in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-input-method
  '((t (:inherit (doom-modeline-emphasis))))
  "Face for input method in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-input-method-alt
  '((t (:inherit (doom-modeline font-lock-doc-face) :slant normal)))
  "Alternative face for input method in the mode-line."
  :group 'doom-modeline-faces)

(defface doom-modeline-debug
  '((t (:inherit (doom-modeline font-lock-doc-face) :slant normal)))
  "Face for debug-level messages in the mode-line. Used by vcs, check, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-info
  '((t (:inherit (doom-modeline success))))
  "Face for info-level messages in the mode-line. Used by vcs, check, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-warning
  '((t (:inherit (doom-modeline warning))))
  "Face for warnings in the mode-line. Used by vcs, check, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-urgent
  '((t (:inherit (doom-modeline error))))
  "Face for errors in the mode-line. Used by vcs, check, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-notification
  '((t (:inherit doom-modeline-warning)))
  "Face for notifications in the mode-line. Used by GitHub, mu4e, etc.
Also see the face `doom-modeline-unread-number'."
  :group 'doom-modeline-faces)

(defface doom-modeline-unread-number
  '((t (:inherit doom-modeline :slant italic)))
  "Face for unread number in the mode-line. Used by GitHub, mu4e, etc."
  :group 'doom-modeline-faces)

(defface doom-modeline-bar
  '((t (:inherit doom-modeline-highlight)))
  "The face used for the left-most bar in the mode-line of an active window."
  :group 'doom-modeline-faces)

(defface doom-modeline-bar-inactive
  `((t (:inherit doom-modeline)))
  "The face used for the left-most bar in the mode-line of an inactive window."
  :group 'doom-modeline-faces)

(defface doom-modeline-debug-visual
  '((((background light)) :foreground "#D4843E" :inherit doom-modeline)
    (((background dark)) :foreground "#915B2D" :inherit doom-modeline))
  "Face to use for the mode-line while debugging."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-emacs-state
  '((t (:inherit (doom-modeline font-lock-builtin-face))))
  "Face for the Emacs state tag in evil indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-insert-state
  '((t (:inherit (doom-modeline font-lock-keyword-face))))
  "Face for the insert state tag in evil indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-motion-state
  '((t (:inherit (doom-modeline font-lock-doc-face) :slant normal)))
  "Face for the motion state tag in evil indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-normal-state
  '((t (:inherit doom-modeline-info)))
  "Face for the normal state tag in evil indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-operator-state
  '((t (:inherit (doom-modeline mode-line))))
  "Face for the operator state tag in evil indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-visual-state
  '((t (:inherit doom-modeline-warning)))
  "Face for the visual state tag in evil indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-replace-state
  '((t (:inherit doom-modeline-urgent)))
  "Face for the replace state tag in evil indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-evil-user-state
  '((t (:inherit doom-modeline-warning)))
  "Face for the replace state tag in evil indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-overwrite
  '((t (:inherit doom-modeline-urgent)))
  "Face for overwrite indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-god
  '((t (:inherit doom-modeline-info)))
  "Face for god-mode indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-ryo
  '((t (:inherit doom-modeline-info)))
  "Face for RYO indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-fly-insert-state
  '((t (:inherit (doom-modeline font-lock-keyword-face))))
  "Face for the insert state in xah-fly-keys indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-fly-normal-state
  '((t (:inherit doom-modeline-info)))
  "Face for the normal state in xah-fly-keys indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-boon-command-state
  '((t (:inherit doom-modeline-info)))
  "Face for the command state tag in boon indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-boon-insert-state
  '((t (:inherit (doom-modeline font-lock-keyword-face))))
  "Face for the insert state tag in boon indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-boon-special-state
  '((t (:inherit (doom-modeline font-lock-builtin-face))))
  "Face for the special state tag in boon indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-boon-off-state
  '((t (:inherit (doom-modeline mode-line))))
  "Face for the off state tag in boon indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-meow-normal-state
  '((t (:inherit doom-modeline-evil-normal-state)))
  "Face for the normal state in meow-edit indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-meow-insert-state
  '((t (:inherit doom-modeline-evil-insert-state)))
  "Face for the insert state in meow-edit indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-meow-beacon-state
  '((t (:inherit doom-modeline-evil-visual-state)))
  "Face for the beacon state in meow-edit indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-meow-motion-state
  '((t (:inherit doom-modeline-evil-motion-state)))
  "Face for the motion state in meow-edit indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-meow-keypad-state
  '((t (:inherit doom-modeline-evil-operator-state)))
  "Face for the keypad state in meow-edit indicator."
  :group 'doom-modeline-faces)

(defface doom-modeline-persp-name
  '((t (:inherit (doom-modeline font-lock-comment-face italic))))
  "Face for the persp name."
  :group 'doom-modeline-faces)

(defface doom-modeline-persp-buffer-not-in-persp
  '((t (:inherit (doom-modeline font-lock-doc-face italic))))
  "Face for the buffers which are not in the persp."
  :group 'doom-modeline-faces)

(defface doom-modeline-repl-success
  '((t (:inherit doom-modeline-info)))
  "Face for REPL success state."
  :group 'doom-modeline-faces)

(defface doom-modeline-repl-warning
  '((t (:inherit doom-modeline-warning)))
  "Face for REPL warning state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-success
  '((t (:inherit doom-modeline-info)))
  "Face for LSP success state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-warning
  '((t (:inherit doom-modeline-warning)))
  "Face for LSP warning state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-error
  '((t (:inherit doom-modeline-urgent)))
  "Face for LSP error state."
  :group 'doom-modeline-faces)

(defface doom-modeline-lsp-running
  '((t (:inherit (doom-modeline compilation-mode-line-run) :weight normal :slant normal)))
  "Face for LSP running state."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-charging
  '((t (:inherit doom-modeline-info)))
  "Face for battery charging status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-full
  '((t (:inherit doom-modeline-info)))
  "Face for battery full status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-normal
  '((t (:inherit (doom-modeline mode-line))))
  "Face for battery normal status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-warning
  '((t (:inherit doom-modeline-warning)))
  "Face for battery warning status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-critical
  '((t (:inherit doom-modeline-urgent)))
  "Face for battery critical status."
  :group 'doom-modeline-faces)

(defface doom-modeline-battery-error
  '((t (:inherit doom-modeline-urgent)))
  "Face for battery error status."
  :group 'doom-modeline-faces)

(defface doom-modeline-buffer-timemachine
  '((t (:inherit doom-modeline-buffer-file :slant italic)))
  "Face for timemachine status."
  :group 'doom-modeline-faces)

(defface doom-modeline-time
  '((t (:inherit doom-modeline)))
  "Face for display time."
  :group 'doom-modeline-faces)

(defface doom-modeline-compilation
  '((t (:inherit doom-modeline-warning :slant italic :height 0.9)))
  "Face for compilation progress."
  :group 'doom-modeline-faces)

;;
;; Externals
;;

(defvar mode-line-right-align-edge)

(declare-function doom-modeline-shorten-irc "doom-modeline-segments")
(declare-function face-remap-remove-relative "face-remap")
(declare-function ffip-project-root "ext:find-file-in-project")
(declare-function project-root "project")
(declare-function projectile-project-root "ext:projectile")


;;
;; Utilities
;;

(defun doom-modeline-add-font-lock ()
  "Fontify `doom-modeline-def-*' statements."
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(doom-modeline-def-.+\\)\\_> +\\(.*?\\)\\_>"
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face)))))
(doom-modeline-add-font-lock)

(defun doom-modeline-add-imenu ()
  "Add to `imenu' index."
  (add-to-list
   'imenu-generic-expression
   '("Modelines"
     "^\\s-*(\\(doom-modeline-def-modeline\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\s'\\|\\\\.\\)+\\)"
     2))
  (add-to-list
   'imenu-generic-expression
   '("Segments"
     "^\\s-*(\\(doom-modeline-def-segment\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
     2))
  (add-to-list
   'imenu-generic-expression
   '("Envs"
     "^\\s-*(\\(doom-modeline-def-env\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
     2)))


;;
;; Core helpers
;;

;; FIXME #183: Force to calculate mode-line height
;; @see https://github.com/seagle0128/doom-modeline/issues/183
;; @see https://github.com/seagle0128/doom-modeline/issues/483
(unless (>= emacs-major-version 29)
  (eval-and-compile
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
        (redisplay t))))
  (advice-add #'fit-window-to-buffer :before #'doom-modeline-redisplay))

;; For `flycheck-color-mode-line'
(with-eval-after-load 'flycheck-color-mode-line
  (defvar flycheck-color-mode-line-face-to-color)
  (setq flycheck-color-mode-line-face-to-color 'doom-modeline))

(defun doom-modeline-icon-displayable-p ()
  "Return non-nil if icons are displayable."
  (and doom-modeline-icon (featurep 'nerd-icons)))

(defun doom-modeline-mwheel-available-p ()
  "Whether mouse wheel is available."
  (and (featurep 'mwheel) (bound-and-true-p mouse-wheel-mode)))

;; Keep `doom-modeline-current-window' up-to-date
(defun doom-modeline--selected-window ()
  "Get the selected window."
  (frame-selected-window))

(defvar doom-modeline-current-window (doom-modeline--selected-window)
  "Current window.")

(defun doom-modeline--active ()
  "Whether is an active window."
  (unless (and (bound-and-true-p mini-frame-frame)
               (and (frame-live-p mini-frame-frame)
                    (frame-visible-p mini-frame-frame)))
    (and doom-modeline-current-window
         (eq (doom-modeline--selected-window) doom-modeline-current-window))))

(defvar-local doom-modeline--limited-width-p nil)

(defun doom-modeline--segment-visible (name)
  "Whether the segment NAME should be displayed."
  (and
   (or (doom-modeline--active)
       (member name doom-modeline-always-visible-segments))
   (not doom-modeline--limited-width-p)))

(defun doom-modeline-set-selected-window (&rest _)
  "Set `doom-modeline-current-window' appropriately."
  (let ((win (doom-modeline--selected-window)))
    (setq doom-modeline-current-window
          (if (minibuffer-window-active-p win)
              (minibuffer-selected-window)
            win))))

(defun doom-modeline-unset-selected-window ()
  "Unset `doom-modeline-current-window' appropriately."
  (setq doom-modeline-current-window nil))

(add-hook 'pre-redisplay-functions #'doom-modeline-set-selected-window)

;; Ensure modeline is inactive when Emacs is unfocused
(defvar doom-modeline--remap-faces '(mode-line
                                     mode-line-active
                                     mode-line-emphasis
                                     mode-line-highlight
                                     mode-line-buffer-id
                                     doom-modeline
                                     solaire-mode-line-face
                                     solaire-mode-line-active-face
                                     paradox-mode-line-face
                                     flycheck-color-mode-line-error-face
                                     flycheck-color-mode-line-warning-face
                                     flycheck-color-mode-line-info-face
                                     flycheck-color-mode-line-success-face))

(defvar doom-modeline--remap-face-cookie-alist nil)
(defun doom-modeline-focus ()
  "Focus mode-line."
  (mapc #'face-remap-remove-relative doom-modeline--remap-face-cookie-alist))

(defun doom-modeline-unfocus ()
  "Unfocus mode-line."
  (dolist (face doom-modeline--remap-faces)
    (add-to-list 'doom-modeline--remap-face-cookie-alist
                 (face-remap-add-relative face 'mode-line-inactive))))

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defun doom-modeline-focus-change (&rest _)
          (if (frame-focus-state (frame-parent))
              (progn
                (doom-modeline-focus)
                ;; HACK: pulse after focusing in the frame to refresh the buffer name.
                ;; @see https://github.com/seagle0128/doom-modeline/issues/591
                (when (fboundp 'pulse-momentary-highlight-region)
                  (pulse-momentary-highlight-region 0 0)))
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
  "Define a modeline segment NAME with BODY and byte compiles it."
  (declare (indent defun) (doc-string 2))
  (let ((sym (intern (format "doom-modeline-segment--%s" name)))
        (docstring (if (stringp (car body))
                       (pop body)
                     (format "%s modeline segment" name))))
    (cond ((and (symbolp (car body))
                (not (cdr body)))
           `(add-to-list 'doom-modeline-var-alist (cons ',name ',(car body))))
          (t
           `(progn
              (defun ,sym () ,docstring ,@body)
              (add-to-list 'doom-modeline-fn-alist (cons ',name ',sym))
              ,(unless (bound-and-true-p byte-compile-current-file)
                 `(let (byte-compile-warnings)
                    (unless (and (fboundp 'subr-native-elisp-p)
                                 (subr-native-elisp-p (symbol-function #',sym)))
                      (byte-compile #',sym)))))))))

(defun doom-modeline--prepare-segments (segments)
  "Prepare mode-line `SEGMENTS'."
  (let (forms it)
    (dolist (seg segments)
      (cond ((stringp seg)
             (push seg forms))
            ((symbolp seg)
             (cond ((setq it (alist-get seg doom-modeline-fn-alist))
                    (push (list :eval (list it)) forms))
                   ((setq it (alist-get seg doom-modeline-var-alist))
                    (push it forms))
                   ((error "%s is not a defined segment" seg))))
            ((error "%s is not a valid segment" seg))))
    (nreverse forms)))

(defun doom-modeline-def-modeline (name lhs &optional rhs)
  "Define a modeline format and byte-compiles it.
NAME is a symbol to identify it (used by `doom-modeline' for retrieval).
LHS and RHS are lists of symbols of modeline segments defined with
`doom-modeline-def-segment'.

Example:
  (doom-modeline-def-modeline \\='minimal
    \\='(bar matches \" \" buffer-info)
    \\='(media-info major-mode))
  (doom-modeline-set-modeline \\='minimal t)"
  (let ((sym (intern (format "doom-modeline-format--%s" name)))
        (lhs-forms (doom-modeline--prepare-segments lhs))
        (rhs-forms (doom-modeline--prepare-segments rhs)))
    (defalias sym
      (lambda ()
        (list lhs-forms
              (let* ((rhs-str (format-mode-line (cons "" rhs-forms)))
                     (rhs-width (progn
                                  (add-face-text-property
                                   0 (length rhs-str) 'mode-line t rhs-str)
                                  (doom-modeline-string-pixel-width rhs-str))))
                (propertize
                 " "
                 'face (doom-modeline-face)
                 'display
                 ;; Backport from `mode-line-right-align-edge' in 30
                 (if (and (display-graphic-p)
                           (not (eq mode-line-right-align-edge 'window)))
		              `(space :align-to (- ,mode-line-right-align-edge
                                           (,rhs-width)))
		            `(space :align-to (,(- (window-pixel-width)
                                           (window-scroll-bar-width)
                                           (window-right-divider-width)
                                           (* (or (cdr (window-margins)) 1)
                                              (frame-char-width))
                                           (pcase mode-line-right-align-edge
                                             ('right-margin
                                              (or (cdr (window-margins)) 0))
                                             ('right-fringe
                                              (or (cadr (window-fringes)) 0))
                                             (_ 0))
                                           rhs-width))))))
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
  (when-let* ((modeline (doom-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            mode-line-format)
          (list "%e" modeline))))

;;
;; Helpers
;;

(defconst doom-modeline-ellipsis
  (if (char-displayable-p ?…) "…" "...")
  "Ellipsis.")

(defsubst doom-modeline-spc ()
  "Whitespace."
  (propertize " " 'face (doom-modeline-face)))

(defsubst doom-modeline-wspc ()
  "Wide Whitespace."
  (propertize "  " 'face (doom-modeline-face)))

(defsubst doom-modeline-vspc ()
  "Thin whitespace."
  (propertize " "
              'face (doom-modeline-face)
              'display '((space :relative-width 0.5))))

(defun doom-modeline-face (&optional face inactive-face)
  "Display FACE in active window, and INACTIVE-FACE in inactive window.
IF FACE is nil, `mode-line' face will be used.
If INACTIVE-FACE is nil, `mode-line-inactive' face will be used."
  (if (doom-modeline--active)
      (or (and (facep face) `(:inherit (doom-modeline ,face)))
          (and (facep 'mode-line-active) '(:inherit (doom-modeline mode-line-active)))
          '(:inherit (doom-modeline mode-line)))
    (or (and (facep inactive-face) `(:inherit (doom-modeline ,inactive-face)))
        '(:inherit (doom-modeline mode-line-inactive)))))

(defun doom-modeline-string-pixel-width (str)
  "Return the width of STR in pixels."
  (if (fboundp 'string-pixel-width)
      (string-pixel-width str)
    (* (string-width str) (window-font-width nil 'mode-line)
       (if (display-graphic-p) 1.05 1.0))))

(defun doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (let ((height (face-attribute 'mode-line :height))
        (char-height (window-font-height nil 'mode-line)))
    (round
     (* 1.0 (cond ((integerp height) (/ height 10))
                  ((floatp height) (* height char-height))
                  (t char-height))))))

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
  (when icon
    (if (doom-modeline-icon-displayable-p)
        (when-let* ((props (get-text-property 0 'face icon)))
          (when (listp props)
            (cl-destructuring-bind (&key family height inherit &allow-other-keys) props
              (propertize icon 'face `(:inherit (doom-modeline ,(or face inherit props))
                                                :family  ,(or family "")
                                                :height  ,(or height 1.0))))))
      (propertize icon 'face `(:inherit (doom-modeline ,face))))))

(defun doom-modeline-icon (icon-set icon-name unicode text &rest args)
  "Display icon of ICON-NAME with ARGS in mode-line.

ICON-SET includes `ipsicon', `octicon', `pomicon', `powerline', `faicon',
`wicon', `sucicon', `devicon', `codicon', `flicon' and `mdicon', etc.
UNICODE is the unicode char fallback. TEXT is the ASCII char fallback.
ARGS is same as `nerd-icons-octicon' and others."
  (let ((face `(:inherit (doom-modeline
                          ,(or (plist-get args :face) 'mode-line)))))
    (cond
     ;; Icon
     ((and (doom-modeline-icon-displayable-p)
           icon-name
           (not (string-empty-p icon-name)))
      (if-let* ((func (nerd-icons--function-name icon-set))
                (icon (and (fboundp func)
                           (apply func icon-name args))))
          (doom-modeline-propertize-icon icon face)
        ""))
     ;; Unicode fallback
     ((and doom-modeline-unicode-fallback
           unicode
           (not (string-empty-p unicode))
           (char-displayable-p (string-to-char unicode)))
      (propertize unicode 'face face))
     ;; ASCII text
     (text
      (propertize text 'face face))
     ;; Fallback
     (t ""))))

(defun doom-modeline-icon-for-buffer ()
  "Get the formatted icon for the current buffer."
  (nerd-icons-icon-for-buffer))

(defun doom-modeline-display-icon (icon)
  "Display ICON in mode-line."
  (if (doom-modeline--active)
      icon
    (doom-modeline-propertize-icon icon 'mode-line-inactive)))

(defun doom-modeline-display-text (text)
  "Display TEXT in mode-line."
  (if (doom-modeline--active)
      text
    (propertize text 'face `(:inherit (mode-line-inactive
                                       ,(get-text-property 0 'face text))))))

(defun doom-modeline-vcs-name ()
  "Display the vcs name."
  (and vc-mode (cadr (split-string (string-trim vc-mode) "^[A-Z]+[-:]+"))))

(defun doom-modeline--create-bar-image (face width height)
  "Create the bar image.

Use FACE for the bar, WIDTH and HEIGHT are the image size in pixels."
  (when (and (image-type-available-p 'pbm)
             (numberp width) (> width 0)
             (numberp height) (> height 0))
    (propertize
     " " 'display
     (let ((color (or (face-background face nil t) "None")))
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" width height)
                  (make-string (* width height) ?1)
                  "\n")
          'pbm t :scale 1 :foreground color :ascent 'center))))))

(defun doom-modeline--create-hud-image
    (face1 face2 width height top-margin bottom-margin)
  "Create the hud image.

Use FACE1 for the bar, FACE2 for the background.
WIDTH and HEIGHT are the image size in pixels.
TOP-MARGIN and BOTTOM-MARGIN are the size of the margin above and below the bar,
respectively."
  (when (and (display-graphic-p)
             (image-type-available-p 'pbm)
             (numberp width) (> width 0)
             (numberp height) (> height 0))
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
(defun doom-modeline-window-size-change-function (&rest _)
  "Function for `window-size-change-functions'."
  (setq doom-modeline--limited-width-p
        (cond
         ((integerp doom-modeline-window-width-limit)
          (<= (window-total-width) doom-modeline-window-width-limit))
         ((floatp doom-modeline-window-width-limit)
          (<= (/ (window-total-width) (frame-width) 1.0)
              doom-modeline-window-width-limit)))))

(add-hook 'after-revert-hook #'doom-modeline-window-size-change-function)
(add-hook 'buffer-list-update-hook #'doom-modeline-window-size-change-function)
(add-hook 'window-size-change-functions #'doom-modeline-window-size-change-function)

(defvar-local doom-modeline--project-root nil)
(defun doom-modeline--project-root ()
  "Get the path to the project root.
Return nil if no project was found."
  (or doom-modeline--project-root
      (setq doom-modeline--project-root
            (cond
             ((and (memq doom-modeline-project-detection '(auto ffip))
                   (fboundp 'ffip-project-root))
              (let ((inhibit-message t))
                (ffip-project-root)))
             ((and (memq doom-modeline-project-detection '(auto projectile))
                   (bound-and-true-p projectile-mode))
              (projectile-project-root))
             ((and (memq doom-modeline-project-detection '(auto project))
                   (fboundp 'project-current))
              (when-let* ((project (project-current)))
                (expand-file-name
                 (if (fboundp 'project-root)
                     (project-root project)
                   (car (with-no-warnings
                          (project-roots project)))))))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-project-detection
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-project-detection val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (setq doom-modeline--project-root nil)
         (and buffer-file-name (revert-buffer t t)))))))

(defun doom-modeline-project-p ()
  "Check if the file is in a project."
  (doom-modeline--project-root))

(defun doom-modeline-project-root ()
  "Get the path to the root of your project.
Return `default-directory' if no project was found."
  (abbreviate-file-name
   (or (doom-modeline--project-root) default-directory)))

(defun doom-modeline--format-buffer-file-name ()
  "Get and format the buffer file name."
  (let ((buffer-file-name (file-local-name
                           (or (buffer-file-name (buffer-base-buffer)) ""))))
    (or (and doom-modeline-buffer-file-name-function
             (funcall doom-modeline-buffer-file-name-function buffer-file-name))
        buffer-file-name)))

(defun doom-modeline--format-buffer-file-truename (b-f-n)
  "Get and format buffer file truename via B-F-N."
  (let ((buffer-file-truename (file-local-name
                               (or (file-truename b-f-n) ""))))
    (or (and doom-modeline-buffer-file-truename-function
             (funcall doom-modeline-buffer-file-truename-function buffer-file-truename))
        buffer-file-truename)))

(defun doom-modeline-buffer-file-name ()
  "Propertize file name based on `doom-modeline-buffer-file-name-style'."
  (let* ((buffer-file-name (doom-modeline--format-buffer-file-name))
         (buffer-file-truename (doom-modeline--format-buffer-file-truename buffer-file-name))
         (file-name
          (pcase doom-modeline-buffer-file-name-style
            ('auto
             (if (doom-modeline-project-p)
                 (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shrink 'hide)
               (propertize "%b" 'face 'doom-modeline-buffer-file)))
            ('truncate-upto-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink))
            ('truncate-from-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil 'shrink))
            ('truncate-with-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shrink 'hide))
            ('truncate-except-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shrink))
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
            ('file-name-with-project
             (format "%s|%s"
                     (propertize (file-name-nondirectory
                                  (directory-file-name (file-local-name (doom-modeline-project-root))))
                                 'face 'doom-modeline-project-dir)
                     (propertize (file-name-nondirectory buffer-file-name)
                                 'face 'doom-modeline-buffer-file)))
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
  "Propertize file name that truncates every dir along path.

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
  "Propertize file name showing directories relative to project's root only.

If INCLUDE-PROJECT is non-nil, the project path will be included."
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
                                        true-file-path
                                        &optional
                                        truncate-project-root-parent
                                        truncate-project-relative-path
                                        hide-project-root-parent)
  "Propertize buffer name given by FILE-PATH or TRUE-FILE-PATH.

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
       (when-let* ((root-path-parent
                    (file-name-directory (directory-file-name project-root))))
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
      (when-let* ((relative-path (file-relative-name
                                  (or (file-name-directory
                                       (if doom-modeline-buffer-file-true-name
                                           true-file-path file-path))
                                      "./")
                                  project-root)))
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
