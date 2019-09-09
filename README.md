# doom-modeline

[![Build Status](https://travis-ci.org/seagle0128/doom-modeline.svg?branch=master)](https://travis-ci.org/seagle0128/doom-modeline)
[![MELPA](https://melpa.org/packages/doom-modeline-badge.svg)](https://melpa.org/#/doom-modeline)
[![MELPA Stable](https://stable.melpa.org/packages/doom-modeline-badge.svg)](https://stable.melpa.org/#/doom-modeline)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [doom-modeline](#doom-modeline)
    - [Feature](#feature)
    - [Screenshots](#screenshots)
    - [Install](#install)
        - [Manual](#manual)
        - [Use-package](#use-package)
    - [Customize](#customize)
    - [FAQ](#faq)

<!-- markdown-toc end -->

A fancy and fast mode-line inspired by minimalism design.

It's integrated into [Centaur Emacs](https://github.com/seagle0128/.emacs.d), [Doom
Emacs](https://github.com/hlissner/doom-emacs) and
[Spacemacs](https://github.com/syl20bnr/spacemacs).

## Feature

The `doom-modeline` was designed for minimalism, and offers:

- A match count panel (for `anzu`, `iedit`, `multiple-cursors`, `symbol-overlay`,
  `evil-search` and `evil-substitute`)
- An indicator for recording a macro
- Current environment version (e.g. `python`, `ruby`, `go`, etc.) in the major-mode
- A customizable mode-line height (see `doom-modeline-height`)
- A minor modes segment which is compatible with `minions`
- An error/warning count segment for `flymake`/`flycheck`
- A workspace number segment for `eyebrowse`
- A perspective name segment for `persp-mode`
- A window number segment for `ace-window`, `winum` and `window-numbering`
- An indicator for modal editing state, including `evil`, `overwrite`, `god`, `ryo` and
  `xah-fly-keys`, etc.
- An indicator for remote host
- An indicator for debug state
- An indicator for current input method
- An indicator for LSP state with `lsp-mode` or `eglot`
- An indicator for GitHub notifications
- An indicator for unread emails with `mu4e-alert`
- An indicator for irc notifications with `circe`
- An indicator for buffer position which is compatible with `nyan-mode`
- An indicator for party parrot
- An indicator for PDF page number with `pdf-tools`
- An indicator for markdown/org preview with `grip`
- An indicator for battery status with `fancy-battery`
- Truncated file name, file icon, buffer state and project name in buffer information segment, which
  is compatible with `project`, `find-file-in-project` and `projectile`.
- New mode-line for `Info-mode` buffers
- New package mode-line for `paradox`
- New mode-line for `helm` buffers
- New mode-line for `git-timemachine` buffers

## Screenshots

![modeline](https://user-images.githubusercontent.com/140797/49694177-10dcd280-fbc0-11e8-8d21-971ede6afdb5.png
 "Mode-line")

![search_replace](https://user-images.githubusercontent.com/140797/49694189-6913d480-fbc0-11e8-93ae-9578455dcd2c.png
"Search and Replace")

![macro](https://user-images.githubusercontent.com/140797/49694199-cc056b80-fbc0-11e8-9bb1-533b1e64da66.png
"Macro")

![no_icons](https://user-images.githubusercontent.com/140797/51301117-0805d900-1a69-11e9-957d-b4c7a70a1cf8.png
"No Icons")

![color_icon_cpp](https://user-images.githubusercontent.com/140797/50414928-3c63ec00-0853-11e9-9b26-6a1df278d332.png
"Color C++ icon")

![color_icon_java](https://user-images.githubusercontent.com/140797/50415081-1428bd00-0854-11e9-9ae5-91805290c524.png
"Color Java icon")

![color_icon_elisp](https://user-images.githubusercontent.com/140797/50415381-9f568280-0855-11e9-9478-34a6dd614d96.png
"Color Elisp icon")

![evil_normal_state](https://user-images.githubusercontent.com/140797/49694476-b8103880-fbc5-11e8-9c18-91f5e9258333.png
"Evil Normal State")

![evil_insert_state](https://user-images.githubusercontent.com/140797/49694461-8b5c2100-fbc5-11e8-993e-d97baa9f01af.png
"Evil Insert State")

![lsp_version](https://user-images.githubusercontent.com/140797/53592864-c751c180-3bc9-11e9-9914-493007c013d5.png
"Perspective, LSP, Version, VCS and Flycheck")

![perspective](https://user-images.githubusercontent.com/140797/49694481-e0983280-fbc5-11e8-8cb2-c8d2e782bcdb.png
 "Perspective, LSP, Version and VCS")

![notifications](https://user-images.githubusercontent.com/140797/53592683-64602a80-3bc9-11e9-8054-91f79aa930b9.png
"Notifications")

![minions](https://user-images.githubusercontent.com/140797/50301291-de857c00-04c1-11e9-84c5-bfbc8de8295f.png
"Minions and minor modes")

![debug](https://user-images.githubusercontent.com/140797/50302008-c57dca80-04c3-11e9-8578-50154ef4f9f0.png
"Debug state")

![nyan_parrot](https://user-images.githubusercontent.com/140797/51301061-da209480-1a68-11e9-9f64-905d889df9d6.png
"Nyan and Parrot")

![battery](https://user-images.githubusercontent.com/140797/53593622-ba35d200-3bcb-11e9-85b3-38d64d05c127.png
"Fancy Battery")

![package](https://user-images.githubusercontent.com/140797/57503916-e769d380-7324-11e9-906d-44c79f7408a3.png
"Package")

![info](https://user-images.githubusercontent.com/140797/57506248-d96c8080-732d-11e9-8167-644c8fc4e0db.png
"Info")

![helm](https://user-images.githubusercontent.com/140797/57506112-6531dd00-732d-11e9-8a5e-22166f42dd4c.png
"Helm")

## Install

### Manual

From melpa, `M-x package-install RET doom-modeline RET`.

In `init.el`,

``` emacs-lisp
(require 'doom-modeline)
(doom-modeline-mode 1)
```

### Use-package

``` emacs-lisp
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))
```

This package requires the fonts included with `all-the-icons` to be installed.
Run `M-x all-the-icons-install-fonts` to do so. Please refer to the
[installation guide](https://github.com/domtronn/all-the-icons.el#installation).

Strongly recommend to use
[doom-themes](https://github.com/hlissner/emacs-doom-themes) at the same time.

## Customize

Run `M-x customize-group RET doom-modeline RET` or set the variables.

``` emacs-lisp
;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 3)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether display icons in mode-line or not.
(setq doom-modeline-icon (display-graphic-p))

;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display color icons for `major-mode'. It respects
;; `doom-modeline-icon' and `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display icons for buffer states. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display buffer modification icon. It respects `doom-modeline-icon'
;; and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display minor modes in mode-line or not.
(setq doom-modeline-minor-modes (featurep 'minions))

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Whether display buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; Whether display icon for persp name. Nil to display a # sign. It respects `doom-modeline-icon'
(setq doom-modeline-persp-name-icon nil)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display GitHub notifications or not. Requires `ghub` package.
(setq doom-modeline-github nil)

;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display mu4e notifications or not. Requires `mu4e-alert' package.
(setq doom-modeline-mu4e t)

;; Whether display irc notifications or not. Requires `circe' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

;; Whether display environment version or not
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-perl t)
(setq doom-modeline-env-enable-go t)
(setq doom-modeline-env-enable-elixir t)
(setq doom-modeline-env-enable-rust t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-ruby-executable "ruby")
(setq doom-modeline-env-perl-executable "perl")
(setq doom-modeline-env-go-executable "go")
(setq doom-modeline-env-elixir-executable "iex")
(setq doom-modeline-env-rust-executable "rustc")

;; What to dispaly as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)
```

## FAQ

1. I am experiencing the laggy issue, how to resolve it?

   Add this configuration into your init file:

   ``` emacs-lisp
   ;; Don’t compact font caches during GC.
   (setq inhibit-compacting-font-caches t)
   ```

1. A ridiculous path is displayed on mode-line while visiting a symbolink.

    It's the default behaviors of Vanilla Emacs. If you want to display the real
    names, please put this into your init file.

    ``` emacs-lisp
    (setq find-file-visit-truename t)
    ```

    If the file is controlled by vc, refer to the documentation of
    `vc-follow-symlinks`.

1. Can I add my mode-line segments myself? How to do that?

   Of course. Just add the segments into `global-mode-string`.

1. Why doesn't change of branch reflect in modeline?

   Actually it's related to `magit` and `vc-mode`.
   - Workaround:
     - Revert the buffers manually.
     - `(setq auto-revert-check-vc-info t)` brings the performance issue.
   - Refer to:
     - [The mode-line information isn’t always
       up-to-date](https://magit.vc/manual/magit/The-mode_002dline-information-isn_0027t-always-up_002dto_002ddate.html)
     - [Maybe provide an alternative to VC's mode-line
       information](https://github.com/magit/magit/issues/2687)

1. How can I define my own mode-line?

   Use `doom-modeline-def-modeline` to define your own mode-line and set it as
   default.

   For example:
   - Define your custom doom-modeline

   ```emacs-lisp
   (doom-modeline-def-modeline 'my-simple-line
     '(bar matches buffer-info remote-host buffer-position parrot selection-info)
     '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))
   ```

   - Set it to default using the `doom-modeline-mode-hook`:

   ```emacs-lisp
   (defun setup-custom-doom-modeline ()
     (doom-modeline-set-modeline 'my-simple-line 'default))

   (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
   ```

1. How to specify font size in modeline?

    For example:

    ``` emacs-lisp
    (setq doom-modeline-height 1)
    (set-face-attribute 'mode-line nil :height 100)
    (set-face-attribute 'mode-line-inactive nil :height 100)
    ```

    or

    ```emacs-lisp
    (custom-set-faces
      '(mode-line ((t (:height 0.9))))
      '(mode-line-inactive ((t (:height 0.9)))))
    ```

    Please refer to [#189](https://github.com/seagle0128/doom-modeline/issues/189).
