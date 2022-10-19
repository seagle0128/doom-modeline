# doom-modeline

[![Build Status](https://github.com/seagle0128/doom-modeline/workflows/CI/badge.svg?branch=master)](https://github.com/seagle0128/doom-modeline/actions)
[![Release Tag](https://img.shields.io/github/tag/seagle0128/doom-modeline.svg?label=Release)](https://github.com/seagle0128/doom-modeline/release)
[![License](http://img.shields.io/:License-GPL3-blue.svg)](License)
[![MELPA](https://melpa.org/packages/doom-modeline-badge.svg)](https://melpa.org/#/doom-modeline)
[![MELPA Stable](https://stable.melpa.org/packages/doom-modeline-badge.svg)](https://stable.melpa.org/#/doom-modeline)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

## Table of Contents

- [Feature](#feature)
- [Screenshots](#screenshots)
- [Install](#install)
  - [Manual](#manual)
  - [Use-package](#use-package)
- [Customize](#customize)
- [FAQ](#faq)
- [Donate](#donate)

<!-- markdown-toc end -->

A fancy and fast mode-line inspired by minimalism design.

It's integrated into [Centaur Emacs](https://github.com/seagle0128/.emacs.d), [Doom
Emacs](https://github.com/hlissner/doom-emacs) and
[Spacemacs](https://github.com/syl20bnr/spacemacs).

## Feature

The `doom-modeline` was designed for minimalism, and offers:

- A match count panel (for `anzu`, `iedit`, `multiple-cursors`, `symbol-overlay`,
  and `evil-search`, etc.)
- An indicator for recording a macro
- Current environment version (e.g. `python`, `ruby`, `go`, etc.) in the major-mode
- A customizable mode-line height (see `doom-modeline-height`)
- A minor modes segment which is compatible with `minions`
- An error/warning count segment for `flymake`/`flycheck`
- A workspace number/name segment for `eyebrowse` or `tab-bar-mode`
- A perspective name segment for `persp-mode`
- A window number segment for `ace-window`, `winum` and `window-numbering`
- An indicator for modal editing state, including `evil`, `overwrite`, `god`, `ryo` and
  `xah-fly-keys`, etc.
- An indicator for `battery` status
- An indicator for current input method
- An indicator for debug state
- An indicator for remote host
- An indicator for LSP state with `lsp-mode` or `eglot`
- An indicator for GitHub notifications
- An indicator for unread emails with `mu4e-alert` and `gnus`
- An indicator for IRC notifications with `circe`, `rcirc` or `erc`
- An indicator for buffer position which is compatible with `nyan-mode` or `poke-line`
- An indicator for party `parrot`
- An indicator for PDF page number with `pdf-tools`
- An indicator for markdown/org preview with `grip`
- Truncated file name, file icon, buffer state and project name in buffer
  information segment, which is compatible with `project`, `projectile` and
  `find-file-in-project`.
- New mode-line for `Info-mode` buffers
- New package mode-line for `paradox`
- New mode-line for `helm` buffers
- New mode-line for `git-timemachine` buffers

## Screenshots

![modeline](https://user-images.githubusercontent.com/140797/49694177-10dcd280-fbc0-11e8-8d21-971ede6afdb5.png "Mode-line")

![search_replace](https://user-images.githubusercontent.com/140797/49694189-6913d480-fbc0-11e8-93ae-9578455dcd2c.png "Search and Replace")

![evil_search](https://user-images.githubusercontent.com/140797/162925065-0ffe40fd-1efd-469b-bf09-c62fe2d22fd2.png "Evil Search")

![macro](https://user-images.githubusercontent.com/140797/49694199-cc056b80-fbc0-11e8-9bb1-533b1e64da66.png "Macro")

![no_icons](https://user-images.githubusercontent.com/140797/51301117-0805d900-1a69-11e9-957d-b4c7a70a1cf8.png "No Icons")

![color_icon_cpp](https://user-images.githubusercontent.com/140797/50414928-3c63ec00-0853-11e9-9b26-6a1df278d332.png "Color C++ icon")

![color_icon_java](https://user-images.githubusercontent.com/140797/50415081-1428bd00-0854-11e9-9ae5-91805290c524.png "Color Java icon")

![color_icon_elisp](https://user-images.githubusercontent.com/140797/50415381-9f568280-0855-11e9-9478-34a6dd614d96.png "Color Elisp icon")

![evil_normal_state_icon](https://user-images.githubusercontent.com/140797/68990534-332aa600-088f-11ea-920f-20c9527a6466.png "Evil Normal State icon")

![evil_insert_state_icon](https://user-images.githubusercontent.com/140797/68990540-4dfd1a80-088f-11ea-8e53-ab77af4c58c2.png "Evil Insert State icon")

![evil_normal_state](https://user-images.githubusercontent.com/140797/49694476-b8103880-fbc5-11e8-9c18-91f5e9258333.png "Evil Normal State")

![evil_insert_state](https://user-images.githubusercontent.com/140797/49694461-8b5c2100-fbc5-11e8-993e-d97baa9f01af.png "Evil Insert State")

![lsp_version](https://user-images.githubusercontent.com/140797/53592864-c751c180-3bc9-11e9-9914-493007c013d5.png "Perspective, LSP, Version, VCS and Flycheck")

![perspective](https://user-images.githubusercontent.com/140797/49694481-e0983280-fbc5-11e8-8cb2-c8d2e782bcdb.png "Perspective, LSP, Version and VCS")

![notifications](https://user-images.githubusercontent.com/140797/53592683-64602a80-3bc9-11e9-8054-91f79aa930b9.png "Notifications")

![minions](https://user-images.githubusercontent.com/140797/50301291-de857c00-04c1-11e9-84c5-bfbc8de8295f.png "Minions and minor modes")

![debug](https://user-images.githubusercontent.com/140797/50302008-c57dca80-04c3-11e9-8578-50154ef4f9f0.png "Debug state")

![nyan_parrot](https://user-images.githubusercontent.com/140797/51301061-da209480-1a68-11e9-9f64-905d889df9d6.png "Nyan and Parrot")

![irc](https://user-images.githubusercontent.com/140797/69004814-20c67000-0954-11ea-8489-f5a527a80574.png "IRC Notifications")

![battery](https://user-images.githubusercontent.com/140797/53593622-ba35d200-3bcb-11e9-85b3-38d64d05c127.png "Battery")

![package](https://user-images.githubusercontent.com/140797/57503916-e769d380-7324-11e9-906d-44c79f7408a3.png "Package")

![info](https://user-images.githubusercontent.com/140797/57506248-d96c8080-732d-11e9-8167-644c8fc4e0db.png "Info")

![helm](https://user-images.githubusercontent.com/140797/57506112-6531dd00-732d-11e9-8a5e-22166f42dd4c.png "Helm")

## Install

### Manual

From melpa, `M-x package-install RET doom-modeline RET`.

In `init.el`,

```emacs-lisp
(require 'doom-modeline)
(doom-modeline-mode 1)
```

or

```elisp
(add-hook 'after-init-hook #'doom-modeline-mode)
```

### Use-package

```emacs-lisp
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
```

or

```elisp
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))
```

This package is able to display icons if `all-the-icons` package and required
fonts are installed. Run `M-x all-the-icons-install-fonts` to install the
necessary fonts. Please refer to the
[installation guide](https://github.com/domtronn/all-the-icons.el#installation).

Add this to `init.el` if you want to use icons,

```elisp
;; Install via `M-x package-install RET all-the-icons RET`
(require 'all-the-icons)
```

or

```elisp
(use-package all-the-icons
  :ensure t)
```

Strongly recommend to use
[doom-themes](https://github.com/hlissner/emacs-doom-themes) at the same time.

## Customize

Run `M-x customize-group RET doom-modeline RET` or set the variables.

```emacs-lisp
;; If non-nil, cause imenu to see `doom-modeline' declarations.
;; This is done by adjusting `lisp-imenu-generic-expression' to
;; include support for finding `doom-modeline-def-*' forms.
;; Must be set before loading doom-modeline.
(setq doom-modeline-support-imenu t)

;; How tall the mode-line should be. It's only respected in GUI.
;; If the actual char height is larger, it respects the actual height.
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 4)

;; Whether to use hud instead of default bar. It's only respected in GUI.
(setq doom-modeline-hud nil)

;; The limit of the window width.
;; If `window-width' is smaller than the limit, some information won't be
;; displayed. It can be an integer or a float number. `nil' means no limit."
(setq doom-modeline-window-width-limit 85)

;; How to detect the project root.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'auto)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   auto => emacs/l/comint.el (in a project) or comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   truncate-nil => ~/Projects/FOSS/emacs/lisp/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are experiencing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'auto)

;; Whether display icons in the mode-line.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon t)

;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether display the time icon. It respects variable `doom-modeline-icon'.
(setq doom-modeline-time-icon t)

;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)

;; Whether display the buffer name.
(setq doom-modeline-buffer-name t)

;; Whether display the minor modes in the mode-line.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;; remove the modes from `doom-modeline-continuous-word-count-modes'.
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display the indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display the workspace name. Non-nil to display in the mode-line.
(setq doom-modeline-workspace-name t)

;; Whether display the perspective name. Non-nil to display in the mode-line.
(setq doom-modeline-persp-name t)

;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)

;; If non nil the perspective name is displayed alongside a folder icon.
(setq doom-modeline-persp-icon t)

;; Whether display the `lsp' state. Non-nil to display in the mode-line.
(setq doom-modeline-lsp t)

;; Whether display the GitHub notifications. It requires `ghub' package.
(setq doom-modeline-github nil)

;; The interval of checking GitHub.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display the modal state.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal t)

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon t)

;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
(setq doom-modeline-mu4e nil)
;; also enable the start of mu4e-alert
(mu4e-alert-enable-mode-line-display)

;; Whether display the gnus notifications.
(setq doom-modeline-gnus t)

;; Whether gnus should automatically be updated and how often (set to 0 or smaller than 0 to disable)
(setq doom-modeline-gnus-timer 2)

;; Wheter groups should be excludede when gnus automatically being updated.
(setq doom-modeline-gnus-excluded-groups '("dummy.group"))

;; Whether display the IRC notifications. It requires `circe' or `erc' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

;; Whether display the time. It respects `display-time-mode'.
(setq doom-modeline-time t)

;; Whether display the misc segment on all mode lines.
;; If nil, display only if the mode line is active.
(setq doom-modeline-display-misc-in-all-mode-lines t)

;; Whether display the environment version.
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

;; What to display as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)
```

## FAQ

1. How to display icons correctly?

   [all-the-icons](https://github.com/domtronn/all-the-icons.el) are necessary.
   Then run `M-x all-the-icons-install-fonts` to install the resource fonts.
   On Windows, the fonts should be installed manually. `all-the-icons` only
   support GUI. If you don't like color icons, `(setq all-the-icons-color-icons nil)`
   to disable it. Please refer to [all-the-icons.el](https://github.com/domtronn/all-the-icons.el)
   for details.

   If the icons are not displayed correctly although `all-the-icons` fonts are
   installed correctly, please install the
   [non-free](http://users.teilar.gr/~g1951d/License.pdf) font
   [Symbola](https://dn-works.com/wp-content/uploads/2020/UFAS-Fonts/Symbola.zip).
   This issue usually occurs on Windows.

   If you are using [cnfonts](https://github.com/tumashu/cnfonts), it will
   conflict with `all-the-icons`. The workaround is
   [here](https://github.com/seagle0128/doom-modeline/issues/278#issuecomment-569510336).

   In terminal mode, no icons or bars will be displayed, even though `all-the-icons`
   fonts are installed successfully, and `doom-modeline-icon` is non-nil.

1. I am experiencing the laggy issue, how to resolve it?

   Add this configuration into your init file:

   ```emacs-lisp
   ;; Don’t compact font caches during GC.
   (setq inhibit-compacting-font-caches t)
   ```

1. A ridiculous path is displayed on the mode-line while visiting a symbolink.

   It's the default behaviors of Vanilla Emacs. If you want to display the real
   names, please put this into your init file.

   ```emacs-lisp
   (setq find-file-visit-truename t)
   ```

   If the file is controlled by vc, refer to the documentation of
   `vc-follow-symlinks`.

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

1. Can I add my mode-line segments myself? How to do that?
   How can I define my own mode-line?

   There are two methods.

   - If the information is simple, just add to `mode-line-misc-info` or `global-mode-string`.

   - Use `doom-modeline-def-modeline` to define your own mode-line and set it as
     default.

     For example:

     ```emacs-lisp
     ;; Define your custom doom-modeline
     (doom-modeline-def-modeline 'my-simple-line
       '(bar matches buffer-info remote-host buffer-position parrot selection-info)
       '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

     ;; Set default mode-line
     (add-hook 'doom-modeline-mode-hook
               (lambda ()
                 (doom-modeline-set-modeline 'my-simple-line 'default)))

     ;; Configure other mode-lines based on major modes
     (add-to-list 'doom-modeline-mode-alist '(my-mode . my-simple-line))

     ;; Or disable other mode-lines
     (setq 'doom-modeline-mode-alist nil)
     ```

1. How to specify font family and size in modeline?

   For example:

   ```emacs-lisp
   (setq doom-modeline-height 1) ; optional
   (if (facep 'mode-line-active)
       (set-face-attribute 'mode-line-active nil :family "Noto Sans" :height 100) ; For 29+
     (set-face-attribute 'mode-line nil :family "Noto Sans" :height 100))
   (set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 100)
   ```

   or

   ```emacs-lisp
   (setq doom-modeline-height 1) ; optional
   (custom-set-faces
     '(mode-line ((t (:family "Noto Sans" :height 0.9))))
     '(mode-line-active ((t (:family "Noto Sans" :height 0.9)))) ; For 29+
     '(mode-line-inactive ((t (:family "Noto Sans" :height 0.9)))))
   ```

   Please refer to
   [#189](https://github.com/seagle0128/doom-modeline/issues/189) and
   [#301](https://github.com/seagle0128/doom-modeline/issues/301).

1. The right side of the modeline is cut off. How to fix it?

   - Tweak `all-the-icons-scale-factor`. e.g. `(setq all-the-icons-scale-factor 1.1)`
   - Use another font family or size. See above.

1. How to disable symbolic links expanding in mode-line?

   If you encounter the issue like this

   ![Screenshot](https://user-images.githubusercontent.com/9449246/62822565-c3f93380-bb74-11e9-95f6-f9c24a6cbd14.png)

   please try this:

   ```elisp
   ;; built-in `project' on 26+
   (setq doom-modeline-project-detection 'project)
   ;; or `find-in-project' if it's installed
   (setq doom-modeline-project-detection 'ffip)
   ```

   For more details, refer to
   [#209](https://github.com/seagle0128/doom-modeline/issues/209) and
   [#224](https://github.com/seagle0128/doom-modeline/issues/224).

1. Can `doom-modeline` show match count while search with `evil-search`?

   Yes. For better experience, should enable `anzu-mode` and load `evil-anzu`.

1. How to show that the R is running in inferior ess R mode?

   The ess R mode modifies `mode-line-buffer-identification` directly to display the status.
   To display the status in `doom-modeline`, you can add the status to `mode-line-process`
   or `global-mode-string` as below.

   ```elisp
   (add-hook 'inferior-ess-mode-hook
         (lambda ()
           (add-to-list 'mode-line-process '(:eval (nth ess--busy-count ess-busy-strings)))))
   ```

1. How to display company mode-line information?

   Enable `doom-modeline-minor-modes` or `(add-to-list 'global-mode-string company-lighter)`.

1. How to display LaTeX compilation information in the mode line?

   Use [process](https://github.com/haji-ali/procress) package.
   Refer to [#387](https://github.com/seagle0128/doom-modeline/issues/387) for more details.

   ```elisp
   (require 'procress)
   (procress-load-default-svg-images)
   (add-hook 'LaTeX-mode-hook #'procress-auctex-mode)
   ```

## Donate

If you think it's helpful for you, please consider paying a cup of coffee for
me. Thank you! :smile:

<img
src="https://user-images.githubusercontent.com/140797/65818854-44204900-e248-11e9-9cc5-3e6339587cd8.png"
alt="Alipay" width="120"/>
&nbsp;&nbsp;&nbsp;&nbsp;
<img
src="https://user-images.githubusercontent.com/140797/65818844-366ac380-e248-11e9-931c-4bd872d0566b.png"
alt="Wechat Pay" width="120"/>

<a href="https://paypal.me/seagle0128" target="_blank">
<img
src="https://www.paypalobjects.com/digitalassets/c/website/marketing/apac/C2/logos-buttons/optimize/44_Grey_PayPal_Pill_Button.png"
alt="PayPal" width="120" />
</a>
&nbsp;&nbsp;&nbsp;&nbsp;
<a href="https://www.buymeacoffee.com/s9giES1" target="_blank">
<img src="https://cdn.buymeacoffee.com/buttons/default-orange.png" alt="Buy Me A Coffee"
width="160"/>
</a>
