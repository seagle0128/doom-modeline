# doom-modeline

[![Build Status](https://travis-ci.org/seagle0128/doom-modeline.svg?branch=master)](https://travis-ci.org/seagle0128/doom-modeline)
[![MELPA](https://melpa.org/packages/doom-modeline-badge.svg)](https://melpa.org/#/doom-modeline)
[![MELPA Stable](https://stable.melpa.org/packages/doom-modeline-badge.svg)](https://stable.melpa.org/#/doom-modeline)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

A fast and fancy mode-line which is extracted from [DOOM
Emacs](https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline),
but it provides more features and it's much faster.

It's integrated into [Centaur Emacs](https://github.com/seagle0128/.emacs.d) by
default.

## Feature

The `doom-modeline` was designed for minimalism and fast, and offers:

- A match count panel (for `anzu`, `iedit`, `multiple-cursors`, `symbol-overlay`,
  `evil-search` and `evil-substitute`)
- An indicator for recording a macro
- Local `python`/`ruby`/`perl`/`go`/`elixir` version in the major-mode
- A customizable mode-line height (see `doom-modeline-height`)
- An error/warning count segment for `flycheck`
- A workspace number segment for `eyebrowse`
- A perspective name segment for `persp-mode`
- A window number segment for `ace-window`, `winum` and `window-numbering`
- An indicator for `evil` state
- An indicator for `god` state
- An indicator for `ryo-modal` state
- An indicator for remote host
- An indicator for current input method
- Truncated file names, file icon, buffer state and project name in buffer
  information segment, which is compatible with `projectile` or `project`

## Install

### Manual

From melpa, `M-x package-install RET doom-modeline RET`.

In `init.el`,

``` emacs-lisp
(require 'doom-modeline)
(doom-modeline-init)
```

### Use-package

``` emacs-lisp
(use-package doom-modeline
      :ensure t
      :defer t
      :hook (after-init . doom-modeline-init))
```

This package requires the fonts included with `all-the-icons` to be installed.
Run `M-x all-the-icons-install-fonts` to do so.

Strongly recommend to use
[doom-themes](https://github.com/hlissner/emacs-doom-themes) at the same time.

## Customize

``` emacs-lisp
;; How tall the mode-line should be (only respected in GUI Emacs).
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be (only respected in GUI Emacs).
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

;; What executable of Python will be used (if nil nothing will be showed).
(setq doom-modeline-python-executable "python")

;; Whether show `all-the-icons' or not (if nil nothing will be showed).
;; The icons may not be showed correctly on Windows. Disable to make it work.
(setq doom-modeline-icon t)

;; Whether show the icon for major mode. It should respect `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)
```

## FAQ

1. I am experiencing the laggy issue on Windows, how to resolve it?

   You need to add this configuration into your init file.

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

    If the file is controlled by vc, refer to `vc-follow-symlinks`.

## Screenshots

![modeline](https://github.com/hlissner/doom-emacs/raw/screenshots/ml.png)

![search](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-search.png)

![subst](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-subst.png)

![macro](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-macro.png)

![version](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-version.png)
![errors](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-errors.png)

![evil normal state](https://user-images.githubusercontent.com/140797/43999448-135ba3c6-9e3f-11e8-93cc-cb5d673a2d0e.png)
![evil emacs state](https://user-images.githubusercontent.com/140797/43999468-6763b42c-9e3f-11e8-8f1c-f4d85cc583b8.png)

![evil insert sate](https://user-images.githubusercontent.com/140797/43999462-5261ac64-9e3f-11e8-8da5-26a6e8cbf04b.png)
![evil viusal state](https://user-images.githubusercontent.com/140797/43999464-5b2dd034-9e3f-11e8-80ba-0fc8af3599a3.png)

![evil motion state](https://user-images.githubusercontent.com/140797/43999480-88548abc-9e3f-11e8-9d78-759546050a12.png)
![evil operator state](https://user-images.githubusercontent.com/140797/43999484-9a7ea40c-9e3f-11e8-8b2b-c7c46aeece1f.png)
