# doom-modeline

[![Build Status](https://travis-ci.org/seagle0128/doom-modeline.svg?branch=master)](https://travis-ci.org/seagle0128/doom-modeline)
[![MELPA](https://melpa.org/packages/doom-modeline-badge.svg)](https://melpa.org/#/doom-modeline)
[![MELPA Stable](https://stable.melpa.org/packages/doom-modeline-badge.svg)](https://stable.melpa.org/#/doom-modeline)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

A fancy mode-line from [DOOM Emacs](https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline).
It's integrated to [Centaur Emacs](https://github.com/seagle0128/.emacs.d).

## Feature

The DOOM modeline was designed for minimalism, and offers:

- A match count panel (for `anzu`, `iedit`, `multi-cusor`, `evil-search` and `evil-substitute`)
- An indicator for recording a macro
- Local `python`/`ruby`/`perl`/`go`/`elixir` version in the major-mode
- A customizable mode-line height (see `doom-modeline-height`)
- An error/warning count segment for `flycheck`
- A workspace number segment for `eyebrowse`
- A window number segment for `ace-window`, `winum` and `window-numbering`
- An indicator for `evil` state
- An indicator for current input method
- Highlight project name in buffer information if `projectile` or `project` is available.

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

Recommand to use [doom-themes](https://github.com/hlissner/emacs-doom-themes);

## Customize

``` emacs-lisp
;; How tall the mode-line should be (only respected in GUI Emacs).
(setq doom-modeline-height 25)

;; How wide the mode-line bar should be (only respected in GUI Emacs).
(setq doom-modeline-bar-width 3)

;; Determines the style used by `doom-modeline-buffer-file-name'.
;; If you are expereicing the laggy issue, especially while editing remote files
;; with tramp, please use `file-name', `truncate-all' or `truncate-upto-root'
;; style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; What executable of Python will be used (if nil nothing will be showed).
(setq doom-modeline-python-executable "python")

;; Whether show `all-the-icons' or not (if nil nothing will be showed).
;; The icons may not be showed correctly on Windows. Disable to make it work.
(setq doom-modeline-icon t)

;; Don’t compact font caches during GC.
;; If you are expereicing the laggy issue especially on Windows, please set to
;; non-nil.
(setq inhibit-compacting-font-caches t)
```

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
