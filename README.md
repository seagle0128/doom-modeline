# doom-modeline

[![Build Status](https://travis-ci.org/seagle0128/doom-modeline.svg?branch=master)](https://travis-ci.org/seagle0128/doom-modeline)
[![MELPA](https://melpa.org/packages/doom-modeline-badge.svg)](https://melpa.org/#/doom-modeline)
[![MELPA Stable](https://stable.melpa.org/packages/doom-modeline-badge.svg)](https://stable.melpa.org/#/doom-modeline)
[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

A fancy and fast mode-line which was from [DOOM
Emacs](https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline),
but it's more powerful and much faster.

It's integrated into [Centaur Emacs](https://github.com/seagle0128/.emacs.d) by
default.

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
- An indicator for `evil` state
- An indicator for `god` state
- An indicator for `ryo-modal` state
- An indicator for `xah-fly-keys` state
- An indicator for remote host
- An indicator for debug state
- An indicator for current input method
- An indicator for `LSP` state
- An indicator for github notifications
- An indicator for buffer position which is compatible with `nyan-mode`
- An indicator for party parrot
- An indicator for PDF page number
- Truncated file name, file icon, buffer state and project name in buffer
  information segment, which is compatible with `projectile` and `project`
  
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

![lsp_version](https://user-images.githubusercontent.com/140797/49694206-edfeee00-fbc0-11e8-9c50-888a2cd7a397.png
"Perspective, LSP, Version, VCS and Flycheck")

![perspective](https://user-images.githubusercontent.com/140797/49694481-e0983280-fbc5-11e8-8cb2-c8d2e782bcdb.png
 "Perspective, LSP, Version and VCS")

![github_notifications](https://user-images.githubusercontent.com/140797/49808406-6b7c5700-fd97-11e8-83bb-d2ddbe5be8eb.png
"Github Notifications")

![minions](https://user-images.githubusercontent.com/140797/50301291-de857c00-04c1-11e9-84c5-bfbc8de8295f.png
"Minions and minor modes")

![debug](https://user-images.githubusercontent.com/140797/50302008-c57dca80-04c3-11e9-8578-50154ef4f9f0.png
"Debug state")

![nyan_parrot](https://user-images.githubusercontent.com/140797/51301061-da209480-1a68-11e9-9f64-905d889df9d6.png
"Nyan and Parrot")

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
(setq doom-modeline-icon t)

;; Whether show the icon for major mode. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon nil)

;; Whether display minor modes or not. Non-nil to display in mode-line.
(setq doom-modeline-minor-modes nil)

;; Whether display perspective name or not. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; Whether display `lsp' state or not. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display github notifications or not. Requires `ghub` package.
(setq doom-modeline-github nil)

;; The interval of checking github.
(setq doom-modeline-github-interval (* 30 60))

;; Whether display environment version or not.
(setq doom-modeline-version t)
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
