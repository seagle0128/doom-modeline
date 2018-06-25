# doom-modeline

[![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

The modeline package extracted from [DOOM Emacs](https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline).

It's also the part of [Centaur Emacs](https://github.com/seagle0128/.emacs.d).

## Feature

The DOOM modeline was designed for minimalism, and offers:

- A match count panel (for anzue, iedit, evil-search and evil-substitute)
- An indicator for recording a macro
- Local python/ruby/go version in the major-mode
- A customizable mode-line height (see doom-modeline-height)
- An error/warning count segment for flycheck
- A workspace number segment for eyebrowse
- A window number segment for window-numbering

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
      :requires (shrink-path eldoc-eval)
      :hook (after-init . doom-modeline-init))
```

This package requires the fonts included with `all-the-icons` to be installed.
Run `M-x all-the-icons-install-fonts` to do so.

If you do not have them already installed, here are the dependencies:

``` emacs-lisp
(use-package shrink-path)
(use-package all-the-icons)
(use-package eldoc-eval)
```

## Screenshots

![modeline](https://github.com/hlissner/doom-emacs/raw/screenshots/ml.png)

![search](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-search.png)

![subst](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-subst.png)

![macro](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-macro.png)

![version](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-version.png)

![errors](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-errors.png)
