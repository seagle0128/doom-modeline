# doom-modeline

The modeline package extracted from [DOOM Emacs](https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline).

It's also the part of [Centaur Emacs](https://github.com/seagle0128/.emacs.d).

## Feature

The DOOM modeline was designed for minimalism, and offers:

- A match count panel (for evil-search, iedit and evil-substitute)
- An indicator for recording a macro
- Local python/ruby version in the major-mode
- A customizable mode-line height (see +doom-modeline-height)
- An error/warning count segment for flycheck

## Install

### Manual

From melpa, `M-x package-install RET doom-modeline RET`.

In `init.el`,

``` emacs-lisp
(require 'doom-modeline)
(+doom-modeline|init)
```

### Use-package

``` emacs-lisp
(use-package doom-modeline
      :ensure t
      :defer t
      :init (add-hook 'after-init-hook #'+doom-modeline|init))
```

This package requires the fonts included with `all-the-icons` to be installed.
Run `M-x all-the-icons-install-fonts` to do so.

## Screenshots

![modeline](https://github.com/hlissner/doom-emacs/raw/screenshots/ml.png)

![search](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-search.png)

![subst](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-subst.png)

![macro](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-macro.png)

![version](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-version.png)

![errors](https://github.com/hlissner/doom-emacs/raw/screenshots/ml-errors.png)
