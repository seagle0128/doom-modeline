;;; doom-modeline.el --- A minimal and modern modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline
;; Version: 0.8.1
;; Package-Requires: ((emacs "25.1") (all-the-icons "1.0.0") (shrink-path "0.2.0") (eldoc-eval "0.1") (dash "2.11.0"))
;; Keywords: faces mode-line

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
;; This package offers a modern modeline them which is extraced from DOOM Emacs
;; (https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline).
;; It's also the part of Centaur Emacs (https://github.com/seagle0128/.emacs.d).
;;
;; The DOOM modeline was designed for minimalism, and offers:
;; 1. A match count panel (for evil-search, iedit and evil-substitute)
;; 2. An indicator for recording a macro
;; 3. Local python/ruby version in the major-mode
;; 4. A customizable mode-line height (see doom-modeline-height)
;; 5. An error/warning count segment for flycheck
;; 6. A workspace number segment for eyebrowse
;; 7. A window number segment for winum and window-numbering
;; 8. An indicator for evil state
;;
;; Installation:
;; From melpa, `M-x package-install RET doom-modeline RET`.
;; In `init.el`,
;; (require 'doom-modeline)
;; (doom-modeline-init)
;; or
;; (use-package doom-modeline
;;   :ensure t
;;   :defer t
;;   :hook (after-init . doom-modeline-init))
;;

;;; Code:

(require 'all-the-icons)
(require 'eldoc-eval)
(require 'shrink-path)
(require 'subr-x)
(when (>= emacs-major-version 26)
  (require 'project))

;;
;; Variables
;;

(defvar doom-modeline-height 25
  "How tall the mode-line should be (only respected in GUI Emacs).")

(defvar doom-modeline-bar-width 3
  "How wide the mode-line bar should be (only respected in GUI Emacs).")

(defvar doom-modeline-buffer-file-name-style 'truncate-upto-project
  "Determines the style used by `doom-modeline-buffer-file-name'.

Given ~/Projects/FOSS/emacs/lisp/comint.el
  truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  truncate-with-project => emacs/l/comint.el
  truncate-except-project => ~/P/F/emacs/l/comint.el
  truncate-upto-root => ~/P/F/e/lisp/comint.el
  truncate-all => ~/P/F/e/l/comint.el
  relative-from-project => emacs/lisp/comint.el
  relative-to-project => lisp/comint.el
  file-name => comint.el")

(defvar doom-modeline-python-executable "python"
  "What executable of Python will be used (if nil nothing will be showed).")

(defvar doom-modeline-icon t
  "Whether show `all-the-icons' or not (if nil nothing will be showed).
The icons may not be showed correctly on Windows. Disable to make it work.")

;;
;; externals
;;

(defvar anzu--current-position)
(defvar anzu--overflow-p)
(defvar anzu--state)
(defvar anzu--total-matched)
(defvar anzu-cons-mode-line-p)
(defvar aw-keys)
(defvar evil-ex-active-highlights-alist)
(defvar evil-ex-argument)
(defvar evil-ex-range)
(defvar evil-local-mode)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-visual-selection)
(defvar flycheck-current-errors)
(defvar iedit-mode)
(defvar iedit-occurrences-overlays)
(defvar projectile-dynamic-mode-line)
(defvar text-scale-mode-amount)
(defvar winum-auto-setup-mode-line)
(defvar mc/mode-line)

(declare-function anzu--reset-status 'anzu)
(declare-function anzu--where-is-here 'anzu)
(declare-function avy-traverse 'avy)
(declare-function avy-tree 'avy)
(declare-function aw-update 'ace-window)
(declare-function aw-window-list 'ace-window)
(declare-function eldoc-in-minibuffer-mode 'eldoc-eval)
(declare-function evil-delimited-arguments 'evil-common)
(declare-function evil-emacs-state-p 'evil-states)
(declare-function evil-force-normal-state 'evil-commands)
(declare-function evil-insert-state-p 'evil-states)
(declare-function evil-motion-state-p 'evil-states)
(declare-function evil-normal-state-p 'evil-states)
(declare-function evil-operator-state-p 'evil-states)
(declare-function evil-replace-state-p 'evil-states)
(declare-function evil-state-property 'evil-common)
(declare-function evil-visual-state-p 'evil-states)
(declare-function eyebrowse--get 'eyebrowse)
(declare-function face-remap-remove-relative 'face-remap)
(declare-function flycheck-count-errors 'flycheck)
(declare-function iedit-find-current-occurrence-overlay 'iedit-lib)
(declare-function iedit-prev-occurrence 'iedit-lib)
(declare-function image-get-display-property 'image-mode)
(declare-function magit-toplevel 'magit-git)
(declare-function project-current 'project)
(declare-function project-roots 'project)
(declare-function projectile-ensure-project 'projectile)
(declare-function projectile-project-root 'projectile)
(declare-function window-numbering-clear-mode-line 'window-numbering)
(declare-function window-numbering-get-number-string 'window-numbering)
(declare-function window-numbering-install-mode-line 'window-numbering)
(declare-function winum--clear-mode-line 'winum)
(declare-function winum--install-mode-line 'winum)
(declare-function winum-get-number-string 'winum)

;;
;; Custom faces
;;

(defgroup doom-modeline nil
  "Doom mode-line faces."
  :group 'faces)

(defface doom-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path.")

(defface doom-modeline-buffer-file
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the filename part of the mode-line buffer path.")

(defface doom-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line.")

(defface doom-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line.")

(defface doom-modeline-project-root-dir
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the project part of the mode-line buffer path.")

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line.")

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `doom-modeline--anzu', `doom-modeline--evil-substitute' and
`iedit'")

(defface doom-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'.")

(defface doom-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface doom-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'")

;; Bar
(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface doom-modeline-eldoc-bar '((t (:inherit shadow)))
  "The face used for the left-most bar on the mode-line when eldoc-eval is
active.")

(defface doom-modeline-inactive-bar `((t (:background
                                          ,(face-foreground 'mode-line-inactive)
                                          :foreground
                                          ,(face-background 'mode-line-inactive))))
  "The face used for the left-most bar on the mode-line of an inactive window.")

(defface doom-modeline-evil-emacs-state '((t (:inherit doom-modeline-warning)))
  "Face for the Emacs state tag in evil state indicator.")

(defface doom-modeline-evil-insert-state'((t (:inherit doom-modeline-urgent)))
  "Face for the insert state tag in evil state indicator.")

(defface doom-modeline-evil-motion-state'((t :inherit doom-modeline-buffer-path))
  "Face for the motion state tag in evil state indicator.")

(defface doom-modeline-evil-normal-state'((t (:inherit doom-modeline-info)))
  "Face for the normal state tag in evil state indicator.")

(defface doom-modeline-evil-operator-state'((t (:inherit doom-modeline-buffer-path)))
  "Face for the operator state tag in evil state indicator.")

(defface doom-modeline-evil-visual-state'((t (:inherit doom-modeline-buffer-file)))
  "Face for the visual state tag in evil state indicator.")

(defface doom-modeline-evil-replace-state'((t (:inherit doom-modeline-buffer-modified)))
  "Face for the replace state tag in evil state indicator.")

(defface doom-modeline-god-state'((t (:inherit doom-modeline-info)))
  "Face for the god state indicator.")

;;
;; Modeline library
;;

(eval-and-compile
  (defvar doom-modeline-fn-alist ())
  (defvar doom-modeline-var-alist ()))

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
        (let ((rhs-str (format-mode-line (cons "" rhs-forms))))
          (list lhs-forms
                (propertize
                 " " 'display
                 `((space :align-to (- (+ right right-fringe right-margin)
                                       ,(+ 1 (string-width rhs-str))))))
                rhs-str)))
      (concat "Modeline:\n"
              (format "  %s\n  %s"
                      (prin1-to-string lhs)
                      (prin1-to-string rhs))))))

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

(defvar-local doom-modeline-project-root nil)
(defun doom-modeline-project-root ()
  "Get the path to the root of your project.

  Return `default-directory' if no project was found."
  (if doom-modeline-project-root
      doom-modeline-project-root
    (setq doom-modeline-project-root
          (or
           (when (featurep 'projectile) (projectile-project-root))
           (when (featurep 'project)
             (when-let ((project (project-current)))
               (expand-file-name (car (project-roots project)))))
           default-directory))))

;; Disable projectile mode-line segment
(setq projectile-dynamic-mode-line nil)

;;
;; Plugins
;;

(defun doom-modeline-eldoc (text)
  "Get eldoc TEXT for mode-line."
  (concat (when (display-graphic-p)
            (doom-modeline--make-xpm 'doom-modeline-eldoc-bar
                                     doom-modeline-height
                                     doom-modeline-bar-width))
          text))

;; Show eldoc in the mode-line with `eval-expression'
(defun doom-modeline--show-eldoc (input)
  "Display string INPUT in the mode-line next to minibuffer."
  (with-current-buffer (eldoc-current-buffer)
    (let* ((str              (and (stringp input) input))
           (mode-line-format (or (and str (or (doom-modeline-eldoc str) str))
                                 mode-line-format))
           mode-line-in-non-selected-windows)
      (force-mode-line-update)
      (sit-for eldoc-show-in-mode-line-delay))))
(setq eldoc-in-minibuffer-show-fn #'doom-modeline--show-eldoc)

(eldoc-in-minibuffer-mode 1)

;; anzu and evil-anzu expose current/total state that can be displayed in the
;; mode-line.
(defun doom-modeline-fix-anzu-count (positions here)
  "Calulate anzu counts via POSITIONS and HERE."
  (cl-loop for (start . end) in positions
           collect t into before
           when (and (>= here start) (<= here end))
           return (length before)
           finally return 0))

(advice-add #'anzu--where-is-here :override #'doom-modeline-fix-anzu-count)

;; Avoid anzu conflicts across buffers
;; (mapc #'make-variable-buffer-local
;;       '(anzu--total-matched anzu--current-position anzu--state
;;                             anzu--cached-count anzu--cached-positions anzu--last-command
;;                             anzu--last-isearch-string anzu--overflow-p))

;; Ensure anzu state is cleared when searches & iedit are done
(with-eval-after-load 'anzu
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
  (advice-add #'evil-force-normal-state :after #'anzu--reset-status))

;; Keep `doom-modeline-current-window' up-to-date
(defvar doom-modeline-current-window (frame-selected-window))
(defun doom-modeline-set-selected-window (&rest _)
  "Set `doom-modeline-current-window' appropriately."
  (when-let ((win (frame-selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq doom-modeline-current-window win)
      (force-mode-line-update))))

(defun doom-modeline-unset-selected-window ()
  "Unset `doom-modeline-current-window' appropriately."
  (setq doom-modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'doom-modeline-set-selected-window)
(advice-add #'handle-switch-frame :after #'doom-modeline-set-selected-window)
(advice-add #'select-window :after #'doom-modeline-set-selected-window)
(advice-add #'make-frame :after #'doom-modeline-set-selected-window)
(advice-add #'delete-frame :after #'doom-modeline-set-selected-window)
(with-no-warnings
  (cond ((not (boundp 'after-focus-change-function))
         (add-hook 'focus-in-hook  #'doom-modeline-set-selected-window)
         (add-hook 'focus-out-hook #'doom-modeline-unset-selected-window))
        ((defun doom-modeline-refresh-frame ()
           (setq doom-modeline-current-window nil)
           (cl-loop for frame in (frame-list)
                    if (eq (frame-focus-state frame) t)
                    return (setq doom-modeline-current-window (frame-selected-window frame)))
           (force-mode-line-update))
         (add-function :after after-focus-change-function #'doom-modeline-refresh-frame))))

;; Show version string for multi-version managers like rvm, rbenv, pyenv, etc.
(defvar-local doom-modeline-env-version nil)
(defvar-local doom-modeline-env-command nil)
(add-hook 'find-file-hook #'doom-modeline-update-env)
(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function #'doom-modeline-update-env)
    (add-hook 'focus-in-hook #'doom-modeline-update-env)))
(defun doom-modeline-update-env ()
  "Update environment info on mode-line."
  (when doom-modeline-env-command
    (let ((default-directory (doom-modeline-project-root))
          (s (shell-command-to-string doom-modeline-env-command)))
      (setq doom-modeline-env-version (if (string-match "[ \t\n\r]+\\'" s)
                                          (replace-match "" t t s)
                                        s)))))


;;
;; Modeline helpers
;;

(defun doom-modeline-icon-octicon (&rest args)
  "Display octicon via `ARGS'."
  (when doom-modeline-icon
    (apply 'all-the-icons-octicon args)))

(defun doom-modeline-icon-faicon (&rest args)
  "Display font awesome icon via `ARGS'."
  (when doom-modeline-icon
    (apply 'all-the-icons-faicon args)))

(defun doom-modeline-icon-material (&rest args)
  "Display material icon via `ARGS'."
  (when doom-modeline-icon
    (apply 'all-the-icons-material args)))

(defun doom-modeline--active ()
  "Whether is an active window."
  (eq (selected-window) doom-modeline-current-window))

(defun doom-modeline--make-xpm (face width height)
  "Create an XPM bitmap via FACE, WIDTH and HEIGHT. Inspired by `powerline''s `pl/make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or (face-background face nil t) "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun doom-modeline-buffer-file-name ()
  "Propertized variable `buffer-file-name' based on `doom-modeline-buffer-file-name-style'."
  (let ((buffer-file-name (or (buffer-file-name (buffer-base-buffer)) "")))
    (unless buffer-file-truename
      (setq buffer-file-truename (file-truename buffer-file-name)))
    (propertize
     (pcase doom-modeline-buffer-file-name-style
       (`truncate-upto-project
        (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink))
       (`truncate-from-project
        (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil 'shrink))
       (`truncate-with-project
        (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shink 'hide))
       (`truncate-except-project
        (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shink))
       (`truncate-upto-root
        (doom-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename))
       (`truncate-all
        (doom-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename t))
       (`relative-to-project
        (doom-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename))
       (`relative-from-project
        (doom-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename 'include-project))
       (`file-name
        (propertize (file-name-nondirectory buffer-file-name)
                    'face
                    (let ((face (or (and (buffer-modified-p)
                                         'doom-modeline-buffer-modified)
                                    (and (doom-modeline--active)
                                         'doom-modeline-buffer-file))))
                      (when face `(:inherit ,face))))))
     'help-echo buffer-file-truename)))

(defun doom-modeline--buffer-file-name-truncate (file-path true-file-path &optional truncate-tail)
  "Propertized variable `buffer-file-name' that truncates every dir along path.
If TRUNCATE-TAIL is t also truncate the parent directory of the file."
  (let ((dirs (shrink-path-prompt (file-name-directory true-file-path)))
        (active (doom-modeline--active)))
    (if (null dirs)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified)))
        (let ((dirname (car dirs))
              (basename (cdr dirs))
              (dir-faces (or modified-faces (if active 'doom-modeline-project-root-dir)))
              (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
          (concat (propertize (concat dirname
                                      (if truncate-tail (substring basename 0 1) basename)
                                      "/")
                              'face (if dir-faces `(:inherit ,dir-faces)))
                  (propertize (file-name-nondirectory file-path)
                              'face (if file-faces `(:inherit ,file-faces)))))))))

(defun doom-modeline--buffer-file-name-relative (_file-path true-file-path &optional include-project)
  "Propertized variable `buffer-file-name' showing directories relative to project's root only."
  (let ((root (doom-modeline-project-root))
        (active (doom-modeline--active)))
    (if (null root)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (let* ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified))
             (relative-dirs (file-relative-name (file-name-directory true-file-path)
                                                (if include-project (concat root "../") root)))
             (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
             (file-faces (or modified-faces (if active 'doom-modeline-buffer-file))))
        (if (equal "./" relative-dirs) (setq relative-dirs ""))
        (concat (propertize relative-dirs 'face (if relative-faces `(:inherit ,relative-faces)))
                (propertize (file-name-nondirectory true-file-path)
                            'face (if file-faces `(:inherit ,file-faces))))))))

(defun doom-modeline--buffer-file-name (file-path _true-file-path &optional truncate-project-root-parent truncate-project-relative-path hide-project-root-parent)
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
  (let ((project-root (doom-modeline-project-root))
        (active (doom-modeline--active))
        (modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified)))
    (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
          (project-faces  (or modified-faces (if active 'font-lock-string-face)))
          (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
          (file-faces     (or modified-faces (if active 'doom-modeline-buffer-file))))
      (let ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
            (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
            (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
            (file-props     `(,@(if file-faces     `(:inherit ,file-faces)))))
        (concat
         ;; project root parent
         (unless hide-project-root-parent
           (propertize
            (when-let (root-path-parent
                       (file-name-directory (directory-file-name project-root)))
              (if truncate-project-root-parent
                  (shrink-path--dirs-internal root-path-parent t)
                (abbreviate-file-name root-path-parent)))
            'face sp-props))
         ;; project
         (propertize
          (concat (file-name-nondirectory (directory-file-name project-root)) "/")
          'face project-props)
         ;; relative path
         (propertize
          (when-let (relative-path (file-relative-name
                                    (or (file-name-directory file-path) "./")
                                    project-root))
            (if (string-equal relative-path "./")
                ""
              (if truncate-project-relative-path
                  (substring (shrink-path--dirs-internal relative-path t) 1)
                relative-path)))
          'face relative-props)
         ;; file name
         (propertize (file-name-nondirectory file-path) 'face file-props))))))


;;
;; buffer information
;;

(doom-modeline-def-segment buffer-default-directory
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (doom-modeline--active) 'doom-modeline-buffer-path)))
    (concat (if (display-graphic-p) " ")
            (doom-modeline-icon-octicon
             "file-directory"
             :face face
             :v-adjust -0.05
             :height 1.25)
            (propertize (concat " " (abbreviate-file-name default-directory))
                        'face face))))

;;
(defun doom-modeline-update-buffer-file-name ()
  "Update buffer file name in mode-line.

  Show buffer name if it doesn't equal the file name.
  Format: \"buffer-file-name[buffer-name]\".
  Except the same buffer names in different directories."
  (when-let ((file-name (doom-modeline-buffer-file-name))
             (buffer-name (buffer-name))
             (buffer-file-name buffer-file-name))
    (if (or (not (file-exists-p buffer-file-name))
            (string-equal
             (file-name-nondirectory buffer-file-name)
             (replace-regexp-in-string "<.+>$" "" buffer-name)))
        file-name
      (format "%s[%s]" file-name buffer-name))))

(doom-modeline-def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (let ((active (doom-modeline--active)))
    (concat
     (if active
         (cond (buffer-read-only
                (concat (doom-modeline-icon-octicon
                         "lock"
                         :face 'doom-modeline-warning
                         :v-adjust -0.05)
                        " "))
               ((buffer-modified-p)
                (concat (doom-modeline-icon-faicon
                         "floppy-o"
                         :face 'doom-modeline-buffer-modified
                         :v-adjust -0.0575)
                        " "))
               ((and buffer-file-name
                     (not (file-exists-p buffer-file-name)))
                (concat (doom-modeline-icon-octicon
                         "circle-slash"
                         :face 'doom-modeline-urgent
                         :v-adjust -0.05)
                        " "))
               ((buffer-narrowed-p)
                (concat (doom-modeline-icon-octicon
                         "fold"
                         :face 'doom-modeline-warning
                         :v-adjust -0.05)
                        " "))))
     (if buffer-file-name
         (doom-modeline-update-buffer-file-name)
       (propertize "%b" 'face (if active 'doom-modeline-buffer-file))))))

(doom-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (propertize
   "%b"
   'face (cond ((and buffer-file-name (buffer-modified-p))
                'doom-modeline-buffer-modified)
               ((doom-modeline--active) 'doom-modeline-buffer-file))))

;;
(doom-modeline-def-segment buffer-encoding
  "Displays the encoding and eol style of the buffer the same way Atom does."
  (concat (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF  ")
            (1 "CRLF  ")
            (2 "CR  "))
          (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))


;;
;; major-mode
;;

(doom-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (propertize
   (concat (format-mode-line
            `(:propertize ("" mode-name)
                          help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                          mouse-face mode-line-highlight
                          local-map ,mode-line-major-mode-keymap))
           (when doom-modeline-env-version
             (format " %s" doom-modeline-env-version))
           (and (boundp 'text-scale-mode-amount)
                (/= text-scale-mode-amount 0)
                (format
                 (if (> text-scale-mode-amount 0)
                     " (%+d)"
                   " (%-d)")
                 text-scale-mode-amount)))
   'face (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))


;;
;; process
;;

(doom-modeline-def-segment process
  "The process info."
  mode-line-process)

;;
;; vcs
;;

(defvar-local doom-modeline--vcs nil)
(defun doom-modeline--update-vcs (&rest _)
  "Update vsc state in mode-line."
  (setq doom-modeline--vcs
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (let ((face    'mode-line-inactive)
                  (active  (doom-modeline--active))
                  (all-the-icons-default-adjust -0.1))
              (concat "  "
                      (cond ((memq state '(edited added))
                             (if active (setq face 'doom-modeline-info))
                             (doom-modeline-icon-octicon
                              "git-compare"
                              :face face
                              :v-adjust -0.05))
                            ((eq state 'needs-merge)
                             (if active (setq face 'doom-modeline-info))
                             (doom-modeline-icon-octicon "git-merge" :face face))
                            ((eq state 'needs-update)
                             (if active (setq face 'doom-modeline-warning))
                             (doom-modeline-icon-octicon "arrow-down" :face face))
                            ((memq state '(removed conflict unregistered))
                             (if active (setq face 'doom-modeline-urgent))
                             (doom-modeline-icon-octicon "alert" :face face))
                            (t
                             (if active (setq face 'font-lock-doc-face))
                             (doom-modeline-icon-octicon
                              "git-branch"
                              :face face
                              :v-adjust -0.05)))
                      " "
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face (if active face))
                      " "))))))
(add-hook 'after-revert-hook #'doom-modeline--update-vcs)
(add-hook 'after-save-hook #'doom-modeline--update-vcs)
(add-hook 'find-file-hook #'doom-modeline--update-vcs t)
(advice-add #'select-window :after #'doom-modeline--update-vcs)

(doom-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (if (doom-modeline--active)
      doom-modeline--vcs
    ""))


;;
;; flycheck
;;

(defvar doom-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "Text style with icons in mode-line.")

(defun doom-modeline-icon (icon &optional text face voffset)
  "Displays an ICON with FACE, followed by TEXT.
Uses `all-the-icons-material' to fetch the icon."
  (concat (if vc-mode " " "  ")
          (when icon
            (concat
             (doom-modeline-icon-material icon :face face :height 0.95 :v-adjust (or voffset -0.2))
             (if text doom-modeline-vspc)))
          (if text (propertize text 'face face))
          (if vc-mode "  " " ")))

(defvar-local doom-modeline--flycheck nil)
(add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck-segment)
(add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck-segment)

(defun doom-modeline-update-flycheck-segment (&optional status)
  "Update flycheck segment via STATUS."
  (setq doom-modeline--flycheck
        (pcase status
          (`finished (if flycheck-current-errors
                         (let-alist (flycheck-count-errors flycheck-current-errors)
                           (let ((sum (+ (or .error 0) (or .warning 0))))
                             (doom-modeline-icon "do_not_disturb_alt"
                                                 (number-to-string sum)
                                                 (if .error 'doom-modeline-urgent 'doom-modeline-warning)
                                                 -0.15)))
                       (doom-modeline-icon "check" nil 'doom-modeline-info)))
          (`running     (doom-modeline-icon "access_time" nil 'font-lock-doc-face -0.25))
          (`no-checker  (doom-modeline-icon "sim_card_alert" "-" 'font-lock-doc-face))
          (`errored     (doom-modeline-icon "sim_card_alert" "Error" 'doom-modeline-urgent))
          (`interrupted (doom-modeline-icon "pause" "Interrupted" 'font-lock-doc-face)))))

(doom-modeline-def-segment flycheck
  "Displays color-coded flycheck error status in the current buffer with pretty
icons."
  (if (doom-modeline--active)
      doom-modeline--flycheck
    ""))


;;
;; selection-info
;;

(defsubst doom-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(defvar-local doom-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline
segment.")

(doom-modeline-def-segment selection-info
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection."
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (doom-modeline--active))
    (cl-destructuring-bind (beg . end)
        (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
            (cons evil-visual-beginning evil-visual-end)
          (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (and (bound-and-true-p evil-visual-selection)
                                 (eq 'block evil-visual-selection)))
                        (let ((cols (abs (- (doom-modeline-column end)
                                            (doom-modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((and (bound-and-true-p evil-visual-selection)
                             (eq evil-visual-selection 'line))
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       ((format "%dC" (- end beg))))
                 (when doom-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))))
       'face 'doom-modeline-highlight))))


;;
;; matches (anzu, evil-substitute, iedit, macro)
;;

(defun doom-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (doom-modeline--active) (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-panel)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'doom-modeline-panel)
              sep
              (doom-modeline-icon-octicon "triangle-right"
                                          :face 'doom-modeline-panel
                                          :v-adjust -0.05)
              sep))))

(defsubst doom-modeline--anzu ()
  "Show the match index and total number thereof.

Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (setq anzu-cons-mode-line-p nil)
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " total))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (doom-modeline--active) 'doom-modeline-panel))))

(defsubst doom-modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and (bound-and-true-p evil-local-mode)
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (doom-modeline--active) 'doom-modeline-panel))))

(defun doom-modeline-themes--overlay-sort (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defsubst doom-modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (progn (iedit-prev-occurrence)
                               (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'doom-modeline-themes--overlay-sort)))
                      -1)
                 "-")
               length))
     'face (if (doom-modeline--active) 'doom-modeline-panel))))

(defsubst doom-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (when (bound-and-true-p multiple-cursors-mode)
    (propertize
     (concat (car mc/mode-line)
             (eval (cadadr mc/mode-line))
             " ")
     'face (if (doom-modeline--active) 'doom-modeline-panel))))

(doom-modeline-def-segment matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--anzu)
                      (doom-modeline--evil-substitute)
                      (doom-modeline--iedit)
                      (doom-modeline--multiple-cursors))))
    (or (and (not (equal meta "")) meta)
        (if buffer-file-name " %I "))))


;;
;; media-info
;;

(doom-modeline-def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO Include other information
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
             (when (fboundp 'image-size)
               (image-size (image-get-display-property) :pixels))
           (format "  %dx%d  " width height)))))


;;
;; bar
;;

(defvar doom-modeline--bar-active nil)
(defvar doom-modeline--bar-inactive nil)
(doom-modeline-def-segment bar
  "The bar regulates the height of the mode-line in GUI Emacs.
  Returns \"\" to not break --no-window-system."
  (if (display-graphic-p)
      (if (doom-modeline--active)
          doom-modeline--bar-active
        doom-modeline--bar-inactive)
    ""))

(when (>= emacs-major-version 26)
  (add-variable-watcher
   'doom-modeline-height
   (lambda (_sym val op _where)
     (when (and (eq op 'set) (integerp val))
       (doom-modeline-refresh-bars doom-modeline-bar-width val))))

  (add-variable-watcher
   'doom-modeline-bar-width
   (lambda (_sym val op _where)
     (when (and (eq op 'set) (integerp val))
       (doom-modeline-refresh-bars val doom-modeline-height)))))

(add-hook 'after-setting-font-hook
          '(lambda ()
             (doom-modeline-refresh-bars)))


;;
;; window number
;;

;; HACK: `ace-window-display-mode' should respect the ignore buffers.
(defun doom-modeline-aw-update ()
  "Update ace-window-path window parameter for all windows.

Ensure all windows are labeled so the user can select a specific
one. The ignored buffers are excluded unless `aw-ignore-on' is nil."
  (let ((ignore-window-parameters t))
    (avy-traverse
     (avy-tree (aw-window-list) aw-keys)
     (lambda (path leaf)
       (set-window-parameter
        leaf 'ace-window-path
        (propertize
         (apply #'string (reverse path))
         'face 'aw-mode-line-face))))))
(advice-add #'aw-update :override #'doom-modeline-aw-update)

(advice-add #'window-numbering-install-mode-line :override #'ignore)
(advice-add #'window-numbering-clear-mode-line :override #'ignore)
(advice-add #'winum--install-mode-line :override #'ignore)
(advice-add #'winum--clear-mode-line :override #'ignore)

(doom-modeline-def-segment window-number
  (let ((num (cond
              ((bound-and-true-p ace-window-display-mode)
               (setq mode-line-format
                     (assq-delete-all 'ace-window-display-mode
                                      (default-value 'mode-line-format)))
               (setq-default mode-line-format mode-line-format)
               (aw-update)
               (window-parameter (selected-window) 'ace-window-path))
              ((bound-and-true-p winum-mode)
               (setq winum-auto-setup-mode-line nil)
               (winum-get-number-string))
              ((bound-and-true-p window-numbering-mode)
               (window-numbering-get-number-string))
              (t ""))))
    (if (< 0 (length num))
        (propertize (format " %s " num)
                    'face (if (doom-modeline--active)
                              'doom-modeline-buffer-major-mode))
      "")))


;;
;; workspace number
;;

(doom-modeline-def-segment workspace-number
  "The current workspace name or number. Requires `eyebrowse-mode' to be
enabled."
  (if (and (bound-and-true-p eyebrowse-mode)
           (< 1 (length (eyebrowse--get 'window-configs))))
      (let* ((num (eyebrowse--get 'current-slot))
             (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
             (str (if (and tag (< 0 (length tag)))
                      tag
                    (when num (int-to-string num)))))
        (propertize (format " %s " str) 'face
                    (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))
    ""))


;;
;; global
;;

(doom-modeline-def-segment global
  "For the time string and whatever uses global-mode-string."
  (if (< 0 (length global-mode-string))
      '(" " global-mode-string "  ")
    ""))


;;
;; position
;;

;; Be compatible with Emacs 25.
(defvar-local doom-modeline-column-zero-based
  (or (bound-and-true-p column-number-indicator-zero-based) t)
  "When non-nil, mode line displays column numbers zero-based.
See `column-number-indicator-zero-based'.")

(defvar-local doom-modeline-percent-position
  (or (bound-and-true-p mode-line-percent-position) '(-3 "%p"))
  "Specification of \"percentage offset\" of window through buffer.
See `mode-line-percent-position'.")

(setq-default mode-line-position
              '((line-number-mode
                 (column-number-mode
                  (doom-modeline-column-zero-based " %l:%c" " %l:%C")
                  " %l")
                 (column-number-mode (doom-modeline-column-zero-based " :%c" " :%C")))
                (if doom-modeline-percent-position (" " doom-modeline-percent-position))
                (:eval (when (or line-number-mode column-number-mode doom-modeline-percent-position) " "))))

(doom-modeline-def-segment buffer-position
  "The buffer position information."
  '(" " mode-line-position))


;;
;; evil-state
;;

(doom-modeline-def-segment evil-state
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (let ((tag (evil-state-property evil-state :tag t)))
      (propertize tag 'face
                  (if (doom-modeline--active)
                      (cond ((evil-normal-state-p) 'doom-modeline-evil-normal-state)
                            ((evil-emacs-state-p) 'doom-modeline-evil-emacs-state)
                            ((evil-insert-state-p) 'doom-modeline-evil-insert-state)
                            ((evil-motion-state-p) 'doom-modeline-evil-motion-state)
                            ((evil-visual-state-p) 'doom-modeline-evil-visual-state)
                            ((evil-operator-state-p) 'doom-modeline-evil-operator-state)
                            ((evil-replace-state-p) 'doom-modeline-evil-replace-state)))))))


;;
;; god-state
;;

(doom-modeline-def-segment god-state
  "The current god state. Requires `god-mode' to be enabled."
  (when (bound-and-true-p god-local-mode)
    (propertize " <G> " 'face 'doom-modeline-god-state)))


;;
;; input method
;;

(doom-modeline-def-segment input-method
  "The current input method."
  (cond
   (current-input-method
    (concat current-input-method-title "  "))
   ((and (bound-and-true-p evil-local-mode)
         (bound-and-true-p evil-input-method))
    (concat
     (nth 3 (assoc default-input-method input-method-alist))
     "  "))))


;;
;; Mode lines
;;

(doom-modeline-def-modeline 'main
                            '(bar workspace-number window-number evil-state god-state matches " " buffer-info buffer-position " " selection-info)
                            '(global input-method buffer-encoding major-mode process vcs flycheck))

(doom-modeline-def-modeline 'minimal
                            '(bar matches " " buffer-info)
                            '(media-info major-mode))

(doom-modeline-def-modeline 'special
                            '(bar window-number evil-state god-state matches " " buffer-info-simple buffer-position " " selection-info)
                            '(global input-method buffer-encoding major-mode process flycheck))

(doom-modeline-def-modeline 'project
                            '(bar window-number buffer-default-directory)
                            '(global major-mode))

(doom-modeline-def-modeline 'media
                            '(bar window-number " %b  ")
                            '(global media-info major-mode))

;;
;; Hooks
;;

(defun doom-modeline-refresh-bars (&optional width height)
  "Refresh mode-line bars with `WIDTH' and `HEIGHT'."
  (setq doom-modeline--bar-active
        (doom-modeline--make-xpm 'doom-modeline-bar
                                 (or width doom-modeline-bar-width)
                                 (max (or height doom-modeline-height)
                                      (frame-char-height)))
        doom-modeline--bar-inactive
        (doom-modeline--make-xpm 'doom-modeline-inactive-bar
                                 (or width doom-modeline-bar-width)
                                 (max (or height doom-modeline-height)
                                      (frame-char-height)))))

;;;###autoload
(defun doom-modeline-init ()
  "Initialize doom mode-line."
  ;; Create bars
  (doom-modeline-refresh-bars)
  (unless after-init-time
    ;; These buffers are already created and don't get modelines. For the love
    ;; of Emacs, someone give the man a modeline!
    (dolist (bname '("*scratch*" "*Messages*"))
      (with-current-buffer bname
        (doom-modeline-set-modeline 'main)))))

;;;###autoload
(defun doom-modeline-set-special-modeline ()
  "Set sepcial mode-line."
  (doom-modeline-set-modeline 'special))

;;;###autoload
(defun doom-modeline-set-media-modeline ()
  "Set media mode-line."
  (doom-modeline-set-modeline 'media))

;;;###autoload
(defun doom-modeline-set-project-modeline ()
  "Set project mode-line."
  (doom-modeline-set-modeline 'project))

;;
;; Bootstrap
;;

(doom-modeline-set-modeline 'main t) ; set default modeline

(add-hook 'image-mode-hook #'doom-modeline-set-media-modeline)
(add-hook 'org-src-mode-hook #'doom-modeline-set-special-modeline)
(add-hook 'circe-mode-hook #'doom-modeline-set-special-modeline)

;; Versions, support Python, Ruby, Perl and Golang, etc.
(add-hook 'python-mode-hook
          (lambda ()
            (when (and doom-modeline-python-executable (executable-find doom-modeline-python-executable) (executable-find "cut") (executable-find "sed"))
              (setq doom-modeline-env-command (concat doom-modeline-python-executable " --version 2>&1 | cut -d' ' -f2 | sed -n '1p'")))))
(add-hook 'ruby-mode-hook
          (lambda ()
            (when (and (executable-find "ruby") (executable-find "cut") (executable-find "sed"))
              (setq doom-modeline-env-command "ruby --version 2>&1 | cut -d' ' -f2 | sed -n '1p'"))))
(add-hook 'perl-mode-hook
          (lambda ()
            (when (and (executable-find "perl") (executable-find "cut") (executable-find "tr") (executable-find "sed"))
              (setq doom-modeline-env-command "perl --version 2>&1 | cut -d'(' -f2 | cut -d')' -f1 | tr -d 'v' | sed -n '2p'"))))
(add-hook 'go-mode-hook
          (lambda ()
            (when (and (executable-find "go") (executable-find "cut") (executable-find "tr") (executable-find "sed"))
              (setq doom-modeline-env-command "go version 2>&1 | cut -d' ' -f3 | tr -d 'go' | sed -n '1p'"))))
(add-hook 'elixir-mode-hook
          (lambda ()
            (when (and (executable-find "iex") (executable-find "cut") (executable-find "sed"))
              (setq doom-modeline-env-command "iex --version 2>&1 | cut -d' ' -f2 | sed -n '1p'"))))


;; Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(defvar doom-modeline-remap-face-cookie nil)
(defun doom-modeline-focus ()
  "Focus mode-line."
  (when doom-modeline-remap-face-cookie
    (require 'face-remap)
    (face-remap-remove-relative doom-modeline-remap-face-cookie)))
(defun doom-modeline-unfocus ()
  "Unfocus mode-line."
  (setq doom-modeline-remap-face-cookie (face-remap-add-relative 'mode-line 'mode-line-inactive)))

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (defun doom-modeline-focus-change ()
          (if (frame-focus-state)
              (doom-modeline-focus)
            (doom-modeline-unfocus)))
        (add-function :after after-focus-change-function #'doom-modeline-focus-change))
    (progn
      (add-hook 'focus-in-hook #'doom-modeline-focus)
      (add-hook 'focus-out-hook #'doom-modeline-unfocus))))

(provide 'doom-modeline)

;;; doom-modeline.el ends here
