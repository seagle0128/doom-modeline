;;; doom-modeline.el --- A minimal and modern mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline
;; Version: 1.5.1
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
;; This package offers a fancy and fast mode-line which was from DOOM Emacs
;; (https://github.com/hlissner/doom-emacs/tree/master/modules/ui/doom-modeline),
;; but it's more powerful and much faster.
;; It's also integrated into Centaur Emacs (https://github.com/seagle0128/.emacs.d).
;;
;; The doom-modeline was designed for minimalism, and offers:
;; - A match count panel (for anzu, iedit, multiple-cursors, symbol-overlay,
;;   evil-search and evil-substitute)
;; - An indicator for recording a macro
;; - Current environment version (e.g. python, ruby, go, etc.) in the major-mode
;; - A customizable mode-line height (see doom-modeline-height)
;; - A minor modes segment which is compatible with minions
;; - An error/warning count segment for flymake/flycheck
;; - A workspace number segment for eyebrowse
;; - A perspective name segment for persp-mode
;; - A window number segment for winum and window-numbering
;; - An indicator for evil state
;; - An indicator for god state
;; - An indicator for ryo-modal state
;; - An indicator for xah-fly-keys state
;; - An indicator for remote host
;; - An indicator for current input method
;; - An indicator for debug state
;; - An indicator for LSP state
;; - An indicator for github notifications
;; - An indicator for buffer position which is compatible with nyan-mode
;; - An indicator for party parrot
;; - Truncated file name, file icon, buffer state and project name in buffer
;;   information segment, which is compatible with projectile and project
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
(require 'doom-modeline-env)


;;
;; Variables
;;

(defvar doom-modeline-height (let ((font (face-font 'mode-line)))
                               (if (and font (fboundp 'font-info))
                                   (* 2 (aref (font-info font) 2))
                                 25))
  "How tall the mode-line should be (only respected in GUI Emacs).")

(defvar doom-modeline-bar-width (if (eq system-type 'darwin) 3 6)
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
  file-name => comint.el
  buffer-name => comint.el<2> (uniquify buffer name)")

(defvar doom-modeline-python-executable "python"
  "What executable of Python will be used (if nil nothing will be showed).")

(defvar doom-modeline-icon (display-graphic-p)
  "Whether show `all-the-icons' or not.

Non-nil to show the icons in mode-line.
The icons may not be showed correctly in terminal and on Windows.")

(defvar doom-modeline-major-mode-icon t
  "Whether show the icon for major mode. It respects `doom-modeline-icon'.")

(defvar doom-modeline-major-mode-color-icon nil
  "Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.")

(defvar doom-modeline-minor-modes nil
  "Whether display minor modes or not. Non-nil to display in mode-line.")

(defvar doom-modeline-persp-name t
  "Whether display perspective name or not. Non-nil to display in mode-line.")

(defvar doom-modeline-lsp t
  "Whether display `lsp' state or not. Non-nil to display in mode-line.")

(defvar doom-modeline-github t
  "Whether display github notifications or not. Requires `ghub' package.")

(defvar doom-modeline-github-interval (* 30 60)
  "The interval of checking github.")

(defvar doom-modeline-version t
  "Whether display environment version or not.")


;;
;; Compatibilities
;;

(unless (>= emacs-major-version 26)
  (with-no-warnings
    ;; Define `if-let*' and `when-let*' variants for 25 users.
    (defalias 'if-let* #'if-let)
    (defalias 'when-let* #'when-let)))

;; Don’t compact font caches during GC.
(if (eq system-type 'windows-nt)
    (setq inhibit-compacting-font-caches t))

;;`file-local-name' is introduced in 25.2.2.
(unless (fboundp 'file-local-name)
  (defun file-local-name (file)
    "Return the local name component of FILE.
It returns a file name which can be used directly as argument of
`process-file', `start-file-process', or `shell-command'."
    (or (file-remote-p file 'localname) file)))


;;
;; Externals
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
(defvar flycheck-mode-menu-map)
(defvar flymake--backend-state)
(defvar flymake--mode-line-format)
(defvar flymake-menu)
(defvar iedit-mode)
(defvar iedit-occurrences-overlays)
(defvar mc/mode-line)
(defvar minions-mode)
(defvar minions-mode-line-lighter)
(defvar symbol-overlay-keywords-alist)
(defvar symbol-overlay-temp-symbol)
(defvar text-scale-mode-amount)
(defvar winum-auto-setup-mode-line)
(defvar xah-fly-insert-state-q)

(declare-function anzu--reset-status 'anzu)
(declare-function anzu--where-is-here 'anzu)
(declare-function async-inject-variables 'async)
(declare-function avy-traverse 'avy)
(declare-function avy-tree 'avy)
(declare-function aw-update 'ace-window)
(declare-function aw-window-list 'ace-window)
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
(declare-function flycheck-list-errors 'flycheck)
(declare-function flycheck-next-error 'flycheck)
(declare-function flycheck-previous-error 'flycheck)
(declare-function flymake--backend-state-diags 'flymake)
(declare-function flymake--diag-type 'flymake)
(declare-function flymake--handle-report 'flymake)
(declare-function flymake-disabled-backends 'flymake)
(declare-function flymake-goto-next-error 'flymake)
(declare-function flymake-goto-prev-error 'flymake)
(declare-function flymake-reporting-backends 'flymake)
(declare-function flymake-running-backends 'flymake)
(declare-function flymake-show-diagnostics-buffer 'flymake)
(declare-function iedit-find-current-occurrence-overlay 'iedit-lib)
(declare-function iedit-prev-occurrence 'iedit-lib)
(declare-function image-get-display-property 'image-mode)
(declare-function lsp-mode-line 'lsp-mode)
(declare-function magit-toplevel 'magit-git)
(declare-function minions-minor-modes-menu 'minions)
(declare-function nyan-create 'nyan-mode)
(declare-function parrot-create 'parrot)
(declare-function persp-add-buffer 'persp-mode)
(declare-function persp-contain-buffer-p 'persp-mode)
(declare-function persp-remove-buffer 'persp-mode)
(declare-function persp-switch 'persp-mode)
(declare-function project-current 'project)
(declare-function project-roots 'project)
(declare-function projectile-project-root 'projectile)
(declare-function symbol-overlay-assoc 'symbol-overlay)
(declare-function symbol-overlay-get-list 'symbol-overlay)
(declare-function symbol-overlay-get-symbol 'symbol-overlay)
(declare-function undo-tree-redo 'undo-tree)
(declare-function undo-tree-undo 'undo-tree)
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

(defface doom-modeline-buffer-minor-mode
  '((t (:inherit (mode-line-buffer-id bold))))
  "Face used for the minor-modes segment in the mode-line.")

(defface doom-modeline-project-root-dir
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the project part of the mode-line buffer path.")

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line.")

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `doom-modeline--anzu',
`doom-modeline--evil-substitute' and`iedit'")

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

(defface doom-modeline-eldoc-bar `((t (:background ,(face-foreground 'success))))
  "The face used for the left-most bar on the mode-line when eldoc-eval is active.")

(defface doom-modeline-inactive-bar `((t (:background ,(face-foreground 'mode-line-inactive))))
  "The face used for the left-most bar on the mode-line of an inactive window.")

(defface doom-modeline-evil-emacs-state '((t (:inherit doom-modeline-warning)))
  "Face for the Emacs state tag in evil state indicator.")

(defface doom-modeline-evil-insert-state '((t (:inherit doom-modeline-urgent)))
  "Face for the insert state tag in evil state indicator.")

(defface doom-modeline-evil-motion-state '((t :inherit doom-modeline-buffer-path))
  "Face for the motion state tag in evil state indicator.")

(defface doom-modeline-evil-normal-state '((t (:inherit doom-modeline-info)))
  "Face for the normal state tag in evil state indicator.")

(defface doom-modeline-evil-operator-state '((t (:inherit doom-modeline-buffer-path)))
  "Face for the operator state tag in evil state indicator.")

(defface doom-modeline-evil-visual-state '((t (:inherit doom-modeline-buffer-file)))
  "Face for the visual state tag in evil state indicator.")

(defface doom-modeline-evil-replace-state '((t (:inherit doom-modeline-buffer-modified)))
  "Face for the replace state tag in evil state indicator.")

(defface doom-modeline-persp-name '((t (:inherit font-lock-comment-face :italic t)))
  "Face for the replace state tag in evil state indicator.")

(defface doom-modeline-persp-buffer-not-in-persp '((t (:inherit font-lock-doc-face :bold t)))
  "Face for the replace state tag in evil state indicator.")


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

(defvar-local doom-modeline-project-root nil)
(defun doom-modeline-project-root ()
  "Get the path to the root of your project.

  Return `default-directory' if no project was found."
  (or doom-modeline-project-root
      (setq doom-modeline-project-root
            (file-local-name
             (or
              (when (featurep 'projectile)
                (ignore-errors (projectile-project-root)))
              (when (featurep 'project)
                (ignore-errors
                  (when-let ((project (project-current)))
                    (expand-file-name (car (project-roots project))))))
              default-directory)))))


;;
;; Plugins
;;

(defun doom-modeline-eldoc (text)
  "Get eldoc TEXT for mode-line."
  (concat (doom-modeline--make-xpm 'doom-modeline-eldoc-bar
                                   doom-modeline-bar-width
                                   doom-modeline-height)
          " "
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
(defvar-local doom-modeline-env-version nil
  "The version to display with major-mode in mode-line.
Example: \"2.6.0\"")
(defvar-local doom-modeline-env-command nil
  "A program that we're looking to extract version information from.
Example: \"ruby\"")
(defvar-local doom-modeline-env-command-args nil
  "A list of arguments to pass to `doom-modeline-env-command` to extract the version from.
Example: '(\"--version\") ")
(defvar-local doom-modeline-env-parser nil
  "A function that returns version number from a programs --version (or similar) command.
Example: 'doom-modeline-env--ruby")

(defun doom-modeline-update-env ()
  "Update environment info on mode-line."
  (when (and doom-modeline-version
             doom-modeline-env-command
             (executable-find doom-modeline-env-command)
             doom-modeline-env-command-args
             doom-modeline-env-parser)
    (let ((default-directory (doom-modeline-project-root)))
      (doom-modeline-env--get doom-modeline-env-command
                              doom-modeline-env-command-args
                              (lambda (prog-version)
                                (setq doom-modeline-env-version
                                      (funcall doom-modeline-env-parser prog-version)))))))

(add-hook 'find-file-hook #'doom-modeline-update-env)
(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda ()
                      (if (frame-focus-state)
                          (doom-modeline-update-env))))
    (add-hook 'focus-in-hook #'doom-modeline-update-env)))


;;
;; Modeline helpers
;;

(defvar doom-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "Text style with icons in mode-line.")

(defun doom-modeline-icon-octicon (&rest args)
  "Display octicon via ARGS."
  (when doom-modeline-icon
    (apply #'all-the-icons-octicon args)))

(defun doom-modeline-icon-faicon (&rest args)
  "Display font awesome icon via ARGS."
  (when doom-modeline-icon
    (apply #'all-the-icons-faicon args)))

(defun doom-modeline-icon-material (&rest args)
  "Display material icon via ARGS."
  (when doom-modeline-icon
    (apply #'all-the-icons-material args)))

(defun doom-modeline-icon-for-mode (&rest args)
  "Display icon for major mode via ARGS."
  (when doom-modeline-icon
    (apply #'all-the-icons-icon-for-mode args)))

(defun doom-modeline-icon-for-file (&rest args)
  "Display icon for major mode via ARGS."
  (when doom-modeline-icon
    (apply #'all-the-icons-icon-for-file args)))

(defun doom-modeline-icon-height (height)
  "Calculate the actual HEIGHT of the icon."
  (* (/ height 1.2) all-the-icons-scale-factor))

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
  (let* ((buffer-file-name (file-local-name (or (buffer-file-name (buffer-base-buffer)) "")))
         (buffer-file-truename (file-local-name (or buffer-file-truename (file-truename buffer-file-name) ""))))
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
       (style
        (propertize
         (pcase style
           (`file-name (file-name-nondirectory buffer-file-name))
           (`buffer-name (buffer-name)))
         'face
         (let ((face (or (and (buffer-modified-p)
                              'doom-modeline-buffer-modified)
                         (and (doom-modeline--active)
                              'doom-modeline-buffer-file))))
           (when face `(:inherit ,face))))))
     'help-echo (concat buffer-file-truename
                        (unless (string= (file-name-nondirectory buffer-file-truename)
                                         (buffer-name))
                          (concat "\n" (buffer-name)))
                        "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
     'local-map mode-line-buffer-identification-keymap)))

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
           (when-let (root-path-parent
                      (file-name-directory (directory-file-name project-root)))
             (propertize
              (if (and truncate-project-root-parent
                       (not (string-empty-p root-path-parent))
                       (not (string= root-path-parent "/")))
                  (shrink-path--dirs-internal root-path-parent t)
                (abbreviate-file-name root-path-parent))
              'face sp-props)))
         ;; project
         (propertize
          (concat (file-name-nondirectory (directory-file-name project-root)) "/")
          'face project-props)
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
(defvar-local doom-modeline--buffer-file-icon nil)
(defun doom-modeline-update-buffer-file-icon (&rest _)
  "Update file icon in mode-line."
  (setq doom-modeline--buffer-file-icon
        (let* ((height (/ all-the-icons-scale-factor 1.3))
               (icon (doom-modeline-icon-for-mode major-mode :height height)))
          (if (symbolp icon)
              (setq icon (doom-modeline-icon-for-file (buffer-name) :height height)))
          (unless (symbolp icon)
            (propertize icon
                        'help-echo (format "Major-mode: %s" mode-name)
                        'display '(raise -0.125))))))
(add-hook 'find-file-hook #'doom-modeline-update-buffer-file-icon)
(add-hook 'after-change-major-mode-hook #'doom-modeline-update-buffer-file-icon)
(add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-file-icon)

(defun doom-modeline-buffer-file-state-icon (icon &optional face height voffset)
  "Displays an ICON with FACE, HEIGHT and VOFFSET.
Uses `all-the-icons-material' to fetch the icon."
  (when icon
    (doom-modeline-icon-material
     icon
     :face (if (doom-modeline--active) face)
     :height (or height 1.1)
     :v-adjust (or voffset -0.225))))

(defvar-local doom-modeline--buffer-file-state-icon nil)
(defun doom-modeline-update-buffer-file-state-icon (&rest _)
  "Update the buffer or file state in mode-line."
  (setq doom-modeline--buffer-file-state-icon
        (cond (buffer-read-only
               (doom-modeline-buffer-file-state-icon
                "lock"
                'doom-modeline-warning))
              ((buffer-modified-p)
               (doom-modeline-buffer-file-state-icon
                "save"
                'doom-modeline-buffer-modified))
              ((and buffer-file-name
                    (not (file-exists-p buffer-file-name)))
               (doom-modeline-buffer-file-state-icon
                "do_not_disturb_alt"
                'doom-modeline-urgent))
              ((buffer-narrowed-p)
               (doom-modeline-buffer-file-state-icon
                "vertical_align_center"
                'doom-modeline-warning)))))
(add-hook 'find-file-hook #'doom-modeline-update-buffer-file-state-icon)
(add-hook 'after-save-hook #'doom-modeline-update-buffer-file-state-icon)
(add-hook 'read-only-mode-hook #'doom-modeline-update-buffer-file-state-icon)
(add-hook 'after-change-functions #'doom-modeline-update-buffer-file-state-icon)
(add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-file-state-icon)
(advice-add #'undo :after #'doom-modeline-update-buffer-file-state-icon)
(advice-add #'undo-tree-undo :after #'doom-modeline-update-buffer-file-state-icon)
(advice-add #'undo-tree-redo :after #'doom-modeline-update-buffer-file-state-icon)
(advice-add #'narrow-to-region :after #'doom-modeline-update-buffer-file-state-icon)
(advice-add #'widen :after #'doom-modeline-update-buffer-file-state-icon)
;; (advice-add #'set-buffer-modified-p :after #'doom-modeline-update-buffer-file-state-icon)

(when (>= emacs-major-version 26)
  (add-variable-watcher
   'buffer-read-only
   (lambda (_sym val op _where)
     (when (eq op 'set)
       (setq buffer-read-only val)
       (doom-modeline-update-buffer-file-state-icon))))

  (add-variable-watcher
   'all-the-icons-scale-factor
   (lambda (_sym val op _where)
     (when (eq op 'set)
       (setq all-the-icons-scale-factor val)
       (doom-modeline-update-buffer-file-icon)
       (doom-modeline-update-buffer-file-state-icon)))))

(defvar-local doom-modeline--buffer-file-name nil)
(defun doom-modeline-update-buffer-file-name (&rest _)
  "Update buffer file name in mode-line."
  (setq doom-modeline--buffer-file-name
        (if buffer-file-name
            (doom-modeline-buffer-file-name)
          (propertize "%b"
                      'face (if (doom-modeline--active) 'doom-modeline-buffer-file)
                      'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                      'local-map mode-line-buffer-identification-keymap))))
(add-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'after-save-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'after-change-functions #'doom-modeline-update-buffer-file-name)
(add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-file-name)
(advice-add #'rename-buffer :after #'doom-modeline-update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'doom-modeline-update-buffer-file-name)
;; (advice-add #'select-window :after #'doom-modeline-update-buffer-file-name)
(advice-add #'undo :after #'doom-modeline-update-buffer-file-name)
(advice-add #'undo-tree-undo :after #'doom-modeline-update-buffer-file-name)
(advice-add #'undo-tree-redo :after #'doom-modeline-update-buffer-file-name)
;; (advice-add #'set-buffer-modified-p :after #'doom-modeline-update-buffer-file-name)

(doom-modeline-def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (let ((active (doom-modeline--active)))
    (concat
     ;; major mode icon
     (when (and doom-modeline-icon doom-modeline-major-mode-icon)
       (when-let ((icon (or doom-modeline--buffer-file-icon
                            (doom-modeline-update-buffer-file-icon))))
         (concat
          (if (and active doom-modeline-major-mode-color-icon)
              icon
            (propertize icon
                        'face `(:height
                                ,(doom-modeline-icon-height 1.1)
                                :family
                                ,(all-the-icons-icon-family icon)
                                :inherit)))
          doom-modeline-vspc)))

     ;; state icon
     (when doom-modeline-icon
       (when-let ((icon (or doom-modeline--buffer-file-state-icon
                            (doom-modeline-update-buffer-file-state-icon))))
         (concat
          (if active
              icon
            (propertize icon
                        'face `(:height
                                ,(doom-modeline-icon-height 1.3)
                                :family
                                ,(all-the-icons-icon-family icon)
                                :inherit)))
          doom-modeline-vspc)))

     ;; buffer file name
     (let ((name (or doom-modeline--buffer-file-name
                     (doom-modeline-update-buffer-file-name))))
       (if active
           name
         (propertize name 'face 'mode-line-inactive))))))

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
  (propertize
   (concat (pcase (coding-system-eol-type buffer-file-coding-system)
             (0 " LF")
             (1 " RLF")
             (2 " CR"))
           (let ((sys (coding-system-plist buffer-file-coding-system)))
             (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                    " UTF-8 ")
                   (t (upcase (symbol-name (plist-get sys :name))))))
           " ")
   'help-echo 'mode-line-mule-info-help-echo
   'mouse-face '(:box 1)
   'local-map mode-line-coding-system-map))


;;
;; remote host
;;

(doom-modeline-def-segment remote-host
  "Hostname for remote buffers."
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (concat "@" host))))


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
           (when (and doom-modeline-version doom-modeline-env-version)
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
;; minor modes
;;

(doom-modeline-def-segment minor-modes
  (when doom-modeline-minor-modes
    (let ((active (doom-modeline--active)))
      (if (bound-and-true-p minions-mode)
          (concat
           " "
           (propertize minions-mode-line-lighter
                       'face (if active 'doom-modeline-buffer-minor-mode)
                       'help-echo "Minions
mouse-1: Display minor modes menu"
                       'mouse-face 'mode-line-highlight
                       'local-map (make-mode-line-mouse-map
                                   'mouse-1 #'minions-minor-modes-menu))
           " ")
        (propertize
         (concat (format-mode-line `(:propertize ("" minor-mode-alist))) " ")
         'face (if active 'doom-modeline-buffer-minor-mode))))))


;;
;; vcs
;;

(defun doom-modeline-vcs-icon (icon &optional text face voffset)
  "Displays the vcs ICON with FACE and VOFFSET.
TEXT is the alternative if it is not applicable.
Uses `all-the-icons-octicon' to fetch the icon."
  (if doom-modeline-icon
      (if icon
          (doom-modeline-icon-octicon icon :face face :v-adjust (or voffset -0.1))
        "")
    (if text
        (propertize text 'face face)
      "")))

(defvar-local doom-modeline--vcs-icon nil)
(defun doom-modeline--update-vcs-icon (&rest _)
  "Update icon of vsc state in mode-line."
  (setq doom-modeline--vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (cond ((memq state '(edited added))
                   (doom-modeline-vcs-icon "git-compare" "*" 'doom-modeline-info -0.05))
                  ((eq state 'needs-merge)
                   (doom-modeline-vcs-icon "git-merge" "?" 'doom-modeline-info))
                  ((eq state 'needs-update)
                   (doom-modeline-vcs-icon "arrow-down" "!" 'doom-modeline-warning))
                  ((memq state '(removed conflict unregistered))
                   (doom-modeline-vcs-icon "alert" "!" 'doom-modeline-urgent))
                  (t
                   (doom-modeline-vcs-icon "git-branch" "@" 'doom-modeline-info -0.05)))))))
(add-hook 'find-file-hook #'doom-modeline--update-vcs-icon t)
(add-hook 'after-save-hook #'doom-modeline--update-vcs-icon)
(advice-add #'vc-refresh-state :after #'doom-modeline--update-vcs-icon)

(defvar-local doom-modeline--vcs-text nil)
(defun doom-modeline--update-vcs-text (&rest _)
  "Update text of vsc state in mode-line."
  (setq doom-modeline--vcs-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        'face (cond ((eq state 'needs-update)
                                     'doom-modeline-warning)
                                    ((memq state '(removed conflict unregistered))
                                     'doom-modeline-urgent)
                                    (t 'doom-modeline-info)))))))
(add-hook 'find-file-hook #'doom-modeline--update-vcs-text t)
(add-hook 'after-save-hook #'doom-modeline--update-vcs-text)
(advice-add #'vc-refresh-state :after #'doom-modeline--update-vcs-text)

(doom-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (let ((active (doom-modeline--active)))
    (when-let ((icon (or doom-modeline--vcs-icon (doom-modeline--update-vcs-icon)))
               (text (or doom-modeline--vcs-text (doom-modeline--update-vcs-text))))
      (concat
       "  "
       (if active
           (concat icon doom-modeline-vspc text)
         (concat
          (propertize icon
                      'face
                      (if doom-modeline-icon
                          `(:height
                            ,(doom-modeline-icon-height 1.2)
                            :family
                            ,(all-the-icons-icon-family icon)
                            :inherit)
                        'mode-line-inactive))
          doom-modeline-vspc
          (propertize text 'face 'mode-line-inactive)))
       " "))))


;;
;; checker
;;

;; flycheck

(defun doom-modeline-checker-icon (icon &optional text face voffset)
  "Displays the checker ICON with FACE and VOFFSET.
TEXT is the alternative if it is not applicable.
Uses `all-the-icons-material' to fetch the icon."
  (if doom-modeline-icon
      (if icon
          (doom-modeline-icon-material icon :face face :height 1.1 :v-adjust (or voffset -0.225))
        "")
    (if text
        (propertize text 'face face)
      "")))

(defun doom-modeline-checker-text (text &optional face)
  "Displays TEXT with FACE."
  (if text
      (propertize text 'face face)
    ""))

(defvar-local doom-modeline--flycheck-icon nil)
(defun doom-modeline-update-flycheck-icon (&optional status)
  "Update flycheck icon via STATUS."
  (setq doom-modeline--flycheck-icon
        (propertize
         (pcase status
           (`finished  (if flycheck-current-errors
                           (let-alist (flycheck-count-errors flycheck-current-errors)
                             (doom-modeline-checker-icon "do_not_disturb_alt" "!"
                                                         (cond (.error 'doom-modeline-urgent)
                                                               (.warning 'doom-modeline-warning)
                                                               (t 'doom-modeline-info))))
                         (doom-modeline-checker-icon "check" "*" 'doom-modeline-info)))
           (`running     (doom-modeline-checker-icon "access_time" "*" 'font-lock-doc-face))
           (`no-checker  (doom-modeline-checker-icon "sim_card_alert" "?" 'font-lock-doc-face))
           (`errored     (doom-modeline-checker-icon "sim_card_alert" "!" 'doom-modeline-urgent))
           (`interrupted (doom-modeline-checker-icon "pause" "!" 'font-lock-doc-face))
           (`suspicious  (doom-modeline-checker-icon "priority_high" "!" 'doom-modeline-urgent))
           (_ ""))
         'help-echo (concat "Flycheck\n"
                            (pcase status
                              ('finished
                               "mouse-1: Display minor mode menu
mouse-2: Show help for minor mode")
                              ('running "Running...")
                              ('no-checker "No Checker")
                              ('errored "Error")
                              ('interrupted "Interrupted")
                              ('suspicious "Suspicious")))
         'mouse-face '(:box 1)
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map [mode-line down-mouse-1]
                        flycheck-mode-menu-map)
                      (define-key map [mode-line mouse-2]
                        (lambda ()
                          (interactive)
                          (describe-function 'flycheck-mode)))
                      map))))
(add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck-icon)
(add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck-icon)

(defvar-local doom-modeline--flycheck-text nil)
(defun doom-modeline-update-flycheck-text (&optional status)
  "Update flycheck text via STATUS."
  (setq doom-modeline--flycheck-text
        (propertize
         (pcase status
           (`finished  (if flycheck-current-errors
                           (let-alist (flycheck-count-errors flycheck-current-errors)
                             (let ((error (or .error 0))
                                   (warning (or .warning 0))
                                   (info (or .info 0)))
                               (format "%s/%s/%s"
                                       (doom-modeline-checker-text (number-to-string error)
                                                                   'doom-modeline-urgent)
                                       (doom-modeline-checker-text (number-to-string warning)
                                                                   'doom-modeline-warning)
                                       (doom-modeline-checker-text (number-to-string info)
                                                                   'doom-modeline-info))))
                         ""))
           (`running     "")
           (`no-checker  (doom-modeline-checker-text "-" 'font-lock-doc-face))
           (`errored     (doom-modeline-checker-text "Error" 'doom-modeline-urgent))
           (`interrupted (doom-modeline-checker-text "Interrupted" 'font-lock-doc-face))
           (`suspicious  (doom-modeline-checker-text "Suspicious" 'doom-modeline-urgent))
           (_ ""))
         'help-echo (pcase status
                      ('finished
                       (concat
                        (if flycheck-current-errors
                            (let-alist (flycheck-count-errors flycheck-current-errors)
                              (format "error: %d, warning: %d, info: %d\n" (or .error 0) (or .warning 0) (or .info 0))))
                        "mouse-1: Show all errors
mouse-3: Next error
wheel-up/wheel-down: Previous/next error"))
                      ('running "Running...")
                      ('no-checker "No Checker")
                      ('errored "Error")
                      ('interrupted "Interrupted")
                      ('suspicious "Suspicious"))
         'mouse-face 'mode-line-highlight
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map [mode-line mouse-1]
                        #'flycheck-list-errors)
                      (define-key map [mode-line mouse-3]
                        #'flycheck-next-error)
                      (define-key map (vector 'mode-line
                                              mouse-wheel-down-event)
                        (lambda (event)
                          (interactive "e")
                          (with-selected-window (posn-window (event-start event))
                            (flycheck-previous-error 1))))
                      (define-key map (vector 'mode-line
                                              mouse-wheel-up-event)
                        (lambda (event)
                          (interactive "e")
                          (with-selected-window (posn-window (event-start event))
                            (flycheck-next-error 1))))
                      map))))
(add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck-text)
(add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck-text)

;; flymake

(defvar-local doom-modeline--flymake-icon nil)
(defun doom-modeline-update-flymake-icon (&rest _)
  "Update flymake icon."
  (setq flymake--mode-line-format nil) ; remove the lighter of minor mode
  (setq doom-modeline--flymake-icon
        (let* ((known (hash-table-keys flymake--backend-state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (diags-by-type (make-hash-table))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported)))
          (maphash (lambda (_b state)
                     (mapc (lambda (diag)
                             (push diag
                                   (gethash (flymake--diag-type diag)
                                            diags-by-type)))
                           (flymake--backend-state-diags state)))
                   flymake--backend-state)
          (propertize
           (cond
            (some-waiting (doom-modeline-checker-icon "access_time" "*" 'font-lock-doc-face))
            ((null known) (doom-modeline-checker-icon "sim_card_alert" "?" 'font-lock-doc-face))
            (all-disabled (doom-modeline-checker-icon "sim_card_alert" "!" 'doom-modeline-urgent))
            (t (let ((.error (length (gethash :error diags-by-type)))
                     (.warning (length (gethash :warning diags-by-type)))
                     (.note (length (gethash :note diags-by-type))))
                 (if (> (+ .error .warning .note) 0)
                     (doom-modeline-checker-icon "do_not_disturb_alt" "!"
                                                 (cond ((> .error 0) 'doom-modeline-urgent)
                                                       ((> .warning 0) 'doom-modeline-warning)
                                                       (t 'doom-modeline-info)))
                   (doom-modeline-checker-icon "check" "*" 'doom-modeline-info)))))
           'help-echo (concat "Flymake\n"
                              (cond
                               (some-waiting "Running...")
                               ((null known) "No Checker")
                               (all-disabled "All Checkers Disabled")
                               (t (format "%d/%d backends running
mouse-1: Display minor mode menu
mouse-2: Show help for minor mode"
                                          (length running) (length known)))))
           'mouse-face '(:box 1)
           'local-map (let ((map (make-sparse-keymap)))
                        (define-key map [mode-line down-mouse-1]
                          flymake-menu)
                        (define-key map [mode-line mouse-2]
                          (lambda ()
                            (interactive)
                            (describe-function 'flymake-mode)))
                        map)))))
(advice-add #'flymake--handle-report :after #'doom-modeline-update-flymake-icon)

(defvar-local doom-modeline--flymake-text nil)
(defun doom-modeline-update-flymake-text (&rest _)
  "Update flymake text."
  (setq flymake--mode-line-format nil) ; remove the lighter of minor mode
  (setq doom-modeline--flymake-text
        (let* ((known (hash-table-keys flymake--backend-state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (diags-by-type (make-hash-table))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported)))
          (maphash (lambda (_b state)
                     (mapc (lambda (diag)
                             (push diag
                                   (gethash (flymake--diag-type diag)
                                            diags-by-type)))
                           (flymake--backend-state-diags state)))
                   flymake--backend-state)
          (let ((.error (length (gethash :error diags-by-type)))
                (.warning (length (gethash :warning diags-by-type)))
                (.note (length (gethash :note diags-by-type))))
            (propertize
             (cond
              (some-waiting "Running..." "")
              ((null known) (doom-modeline-checker-text "-" 'font-lock-doc-face))
              (all-disabled (doom-modeline-checker-text "-" 'doom-modeline-urgent))
              (t (if (> (+ .error .warning .note) 0)
                     (format "%s/%s/%s"
                             (doom-modeline-checker-text (number-to-string .error)
                                                         'doom-modeline-urgent)
                             (doom-modeline-checker-text (number-to-string .warning)
                                                         'doom-modeline-warning)
                             (doom-modeline-checker-text (number-to-string .note)
                                                         'doom-modeline-info))
                   "")))
             'help-echo (cond
                         (some-waiting "Running...")
                         ((null known) "No Checker")
                         (all-disabled "All Checkers Disabled")
                         (t (format "error: %d, warning: %d, note: %d
mouse-1: List all problems
wheel-up/wheel-down: Previous/next problem"
                                    .error .warning .note)))
             'mouse-face 'mode-line-highlight
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line mouse-1]
                            #'flymake-show-diagnostics-buffer)
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-down-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flymake-goto-prev-error 1 nil t))))
                          (define-key map (vector 'mode-line
                                                  mouse-wheel-up-event)
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flymake-goto-next-error 1 nil t))))
                          map))))))
(advice-add #'flymake--handle-report :after #'doom-modeline-update-flymake-text)

(doom-modeline-def-segment checker
  "Displays color-coded error status in the current buffer with pretty
icons."
  (let ((active (doom-modeline--active))
        (seg (cond ((and (bound-and-true-p flymake-mode)
                         (bound-and-true-p flymake--backend-state)) ; only support 26+
                    `(,doom-modeline--flymake-icon . ,doom-modeline--flymake-text))
                   ((bound-and-true-p flycheck-mode)
                    `(,doom-modeline--flycheck-icon . ,doom-modeline--flycheck-text)))))
    (when-let ((icon (car seg))
               (text (cdr seg)))
      (concat
       (if vc-mode " " "  ")
       (if active
           (concat icon doom-modeline-vspc text)
         (concat
          (propertize icon
                      'face
                      (if doom-modeline-icon
                          `(:height
                            ,(doom-modeline-icon-height 1.3)
                            :family
                            ,(all-the-icons-icon-family icon)
                            :inherit)
                        'mode-line-inactive))
          doom-modeline-vspc
          (propertize text 'face 'mode-line-inactive)))
       "  "))))


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

(defsubst doom-modeline--symbol-overlay ()
  "Show the number of matches for symbol overlay."
  (when (and (bound-and-true-p symbol-overlay-keywords-alist)
             (not (bound-and-true-p symbol-overlay-temp-symbol))
             (not (bound-and-true-p iedit-mode)))
    (let* ((keyword (symbol-overlay-assoc
                     (ignore-errors (symbol-overlay-get-symbol))))
           (symbol (car keyword))
           (before (symbol-overlay-get-list symbol 'car))
           (after (symbol-overlay-get-list symbol 'cdr))
           (count (length before)))
      (if (symbol-overlay-assoc symbol)
          (propertize
           (format (concat  " %d/%d " (and (cadr keyword) "in scope "))
                   (+ count 1)
                   (+ count (length after)))
           'face (if (doom-modeline--active) 'doom-modeline-panel))))))

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
current search term (with `anzu'), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions,
5. The current/total for the highlight term (with `symbol-overlay'), 6. The number
of active `multiple-cursors'."
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--anzu)
                      (doom-modeline--evil-substitute)
                      (doom-modeline--iedit)
                      (doom-modeline--symbol-overlay)
                      (doom-modeline--multiple-cursors))))
    (or (and (not (equal meta "")) meta)
        (if (and buffer-file-name size-indication-mode)
            (propertize " %I "
                        'help-echo "Buffer size\n\
mouse-1: Display Line and Column Mode Menu"
                        'mouse-face '(:box 1)
                        'local-map mode-line-column-line-number-mode-map)))))


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
          (lambda ()
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

;; Remove original window number of `ace-window-display-mode'.
(add-hook 'ace-window-display-mode-hook
          (lambda ()
            (setq-default mode-line-format
                          (assq-delete-all 'ace-window-display-mode
                                           (default-value 'mode-line-format)))))

(advice-add #'window-numbering-install-mode-line :override #'ignore)
(advice-add #'window-numbering-clear-mode-line :override #'ignore)
(advice-add #'winum--install-mode-line :override #'ignore)
(advice-add #'winum--clear-mode-line :override #'ignore)

(doom-modeline-def-segment window-number
  (let ((num (cond
              ((bound-and-true-p ace-window-display-mode)
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
  "The current workspace name or number.
Requires `eyebrowse-mode' to be enabled."
  (if (and (bound-and-true-p eyebrowse-mode)
           (< 1 (length (eyebrowse--get 'window-configs))))
      (let* ((num (eyebrowse--get 'current-slot))
             (tag (when num (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
             (str (if (and tag (< 0 (length tag)))
                      tag
                    (when num (int-to-string num)))))
        (assq-delete-all 'eyebrowse-mode mode-line-misc-info)
        (propertize (format " %s " str) 'face
                    (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)))
    ""))


;;
;; perspective name
;;

(defvar-local doom-modeline--persp-name nil)
(defun doom-modeline-update-persp-name (&rest _)
  "Update perspective name in mode-line."
  (setq doom-modeline--persp-name
        ;; Support `persp-mode', while not support `perspective'
        (when (and doom-modeline-persp-name
                   (bound-and-true-p persp-mode)
                   (fboundp 'safe-persp-name)
                   (fboundp 'get-current-persp))
          (let ((persp (get-current-persp)))
            (propertize
             (format " #%s " (safe-persp-name persp))
             'face (if (and persp
                            (not (persp-contain-buffer-p (current-buffer) persp)))
                       'doom-modeline-persp-buffer-not-in-persp
                     'doom-modeline-persp-name)
             'help-echo "mouse-1: Switch perspective
mouse-2: Show help for minor mode"
             'mouse-face '(:box 1)
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line mouse-1]
                            #'persp-switch)
                          (define-key map [mode-line mouse-2]
                            (lambda ()
                              (interactive)
                              (describe-function 'persp-mode)))
                          map))))))

(add-hook 'find-file-hook #'doom-modeline-update-persp-name)
(add-hook 'persp-activated-functions #'doom-modeline-update-persp-name)
(add-hook 'persp-renamed-functions #'doom-modeline-update-persp-name)
(advice-add #'select-window :after #'doom-modeline-update-persp-name)
;; (advice-add #'persp-window-switch :after #'doom-modeline-update-persp-name)
;; (advice-add #'persp-frame-switch :after #'doom-modeline-update-persp-name)
;; (advice-add #'persp-add-buffer :after #'doom-modeline-update-persp-name)
;; (advice-add #'persp-remove-buffer :after #'doom-modeline-update-persp-name)

(doom-modeline-def-segment persp-name
  "The current perspective name."
  (if (doom-modeline--active)
      doom-modeline--persp-name
    ""))


;;
;; misc info
;;

(doom-modeline-def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (if (doom-modeline--active)
      mode-line-misc-info))


;;
;; position
;;

;; Be compatible with Emacs 25.
(defvar doom-modeline-column-zero-based
  (if (boundp 'column-number-indicator-zero-based)
      column-number-indicator-zero-based
    t)
  "When non-nil, mode line displays column numbers zero-based.
See `column-number-indicator-zero-based'.")

(defvar doom-modeline-percent-position
  (if (boundp 'mode-line-percent-position)
      mode-line-percent-position
    '(-3 "%p"))
  "Specification of \"percentage offset\" of window through buffer.
See `mode-line-percent-position'.")

(when (>= emacs-major-version 26)
  (add-variable-watcher
   'column-number-indicator-zero-based
   (lambda (_sym val op _where)
     (when (eq op 'set)
       (setq doom-modeline-column-zero-based val))))

  (add-variable-watcher
   'mode-line-percent-position
   (lambda (_sym val op _where)
     (when (eq op 'set)
       (setq doom-modeline-percent-position val)))))

(doom-modeline-def-segment buffer-position
  "The buffer position information."
  (let ((lc '(line-number-mode
              (column-number-mode
               (doom-modeline-column-zero-based " %l:%c" " %l:%C")
               " %l")
              (column-number-mode (doom-modeline-column-zero-based " :%c" " :%C")))))
    (if (and (bound-and-true-p nyan-mode)
             (doom-modeline--active))
        (concat "  " (nyan-create) " "
                (propertize (format-mode-line lc)
                            'help-echo "Buffer position\n\
mouse-1: Display Line and Column Mode Menu"
                            'mouse-face '(:box 1)
                            'local-map mode-line-column-line-number-mode-map))
      `(:propertize (" "
                     ,lc
                     (if doom-modeline-percent-position (" " doom-modeline-percent-position))
                     (when (or line-number-mode column-number-mode doom-modeline-percent-position) " "))
                    help-echo "Buffer position\n\
mouse-1: Display Line and Column Mode Menu"
                    mouse-face (:box 1)
                    local-map ,mode-line-column-line-number-mode-map))))

;;
;; party parrot
;;
(doom-modeline-def-segment parrot
  "The party parrot animated icon. Requires `parrot-mode' to be enabled."
  (when (and (bound-and-true-p parrot-mode)
             (doom-modeline--active))
    (concat "  " (parrot-create) " ")))

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
    (propertize " <G> " 'face (if (doom-modeline--active)
                                  'doom-modeline-evil-normal-state))))


;;
;; ryo-modal state
;;

(doom-modeline-def-segment ryo-modal ()
  "The current ryo-modal state. Requires `ryo-modal-mode' to be enabled."
  (when (bound-and-true-p ryo-modal-mode)
    (propertize " <R> " 'face (if (doom-modeline--active)
                                  'doom-modeline-evil-normal-state))))


;;
;; xah-fly-keys state
;;

(doom-modeline-def-segment xah-fly-keys ()
  "The current xah-fly-keys state."
  (when (boundp 'xah-fly-insert-state-q)
    (propertize (if xah-fly-insert-state-q
                    " <I> "
                  " <C> ")
                'face (if (doom-modeline--active)
                          'doom-modeline-evil-normal-state))))

;;
;; input method
;;

(doom-modeline-def-segment input-method
  "The current input method."
  (propertize
   (cond
    (current-input-method
     (concat " " current-input-method-title " "))
    ((and (bound-and-true-p evil-local-mode)
          (bound-and-true-p evil-input-method))
     (concat
      " "
      (nth 3 (assoc default-input-method input-method-alist))
      " "))
    (t ""))
   'face (if (doom-modeline--active) 'doom-modeline-buffer-major-mode)
   'help-echo (concat
               "Current input method: "
               current-input-method
               "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
   'mouse-face 'mode-line-highlight
   'local-map mode-line-input-method-map))


;;
;; LSP
;;

(doom-modeline-def-segment lsp
  "The LSP server state."
  (if (and doom-modeline-lsp
           (bound-and-true-p lsp-mode))
      (concat (lsp-mode-line) " ")))


;;
;; github
;;

(defvar doom-modeline--github-notifications-number 0)
(defun doom-modeline--github-fetch-notifications ()
  "Fetch github notifications."
  (when (and doom-modeline-github
             (fboundp 'async-start))
    ;; load `async' if it exists but is not loaded
    (unless (fboundp 'async-inject-variables)
      (require 'async nil t))
    (async-start
     `(lambda ()
        ,(async-inject-variables "\\`load-path\\'")
        (require 'ghub nil t)
        (when (fboundp 'ghub-get)
          (with-timeout (10)
            (ignore-errors
              (if (ghub--token ghub-default-host
                               (ghub--username ghub-default-host)
                               'ghub
                               t)
                  (ghub-get "/notifications"
                            nil
                            :query '((notifications . "true"))
                            :noerror t))))))
     (lambda (result)
       (setq doom-modeline--github-notifications-number
             (length result))))))

(defvar doom-modeline--github-timer nil)
(defun doom-modeline-github-timer ()
  "Start/Stop the timer for github fetching."
  (if (timerp doom-modeline--github-timer)
      (cancel-timer doom-modeline--github-timer))
  (setq doom-modeline--github-timer
        (and doom-modeline-github
             (run-with-timer 30
                             doom-modeline-github-interval
                             #'doom-modeline--github-fetch-notifications))))

(when (>= emacs-major-version 26)
  (add-variable-watcher
   'doom-modeline-github
   (lambda (_sym val op _where)
     (when (eq op 'set)
       (setq doom-modeline-github val)
       (doom-modeline-github-timer)))))

(doom-modeline-github-timer)

(defun doom-modeline--github-open-notifications ()
  "Open GitHub Notifications page."
  (interactive)
  (browse-url "https://github.com/notifications")
  (run-with-timer 60 nil #'doom-modeline--github-fetch-notifications))

(doom-modeline-def-segment github
  "The github notifications."
  (if (and doom-modeline-github
           (doom-modeline--active)
           (> doom-modeline--github-notifications-number 0))
      (propertize
       (concat (if doom-modeline-icon " ")
               (doom-modeline-icon-faicon "github"
                                          :v-adjust -0.0575
                                          :face 'doom-modeline-warning)
               (if doom-modeline-icon doom-modeline-vspc " ")
               (propertize (number-to-string doom-modeline--github-notifications-number)
                           'face 'doom-modeline-warning)
               " ")
       'help-echo "Github
mouse-1: Show notifications
mouse-3: Fetch notifications"
       'mouse-face '(:box 1)
       'local-map (let ((map (make-sparse-keymap)))
                    (define-key map [mode-line mouse-1]
                      #'doom-modeline--github-open-notifications)
                    (define-key map [mode-line mouse-3]
                      (lambda ()
                        (interactive)
                        (message "Fetching github notifications...")
                        (doom-modeline--github-fetch-notifications)))
                    map))))


;;
;; debug state
;;

(doom-modeline-def-segment debug
  "The current debug state."
  (when (doom-modeline--active)
    (concat
     (and (or debug-on-error debug-on-quit) " ")
     (when debug-on-error
       (propertize
        (if doom-modeline-icon
            (doom-modeline-icon-faicon "bug" :v-adjust -0.0575 :face 'doom-modeline-urgent)
          (propertize "!" 'face 'doom-modeline-urgent))
        'help-echo "Debug on Error
mouse-1: Toggle Debug on Error"
        'mouse-face '(:box 1)
        'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-error)))
     (when debug-on-quit
       (propertize
        (if doom-modeline-icon
            (doom-modeline-icon-faicon "bug" :v-adjust -0.0575 :face 'doom-modeline-warning)
          (propertize "!" 'face 'doom-modeline-warning))
        'help-echo "Debug on Quit
mouse-1: Toggle Debug on Quit"
        'mouse-face '(:box 1)
        'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-quit)))
     (and (or debug-on-error debug-on-quit) " "))))


;;
;; Mode lines
;;

(doom-modeline-def-modeline 'main
  '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches " " buffer-info remote-host buffer-position parrot " " selection-info)
  '(misc-info persp-name lsp github debug minor-modes input-method buffer-encoding major-mode process vcs checker))

(doom-modeline-def-modeline 'minimal
  '(bar matches " " buffer-info)
  '(media-info major-mode))

(doom-modeline-def-modeline 'special
  '(bar window-number evil-state god-state ryo-modal xah-fly-keys matches " " buffer-info-simple buffer-position parrot " " selection-info)
  '(misc-info lsp debug minor-modes input-method buffer-encoding major-mode process checker))

(doom-modeline-def-modeline 'project
  '(bar " " buffer-default-directory)
  '(misc-info github debug " " major-mode))

(doom-modeline-def-modeline 'media
  '(bar window-number " %b  ")
  '(misc-info media-info major-mode))


;;
;; Hooks
;;

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
        (doom-modeline-set-main-modeline)))))

;;;###autoload
(defun doom-modeline-set-main-modeline ()
  "Set main mode-line."
  (doom-modeline-set-modeline 'main))

;;;###autoload
(defun doom-modeline-set-minimal-modeline ()
  "Set minimal mode-line."
  (doom-modeline-set-modeline 'minimal))

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

(add-hook 'dashboard-mode-hook #'doom-modeline-set-project-modeline)
(add-hook 'image-mode-hook #'doom-modeline-set-media-modeline)
(add-hook 'circe-mode-hook #'doom-modeline-set-special-modeline)

;; Versions, support Python, Ruby, Perl and Golang, etc.
(add-hook 'python-mode-hook
          (lambda ()
            (cond ((and (fboundp 'pipenv-project-p) (pipenv-project-p) (executable-find "pipenv"))
                   (setq doom-modeline-env-parser 'doom-modeline-env--python)
                   (setq doom-modeline-env-command "pipenv")
                   (setq doom-modeline-env-command-args '("run" "python" "--version")))
                  ((and doom-modeline-python-executable
                        (executable-find doom-modeline-python-executable))
                   (setq doom-modeline-env-parser 'doom-modeline-env--python)
                   (setq doom-modeline-env-command doom-modeline-python-executable)
                   (setq doom-modeline-env-command-args '("--version"))))))
(add-hook 'ruby-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "ruby")
            (setq doom-modeline-env-command-args '("--version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--ruby)))
(add-hook 'perl-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "perl")
            (setq doom-modeline-env-command-args '("--version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--perl)))
(add-hook 'go-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "go")
            (setq doom-modeline-env-command-args '("version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--go)))

(add-hook 'elixir-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "iex")
            (setq doom-modeline-env-command-args '("--version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--elixir)))
(add-hook 'rust-mode-hook
          (lambda ()
            (setq doom-modeline-env-command "rustc")
            (setq doom-modeline-env-command-args '("--version"))
            (setq doom-modeline-env-parser 'doom-modeline-env--rustc)))


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
