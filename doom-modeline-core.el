;;; doom-modeline-core.el --- The core libraries for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Vincent Zhang

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
;; The core libraries for doom-modeline.
;;

;;; Code:

(require 'all-the-icons)
(require 'eldoc-eval)
(require 'shrink-path)
(require 'subr-x)


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

(defvar doom-modeline-icon (display-graphic-p)
  "Whether show `all-the-icons' or not.

Non-nil to show the icons in mode-line.
The icons may not be showed correctly in terminal.")

(defvar doom-modeline-major-mode-icon t
  "Whether show the icon for major mode. It respects `doom-modeline-icon'.")

(defvar doom-modeline-major-mode-color-icon t
  "Display color icons for `major-mode'. It respects `all-the-icons-color-icons'.")

(defvar doom-modeline-minor-modes nil
  "Whether display minor modes or not. Non-nil to display in mode-line.")

(defvar doom-modeline-enable-word-count nil
  "If non-nil, a word count will be added to the selection-info modeline segment.")

(defvar doom-modeline-checker-simple-format t
  "If non-nil, only display one number for checker information if applicable.")

(defvar doom-modeline-vcs-max-length 12
  "The maximum displayed length of the branch name of version control.")

(defvar doom-modeline-persp-name t
  "Whether display perspective name or not. Non-nil to display in mode-line.")

(defvar doom-modeline-lsp t
  "Whether display `lsp' state or not. Non-nil to display in mode-line.")

(defvar doom-modeline-github nil
  "Whether display github notifications or not. Requires `ghub' package.")

(defvar doom-modeline-github-interval (* 30 60)
  "The interval of checking github.")

(defvar doom-modeline-env-version t
  "Whether display environment version or not.")
(define-obsolete-variable-alias 'doom-modeline-version 'doom-modeline-env-version "1.7.4")

(defvar doom-modeline-mu4e t
  "Whether display mu4e notifications or not. Requires `mu4e-alert' package.")

(defvar doom-modeline-irc t
  "Whether display irc notifications or not. Requires `circe' package.")

(defvar doom-modeline-irc-stylize 'identity
  "Function to stylize the irc buffer names.")

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

(defface doom-modeline-project-parent-dir
  '((t (:inherit (font-lock-comment-face bold))))
  "Face used for the project parent directory of the mode-line buffer path.")

(defface doom-modeline-project-dir
  '((t (:inherit (font-lock-string-face bold))))
  "Face used for the project directory of the mode-line buffer path.")

(defface doom-modeline-project-root-dir
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the project part of the mode-line buffer path.")

(defface doom-modeline-highlight
  '((t (:inherit mode-line-emphasis)))
  "Face for bright segments of the mode-line.")

(defface doom-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `anzu', `evil-substitute'
 and`iedit', etc.")

(defface doom-modeline-info
  `((t (:inherit (success bold))))
  "Face for info-level messages in the modeline. Used by `*vc'.")

(defface doom-modeline-warning
  `((t (:inherit (warning bold))))
  "Face for warnings in the modeline. Used by `*flycheck'")

(defface doom-modeline-urgent
  `((t (:inherit (error bold))))
  "Face for errors in the modeline. Used by `*flycheck'")

(defface doom-modeline-unread-number
  `((t (:inherit italic)))
  "Face for unread number in the modeline. Used by `github', `mu4e', etc.")

(defface doom-modeline-bar '((t (:inherit highlight)))
  "The face used for the left-most bar on the mode-line of an active window.")

(defface doom-modeline-eldoc-bar `((t (:background ,(face-foreground 'default))))
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

(defface doom-modeline-persp-name '((t (:inherit (font-lock-comment-face italic))))
  "Face for the replace state tag in evil state indicator.")

(defface doom-modeline-persp-buffer-not-in-persp '((t (:inherit (font-lock-doc-face bold italic))))
  "Face for the replace state tag in evil state indicator.")

;;
;; Externals
;;

(declare-function face-remap-remove-relative 'face-remap)
(declare-function project-roots 'project)
(declare-function projectile-project-root 'projectile)


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
        (list lhs-forms
              (propertize
               " "
               'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
               'display `((space :align-to (- (+ right right-fringe right-margin)
                                              ,(+ 1 (string-width
                                                     (format-mode-line
                                                      (cons "" rhs-forms))))))))
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
  (when-let ((modeline (doom-modeline key)))
    (setf (if default
              (default-value 'mode-line-format)
            (buffer-local-value 'mode-line-format (current-buffer)))
          (list "%e" modeline))))


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

(add-hook 'doom-modeline-mode-hook
          (lambda ()
            (if (bound-and-true-p doom-modeline-mode)
                (progn
                  (eldoc-in-minibuffer-mode 1)
                  (setq eldoc-in-minibuffer-show-fn #'doom-modeline--show-eldoc))
              (progn
                (eldoc-in-minibuffer-mode -1)
                (setq eldoc-in-minibuffer-show-fn #'eldoc-show-in-mode-line)))))

;; Keep `doom-modeline-current-window' up-to-date
(defun doom-modeline--get-current-window ()
  "Get the current window but should exclude the child windows."
  (if (and (fboundp 'frame-parent) (frame-parent))
      (frame-selected-window (frame-parent))
    (frame-selected-window)))

(defvar doom-modeline-current-window (doom-modeline--get-current-window))
(defun doom-modeline-set-selected-window (&rest _)
  "Set `doom-modeline-current-window' appropriately."
  (when-let ((win (doom-modeline--get-current-window)))
    (unless (minibuffer-window-active-p win)
      (setq doom-modeline-current-window win)
      (force-mode-line-update))))

(defun doom-modeline-unset-selected-window ()
  "Unset `doom-modeline-current-window' appropriately."
  (setq doom-modeline-current-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook #'doom-modeline-set-selected-window)
(add-hook 'buffer-list-update-hook #'doom-modeline-set-selected-window)
(advice-add #'handle-switch-frame :after #'doom-modeline-set-selected-window)
(advice-add #'make-frame :after #'doom-modeline-set-selected-window)
(advice-add #'delete-frame :after #'doom-modeline-set-selected-window)
(with-no-warnings
  (cond ((not (boundp 'after-focus-change-function))
         (add-hook 'focus-in-hook #'doom-modeline-set-selected-window)
         (add-hook 'focus-out-hook #'doom-modeline-unset-selected-window))
        ((defun doom-modeline-refresh-frame ()
           (setq doom-modeline-current-window nil)
           (cl-loop for frame in (frame-list)
                    if (eq (frame-focus-state frame) t)
                    return (setq doom-modeline-current-window (frame-selected-window frame)))
           (force-mode-line-update))
         (add-function :after after-focus-change-function #'doom-modeline-refresh-frame))))

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


;;
;; Modeline helpers
;;

(defvar doom-modeline-vspc
  (propertize " " 'face 'variable-pitch)
  "Text style with icons in mode-line.")

(defvar doom-modeline-inactive-vspc
  (propertize " " 'face '(:inherit (variable-pitch mode-line-inactive)))
  "Text style with icons in inactive mode-line.")

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

(defun doom-modeline-icon-alltheicon (&rest args)
  "Display alltheicon via ARGS."
  (when doom-modeline-icon
    (apply #'all-the-icons-alltheicon args)))

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

(defvar-local doom-modeline-project-root nil)
(defun doom-modeline-project-root ()
  "Get the path to the root of your project.
Return `default-directory' if no project was found."
  (or doom-modeline-project-root
      (setq doom-modeline-project-root
            (file-local-name
             (or
              (when (bound-and-true-p projectile-mode)
                (ignore-errors (projectile-project-root)))
              (when (fboundp 'project-current)
                (ignore-errors
                  (when-let ((project (project-current)))
                    (expand-file-name (car (project-roots project))))))
              default-directory)))))

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

;; Fix: invalid-regexp "Trailing backslash" while handling $HOME on Windows
(defun doom-modeline-shrink-path--dirs-internal (full-path &optional truncate-all)
  "Return fish-style truncated string based on FULL-PATH.
Optional parameter TRUNCATE-ALL will cause the function to truncate the last
directory too."
  (let* ((home (expand-file-name "~"))
         (path (replace-regexp-in-string
                (s-concat "^" home) "~" full-path))
         (split (s-split "/" path 'omit-nulls))
         (split-len (length split))
         shrunk)
    (->> split
         (--map-indexed (if (= it-index (1- split-len))
                            (if truncate-all (shrink-path--truncate it) it)
                          (shrink-path--truncate it)))
         (s-join "/")
         (setq shrunk))
    (s-concat (unless (s-matches? (rx bos (or "~" "/")) shrunk) "/")
              shrunk
              (unless (s-ends-with? "/" shrunk) "/"))))
(advice-add #'shrink-path--dirs-internal :override #'doom-modeline-shrink-path--dirs-internal)

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
        (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil nil 'hide))
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
    (let ((sp-faces       (or modified-faces (if active 'doom-modeline-project-parent-dir)))
          (project-faces  (or modified-faces (if active 'doom-modeline-project-dir)))
          (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
          (file-faces     (or modified-faces (if active 'doom-modeline-buffer-file))))
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
            'face sp-faces)))
       ;; project
       (propertize
        (concat (file-name-nondirectory (directory-file-name project-root)) "/")
        'face project-faces)
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
        'face relative-faces)
       ;; file name
       (propertize (file-name-nondirectory file-path) 'face file-faces)))))

(provide 'doom-modeline-core)

;;; doom-modeline-core.el ends here
