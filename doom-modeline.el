;;; doom-modeline.el --- A minimal and modern mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2023 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline
;; Version: 4.0.0
;; Package-Requires: ((emacs "25.1") (compat "28.1.1.1") (nerd-icons "0.0.1") (shrink-path "0.2.0"))
;; Keywords: faces mode-line

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; This package offers a fancy and fast mode-line inspired by minimalism design.
;;
;; It's integrated into Doom Emacs (https://github.com/hlissner/doom-emacs) and
;; Centaur Emacs (https://github.com/seagle0128/.emacs.d).
;;
;; The doom-modeline offers:
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
;; - An indicator for modal editing state, including evil, overwrite, god, ryo
;;   and xah-fly-keys, etc.
;; - An indicator for battery status
;; - An indicator for current input method
;; - An indicator for debug state
;; - An indicator for remote host
;; - An indicator for LSP state with lsp-mode or eglot
;; - An indicator for github notifications
;; - An indicator for unread emails with mu4e-alert
;; - An indicator for unread emails with gnus (basically builtin)
;; - An indicator for irc notifications with circe, rcirc or erc.
;; - An indicator for buffer position which is compatible with nyan-mode or poke-line
;; - An indicator for party parrot
;; - An indicator for PDF page number with pdf-tools
;; - An indicator for markdown/org previews with grip
;; - Truncated file name, file icon, buffer state and project name in buffer
;;   information segment, which is compatible with project, find-file-in-project
;;   and projectile
;; - New mode-line for Info-mode buffers
;; - New package mode-line for paradox
;; - New mode-line for helm buffers
;; - New mode-line for git-timemachine buffers
;;
;; Installation:
;; From melpa, `M-x package-install RET doom-modeline RET`.
;; In `init.el`,
;; (require 'doom-modeline)
;; (doom-modeline-mode 1)
;; or
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))
;;

;;; Code:

(require 'doom-modeline-core)
(require 'doom-modeline-segments)


;;
;; Mode lines
;;

(doom-modeline-def-modeline 'main
  '(bar workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker time))

(doom-modeline-def-modeline 'minimal
  '(bar matches buffer-info-simple)
  '(media-info major-mode time))

(doom-modeline-def-modeline 'special
  '(bar window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process time))

(doom-modeline-def-modeline 'project
  '(bar window-number modals buffer-default-directory remote-host buffer-position)
  '(compilation misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time))

(doom-modeline-def-modeline 'dashboard
  '(bar window-number buffer-default-directory-simple remote-host)
  '(compilation misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time))

(doom-modeline-def-modeline 'vcs
  '(bar window-number modals matches buffer-info remote-host buffer-position parrot selection-info)
  '(compilation misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process time))

(doom-modeline-def-modeline 'package
  '(bar window-number package)
  '(compilation misc-info major-mode process time))

(doom-modeline-def-modeline 'info
  '(bar window-number buffer-info info-nodes buffer-position parrot selection-info)
  '(compilation misc-info buffer-encoding major-mode time))

(doom-modeline-def-modeline 'media
  '(bar window-number buffer-size buffer-info)
  '(compilation misc-info media-info major-mode process vcs time))

(doom-modeline-def-modeline 'message
  '(bar window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode time))

(doom-modeline-def-modeline 'pdf
  '(bar window-number matches buffer-info pdf-pages)
  '(compilation  misc-info major-mode process vcs time))

(doom-modeline-def-modeline 'org-src
  '(bar window-number modals matches buffer-info buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info debug lsp minor-modes input-method indent-info buffer-encoding major-mode process checker time))

(doom-modeline-def-modeline 'helm
  '(bar helm-buffer-id helm-number helm-follow helm-prefix-argument)
  '(helm-help time))

(doom-modeline-def-modeline 'timemachine
  '(bar window-number modals matches git-timemachine buffer-position word-count parrot selection-info)
  '(misc-info minor-modes indent-info buffer-encoding major-mode time))

(doom-modeline-def-modeline 'calculator
  '(window-number modals matches calc buffer-position)
  '(misc-info minor-modes major-mode process))


;;
;; Interfaces
;;

;;;###autoload
(defun doom-modeline-set-main-modeline (&optional default)
  "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (doom-modeline-set-modeline 'main default))


;;
;; Minor mode
;;

;; Suppress warnings
(defvar 2C-mode-line-format)
(defvar helm-ag-show-status-function)
(declare-function helm-display-mode-line "ext:helm-core")

(defvar doom-modeline-mode-map (make-sparse-keymap))

(defvar doom-modeline-mode-alist
  '((message-mode . message)
    (git-commit-mode . message)
    (magit-mode . vcs)
    (dashboard-mode . dashboard)
    (Info-mode .  info)
    (image-mode . media)
    (pdf-view-mode . pdf)
    (org-src-mode . org-src)
    (paradox-menu-mode . package)
    (xwidget-webkit-mode . minimal)
    (git-timemachine-mode . timemachine)
    (calc-mode . calculator)
    (calc-trail-mode . calculator)
    (circe-mode . special)
    (erc-mode . special)
    (rcirc-mode . special))
  "Alist of major modes and mode-lines.")

(defun doom-modeline-auto-set-modeline ()
  "Set mode-line base on major-mode."
  (catch 'found
    (dolist (x doom-modeline-mode-alist)
      (when (derived-mode-p (car x))
        (doom-modeline-set-modeline (cdr x))
        (throw 'found x)))))

(defun doom-modeline-set-helm-modeline (&rest _) ; To advice helm
  "Set helm mode-line."
  (doom-modeline-set-modeline 'helm))

;;;###autoload
(define-minor-mode doom-modeline-mode
  "Toggle `doom-modeline' on or off."
  :group 'doom-modeline
  :global t
  :lighter nil
  :keymap doom-modeline-mode-map
  (if doom-modeline-mode
      (progn
        (doom-modeline-refresh-bars)        ; Create bars
        (doom-modeline-set-main-modeline t) ; Set default mode-line

        ;; Apply to all existing buffers.
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (unless (doom-modeline-auto-set-modeline)
              (doom-modeline-set-main-modeline))))

        ;; For two-column editing
        (setq 2C-mode-line-format (doom-modeline 'special))

        ;; Automatically set mode-lines
        (add-hook 'after-change-major-mode-hook #'doom-modeline-auto-set-modeline)

        ;; Special handles
        (advice-add #'helm-display-mode-line :after #'doom-modeline-set-helm-modeline)
        (setq helm-ag-show-status-function #'doom-modeline-set-helm-modeline))
    (progn
      ;; Restore mode-line
      (let ((original-format (doom-modeline--original-value 'mode-line-format)))
        (setq-default mode-line-format original-format)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq mode-line-format original-format))))

      ;; For two-column editing
      (setq 2C-mode-line-format (doom-modeline--original-value '2C-mode-line-format))

      ;; Cleanup
      (remove-hook 'after-change-major-mode-hook #'doom-modeline-auto-set-modeline)
      (advice-remove #'helm-display-mode-line #'doom-modeline-set-helm-modeline)
      (setq helm-ag-show-status-function (default-value 'helm-ag-show-status-function)))))

(provide 'doom-modeline)

;;; doom-modeline.el ends here
