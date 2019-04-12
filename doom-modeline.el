;;; doom-modeline.el --- A minimal and modern mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2019 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline
;; Version: 1.9.8
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
;; - An indicator for evil state
;; - An indicator for god state
;; - An indicator for ryo-modal state
;; - An indicator for xah-fly-keys state
;; - An indicator for remote host
;; - An indicator for current input method
;; - An indicator for debug state
;; - An indicator for LSP state with lsp-mode or eglot
;; - An indicator for github notifications
;; - An indicator for unread emails with mu4e-alert
;; - An indicator for irc notifications with circe
;; - An indicator for buffer position which is compatible with nyan-mode
;; - An indicator for party parrot
;; - An indicator for PDF page number
;; - An indicator for battery status with fancy-battery
;; - Truncated file name, file icon, buffer state and project name in buffer
;;   information segment, which is compatible with projectile and project
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
  '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
  '(misc-info persp-name lsp irc mu4e github debug fancy-battery minor-modes input-method buffer-encoding major-mode process vcs checker))

(doom-modeline-def-modeline 'minimal
  '(bar matches " " buffer-info)
  '(media-info major-mode " "))

(doom-modeline-def-modeline 'special
  '(bar window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info-simple buffer-position parrot selection-info)
  '(misc-info lsp irc-buffers debug fancy-battery minor-modes input-method buffer-encoding major-mode process checker))

(doom-modeline-def-modeline 'project
  '(bar " " buffer-default-directory)
  '(misc-info mu4e github debug fancy-battery " " major-mode " "))

(doom-modeline-def-modeline 'media
  '(bar window-number buffer-size buffer-info)
  '(misc-info media-info major-mode vcs " "))

(doom-modeline-def-modeline 'pdf
  '(bar window-number buffer-size buffer-info pdf-pages)
  '(misc-info " " major-mode vcs " "))


;;
;; Interfaces
;;

;;;###autoload
(defun doom-modeline-init ()
  "Initialize doom mode-line."
  (doom-modeline-mode 1))
(make-obsolete 'doom-modeline-init 'doom-modeline-mode "1.6.0")

;;;###autoload
(defun doom-modeline-set-main-modeline (&optional default)
  "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (doom-modeline-set-modeline 'main default))

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

;;;###autoload
(defun doom-modeline-set-pdf-modeline ()
  "Set pdf mode-line."
  (doom-modeline-set-modeline 'pdf))


;;
;; Mode
;;

(defvar doom-modeline--default-mode-line mode-line-format)

;;;###autoload
(define-minor-mode doom-modeline-mode
  "Toggle doom-modeline on or off."
  :group 'doom-modeline
  :global t
  :lighter nil
  (if doom-modeline-mode
      (progn
        (doom-modeline-refresh-bars)    ; create bars
        (doom-modeline-set-main-modeline t) ; set default mode-line
        (unless after-init-time
          ;; These buffers are already created and don't get modelines. For the love
          ;; of Emacs, someone give the man a modeline!
          (dolist (bname '("*scratch*" "*Messages*"))
            (with-current-buffer bname
              (doom-modeline-set-main-modeline))))
        ;; Add hooks
        (add-hook 'magit-mode-hook #'doom-modeline-set-project-modeline)
        (add-hook 'dashboard-mode-hook #'doom-modeline-set-project-modeline)
        (add-hook 'image-mode-hook #'doom-modeline-set-media-modeline)
        (add-hook 'circe-mode-hook #'doom-modeline-set-special-modeline)
        (add-hook 'pdf-tools-enabled-hook #'doom-modeline-set-pdf-modeline))
    (progn
      ;; Restore mode-line
      (setq-default mode-line-format doom-modeline--default-mode-line)
      ;; Remove hooks
      (remove-hook 'magit-mode-hook #'doom-modeline-set-project-modeline)
      (remove-hook 'dashboard-mode-hook #'doom-modeline-set-project-modeline)
      (remove-hook 'image-mode-hook #'doom-modeline-set-media-modeline)
      (remove-hook 'circe-mode-hook #'doom-modeline-set-special-modeline)
      (remove-hook 'pdf-tools-enabled-hook #'doom-modeline-set-pdf-modeline))))

(provide 'doom-modeline)

;;; doom-modeline.el ends here
