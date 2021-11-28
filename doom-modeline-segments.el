;;; doom-modeline-segments.el --- The segments for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2020 Vincent Zhang

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
;; The segments for doom-modeline.
;; Use `doom-modeline-def-segment' to create a new segment.
;;

;;; Code:

(require 'all-the-icons)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'doom-modeline-core)
(require 'doom-modeline-env)


;;
;; Externals
;;

(defvar Info-current-file)
(defvar Info-current-node)
(defvar Info-mode-line-node-keymap)
(defvar anzu--cached-count)
(defvar anzu--current-position)
(defvar anzu--overflow-p)
(defvar anzu--state)
(defvar anzu--total-matched)
(defvar anzu-cons-mode-line-p)
(defvar aw-keys)
(defvar battery-echo-area-format)
(defvar battery-load-critical)
(defvar battery-mode-line-format)
(defvar battery-mode-line-limit)
(defvar battery-status-function)
(defvar boon-command-state)
(defvar boon-insert-state)
(defvar boon-off-state)
(defvar boon-special-state)
(defvar edebug-execution-mode)
(defvar eglot--managed-mode)
(defvar erc-modified-channels-alist)
(defvar evil-ex-active-highlights-alist)
(defvar evil-ex-argument)
(defvar evil-ex-range)
(defvar evil-mc-frozen)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-visual-selection)
(defvar flycheck-current-errors)
(defvar flycheck-mode-menu-map)
(defvar flymake--state)
(defvar flymake--mode-line-format)
(defvar flymake-menu)
(defvar gnus-newsrc-alist)
(defvar gnus-newsrc-hashtb)
(defvar grip--process)
(defvar helm--mode-line-display-prefarg)
(defvar iedit-occurrences-overlays)
(defvar meow--indicator)
(defvar minions-direct)
(defvar minions-mode-line-minor-modes-map)
(defvar mlscroll-minimum-current-width)
(defvar mlscroll-right-align)
(defvar mu4e-alert-mode-line)
(defvar mu4e-alert-modeline-formatter)
(defvar nyan-minimum-window-width)
(defvar objed--obj-state)
(defvar objed--object)
(defvar objed-modeline-setup-func)
(defvar persp-nil-name)
(defvar phi-replace--mode-line-format)
(defvar phi-search--selection)
(defvar phi-search-mode-line-format)
(defvar poke-line-minimum-window-width)
(defvar rcirc-activity)
(defvar sml-modeline-len)
(defvar symbol-overlay-keywords-alist)
(defvar symbol-overlay-temp-symbol)
(defvar text-scale-mode-amount)
(defvar tracking-buffers)
(defvar winum-auto-setup-mode-line)
(defvar xah-fly-insert-state-q)

(declare-function anzu--reset-status 'anzu)
(declare-function anzu--where-is-here 'anzu)
(declare-function async-inject-variables 'async)
(declare-function async-start 'async)
(declare-function avy-traverse 'avy)
(declare-function avy-tree 'avy)
(declare-function aw-update 'ace-window)
(declare-function aw-window-list 'ace-window)
(declare-function battery-format 'battery)
(declare-function battery-update 'battery)
(declare-function boon-modeline-string 'boon)
(declare-function boon-state-string 'boon)
(declare-function cider--connection-info 'cider)
(declare-function cider-connected-p 'cider)
(declare-function cider-current-repl 'cider)
(declare-function cider-jack-in 'cider)
(declare-function cider-quit 'cider)
(declare-function citre-mode 'citre)
(declare-function dap--cur-session 'dap-mode)
(declare-function dap--debug-session-name 'dap-mode)
(declare-function dap--debug-session-state 'dap-mode)
(declare-function dap--session-running 'dap-mode)
(declare-function dap-debug-recent 'dap-mode)
(declare-function dap-disconnect 'dap-mode)
(declare-function dap-hydra 'dap-hydra)
(declare-function edebug-help 'edebug)
(declare-function edebug-next-mode 'edebug)
(declare-function edebug-stop 'edebug)
(declare-function eglot 'eglot)
(declare-function eglot--major-mode 'eglot)
(declare-function eglot--project-nickname 'eglot)
(declare-function eglot--spinner 'eglot)
(declare-function eglot-clear-status 'eglot)
(declare-function eglot-current-server 'eglot)
(declare-function eglot-events-buffer 'eglot)
(declare-function eglot-forget-pending-continuations 'eglot)
(declare-function eglot-reconnect 'eglot)
(declare-function eglot-shutdown 'eglot)
(declare-function eglot-stderr-buffer 'eglot)
(declare-function erc-switch-to-buffer 'erc)
(declare-function erc-track-switch-buffer 'erc-track)
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
(declare-function fancy-narrow-active-p 'fancy-narrow)
(declare-function flycheck-buffer 'flycheck)
(declare-function flycheck-count-errors 'flycheck)
(declare-function flycheck-error-level-compilation-level 'flycheck)
(declare-function flycheck-list-errors 'flycheck)
(declare-function flycheck-next-error 'flycheck)
(declare-function flycheck-previous-error 'flycheck)
(declare-function flymake--state-diags 'flymake)
(declare-function flymake--diag-type 'flymake)
(declare-function flymake--handle-report 'flymake)
(declare-function flymake--lookup-type-property 'flymake)
(declare-function flymake-disabled-backends 'flymake)
(declare-function flymake-goto-next-error 'flymake)
(declare-function flymake-goto-prev-error 'flymake)
(declare-function flymake-reporting-backends 'flymake)
(declare-function flymake-running-backends 'flymake)
(declare-function flymake-show-diagnostics-buffer 'flymake)
(declare-function flymake-start 'flymake)
(declare-function gnus-demon-add-handler 'gnus-demon)
(declare-function grip--preview-url 'grip-mode)
(declare-function grip-browse-preview 'grip-mode)
(declare-function grip-restart-preview 'grip-mode)
(declare-function grip-stop-preview 'grip-mode)
(declare-function helm-candidate-number-at-point 'helm)
(declare-function helm-get-candidate-number 'helm)
(declare-function iedit-find-current-occurrence-overlay 'iedit-lib)
(declare-function iedit-prev-occurrence 'iedit-lib)
(declare-function image-get-display-property 'image-mode)
(declare-function jsonrpc--request-continuations 'jsonrpc)
(declare-function jsonrpc-last-error 'jsonrpc)
(declare-function lsp--workspace-print 'lsp-mode)
(declare-function lsp-describe-session 'lsp-mode)
(declare-function lsp-workspace-folders-open 'lsp-mode)
(declare-function lsp-workspace-restart 'lsp-mode)
(declare-function lsp-workspace-shutdown 'lsp-mode)
(declare-function lsp-workspaces 'lsp-mode)
(declare-function lv-message 'lv)
(declare-function mc/num-cursors 'multiple-cursors-core)
(declare-function mlscroll-mode-line 'mlscroll)
(declare-function mu4e-alert-default-mode-line-formatter 'mu4e-alert)
(declare-function mu4e-alert-enable-mode-line-display 'mu4e-alert)
(declare-function nyan-create 'nyan-mode)
(declare-function org-edit-src-save 'org-src)
(declare-function parrot-create 'parrot)
(declare-function pdf-cache-number-of-pages 'pdf-cache)
(declare-function persp-add-buffer 'persp-mode)
(declare-function persp-contain-buffer-p 'persp-mode)
(declare-function persp-switch 'persp-mode)
(declare-function phi-search--initialize 'phi-search)
(declare-function poke-line-create 'poke-line)
(declare-function popup-create 'popup)
(declare-function popup-delete 'popup)
(declare-function rcirc-next-active-buffer 'rcirc)
(declare-function rcirc-short-buffer-name 'rcirc)
(declare-function rcirc-switch-to-server-buffer 'rcirc)
(declare-function rcirc-window-configuration-change 'rcirc)
(declare-function rime--should-enable-p 'rime)
(declare-function rime--should-inline-ascii-p 'rime)
(declare-function sml-modeline-create 'sml-modeline)
(declare-function symbol-overlay-assoc 'symbol-overlay)
(declare-function symbol-overlay-get-list 'symbol-overlay)
(declare-function symbol-overlay-get-symbol 'symbol-overlay)
(declare-function symbol-overlay-rename 'symbol-overlay)
(declare-function tab-bar--current-tab 'tab-bar)
(declare-function tab-bar--current-tab-index 'tab-bar)
(declare-function tracking-next-buffer 'tracking)
(declare-function tracking-previous-buffer 'tracking)
(declare-function tracking-shorten 'tracking)
(declare-function undo-tree-redo-1 'undo-tree)
(declare-function undo-tree-undo-1 'undo-tree)
(declare-function warning-numeric-level 'warnings)
(declare-function window-numbering-clear-mode-line 'window-numbering)
(declare-function window-numbering-get-number-string 'window-numbering)
(declare-function window-numbering-install-mode-line 'window-numbering)
(declare-function winum--clear-mode-line 'winum)
(declare-function winum--install-mode-line 'winum)
(declare-function winum-get-number-string 'winum)



;;
;; Buffer information
;;

(defvar-local doom-modeline--buffer-file-icon nil)
(defun doom-modeline-update-buffer-file-icon (&rest _)
  "Update file icon in mode-line."
  (setq doom-modeline--buffer-file-icon
        (when (and (display-graphic-p)
                   doom-modeline-icon
                   doom-modeline-major-mode-icon)
          (let ((icon (all-the-icons-icon-for-buffer)))
            (propertize (if (or (null icon) (symbolp icon))
                            (doom-modeline-icon 'faicon "file-o" nil nil
                                                :face 'all-the-icons-dsilver
                                                :height 0.9
                                                :v-adjust 0.0)
                          icon)
                        'help-echo (format "Major-mode: %s" (format-mode-line mode-name))
                        'display '(raise -0.135))))))
(add-hook 'find-file-hook #'doom-modeline-update-buffer-file-icon)
(add-hook 'after-change-major-mode-hook #'doom-modeline-update-buffer-file-icon)
(add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-file-icon)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-buffer-file-icon))))))

(defun doom-modeline-buffer-file-state-icon (icon unicode text face)
  "Displays an ICON of buffer state with FACE.
UNICODE and TEXT are the alternatives if it is not applicable.
Uses `all-the-icons-material' to fetch the icon."
  (doom-modeline-icon 'material icon unicode text
                      :face face
                      :height  1.1
                      :v-adjust -0.225))

(defvar-local doom-modeline--buffer-file-state-icon nil)
(defun doom-modeline-update-buffer-file-state-icon (&rest _)
  "Update the buffer or file state in mode-line."
  (setq doom-modeline--buffer-file-state-icon
        (when doom-modeline-buffer-state-icon
          (ignore-errors
            (concat
             (cond (buffer-read-only
                    (doom-modeline-buffer-file-state-icon
                     "lock" "🔒" "%1*" `(:inherit doom-modeline-warning
                                         :weight ,(if doom-modeline-icon
                                                      'normal
                                                    'bold))))
                   ((and buffer-file-name (buffer-modified-p)
                         doom-modeline-buffer-modification-icon)
                    (doom-modeline-buffer-file-state-icon
                     "save" "💾" "%1*" `(:inherit doom-modeline-buffer-modified
                                         :weight ,(if doom-modeline-icon
                                                      'normal
                                                    'bold))))
                   ((and buffer-file-name
                         (not (file-remote-p buffer-file-name)) ; Avoid freezing while connection is lost
                         (not (file-exists-p buffer-file-name)))
                    (doom-modeline-buffer-file-state-icon
                     "do_not_disturb_alt" "🚫" "!" 'doom-modeline-urgent))
                   (t ""))
             (when (or (buffer-narrowed-p)
                       (and (bound-and-true-p fancy-narrow-mode)
                            (fancy-narrow-active-p))
                       (bound-and-true-p dired-narrow-mode))
               (doom-modeline-buffer-file-state-icon
                "vertical_align_center" "↕" "><" 'doom-modeline-warning)))))))

(defvar-local doom-modeline--buffer-file-name nil)
(defun doom-modeline-update-buffer-file-name (&rest _)
  "Update buffer file name in mode-line."
  (setq doom-modeline--buffer-file-name
        (ignore-errors
          (save-match-data
            (if buffer-file-name
                (doom-modeline-buffer-file-name)
              (propertize "%b"
                          'face 'doom-modeline-buffer-file
                          'mouse-face 'mode-line-highlight
                          'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                          'local-map mode-line-buffer-identification-keymap))))))
(add-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'after-save-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'evil-insert-state-exit-hook #'doom-modeline-update-buffer-file-name)
(advice-add #'not-modified :after #'doom-modeline-update-buffer-file-name)
(advice-add #'rename-buffer :after #'doom-modeline-update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'doom-modeline-update-buffer-file-name)
(advice-add #'pop-to-buffer :after #'doom-modeline-update-buffer-file-name)
(advice-add #'undo :after #'doom-modeline-update-buffer-file-name)
(advice-add #'undo-tree-undo-1 :after #'doom-modeline-update-buffer-file-name)
(advice-add #'undo-tree-redo-1 :after #'doom-modeline-update-buffer-file-name)
(advice-add #'fill-paragraph :after #'doom-modeline-update-buffer-file-name)
(advice-add #'popup-create :after #'doom-modeline-update-buffer-file-name)
(advice-add #'popup-delete :after #'doom-modeline-update-buffer-file-name)
(advice-add #'org-edit-src-save :after #'doom-modeline-update-buffer-file-name)
(advice-add #'symbol-overlay-rename :after #'doom-modeline-update-buffer-file-name)

(doom-modeline-add-variable-watcher
 'doom-modeline-buffer-file-name-style
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-buffer-file-name-style val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when buffer-file-name
           (doom-modeline-update-buffer-file-name)))))))

(defsubst doom-modeline--buffer-mode-icon ()
  "The icon of the current major mode."
  (when (and doom-modeline-icon doom-modeline-major-mode-icon)
    (when-let ((icon (or doom-modeline--buffer-file-icon
                         (doom-modeline-update-buffer-file-icon))))
      (concat
       (let ((active (doom-modeline--active)))
         (if (and active doom-modeline-major-mode-color-icon)
             icon
           (doom-modeline-propertize-icon icon (if active
                                                   'mode-line
                                                 'mode-line-inactive))))
       (doom-modeline-vspc)))))

(defsubst doom-modeline--buffer-state-icon ()
  "The icon of the current buffer state."
  (when doom-modeline-buffer-state-icon
    (when-let ((icon (doom-modeline-update-buffer-file-state-icon)))
      (concat
       (if (doom-modeline--active)
           icon
         (doom-modeline-propertize-icon icon 'mode-line-inactive))
       (doom-modeline-vspc)))))

(defsubst doom-modeline--buffer-name ()
  "The current buffer name."
  ;; Only display the buffer name if the window is small, but doesn't need to
  ;; respect file-name style.
  (if (and (not (eq doom-modeline-buffer-file-name-style 'file-name))
           doom-modeline--limited-width-p)
      (propertize "%b"
                  'face (cond ((and buffer-file-name (buffer-modified-p))
                               'doom-modeline-buffer-modified)
                              ((doom-modeline--active) 'doom-modeline-buffer-file)
                              (t 'mode-line-inactive))
                  'mouse-face 'mode-line-highlight
                  'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                  'local-map mode-line-buffer-identification-keymap)
    (when-let ((name (or doom-modeline--buffer-file-name
                         (doom-modeline-update-buffer-file-name))))
      (if (doom-modeline--active)
          ;; Check if the buffer is modified
          (if (and buffer-file-name (buffer-modified-p))
              (propertize name 'face 'doom-modeline-buffer-modified)
            name)
        (propertize name 'face 'mode-line-inactive)))))

(doom-modeline-def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-mode-icon)
   (doom-modeline--buffer-state-icon)
   (doom-modeline--buffer-name)))

(doom-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-mode-icon)
   (doom-modeline--buffer-state-icon)
   (propertize "%b"
               'face (cond ((and buffer-file-name (buffer-modified-p))
                            'doom-modeline-buffer-modified)
                           ((doom-modeline--active) 'doom-modeline-buffer-file)
                           (t 'mode-line-inactive))
               'mouse-face 'mode-line-highlight
               'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
               'local-map mode-line-buffer-identification-keymap)))

(doom-modeline-def-segment buffer-default-directory
  "Displays `default-directory' with the icon and state . This is for special
buffers like the scratch buffer where knowing the current project directory is
important."
  (let ((face (cond ((buffer-modified-p)
                     'doom-modeline-buffer-modified)
                    ((doom-modeline--active) 'doom-modeline-buffer-path)
                    (t 'mode-line-inactive))))
    (concat (doom-modeline-spc)
            (and doom-modeline-major-mode-icon
                 (concat (doom-modeline-icon
                          'octicon "file-directory" "🖿" ""
                          :face face :v-adjust -0.05 :height 1.25)
                         (doom-modeline-vspc)))
            (doom-modeline--buffer-state-icon)
            (propertize (abbreviate-file-name default-directory) 'face face))))

(doom-modeline-def-segment buffer-default-directory-simple
  "Displays `default-directory'. This is for special buffers like the scratch
buffer where knowing the current project directory is important."
  (let ((face (if (doom-modeline--active) 'doom-modeline-buffer-path 'mode-line-inactive)))
    (concat (doom-modeline-spc)
            (and doom-modeline-major-mode-icon
                 (concat (doom-modeline-icon
                          'octicon "file-directory" "🖿" ""
                          :face face :v-adjust -0.05 :height 1.25)
                         (doom-modeline-vspc)))
            (propertize (abbreviate-file-name default-directory) 'face face))))


;;
;; Encoding
;;

(doom-modeline-def-segment buffer-encoding
  "Displays the eol and the encoding style of the buffer the same way Atom does."
  (when doom-modeline-buffer-encoding
    (let ((face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))
          (mouse-face 'mode-line-highlight))
      (concat
       (doom-modeline-spc)

       ;; eol type
       (let ((eol (coding-system-eol-type buffer-file-coding-system)))
         (when (or (eq doom-modeline-buffer-encoding t)
                   (and (eq doom-modeline-buffer-encoding 'nondefault)
                        (not (equal eol doom-modeline-default-eol-type))))
           (propertize
            (pcase eol
              (0 "LF ")
              (1 "CRLF ")
              (2 "CR ")
              (_ ""))
            'face face
            'mouse-face mouse-face
            'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
                               (pcase eol
                                 (0 "Unix-style LF")
                                 (1 "DOS-style CRLF")
                                 (2 "Mac-style CR")
                                 (_ "Undecided")))
            'local-map (let ((map (make-sparse-keymap)))
                         (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                         map))))

       ;; coding system
       (let* ((sys (coding-system-plist buffer-file-coding-system))
              (cat (plist-get sys :category))
              (sym (if (memq cat
                             '(coding-category-undecided coding-category-utf-8))
                       'utf-8
                     (plist-get sys :name))))
         (when (or (eq doom-modeline-buffer-encoding t)
                   (and (eq doom-modeline-buffer-encoding 'nondefault)
                        (not (eq cat 'coding-category-undecided))
                        (not (eq sym doom-modeline-default-coding-system))))
           (propertize
            (upcase (symbol-name sym))
            'face face
            'mouse-face mouse-face
            'help-echo 'mode-line-mule-info-help-echo
            'local-map mode-line-coding-system-map)))

       (doom-modeline-spc)))))


;;
;; Indentation
;;

(doom-modeline-def-segment indent-info
  "Displays the indentation information."
  (when doom-modeline-indent-info
    (let ((do-propertize
           (lambda (mode size)
             (propertize
              (format " %s %d " mode size)
              'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)))))
      (if indent-tabs-mode
          (funcall do-propertize "TAB" tab-width)
        (let ((lookup-var
               (seq-find (lambda (var)
                           (and var (boundp var) (symbol-value var)))
                         (cdr (assoc major-mode doom-modeline-indent-alist)) nil)))
          (funcall do-propertize "SPC"
                   (if lookup-var
                       (symbol-value lookup-var)
                     tab-width)))))))

;;
;; Remote host
;;

(doom-modeline-def-segment remote-host
  "Hostname for remote buffers."
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (propertize
       (concat "@" host)
       'face (if (doom-modeline--active) 'doom-modeline-host 'mode-line-inactive)))))


;;
;; Major mode
;;

(doom-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (propertize
   (concat
    (doom-modeline-spc)
    (propertize (format-mode-line
                 (or (and (boundp 'delighted-modes)
                          (cadr (assq major-mode delighted-modes)))
                     mode-name))
                'help-echo "Major mode\n\
  mouse-1: Display major mode menu\n\
  mouse-2: Show help for major mode\n\
  mouse-3: Toggle minor modes"
                'mouse-face 'mode-line-highlight
                'local-map mode-line-major-mode-keymap)
    (when (and doom-modeline-env-version doom-modeline-env--version)
      (format " %s" doom-modeline-env--version))
    (and (boundp 'text-scale-mode-amount)
         (/= text-scale-mode-amount 0)
         (format
          (if (> text-scale-mode-amount 0)
              " (%+d)"
            " (%-d)")
          text-scale-mode-amount))
    (doom-modeline-spc))
   'face (if (doom-modeline--active)
             'doom-modeline-buffer-major-mode
           'mode-line-inactive)))


;;
;; Process
;;

(doom-modeline-def-segment process
  "The process info."
  (if (doom-modeline--active)
      mode-line-process
    (propertize (format-mode-line mode-line-process)
                'face 'mode-line-inactive)))


;;
;; Minor modes
;;

(doom-modeline-def-segment minor-modes
  (when doom-modeline-minor-modes
    (let ((face (if (doom-modeline--active)
                    'doom-modeline-buffer-minor-mode
                  'mode-line-inactive))
          (mouse-face 'mode-line-highlight)
          (help-echo "Minor mode
  mouse-1: Display minor mode menu
  mouse-2: Show help for minor mode
  mouse-3: Toggle minor modes"))
      (if (bound-and-true-p minions-mode)
          `((:propertize ("" ,(--filter (memq (car it) minions-direct)
                                        minor-mode-alist))
             face ,face
		     mouse-face ,mouse-face
		     help-echo ,help-echo
		     local-map ,mode-line-minor-mode-keymap)
            ,(doom-modeline-spc)
            (:propertize ("" ,(doom-modeline-icon 'octicon "gear" "⚙" ";-"
  :face face :v-adjust -0.05))
             mouse-face ,mouse-face
             help-echo "Minions
mouse-1: Display minor modes menu"
             local-map ,minions-mode-line-minor-modes-map)
            ,(doom-modeline-spc))
        `((:propertize ("" minor-mode-alist)
           face ,face
           mouse-face ,mouse-face
           help-echo ,help-echo
           local-map ,mode-line-minor-mode-keymap)
          ,(doom-modeline-spc))))))


;;
;; VCS
;;

(defun doom-modeline-vcs-icon (icon &optional unicode text face voffset)
  "Displays the vcs ICON with FACE and VOFFSET.

UNICODE and TEXT are fallbacks.
Uses `all-the-icons-octicon' to fetch the icon."
  (doom-modeline-icon 'octicon icon unicode text
                      :face face :v-adjust (or voffset -0.1)))

(defvar-local doom-modeline--vcs-icon nil)
(defun doom-modeline-update-vcs-icon (&rest _)
  "Update icon of vcs state in mode-line."
  (setq doom-modeline--vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state (file-local-name buffer-file-name) backend)))
            (cond ((memq state '(edited added))
                   (doom-modeline-vcs-icon "git-compare" "⇆" "*" 'doom-modeline-info -0.05))
                  ((eq state 'needs-merge)
                   (doom-modeline-vcs-icon "git-merge" "⛙" "?" 'doom-modeline-info))
                  ((eq state 'needs-update)
                   (doom-modeline-vcs-icon "arrow-down" "↓" "!" 'doom-modeline-warning))
                  ((memq state '(removed conflict unregistered))
                   (doom-modeline-vcs-icon "alert" "⚠" "!" 'doom-modeline-urgent))
                  (t
                   (doom-modeline-vcs-icon "git-branch" "" "@" 'doom-modeline-info -0.05)))))))
(add-hook 'find-file-hook #'doom-modeline-update-vcs-icon)
(add-hook 'after-save-hook #'doom-modeline-update-vcs-icon)
(advice-add #'vc-refresh-state :after #'doom-modeline-update-vcs-icon)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs-icon))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs-icon))))))

(defvar-local doom-modeline--vcs-text nil)
(defun doom-modeline-update-vcs-text (&rest _)
  "Update text of vcs state in mode-line."
  (setq doom-modeline--vcs-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state (file-local-name buffer-file-name) backend))
                 (str (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        "")))
            (propertize (if (> (length str) doom-modeline-vcs-max-length)
                            (concat
                             (substring str 0 (- doom-modeline-vcs-max-length 3))
                             "...")
                          str)
                        'mouse-face 'mode-line-highlight
                        'face (cond ((eq state 'needs-update)
                                     'doom-modeline-warning)
                                    ((memq state '(removed conflict unregistered))
                                     'doom-modeline-urgent)
                                    (t 'doom-modeline-info)))))))
(add-hook 'find-file-hook #'doom-modeline-update-vcs-text)
(add-hook 'after-save-hook #'doom-modeline-update-vcs-text)
(advice-add #'vc-refresh-state :after #'doom-modeline-update-vcs-text)

(doom-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (let ((active (doom-modeline--active)))
    (when-let ((icon doom-modeline--vcs-icon)
               (text doom-modeline--vcs-text))
      (concat
       (doom-modeline-spc)
       (propertize
        (concat
         (if active
             icon
           (doom-modeline-propertize-icon icon 'mode-line-inactive))
         (doom-modeline-vspc))
        'mouse-face 'mode-line-highlight
        'help-echo (get-text-property 1 'help-echo vc-mode)
        'local-map (get-text-property 1 'local-map vc-mode))
       (if active
           text
         (propertize text 'face 'mode-line-inactive))
       (doom-modeline-spc)))))


;;
;; Checker
;;

(defun doom-modeline-checker-icon (icon unicode text face)
  "Displays the checker ICON with FACE.

UNICODE and TEXT are fallbacks.
Uses `all-the-icons-material' to fetch the icon."
  (doom-modeline-icon 'material icon unicode text
                      :face face :height 1.1 :v-adjust -0.225))

(defun doom-modeline-checker-text (text &optional face)
  "Displays TEXT with FACE."
  (propertize text 'face (or face 'mode-line)))

;; Flycheck

(defun doom-modeline--flycheck-count-errors ()
  "Count the number of ERRORS, grouped by level.

Return an alist, where each ITEM is a cons cell whose `car' is an
error level, and whose `cdr' is the number of errors of that
level."
  (let ((info 0) (warning 0) (error 0))
    (mapc
     (lambda (item)
       (let ((count (cdr item)))
         (pcase (flycheck-error-level-compilation-level (car item))
           (0 (cl-incf info count))
           (1 (cl-incf warning count))
           (2 (cl-incf error count)))))
     (flycheck-count-errors flycheck-current-errors))
    `((info . ,info) (warning . ,warning) (error . ,error))))

(defvar-local doom-modeline--flycheck-icon nil)
(defun doom-modeline-update-flycheck-icon (&optional status)
  "Update flycheck icon via STATUS."
  (setq doom-modeline--flycheck-icon
        (when-let
            ((icon
              (pcase status
                ('finished  (if flycheck-current-errors
                                (let-alist (doom-modeline--flycheck-count-errors)
                                  (doom-modeline-checker-icon
                                   "block" "🚫" "!"
                                   (cond ((> .error 0) 'doom-modeline-urgent)
                                         ((> .warning 0) 'doom-modeline-warning)
                                         (t 'doom-modeline-info))))
                              (doom-modeline-checker-icon "check" "✓" "-" 'doom-modeline-info)))
                ('running     (doom-modeline-checker-icon "access_time" "⏱" "*" 'doom-modeline-debug))
                ('no-checker  (doom-modeline-checker-icon "sim_card_alert" "⚠" "-" 'doom-modeline-debug))
                ('errored     (doom-modeline-checker-icon "sim_card_alert" "⚠" "-" 'doom-modeline-urgent))
                ('interrupted (doom-modeline-checker-icon "pause" "⏸" "=" 'doom-modeline-debug))
                ('suspicious  (doom-modeline-checker-icon "priority_high" "❗" "!" 'doom-modeline-urgent))
                (_ nil))))
          (propertize icon
                      'help-echo (concat "Flycheck\n"
                                         (pcase status
                                           ('finished "mouse-1: Display minor mode menu
mouse-2: Show help for minor mode")
                                           ('running "Running...")
                                           ('no-checker "No Checker")
                                           ('errored "Error")
                                           ('interrupted "Interrupted")
                                           ('suspicious "Suspicious")))
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line down-mouse-1]
                                     flycheck-mode-menu-map)
                                   (define-key map [mode-line mouse-2]
                                     (lambda ()
                                       (interactive)
                                       (describe-function 'flycheck-mode)))
                                   map)))))
(add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck-icon)
(add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck-icon)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flycheck-mode)
           (flycheck-buffer)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flycheck-mode)
           (flycheck-buffer)))))))

(defvar-local doom-modeline--flycheck-text nil)
(defun doom-modeline-update-flycheck-text (&optional status)
  "Update flycheck text via STATUS."
  (setq doom-modeline--flycheck-text
        (when-let
            ((text
              (pcase status
                ('finished  (when flycheck-current-errors
                              (let-alist (doom-modeline--flycheck-count-errors)
                                (if doom-modeline-checker-simple-format
                                    (doom-modeline-checker-text
                                     (number-to-string (+ .error .warning .info))
                                     (cond ((> .error 0) 'doom-modeline-urgent)
                                           ((> .warning 0) 'doom-modeline-warning)
                                           (t 'doom-modeline-info)))
                                  (format "%s/%s/%s"
                                          (doom-modeline-checker-text (number-to-string .error)
                                                                      'doom-modeline-urgent)
                                          (doom-modeline-checker-text (number-to-string .warning)
                                                                      'doom-modeline-warning)
                                          (doom-modeline-checker-text (number-to-string .info)
                                                                      'doom-modeline-info))))))
                ('running     nil)
                ('no-checker  nil)
                ('errored     (doom-modeline-checker-text "Error" 'doom-modeline-urgent))
                ('interrupted (doom-modeline-checker-text "Interrupted" 'doom-modeline-debug))
                ('suspicious  (doom-modeline-checker-text "Suspicious" 'doom-modeline-urgent))
                (_ nil))))
          (propertize
           text
           'help-echo (pcase status
                        ('finished
                         (concat
                          (when flycheck-current-errors
                            (let-alist (doom-modeline--flycheck-count-errors)
                              (format "error: %d, warning: %d, info: %d\n" .error .warning .info)))
                          "mouse-1: Show all errors
mouse-3: Next error"
                          (if (featurep 'mwheel)
                              "\nwheel-up/wheel-down: Previous/next error")))
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
                        (when (featurep 'mwheel)
                          (define-key map [mode-line mouse-wheel-down-event]
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-previous-error 1))))
                          (define-key map [mode-line mouse-wheel-up-event]
                            (lambda (event)
                              (interactive "e")
                              (with-selected-window (posn-window (event-start event))
                                (flycheck-next-error 1))))
                          map))))))
(add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck-text)
(add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck-text)

;; Flymake

;; Compatibility
;; @see https://github.com/emacs-mirror/emacs/commit/6e100869012da9244679696634cab6b9cac96303.
(with-eval-after-load 'flymake
  (unless (boundp 'flymake--state)
    (defvaralias 'flymake--state 'flymake--backend-state))
  (unless (fboundp 'flymake--state-diags)
    (defalias 'flymake--state-diags 'flymake--backend-state-diags)))

(defvar-local doom-modeline--flymake-icon nil)
(defun doom-modeline-update-flymake-icon (&rest _)
  "Update flymake icon."
  (setq flymake--mode-line-format nil) ; remove the lighter of minor mode
  (setq doom-modeline--flymake-icon
        (let* ((known (hash-table-keys flymake--state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported)))
          (when-let
              ((icon
                (cond
                 (some-waiting (doom-modeline-checker-icon "access_time" "⏰" "*" 'doom-modeline-debug))
                 ((null known) (doom-modeline-checker-icon "sim_card_alert" "❓" "?" 'doom-modeline-debug))
                 (all-disabled (doom-modeline-checker-icon "sim_card_alert" "❗" "!" 'doom-modeline-urgent))
                 (t (let ((.error 0)
                          (.warning 0)
                          (.note 0))
                      (progn
                        (cl-loop
                         with warning-level = (warning-numeric-level :warning)
                         with note-level = (warning-numeric-level :debug)
                         for state being the hash-values of flymake--state
                         do (cl-loop
                             with diags = (flymake--state-diags state)
                             for diag in diags do
                             (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                            (warning-numeric-level :error))))
                               (cond ((> severity warning-level) (cl-incf .error))
                                     ((> severity note-level)    (cl-incf .warning))
                                     (t                          (cl-incf .note))))))
                        (if (> (+ .error .warning .note) 0)
                            (doom-modeline-checker-icon "do_not_disturb_alt" "🚫" "!"
                                                        (cond ((> .error 0) 'doom-modeline-urgent)
                                                              ((> .warning 0) 'doom-modeline-warning)
                                                              (t 'doom-modeline-info)))
                          (doom-modeline-checker-icon "check" "✔" "-" 'doom-modeline-info))))))))
            (propertize
             icon
             'help-echo (concat "Flymake\n"
                                (cond
                                 (some-waiting "Running...")
                                 ((null known) "No Checker")
                                 (all-disabled "All Checkers Disabled")
                                 (t (format "%d/%d backends running
mouse-1: Display minor mode menu
mouse-2: Show help for minor mode"
                                            (length running) (length known)))))
             'mouse-face 'mode-line-highlight
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line down-mouse-1]
                            flymake-menu)
                          (define-key map [mode-line mouse-2]
                            (lambda ()
                              (interactive)
                              (describe-function 'flymake-mode)))
                          map))))))
(advice-add #'flymake--handle-report :after #'doom-modeline-update-flymake-icon)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (flymake-start)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (flymake-start)))))))

(defvar-local doom-modeline--flymake-text nil)
(defun doom-modeline-update-flymake-text (&rest _)
  "Update flymake text."
  (setq flymake--mode-line-format nil) ; remove the lighter of minor mode
  (setq doom-modeline--flymake-text
        (let* ((known (hash-table-keys flymake--state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported))
               (warning-level (warning-numeric-level :warning))
               (note-level (warning-numeric-level :debug))
               (.error 0)
               (.warning 0)
               (.note 0))
          (maphash (lambda (_b state)
                     (cl-loop
                      with diags = (flymake--state-diags state)
                      for diag in diags do
                      (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                     (warning-numeric-level :error))))
                        (cond ((> severity warning-level) (cl-incf .error))
                              ((> severity note-level) (cl-incf .warning))
                              (t (cl-incf .note))))))
                   flymake--state)
          (when-let
              ((text
                (cond
                 (some-waiting doom-modeline--flymake-text)
                 ((null known) nil)
                 (all-disabled nil)
                 (t (let ((num (+ .error .warning .note)))
                      (when (> num 0)
                        (if doom-modeline-checker-simple-format
                            (doom-modeline-checker-text (number-to-string num)
                                                        (cond ((> .error 0) 'doom-modeline-urgent)
                                                              ((> .warning 0) 'doom-modeline-warning)
                                                              (t 'doom-modeline-info)))
                          (format "%s/%s/%s"
                                  (doom-modeline-checker-text (number-to-string .error)
                                                              'doom-modeline-urgent)
                                  (doom-modeline-checker-text (number-to-string .warning)
                                                              'doom-modeline-warning)
                                  (doom-modeline-checker-text (number-to-string .note)
                                                              'doom-modeline-info)))))))))
            (propertize
             text
             'help-echo (cond
                         (some-waiting "Running...")
                         ((null known) "No Checker")
                         (all-disabled "All Checkers Disabled")
                         (t (format "error: %d, warning: %d, note: %d
mouse-1: List all problems%s"
                                    .error .warning .note
                                    (if (featurep 'mwheel)
                                        "\nwheel-up/wheel-down: Previous/next problem"))))
             'mouse-face 'mode-line-highlight
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line mouse-1]
                            #'flymake-show-diagnostics-buffer)
                          (when (featurep 'mwheel)
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
                            map)))))))
(advice-add #'flymake--handle-report :after #'doom-modeline-update-flymake-text)

(doom-modeline-def-segment checker
  "Displays color-coded error status in the current buffer with pretty icons."
  (let ((active (doom-modeline--active))
        (seg (cond ((and (bound-and-true-p flymake-mode)
                         (bound-and-true-p flymake--state)) ; only support 26+
                    `(,doom-modeline--flymake-icon . ,doom-modeline--flymake-text))
                   ((bound-and-true-p flycheck-mode)
                    `(,doom-modeline--flycheck-icon . ,doom-modeline--flycheck-text)))))
    (let ((icon (car seg))
          (text (cdr seg)))
      (concat
       (when icon
         (concat
          (doom-modeline-spc)
          (if active
              icon
            (doom-modeline-propertize-icon icon 'mode-line-inactive))))
       (when text
         (concat
          (if icon (doom-modeline-vspc) (doom-modeline-spc))
          (if active
              text
            (propertize text 'face 'mode-line-inactive))))
       (doom-modeline-spc)))))


;;
;; Word Count
;;

(doom-modeline-def-segment word-count
  "The buffer word count.
Displayed when in a major mode in `doom-modeline-continuous-word-count-modes'.
Respects `doom-modeline-enable-word-count'."
  (when (and doom-modeline-enable-word-count
             (member major-mode doom-modeline-continuous-word-count-modes))
    (propertize (format " %dW" (count-words (point-min) (point-max)))
                'face (if (doom-modeline--active)
                          'mode-line
                        'mode-line-inactive))))


;;
;; Selection
;;

(defsubst doom-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

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
         (concat (doom-modeline-spc)
                 (cond ((or (bound-and-true-p rectangle-mark-mode)
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
                       (t
                        (format "%dC" (- end beg))))
                 (when doom-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))
                 (doom-modeline-spc)))
       'face 'doom-modeline-highlight))))


;;
;; Matches (macro, anzu, evil-substitute, iedit, symbol-overlay and multi-cursors)
;;

(defsubst doom-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (doom-modeline--active)
             (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-panel ))
          (vsep (propertize " " 'face
                            '(:inherit (doom-modeline-panel variable-pitch)))))
      (concat
       sep
       (doom-modeline-icon 'material "fiber_manual_record" "●"
                           (if (bound-and-true-p evil-this-macro)
                               (char-to-string evil-this-macro)
                             "Macro")
                           :face 'doom-modeline-panel
                           :v-adjust -0.225)
       vsep
       (doom-modeline-icon 'octicon "triangle-right" "▶" ">"
                           :face 'doom-modeline-panel
                           :v-adjust -0.05)
       sep))))

;; `anzu' and `evil-anzu' expose current/total state that can be displayed in the
;; mode-line.
(defun doom-modeline-fix-anzu-count (positions here)
  "Calulate anzu count via POSITIONS and HERE."
  (cl-loop for (start . end) in positions
           collect t into before
           when (and (>= here start) (<= here end))
           return (length before)
           finally return 0))

(advice-add #'anzu--where-is-here :override #'doom-modeline-fix-anzu-count)

(setq anzu-cons-mode-line-p nil) ; manage modeline segment ourselves
;; Ensure anzu state is cleared when searches & iedit are done
(with-eval-after-load 'anzu
  (add-hook 'isearch-mode-end-hook #'anzu--reset-status t)
  (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
  (advice-add #'evil-force-normal-state :after #'anzu--reset-status)
  ;; Fix matches segment mirroring across all buffers
  (mapc #'make-variable-buffer-local
        '(anzu--total-matched
          anzu--current-position anzu--state anzu--cached-count
          anzu--cached-positions anzu--last-command
          anzu--last-isearch-string anzu--overflow-p)))

(defsubst doom-modeline--anzu ()
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " anzu--cached-count))
             ((eq anzu--state 'replace)
              (format " %d/%d " here total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (doom-modeline--active) 'doom-modeline-panel 'mode-line-inactive))))

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
     'face (if (doom-modeline--active) 'doom-modeline-panel 'mode-line-inactive))))

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
                        (save-excursion (iedit-prev-occurrence)
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
     'face (if (doom-modeline--active) 'doom-modeline-panel 'mode-line-inactive))))

(defsubst doom-modeline--symbol-overlay ()
  "Show the number of matches for symbol overlay."
  (when-let ((active (doom-modeline--active)))
    (when (and (bound-and-true-p symbol-overlay-keywords-alist)
               (not (bound-and-true-p symbol-overlay-temp-symbol))
               (not (bound-and-true-p iedit-mode)))
      (let* ((keyword (symbol-overlay-assoc (symbol-overlay-get-symbol t)))
             (symbol (car keyword))
             (before (symbol-overlay-get-list -1 symbol))
             (after (symbol-overlay-get-list 1 symbol))
             (count (length before)))
        (if (symbol-overlay-assoc symbol)
            (propertize
             (format (concat  " %d/%d " (and (cadr keyword) "in scope "))
                     (+ count 1)
                     (+ count (length after)))
             'face (if active 'doom-modeline-panel 'mode-line-inactive)))))))

(defsubst doom-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (cl-destructuring-bind (count . face)
    (cond ((bound-and-true-p multiple-cursors-mode)
           (cons (mc/num-cursors)
                 (if (doom-modeline--active)
                     'doom-modeline-panel
                   'mode-line-inactive)))
          ((bound-and-true-p evil-mc-cursor-list)
           (cons (length evil-mc-cursor-list)
                 (cond ((not (doom-modeline--active)) 'mode-line-inactive)
                       (evil-mc-frozen 'doom-modeline-bar)
                       ('doom-modeline-panel))))
          ((cons nil nil)))
    (when count
      (concat (propertize " " 'face face)
              (or (doom-modeline-icon 'faicon "i-cursor" nil nil
                                      :face face :v-adjust -0.0575)
                  (propertize "I"
                              'face `(:inherit ,face :height 1.4 :weight normal)
                              'display '(raise -0.1)))
              (propertize (doom-modeline-vspc)
                          'face `(:inherit (variable-pitch ,face)))
              (propertize (format "%d " count)
                          'face face)))))

(defsubst doom-modeline--phi-search ()
  "Show the number of matches for `phi-search' and `phi-replace'."
  (when-let ((active (doom-modeline--active)))
    (when (bound-and-true-p phi-search--overlays)
      (let ((total (length phi-search--overlays))
            (selection phi-search--selection))
        (when selection
          (propertize
           (format " %d/%d " (1+ selection) total)
           'face (if active 'doom-modeline-panel 'mode-line-inactive)))))))

(defun doom-modeline--override-phi-search-mode-line (orig-fun &rest args)
  "Override the mode-line of `phi-search' and `phi-replace'."
  (if (bound-and-true-p doom-modeline-mode)
      (apply orig-fun mode-line-format (cdr args))
    (apply orig-fun args)))
(advice-add #'phi-search--initialize :around #'doom-modeline--override-phi-search-mode-line)

(defsubst doom-modeline--buffer-size ()
  "Show buffer size."
  (when size-indication-mode
    (concat (doom-modeline-spc)
            (propertize "%I"
                        'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                        'help-echo "Buffer size
mouse-1: Display Line and Column Mode Menu"
                        'mouse-face 'mode-line-highlight
                        'local-map mode-line-column-line-number-mode-map)
            (doom-modeline-spc))))

(doom-modeline-def-segment matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with `anzu'), 3. The number of substitutions being
conducted with `evil-ex-substitute', and/or 4. The number of active `iedit'
regions, 5. The current/total for the highlight term (with `symbol-overlay'),
6. The number of active `multiple-cursors'."
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--anzu)
                      (doom-modeline--phi-search)
                      (doom-modeline--evil-substitute)
                      (doom-modeline--iedit)
                      (doom-modeline--symbol-overlay)
                      (doom-modeline--multiple-cursors))))
    (or (and (not (equal meta "")) meta)
        (doom-modeline--buffer-size)))
  )

(doom-modeline-def-segment buffer-size
  "Display buffer size"
  (doom-modeline--buffer-size))

;;
;; Media
;;

(doom-modeline-def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO Include other information
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
           (when (fboundp 'image-size)
             (image-size (image-get-display-property) :pixels))
           (propertize
            (format "  %dx%d  " width height)
            'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))))))


;;
;; Bars
;;

(defvar doom-modeline--bar-active nil)
(defvar doom-modeline--bar-inactive nil)

(defsubst doom-modeline--bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and doom-modeline--bar-active doom-modeline--bar-inactive)
    (let ((width doom-modeline-bar-width)
          (height (max doom-modeline-height
                       (doom-modeline--font-height))))
      (when (and (numberp width) (numberp height))
        (setq doom-modeline--bar-active
              (doom-modeline--create-bar-image 'doom-modeline-bar width height)
              doom-modeline--bar-inactive
              (doom-modeline--create-bar-image
               'doom-modeline-bar-inactive width height)))))
  (if (doom-modeline--active)
      doom-modeline--bar-active
    doom-modeline--bar-inactive))

(defun doom-modeline-refresh-bars ()
  "Refresh mode-line bars on next redraw."
  (setq doom-modeline--bar-active nil
        doom-modeline--bar-inactive nil))

(cl-defstruct doom-modeline--hud-cache active inactive top-margin bottom-margin)

(defsubst doom-modeline--hud ()
  "Powerline's hud segment reimplemented in the style of Doom's bar segment."
  (let* ((ws (window-start))
         (we (window-end))
         (bs (buffer-size))
         (height (max doom-modeline-height
                      (doom-modeline--font-height)))
         (top-margin (if (zerop bs)
                         0
                       (/ (* height (1- ws)) bs)))
         (bottom-margin (if (zerop bs)
                            0
                          (max 0 (/ (* height (- bs we 1)) bs))))
         (cache (or (window-parameter nil 'doom-modeline--hud-cache)
                    (set-window-parameter nil 'doom-modeline--hud-cache
                                          (make-doom-modeline--hud-cache)))))
    (unless (and (doom-modeline--hud-cache-active cache)
                 (doom-modeline--hud-cache-inactive cache)
                 (= top-margin (doom-modeline--hud-cache-top-margin cache))
                 (= bottom-margin
                    (doom-modeline--hud-cache-bottom-margin cache)))
      (setf (doom-modeline--hud-cache-active cache)
            (doom-modeline--create-hud-image
             'doom-modeline-bar 'default doom-modeline-bar-width
             height top-margin bottom-margin)
            (doom-modeline--hud-cache-inactive cache)
            (doom-modeline--create-hud-image
             'doom-modeline-bar-inactive 'default doom-modeline-bar-width
             height top-margin bottom-margin)
            (doom-modeline--hud-cache-top-margin cache) top-margin
            (doom-modeline--hud-cache-bottom-margin cache) bottom-margin))
    (if (doom-modeline--active)
        (doom-modeline--hud-cache-active cache)
      (doom-modeline--hud-cache-inactive cache))))

(defun doom-modeline-invalidate-huds ()
  "Invalidate all cached hud images."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (set-window-parameter window 'doom-modeline--hud-cache nil))))

(doom-modeline-add-variable-watcher
 'doom-modeline-height
 (lambda (_sym val op _where)
   (when (and (eq op 'set) (integerp val))
     (doom-modeline-refresh-bars)
     (doom-modeline-invalidate-huds))))

(doom-modeline-add-variable-watcher
 'doom-modeline-bar-width
 (lambda (_sym val op _where)
   (when (and (eq op 'set) (integerp val))
     (doom-modeline-refresh-bars)
     (doom-modeline-invalidate-huds))))

(add-hook 'after-setting-font-hook #'doom-modeline-refresh-bars)
(add-hook 'after-setting-font-hook #'doom-modeline-invalidate-huds)

(doom-modeline-def-segment bar
  "The bar regulates the height of the mode-line in GUI."
  (if doom-modeline-hud
      (doom-modeline--hud)
    (doom-modeline--bar)))

(doom-modeline-def-segment hud
  "Powerline's hud segment reimplemented in the style of Doom's bar segment."
  (doom-modeline--hud))


;;
;; Window number
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
  "The current window number."
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
    (if (and (< 0 (length num))
             (< 1 (length (cl-mapcan
                           (lambda (frame)
                             ;; Exclude minibuffer and child frames
                             (unless (and (fboundp 'frame-parent)
                                          (frame-parent frame))
                               (window-list frame 'never)))
                           (visible-frame-list)))))
        (propertize (format " %s " num)
                    'face (if (doom-modeline--active)
                              'doom-modeline-buffer-major-mode
                            'mode-line-inactive)))))


;;
;; Workspace
;;

(doom-modeline-def-segment workspace-name
  "The current workspace name or number.
Requires `eyebrowse-mode' to be enabled or `tab-bar-mode' tabs to be created."
  (when doom-modeline-workspace-name
    (when-let
        ((name (cond
                ((and (bound-and-true-p eyebrowse-mode)
                      (< 1 (length (eyebrowse--get 'window-configs))))
                 (assq-delete-all 'eyebrowse-mode mode-line-misc-info)
                 (when-let*
                     ((num (eyebrowse--get 'current-slot))
                      (tag (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                   (if (< 0 (length tag)) tag (int-to-string num))))
                ((and (fboundp 'tab-bar-mode)
                      (< 1 (length (frame-parameter nil 'tabs))))
                 (let* ((current-tab (tab-bar--current-tab))
                        (tab-index (tab-bar--current-tab-index))
                        (explicit-name (alist-get 'explicit-name current-tab))
                        (tab-name (alist-get 'name current-tab)))
                   (if explicit-name tab-name (+ 1 tab-index)))))))
      (propertize (format " %s " name) 'face
                  (if (doom-modeline--active)
                      'doom-modeline-buffer-major-mode
                    'mode-line-inactive)))))


;;
;; Perspective
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
          (let* ((persp (get-current-persp))
                 (name (safe-persp-name persp))
                 (face (if (and persp
                                (not (persp-contain-buffer-p (current-buffer) persp)))
                           'doom-modeline-persp-buffer-not-in-persp
                         'doom-modeline-persp-name))
                 (icon (doom-modeline-icon 'material "folder" "🖿" "#"
                                           :face `(:inherit ,face :slant normal)
                                           :height 1.1
                                           :v-adjust -0.225)))
            (when (or doom-modeline-display-default-persp-name
                      (not (string-equal persp-nil-name name)))
              (concat (doom-modeline-spc)
                      (propertize (concat (and doom-modeline-persp-icon
                                               (concat icon (doom-modeline-vspc)))
                                          (propertize name 'face face))
                                  'help-echo "mouse-1: Switch perspective
mouse-2: Show help for minor mode"
                                  'mouse-face 'mode-line-highlight
                                  'local-map (let ((map (make-sparse-keymap)))
                                               (define-key map [mode-line mouse-1]
                                                 #'persp-switch)
                                               (define-key map [mode-line mouse-2]
                                                 (lambda ()
                                                   (interactive)
                                                   (describe-function 'persp-mode)))
                                               map))
                      (doom-modeline-spc)))))))

(add-hook 'buffer-list-update-hook #'doom-modeline-update-persp-name)
(add-hook 'find-file-hook #'doom-modeline-update-persp-name)
(add-hook 'persp-activated-functions #'doom-modeline-update-persp-name)
(add-hook 'persp-renamed-functions #'doom-modeline-update-persp-name)
(advice-add #'lv-message :after #'doom-modeline-update-persp-name)

(doom-modeline-def-segment persp-name
  "The current perspective name."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p))
    doom-modeline--persp-name))


;;
;; Misc info
;;

(doom-modeline-def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p))
    '("" mode-line-misc-info)))


;;
;; Position
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

(doom-modeline-add-variable-watcher
 'column-number-indicator-zero-based
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-column-zero-based val))))

(doom-modeline-add-variable-watcher
 'mode-line-percent-position
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-percent-position val))))

(doom-modeline-def-segment buffer-position
  "The buffer position information."
  (let* ((active (doom-modeline--active))
         (lc '(line-number-mode
               (column-number-mode
                (doom-modeline-column-zero-based "%l:%c" "%l:%C")
                "%l")
               (column-number-mode (doom-modeline-column-zero-based ":%c" ":%C"))))
         (face (if active 'mode-line 'mode-line-inactive))
         (mouse-face 'mode-line-highlight)
         (local-map mode-line-column-line-number-mode-map))
    (concat
     (doom-modeline-wspc)

     ;; Line and column
     (propertize (format-mode-line lc)
                 'face face
                 'help-echo "Buffer position\n\
mouse-1: Display Line and Column Mode Menu"
                 'mouse-face mouse-face
                 'local-map local-map)

     ;; Position
     (cond ((and active
                 (bound-and-true-p nyan-mode)
                 (not doom-modeline--limited-width-p)
                 (>= (window-width) nyan-minimum-window-width))
            (concat
             (doom-modeline-wspc)
             (propertize (nyan-create) 'mouse-face mouse-face)))
           ((and active
                 (bound-and-true-p poke-line-mode)
                 (not doom-modeline--limited-width-p)
                 (>= (window-width) poke-line-minimum-window-width))
            (concat
             (doom-modeline-wspc)
             (propertize (poke-line-create) 'mouse-face mouse-face)))
           ((and active
                 (bound-and-true-p mlscroll-mode)
                 (not doom-modeline--limited-width-p)
                 (>= (window-width) mlscroll-minimum-current-width))
            (concat
             (doom-modeline-wspc)
             (let ((mlscroll-right-align nil))
               (format-mode-line (mlscroll-mode-line)))))
           ((and active
                 (bound-and-true-p sml-modeline-mode)
                 (not doom-modeline--limited-width-p)
                 (>= (window-width) sml-modeline-len))
            (concat
             (doom-modeline-wspc)
             (propertize (sml-modeline-create) 'mouse-face mouse-face)))
           (t ""))

     ;; Percent position
     (when doom-modeline-percent-position
       (concat
        (doom-modeline-spc)
        (propertize (format-mode-line '("" doom-modeline-percent-position "%%"))
                    'face face
                    'help-echo "Buffer percentage\n\
mouse-1: Display Line and Column Mode Menu"
                    'mouse-face mouse-face
                    'local-map local-map)))

     (when (or line-number-mode column-number-mode doom-modeline-percent-position)
       (doom-modeline-spc)))))

;;
;; Party parrot
;;
(doom-modeline-def-segment parrot
  "The party parrot animated icon. Requires `parrot-mode' to be enabled."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p)
             (bound-and-true-p parrot-mode))
    (concat (doom-modeline-wspc)
            (parrot-create)
            (doom-modeline-spc))))

;;
;; Modals (evil, overwrite, god, ryo and xah-fly-keys, etc.)
;;

(defun doom-modeline--modal-icon (text face help-echo)
  "Display the model icon with FACE and HELP-ECHO.
TEXT is alternative if icon is not available."
  (propertize (doom-modeline-icon
               'material
               (when doom-modeline-modal-icon "fiber_manual_record")
               "●"
               text
               :face (if (doom-modeline--active) face 'mode-line-inactive)
               :v-adjust -0.225)
              'help-echo help-echo))

(defsubst doom-modeline--evil ()
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (doom-modeline--modal-icon
     (let ((tag (evil-state-property evil-state :tag t)))
       (if (stringp tag) tag (funcall tag)))
     (cond
      ((evil-normal-state-p) 'doom-modeline-evil-normal-state)
      ((evil-emacs-state-p) 'doom-modeline-evil-emacs-state)
      ((evil-insert-state-p) 'doom-modeline-evil-insert-state)
      ((evil-motion-state-p) 'doom-modeline-evil-motion-state)
      ((evil-visual-state-p) 'doom-modeline-evil-visual-state)
      ((evil-operator-state-p) 'doom-modeline-evil-operator-state)
      ((evil-replace-state-p) 'doom-modeline-evil-replace-state)
      (t 'doom-modeline-evil-normal-state))
     (evil-state-property evil-state :name t))))

(defsubst doom-modeline--overwrite ()
  "The current overwrite state which is enabled by command `overwrite-mode'."
  (when (and (bound-and-true-p overwrite-mode)
             (not (bound-and-true-p evil-local-mode)))
    (doom-modeline--modal-icon " <O> " 'doom-modeline-urgent "Overwrite mode")))

(defsubst doom-modeline--god ()
  "The current god state which is enabled by the command `god-mode'."
  (when (bound-and-true-p god-local-mode)
    (doom-modeline--modal-icon " <G> " 'doom-modeline-evil-normal-state "God mode")))

(defsubst doom-modeline--ryo ()
  "The current ryo-modal state which is enabled by the command `ryo-modal-mode'."
  (when (bound-and-true-p ryo-modal-mode)
    (doom-modeline--modal-icon "<R>" 'doom-modeline-evil-normal-state "Ryo modal")))

(defsubst doom-modeline--xah-fly-keys ()
  "The current `xah-fly-keys' state."
  (when (bound-and-true-p xah-fly-keys)
    (if xah-fly-insert-state-q
        (doom-modeline--modal-icon " <I> "
                                   'doom-modeline-evil-insert-state
                                   (format "Xah-fly insert mode"))
      (doom-modeline--modal-icon " <C> "
                                 'doom-modeline-evil-normal-state
                                 (format "Xah-fly command mode")))))

(defsubst doom-modeline--boon ()
  "The current Boon state. Requires `boon-mode' to be enabled."
  (when (bound-and-true-p boon-local-mode)
    (doom-modeline--modal-icon
     (boon-state-string)
     (cond
      (boon-command-state 'doom-modeline-evil-normal-state)
      (boon-insert-state 'doom-modeline-evil-insert-state)
      (boon-special-state 'doom-modeline-evil-emacs-state)
      (boon-off-state 'doom-modeline-evil-operator-state)
      (t 'doom-modeline-evil-operator-state))
     (boon-modeline-string))))

(defsubst doom-modeline--meow ()
  "The current Meow state. Requires `meow-mode' to be enabled."
  (when (bound-and-true-p meow-mode)
    meow--indicator))

(doom-modeline-def-segment modals
  "Displays modal editing states, including `evil', `overwrite', `god', `ryo'
and `xha-fly-kyes', etc."
  (let* ((evil (doom-modeline--evil))
         (ow (doom-modeline--overwrite))
         (god (doom-modeline--god))
         (ryo (doom-modeline--ryo))
         (xf (doom-modeline--xah-fly-keys))
         (boon (doom-modeline--boon))
         (vsep (doom-modeline-vspc))
         (meow (doom-modeline--meow))
         (sep (and (or evil ow god ryo xf boon) (doom-modeline-spc))))
    (concat sep
            (and evil (concat evil (and (or ow god ryo xf boon meow) vsep)))
            (and ow (concat ow (and (or god ryo xf boon meow) vsep)))
            (and god (concat god (and (or ryo xf boon meow) vsep)))
            (and ryo (concat ryo (and (or xf boon meow) vsep)))
            (and xf (concat xf (and (or boon meow) vsep)))
            (and boon (concat boon (and meow vsep)))
            meow
            sep)))

;;
;; Objed state
;;

(defvar doom-modeline--objed-active nil)

(defun doom-modeline-update-objed (_ &optional reset)
  "Update `objed' status, inactive when RESET is true."
  (setq doom-modeline--objed-active (not reset)))

(setq objed-modeline-setup-func #'doom-modeline-update-objed)

(doom-modeline-def-segment objed-state ()
  "The current objed state."
  (when (and doom-modeline--objed-active
             (doom-modeline--active))
    (propertize (format " %s(%s) "
                        (symbol-name objed--object)
                        (char-to-string (aref (symbol-name objed--obj-state) 0)))
                'face 'doom-modeline-evil-emacs-state
                'help-echo (format "Objed object: %s (%s)"
                                   (symbol-name objed--object)
                                   (symbol-name objed--obj-state)))))


;;
;; Input method
;;

(doom-modeline-def-segment input-method
  "The current input method."
  (propertize (cond (current-input-method
                     (concat (doom-modeline-spc)
                             current-input-method-title
                             (doom-modeline-spc)))
                    ((and (bound-and-true-p evil-local-mode)
                          (bound-and-true-p evil-input-method))
                     (concat
                      (doom-modeline-spc)
                      (nth 3 (assoc default-input-method input-method-alist))
                      (doom-modeline-spc)))
                    (t ""))
              'face (if (doom-modeline--active)
                        (if (and (bound-and-true-p rime-mode)
                                 (equal current-input-method "rime"))
                            (if (and (rime--should-enable-p)
                                     (not (rime--should-inline-ascii-p)))
                                'doom-modeline-input-method
                              'doom-modeline-input-method-alt)
                          'doom-modeline-input-method)
                      'mode-line-inactive)
              'help-echo (concat
                          "Current input method: "
                          current-input-method
                          "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
              'mouse-face 'mode-line-highlight
              'local-map mode-line-input-method-map))


;;
;; Info
;;

(doom-modeline-def-segment info-nodes
  "The topic and nodes in the Info buffer."
  (let ((active (doom-modeline--active)))
    (concat
     (propertize " (" 'face (if active 'mode-line 'mode-line-inactive))
     ;; topic
     (propertize (if (stringp Info-current-file)
                     (replace-regexp-in-string
                      "%" "%%"
                      (file-name-sans-extension
                       (file-name-nondirectory Info-current-file)))
                   (format "*%S*" Info-current-file))
                 'face (if active 'doom-modeline-info 'mode-line-inactive))
     (propertize ") " 'face (if active 'mode-line 'mode-line-inactive))
     ;; node
     (when Info-current-node
       (propertize (replace-regexp-in-string
                    "%" "%%" Info-current-node)
                   'face (if active 'doom-modeline-buffer-path 'mode-line-inactive)
                   'help-echo
                   "mouse-1: scroll forward, mouse-3: scroll back"
                   'mouse-face 'mode-line-highlight
                   'local-map Info-mode-line-node-keymap)))))


;;
;; REPL
;;

(defun doom-modeline-repl-icon (text face)
  "Display REPL icon (or TEXT in terminal) with FACE."
  (doom-modeline-icon 'faicon "terminal" "$" text
                      :face face :height 1.0 :v-adjust -0.0575))

(defvar doom-modeline--cider nil)

(defun doom-modeline-update-cider ()
  "Update cider repl state."
  (setq doom-modeline--cider
        (let* ((connected (cider-connected-p))
               (face (if connected 'doom-modeline-repl-success 'doom-modeline-repl-warning))
               (repl-buffer (cider-current-repl nil nil))
               (cider-info (when repl-buffer
                             (cider--connection-info repl-buffer t)))
               (icon (doom-modeline-repl-icon "REPL" face)))
          (propertize icon
                      'help-echo
                      (if connected
                          (format "CIDER Connected %s\nmouse-2: CIDER quit" cider-info)
                        "CIDER Disconnected\nmouse-1: CIDER jack-in")
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (if connected
                                       (define-key map [mode-line mouse-2]
                                         #'cider-quit)
                                     (define-key map [mode-line mouse-1]
                                       #'cider-jack-in))
                                   map)))))

(add-hook 'cider-connected-hook #'doom-modeline-update-cider)
(add-hook 'cider-disconnected-hook #'doom-modeline-update-cider)
(add-hook 'cider-mode-hook #'doom-modeline-update-cider)

(doom-modeline-def-segment repl
  "The REPL state."
  (when doom-modeline-repl
    (when-let (icon (when (bound-and-true-p cider-mode)
                      doom-modeline--cider))
      (concat
       (doom-modeline-spc)
       (if (doom-modeline--active)
           icon
         (doom-modeline-propertize-icon icon 'mode-line-inactive))
       (doom-modeline-spc)))))


;;
;; LSP
;;

(defun doom-modeline-lsp-icon (text face)
  "Display LSP icon (or TEXT in terminal) with FACE."
  (doom-modeline-icon 'faicon "rocket" "🚀" text
                      :face face :height 1.0 :v-adjust -0.0575))

(defvar-local doom-modeline--lsp nil)
(defun doom-modeline-update-lsp (&rest _)
  "Update `lsp-mode' state."
  (setq doom-modeline--lsp
        (let* ((workspaces (lsp-workspaces))
               (face (if workspaces 'doom-modeline-lsp-success 'doom-modeline-lsp-warning))
               (icon (doom-modeline-lsp-icon "LSP" face)))
          (propertize icon
                      'help-echo
                      (if workspaces
                          (concat "LSP Connected "
                                  (string-join
                                   (mapcar (lambda (w)
                                             (format "[%s]\n" (lsp--workspace-print w)))
                                           workspaces))
                                  "C-mouse-1: Switch to another workspace folder
mouse-1: Describe current session
mouse-2: Quit server
mouse-3: Reconnect to server")
                        "LSP Disconnected
mouse-1: Reload to start server")
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (if workspaces
                                       (progn
                                         (define-key map [mode-line C-mouse-1]
                                           #'lsp-workspace-folders-open)
                                         (define-key map [mode-line mouse-1]
                                           #'lsp-describe-session)
                                         (define-key map [mode-line mouse-2]
                                           #'lsp-workspace-shutdown)
                                         (define-key map [mode-line mouse-3]
                                           #'lsp-workspace-restart))
                                     (progn
                                       (define-key map [mode-line mouse-1]
                                         (lambda ()
                                           (interactive)
                                           (ignore-errors (revert-buffer t t))))))
                                   map)))))
(add-hook 'lsp-before-initialize-hook #'doom-modeline-update-lsp)
(add-hook 'lsp-after-initialize-hook #'doom-modeline-update-lsp)
(add-hook 'lsp-after-uninitialized-functions #'doom-modeline-update-lsp)
(add-hook 'lsp-before-open-hook #'doom-modeline-update-lsp)
(add-hook 'lsp-after-open-hook #'doom-modeline-update-lsp)

(defvar-local doom-modeline--eglot nil)
(defun doom-modeline-update-eglot ()
  "Update `eglot' state."
  (setq doom-modeline--eglot
        (pcase-let* ((server (eglot-current-server))
                     (nick (and server (eglot--project-nickname server)))
                     (pending (and server (hash-table-count
                                           (jsonrpc--request-continuations server))))
                     (`(,_id ,doing ,done-p ,detail) (and server (eglot--spinner server)))
                     (last-error (and server (jsonrpc-last-error server)))
                     (face (cond (last-error 'doom-modeline-lsp-error)
                                 ((and doing (not done-p)) 'doom-modeline-lsp-running)
                                 ((and pending (cl-plusp pending)) 'doom-modeline-lsp-warning)
                                 (nick 'doom-modeline-lsp-success)
                                 (t 'doom-modeline-lsp-warning)))
                     (icon (doom-modeline-lsp-icon "EGLOT" face)))
          (propertize icon
                      'help-echo (cond
                                  (last-error
                                   (format "EGLOT\nAn error occured: %s
mouse-3: Clear this status" (plist-get last-error :message)))
                                  ((and doing (not done-p))
                                   (format "EGLOT\n%s%s" doing
                                           (if detail (format "%s" detail) "")))
                                  ((and pending (cl-plusp pending))
                                   (format "EGLOT\n%d outstanding requests" pending))
                                  (nick (format "EGLOT Connected (%s/%s)
C-mouse-1: Go to server errors
mouse-1: Go to server events
mouse-2: Quit server
mouse-3: Reconnect to server" nick (eglot--major-mode server)))
                                  (t "EGLOT Disconnected
mouse-1: Start server"))
                      'mouse-face 'mode-line-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (cond (last-error
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-clear-status))
                                         ((and pending (cl-plusp pending))
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-forget-pending-continuations))
                                         (nick
                                          (define-key map [mode-line C-mouse-1]
                                            #'eglot-stderr-buffer)
                                          (define-key map [mode-line mouse-1]
                                            #'eglot-events-buffer)
                                          (define-key map [mode-line mouse-2]
                                            #'eglot-shutdown)
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-reconnect))
                                         (t (define-key map [mode-line mouse-1]
                                              #'eglot)))
                                   map)))))
(add-hook 'eglot--managed-mode-hook #'doom-modeline-update-eglot)

(defvar-local doom-modeline--tags nil)
(defun doom-modeline-update-tags ()
  "Update tags state."
  (setq doom-modeline--tags
        (propertize
         (doom-modeline-lsp-icon "LSP" 'doom-modeline-lsp-success)
         'help-echo "TAGS: Citre mode
mouse-1: Toggle citre mode"
         'mouse-face 'mode-line-highlight
         'local-map (make-mode-line-mouse-map 'mouse-1 #'citre-mode))))
(add-hook 'citre-mode-hook #'doom-modeline-update-tags)

(defun doom-modeline-update-lsp-icon ()
  "Update lsp icon."
  (cond ((bound-and-true-p lsp-mode)
         (doom-modeline-update-lsp))
        ((bound-and-true-p eglot--managed-mode)
         (doom-modeline-update-eglot))
        ((bound-and-true-p citre-mode)
         (doom-modeline-update-tags))))

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-lsp-icon))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-lsp-icon))))))

(doom-modeline-def-segment lsp
  "The LSP server state."
  (when (and doom-modeline-lsp
             (not doom-modeline--limited-width-p))
    (let ((active (doom-modeline--active))
          (icon (cond ((bound-and-true-p lsp-mode)
                       doom-modeline--lsp)
                      ((bound-and-true-p eglot--managed-mode)
                       doom-modeline--eglot)
                      ((bound-and-true-p citre-mode)
                       doom-modeline--tags))))
      (when icon
        (concat
         (doom-modeline-spc)
         (if active
             icon
           (doom-modeline-propertize-icon icon 'mode-line-inactive))
         (doom-modeline-spc))))))

(defun doom-modeline-override-eglot-modeline ()
  "Override `eglot' mode-line."
  (if (bound-and-true-p doom-modeline-mode)
      (setq mode-line-misc-info
            (delq (assq 'eglot--managed-mode mode-line-misc-info) mode-line-misc-info))
    (add-to-list 'mode-line-misc-info
                 `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))))
(add-hook 'eglot--managed-mode-hook #'doom-modeline-override-eglot-modeline)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-eglot-modeline)


;;
;; GitHub
;;

(defvar doom-modeline--github-notification-number 0)
(defvar doom-modeline-before-github-fetch-notification-hook nil
  "Hooks before fetching GitHub notifications.
Example:
  (add-hook 'doom-modeline-before-github-fetch-notification-hook
            #'auth-source-pass-enable)")
(defun doom-modeline--github-fetch-notifications ()
  "Fetch GitHub notifications."
  (when (and doom-modeline-github
             (require 'async nil t))
    (async-start
     `(lambda ()
        ,(async-inject-variables
          "\\`\\(load-path\\|auth-sources\\|doom-modeline-before-github-fetch-notification-hook\\)\\'")
        (run-hooks 'doom-modeline-before-github-fetch-notification-hook)
        (when (require 'ghub nil t)
          (with-timeout (10)
            (ignore-errors
              (when-let* ((username (ghub--username ghub-default-host))
                          (token (ghub--token ghub-default-host username 'ghub t)))
                (ghub-get "/notifications" nil
                          :query '((notifications . "true"))
                          :username username
                          :auth token
                          :noerror t))))))
     (lambda (result)
       (message "")                     ; suppress message
       (setq doom-modeline--github-notification-number (length result))))))

(defvar doom-modeline--github-timer nil)
(defun doom-modeline-github-timer ()
  "Start/Stop the timer for GitHub fetching."
  (if (timerp doom-modeline--github-timer)
      (cancel-timer doom-modeline--github-timer))
  (setq doom-modeline--github-timer
        (and doom-modeline-github
             (run-with-idle-timer 30
                                  doom-modeline-github-interval
                                  #'doom-modeline--github-fetch-notifications))))

(doom-modeline-add-variable-watcher
 'doom-modeline-github
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-github val)
     (doom-modeline-github-timer))))

(doom-modeline-github-timer)

(doom-modeline-def-segment github
  "The GitHub notifications."
  (when (and doom-modeline-github
             (doom-modeline--active)
             (not doom-modeline--limited-width-p)
             (numberp doom-modeline--github-notification-number)
             (> doom-modeline--github-notification-number 0))
    (concat
     (doom-modeline-spc)
     (propertize
      (concat
       (doom-modeline-icon 'faicon "github" "🔔" "#"
                           :face 'doom-modeline-notification
                           :v-adjust -0.0575)
       (doom-modeline-vspc)
       ;; GitHub API is paged, and the limit is 50
       (propertize
        (if (>= doom-modeline--github-notification-number 50)
            "50+"
          (number-to-string doom-modeline--github-notification-number))
        'face '(:inherit
                (doom-modeline-unread-number doom-modeline-notification))))
      'help-echo "Github Notifications
mouse-1: Show notifications
mouse-3: Fetch notifications"
      'mouse-face 'mode-line-highlight
      'local-map (let ((map (make-sparse-keymap)))
                   (define-key map [mode-line mouse-1]
                     (lambda ()
                       "Open GitHub notifications page."
                       (interactive)
                       (run-with-idle-timer 300 nil #'doom-modeline--github-fetch-notifications)
                       (browse-url "https://github.com/notifications")))
                   (define-key map [mode-line mouse-3]
                     (lambda ()
                       "Fetching GitHub notifications."
                       (interactive)
                       (message "Fetching GitHub notifications...")
                       (doom-modeline--github-fetch-notifications)))
                   map))
     (doom-modeline-spc))))


;;
;; Debug states
;;

;; Highlight the mode-line while debugging.
(defvar-local doom-modeline--debug-cookie nil)
(defun doom-modeline--debug-visual (&rest _)
  "Update the face of mode-line for debugging."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (setq doom-modeline--debug-cookie
                  (face-remap-add-relative 'mode-line 'doom-modeline-debug-visual))
            (force-mode-line-update)))
        (buffer-list)))

(defun doom-modeline--normal-visual (&rest _)
  "Restore the face of mode-line."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (when doom-modeline--debug-cookie
              (face-remap-remove-relative doom-modeline--debug-cookie)
              (force-mode-line-update))))
        (buffer-list)))

(add-hook 'dap-session-created-hook #'doom-modeline--debug-visual)
(add-hook 'dap-terminated-hook #'doom-modeline--normal-visual)

(defun doom-modeline-debug-icon (face &rest args)
  "Display debug icon with FACE and ARGS."
  (doom-modeline-icon 'faicon "bug" "🐛" "!" :face face :v-adjust -0.0575 args))

(defun doom-modeline--debug-dap ()
  "The current `dap-mode' state."
  (when (and (bound-and-true-p dap-mode)
             (bound-and-true-p lsp-mode))
    (when-let ((session (dap--cur-session)))
      (when (dap--session-running session)
        (propertize (doom-modeline-debug-icon 'doom-modeline-info)
                    'help-echo (format "DAP (%s - %s)
mouse-1: Display debug hydra
mouse-2: Display recent configurations
mouse-3: Disconnect session"
                                       (dap--debug-session-name session)
                                       (dap--debug-session-state session))
                    'mouse-face 'mode-line-highlight
                    'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [mode-line mouse-1]
                                   #'dap-hydra)
                                 (define-key map [mode-line mouse-2]
                                   #'dap-debug-recent)
                                 (define-key map [mode-line mouse-3]
                                   #'dap-disconnect)
                                 map))))))

(defvar-local doom-modeline--debug-dap nil)
(defun doom-modeline-update-debug-dap (&rest _)
  "Update dap debug state."
  (setq doom-modeline--debug-dap (doom-modeline--debug-dap)))

(add-hook 'dap-session-created-hook #'doom-modeline-update-debug-dap)
(add-hook 'dap-session-changed-hook #'doom-modeline-update-debug-dap)
(add-hook 'dap-terminated-hook #'doom-modeline-update-debug-dap)

(defsubst doom-modeline--debug-edebug ()
  "The current `edebug' state."
  (when (bound-and-true-p edebug-mode)
    (propertize (doom-modeline-debug-icon 'doom-modeline-info)
                'help-echo (format "EDebug (%s)
mouse-1: Show help
mouse-2: Next
mouse-3: Stop debugging"
                                   edebug-execution-mode)
                'mouse-face 'mode-line-highlight
                'local-map (let ((map (make-sparse-keymap)))
                             (define-key map [mode-line mouse-1]
                               #'edebug-help)
                             (define-key map [mode-line mouse-2]
                               #'edebug-next-mode)
                             (define-key map [mode-line mouse-3]
                               #'edebug-stop)
                             map))))

(defsubst doom-modeline--debug-on-error ()
  "The current `debug-on-error' state."
  (when debug-on-error
    (propertize (doom-modeline-debug-icon 'doom-modeline-urgent)
                'help-echo "Debug on Error
mouse-1: Toggle Debug on Error"
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-error))))

(defsubst doom-modeline--debug-on-quit ()
  "The current `debug-on-quit' state."
  (when debug-on-quit
    (propertize (doom-modeline-debug-icon 'doom-modeline-warning)
                'help-echo "Debug on Quit
mouse-1: Toggle Debug on Quit"
                'mouse-face 'mode-line-highlight
                'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-quit))))

(doom-modeline-def-segment debug
  "The current debug state."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p))
    (let* ((dap doom-modeline--debug-dap)
           (edebug (doom-modeline--debug-edebug))
           (on-error (doom-modeline--debug-on-error))
           (on-quit (doom-modeline--debug-on-quit))
           (vsep (doom-modeline-vspc))
           (sep (and (or dap edebug on-error on-quit) (doom-modeline-spc))))
      (concat sep
              (and dap (concat dap (and (or edebug on-error on-quit) vsep)))
              (and edebug (concat edebug (and (or on-error on-quit) vsep)))
              (and on-error (concat on-error (and on-quit vsep)))
              on-quit
              sep))))


;;
;; PDF pages
;;

(defvar-local doom-modeline--pdf-pages nil)
(defun doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
        (format "  P%d/%d "
                (or (eval `(pdf-view-current-page)) 0)
                (pdf-cache-number-of-pages))))
(add-hook 'pdf-view-change-page-hook #'doom-modeline-update-pdf-pages)

(doom-modeline-def-segment pdf-pages
  "Display PDF pages."
  (propertize doom-modeline--pdf-pages
              'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)))


;;
;; `mu4e-alert' notifications
;;

(doom-modeline-def-segment mu4e
  "Show notifications of any unread emails in `mu4e'."
  (when (and doom-modeline-mu4e
             (doom-modeline--active)
             (not doom-modeline--limited-width-p)
             (bound-and-true-p mu4e-alert-mode-line)
             (numberp mu4e-alert-mode-line)
             ;; don't display if the unread mails count is zero
             (> mu4e-alert-mode-line 0))
    (concat
     (doom-modeline-spc)
     (propertize
      (concat
       (doom-modeline-icon 'material "email" "📧" "#"
                           :face 'doom-modeline-notification
                           :height 1.1 :v-adjust -0.2)
       (doom-modeline-vspc)
       (propertize
        (if (> mu4e-alert-mode-line doom-modeline-number-limit)
            (format "%d+" doom-modeline-number-limit)
          (number-to-string mu4e-alert-mode-line))
        'face '(:inherit
                (doom-modeline-unread-number doom-modeline-notification))))
      'mouse-face 'mode-line-highlight
      'keymap '(mode-line keymap
                          (mouse-1 . mu4e-alert-view-unread-mails)
                          (mouse-2 . mu4e-alert-view-unread-mails)
                          (mouse-3 . mu4e-alert-view-unread-mails))
      'help-echo (concat (if (= mu4e-alert-mode-line 1)
                             "You have an unread email"
                           (format "You have %s unread emails" mu4e-alert-mode-line))
                         "\nClick here to view "
                         (if (= mu4e-alert-mode-line 1) "it" "them")))
     (doom-modeline-spc))))

(defun doom-modeline-override-mu4e-alert-modeline (&rest _)
  "Delete `mu4e-alert-mode-line' from global modeline string."
  (when (featurep 'mu4e-alert)
    (if (and doom-modeline-mu4e
             (bound-and-true-p doom-modeline-mode))
        ;; Delete original modeline
        (progn
          (setq global-mode-string
                (delete '(:eval mu4e-alert-mode-line) global-mode-string))
          (setq mu4e-alert-modeline-formatter #'identity))
      ;; Recover default settings
      (setq mu4e-alert-modeline-formatter #'mu4e-alert-default-mode-line-formatter))))
(advice-add #'mu4e-alert-enable-mode-line-display
            :after #'doom-modeline-override-mu4e-alert-modeline)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-mu4e-alert-modeline)


;;
;; `gnus' notifications
;;

(defvar doom-modeline--gnus-unread-mail 0)
(defvar doom-modeline--gnus-started nil
  "Used to determine if gnus has started.")
(defun doom-modeline-update-gnus-status (&rest _)
  "Get the total number of unread news of gnus group."
  (setq doom-modeline--gnus-unread-mail
        (when (and doom-modeline-gnus
                   doom-modeline--gnus-started)
          (let ((total-unread-news-number 0))
            (mapc (lambda (g)
                    (let* ((group (car g))
                           (unread (eval `(gnus-group-unread ,group))))
                      (when (and (not (seq-contains-p doom-modeline-gnus-excluded-groups group))
                                 (numberp unread)
                                 (> unread 0))
                        (setq total-unread-news-number (+ total-unread-news-number unread)))))
                  gnus-newsrc-alist)
            total-unread-news-number))))

;; Update the modeline after changes have been made
(add-hook 'gnus-group-update-hook #'doom-modeline-update-gnus-status)
(add-hook 'gnus-summary-update-hook #'doom-modeline-update-gnus-status)
(add-hook 'gnus-group-update-group-hook #'doom-modeline-update-gnus-status)
(add-hook 'gnus-after-getting-new-news-hook #'doom-modeline-update-gnus-status)

;; Only start to listen to gnus when gnus is actually running
(defun doom-modeline-start-gnus-listener ()
  "Start GNUS listener."
  (when (and doom-modeline-gnus
             (not doom-modeline--gnus-started))
    (setq doom-modeline--gnus-started t)
    ;; Scan gnus in the background if the timer is higher than 0
    (doom-modeline-update-gnus-status)
    (if (> doom-modeline-gnus-timer 0)
        (gnus-demon-add-handler 'gnus-demon-scan-news doom-modeline-gnus-timer nil))))
(add-hook 'gnus-started-hook #'doom-modeline-start-gnus-listener)

;; Stop the listener if gnus isn't running
(defun doom-modeline-stop-gnus-listener ()
  "Stop GNUS listener."
  (setq doom-modeline--gnus-started nil))
(add-hook 'gnus-exit-gnus-hook #'doom-modeline-stop-gnus-listener)

(doom-modeline-def-segment gnus
  "Show notifications of any unread emails in `gnus'."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p)
             doom-modeline-gnus
             doom-modeline--gnus-started
             ;; Don't display if the unread mails count is zero
             (numberp doom-modeline--gnus-unread-mail)
             (> doom-modeline--gnus-unread-mail 0))
    (concat
     (doom-modeline-spc)
     (propertize
      (concat
       (doom-modeline-icon 'material "email" "📧" "#"
                           :face 'doom-modeline-notification
                           :height 1.1 :v-adjust -0.2)
       (doom-modeline-vspc)
       (propertize
        (if (> doom-modeline--gnus-unread-mail doom-modeline-number-limit)
            (format "%d+" doom-modeline-number-limit)
          (number-to-string doom-modeline--gnus-unread-mail))
        'face '(:inherit
                (doom-modeline-unread-number doom-modeline-notification))))
      'mouse-face 'mode-line-highlight
      'help-echo (if (= doom-modeline--gnus-unread-mail 1)
                     "You have an unread email"
                   (format "You have %s unread emails" doom-modeline--gnus-unread-mail)))
     (doom-modeline-spc))))


;;
;; IRC notifications
;;

(defun doom-modeline--shorten-irc (name)
  "Wrapper for `tracking-shorten' and `erc-track-shorten-function' with NAME.

One key difference is that when `tracking-shorten' and
`erc-track-shorten-function' returns nil we will instead return the original
value of name. This is necessary in cases where the user has stylized the name
to be an icon and we don't want to remove that so we just return the original."
  (or (and (boundp 'tracking-shorten)
           (car (tracking-shorten (list name))))
      (and (boundp 'erc-track-shorten-function)
           (functionp erc-track-shorten-function)
	       (car (funcall erc-track-shorten-function (list name))))
      (and (boundp 'rcirc-short-buffer-name)
           (rcirc-short-buffer-name name))
      name))

(defun doom-modeline--tracking-buffers (buffers)
  "Logic to convert some irc BUFFERS to their font-awesome icon."
  (mapconcat
   (lambda (b)
     (propertize
      (doom-modeline--shorten-irc (funcall doom-modeline-irc-stylize b))
      'face '(:inherit (doom-modeline-unread-number doom-modeline-notification))
      'help-echo (format "IRC Notification: %s\nmouse-1: Switch to buffer" b)
      'mouse-face 'mode-line-highlight
      'local-map (make-mode-line-mouse-map 'mouse-1
                                           (lambda ()
                                             (interactive)
                                             (when (buffer-live-p (get-buffer b))
                                               (switch-to-buffer b))))))
   buffers
   (doom-modeline-vspc)))

(defun doom-modeline--circe-p ()
  "Check if `circe' is in use."
  (boundp 'tracking-mode-line-buffers))

(defun doom-modeline--erc-p ()
  "Check if `erc' is in use."
  (boundp 'erc-modified-channels-alist))

(defun doom-modeline--rcirc-p ()
  "Check if `rcirc' is in use."
  (bound-and-true-p rcirc-track-minor-mode))

(defun doom-modeline--get-buffers ()
  "Gets the buffers that have activity."
  (cond
   ((doom-modeline--circe-p)
    tracking-buffers)
   ((doom-modeline--erc-p)
    (mapcar (lambda (l)
              (buffer-name (car l)))
            erc-modified-channels-alist))
   ((doom-modeline--rcirc-p)
    (mapcar (lambda (b)
              (buffer-name b))
            rcirc-activity))))

;; Create a modeline segment that contains all the irc tracked buffers
(doom-modeline-def-segment irc-buffers
  "The list of shortened, unread irc buffers."
  (when (and doom-modeline-irc
             (doom-modeline--active)
             (not doom-modeline--limited-width-p))
    (let* ((buffers (doom-modeline--get-buffers))
           (number (length buffers)))
      (when (> number 0)
        (concat
         (doom-modeline-spc)
         (doom-modeline--tracking-buffers buffers)
         (doom-modeline-spc))))))

(doom-modeline-def-segment irc
  "A notification icon for any unread irc buffer."
  (when (and doom-modeline-irc
             (doom-modeline--active)
             (not doom-modeline--limited-width-p))
    (let* ((buffers (doom-modeline--get-buffers))
           (number (length buffers)))
      (when (> number 0)
        (concat
         (doom-modeline-spc)

         (propertize (concat
                      (doom-modeline-icon 'material "message" "🗊" "#"
                                          :face 'doom-modeline-notification
                                          :height 1.0 :v-adjust -0.225)
                      (doom-modeline-vspc)
                      ;; Display the number of unread buffers
                      (propertize (number-to-string number)
                                  'face '(:inherit
                                          (doom-modeline-unread-number
                                           doom-modeline-notification))))
                     'help-echo (format "IRC Notifications: %s\n%s"
                                        (mapconcat
                                         (lambda (b) (funcall doom-modeline-irc-stylize b))
                                         buffers
                                         ", ")
                                        (cond
                                         ((doom-modeline--circe-p)
                                          "mouse-1: Switch to previous unread buffer
mouse-3: Switch to next unread buffer")
                                         ((doom-modeline--erc-p)
                                          "mouse-1: Switch to buffer
mouse-3: Switch to next unread buffer")
                                         ((doom-modeline--rcirc-p)
                                          "mouse-1: Switch to server buffer
mouse-3: Switch to next unread buffer")))
                     'mouse-face 'mode-line-highlight
                     'local-map (let ((map (make-sparse-keymap)))
                                  (cond
                                   ((doom-modeline--circe-p)
                                    (define-key map [mode-line mouse-1]
                                      #'tracking-previous-buffer)
                                    (define-key map [mode-line mouse-3]
                                      #'tracking-next-buffer))
                                   ((doom-modeline--erc-p)
                                    (define-key map [mode-line mouse-1]
                                      #'erc-switch-to-buffer)
                                    (define-key map [mode-line mouse-3]
                                      #'erc-track-switch-buffer))
                                   ((doom-modeline--rcirc-p)
                                    (define-key map [mode-line mouse-1]
                                      #'rcirc-switch-to-server-buffer)
                                    (define-key map [mode-line mouse-3]
                                      #'rcirc-next-active-buffer)))
                                  map))

         ;; Display the unread irc buffers as well
         (when doom-modeline-irc-buffers
           (concat (doom-modeline-spc)
                   (doom-modeline--tracking-buffers buffers)))

         (doom-modeline-spc))))))

(defun doom-modeline-override-rcirc-modeline ()
  "Override default `rcirc' mode-line."
  (if (bound-and-true-p doom-modeline-mode)
      (setq global-mode-string
		    (delq 'rcirc-activity-string global-mode-string))
    (when (and rcirc-track-minor-mode
               (not (memq 'rcirc-activity-string global-mode-string)))
	  (setq global-mode-string
		    (append global-mode-string '(rcirc-activity-string))))))
(add-hook 'rcirc-track-minor-mode-hook #'doom-modeline-override-rcirc-modeline)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-rcirc-modeline)


;;
;; Battery status
;;

(defvar doom-modeline--battery-status nil)
(defun doom-modeline-update-battery-status ()
  "Update battery status."
  (setq doom-modeline--battery-status
        (when (bound-and-true-p display-battery-mode)
          (let* ((data (and battery-status-function
                            (functionp battery-status-function)
                            (funcall battery-status-function)))
                 (charging? (string-equal "AC" (cdr (assoc ?L data))))
                 (percentage (car (read-from-string (or (cdr (assq ?p data)) "ERR"))))
                 (valid-percentage? (and (numberp percentage)
                                         (>= percentage 0)
                                         (<= percentage battery-mode-line-limit)))
                 (face (if valid-percentage?
                           (cond (charging? 'doom-modeline-battery-charging)
                                 ((< percentage battery-load-critical) 'doom-modeline-battery-critical)
                                 ((< percentage 25) 'doom-modeline-battery-warning)
                                 ((< percentage 95) 'doom-modeline-battery-normal)
                                 (t 'doom-modeline-battery-full))
                         'doom-modeline-battery-error))
                 (icon (if valid-percentage?
                           (cond (charging?
                                  (doom-modeline-icon 'alltheicon "battery-charging" "🔋" "+"
                                                      :face face :height 1.4 :v-adjust -0.1))
                                 ((> percentage 95)
                                  (doom-modeline-icon 'faicon "battery-full" "🔋" "-"
                                                      :face face :v-adjust -0.0575))
                                 ((> percentage 70)
                                  (doom-modeline-icon 'faicon "battery-three-quarters" "🔋" "-"
                                                      :face face :v-adjust -0.0575))
                                 ((> percentage 40)
                                  (doom-modeline-icon 'faicon "battery-half" "🔋" "-"
                                                      :face face :v-adjust -0.0575))
                                 ((> percentage battery-load-critical)
                                  (doom-modeline-icon 'faicon "battery-quarter" "🔋" "-"
                                                      :face face :v-adjust -0.0575))
                                 (t (doom-modeline-icon 'faicon "battery-empty" "🔋" "!"
                                                        :face face :v-adjust -0.0575)))
                         (doom-modeline-icon 'faicon "battery-empty" "⚠" "N/A"
                                             :face face :v-adjust -0.0575)))
                 (text (if valid-percentage? (format "%d%%%%" percentage) ""))
                 (help-echo (if (and battery-echo-area-format data valid-percentage?)
                                (battery-format battery-echo-area-format data)
                              "Battery status not available")))
            (cons (propertize icon 'help-echo help-echo)
                  (propertize text 'face face 'help-echo help-echo))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-battery-status))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-battery-status))))))

(doom-modeline-def-segment battery
  "Display battery status."
  (when (and (doom-modeline--active)
             (not doom-modeline--limited-width-p)
             (bound-and-true-p display-battery-mode))
    (concat (doom-modeline-spc)
            (concat
             (car doom-modeline--battery-status)
             (doom-modeline-vspc)
             (cdr doom-modeline--battery-status))
            (doom-modeline-spc))))

(defun doom-modeline-override-battery-modeline ()
  "Override default battery mode-line."
  (if (bound-and-true-p doom-modeline-mode)
      (progn
        (advice-add #'battery-update :override #'doom-modeline-update-battery-status)
        (setq global-mode-string
		      (delq 'battery-mode-line-string global-mode-string))
        (and (bound-and-true-p display-battery-mode) (battery-update)))
    (progn
      (advice-remove #'battery-update #'doom-modeline-update-battery-status)
      (when (and display-battery-mode battery-status-function battery-mode-line-format
                 (not (memq 'battery-mode-line-string global-mode-string)))
        (setq global-mode-string
		      (append global-mode-string '(battery-mode-line-string)))))))
(add-hook 'display-battery-mode-hook #'doom-modeline-override-battery-modeline)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-battery-modeline)


;;
;; Package information
;;

(doom-modeline-def-segment package
  "Show package information via `paradox'."
  (let ((active (doom-modeline--active)))
    (concat
     (let ((front (format-mode-line 'mode-line-front-space)))
       (if active
           front
         (propertize front 'face 'mode-line-inactive)))

     (when (and doom-modeline-icon doom-modeline-major-mode-icon)
       (concat
        (doom-modeline-spc)
        (doom-modeline-icon 'faicon "archive" nil nil
                            :face (if active
                                      (if doom-modeline-major-mode-color-icon
                                          'all-the-icons-silver
                                        'mode-line)
                                    'mode-line-inactive)
                            :height 1.0
                            :v-adjust -0.0575)))
     (let ((info (format-mode-line 'mode-line-buffer-identification)))
       (if active
           info
         (propertize info 'face 'mode-line-inactive))))))


;;
;; Helm
;;

(defvar doom-modeline--helm-buffer-ids
  '(("*helm*" . "HELM")
    ("*helm M-x*" . "HELM M-x")
    ("*swiper*" . "SWIPER")
    ("*Projectile Perspectives*" . "HELM Projectile Perspectives")
    ("*Projectile Layouts*" . "HELM Projectile Layouts")
    ("*helm-ag*" . (lambda ()
                     (format "HELM Ag: Using %s"
                             (car (split-string helm-ag-base-command))))))
  "Alist of custom helm buffer names to use.
The cdr can also be a function that returns a name to use.")

(doom-modeline-def-segment helm-buffer-id
  "Helm session identifier."
  (when (bound-and-true-p helm-alive-p)
    (let ((active (doom-modeline--active)))
      (concat
       (doom-modeline-spc)
       (when doom-modeline-icon
         (concat
          (doom-modeline-icon 'fileicon "elisp" nil nil
                              :face (if active
                                        (if doom-modeline-major-mode-color-icon
                                            'all-the-icons-blue
                                          'mode-line)
                                      'mode-line-inactive)
                              :height 1.0
                              :v-adjust -0.15)
          (doom-modeline-spc)))
       (propertize
        (let ((custom (cdr (assoc (buffer-name) doom-modeline--helm-buffer-ids)))
              (case-fold-search t)
              (name (replace-regexp-in-string "-" " " (buffer-name))))
          (cond ((stringp custom) custom)
                ((functionp custom) (funcall custom))
                (t
                 (string-match "\\*helm:? \\(mode \\)?\\([^\\*]+\\)\\*" name)
                 (concat "HELM " (capitalize (match-string 2 name))))))
        'face (if active' doom-modeline-buffer-file 'mode-line-inactive))
       (doom-modeline-spc)))))

(doom-modeline-def-segment helm-number
  "Number of helm candidates."
  (when (bound-and-true-p helm-alive-p)
    (let ((active (doom-modeline--active)))
      (concat
       (propertize (format " %d/%d"
                           (helm-candidate-number-at-point)
                           (helm-get-candidate-number t))
                   'face (if active 'doom-modeline-buffer-path 'mode-line-inactive))
       (propertize (format " (%d total) " (helm-get-candidate-number))
                   'face (if active 'doom-modeline-info 'mode-line-inactive))))))

(doom-modeline-def-segment helm-help
  "Helm keybindings help."
  (when (bound-and-true-p helm-alive-p)
    (let ((active (doom-modeline--active)))
      (-interleave
       (mapcar (lambda (s)
                 (propertize (substitute-command-keys s)
                             'face (if active
                                       'doom-modeline-buffer-file
                                     'mode-line-inactive)))
               '("\\<helm-map>\\[helm-help]"
                 "\\<helm-map>\\[helm-select-action]"
                 "\\<helm-map>\\[helm-maybe-exit-minibuffer]/F1/F2..."))
       (mapcar (lambda (s)
                 (propertize s 'face (if active 'mode-line 'mode-line-inactive)))
               '("(help) " "(actions) " "(action) "))))))

(doom-modeline-def-segment helm-prefix-argument
  "Helm prefix argument."
  (when (and (bound-and-true-p helm-alive-p)
             helm--mode-line-display-prefarg)
    (let ((arg (prefix-numeric-value (or prefix-arg current-prefix-arg))))
      (unless (= arg 1)
        (propertize (format "C-u %s" arg)
                    'face (if (doom-modeline--active)
                              'doom-modeline-info
                            'mode-line-inactive))))))

(defvar doom-modeline--helm-current-source nil
  "The currently active helm source.")
(doom-modeline-def-segment helm-follow
  "Helm follow indicator."
  (when (and (bound-and-true-p helm-alive-p)
             doom-modeline--helm-current-source
             (eq 1 (cdr (assq 'follow doom-modeline--helm-current-source))))
    (propertize "HF" 'face (if (doom-modeline--active)
                               'mode-line
                             'mode-line-inactive))))

;;
;; Git timemachine
;;

(doom-modeline-def-segment git-timemachine
  (let ((active (doom-modeline--active)))
    (concat
     (doom-modeline-spc)
     (doom-modeline--buffer-mode-icon)
     (doom-modeline--buffer-state-icon)
     (propertize "*%b*" 'face (if active
                                  'doom-modeline-buffer-timemachine
                                'mode-line-inactive)))))

;;
;; Markdown/Org preview
;;

(doom-modeline-def-segment grip
  (when (bound-and-true-p grip-mode)
    (concat
     (doom-modeline-spc)
     (let ((face (if (doom-modeline--active)
                     (if grip--process
                         (pcase (process-status grip--process)
                           ('run 'doom-modeline-buffer-path)
                           ('exit 'doom-modeline-warning)
                           (_ 'doom-modeline-urgent))
                       'doom-modeline-urgent)
                   'mode-line-inactive)))
       (propertize (doom-modeline-icon 'material "pageview" "🗐" "@"
                                       :face (if doom-modeline-icon
                                                 `(:inherit ,face :weight normal)
                                               face)
                                       :height 1.2 :v-adjust -0.2)
                   'help-echo (format "Preview on %s
mouse-1: Preview in browser
mouse-2: Stop preview
mouse-3: Restart preview"
                                      (grip--preview-url))
                   'mouse-face 'mode-line-highlight
                   'local-map (let ((map (make-sparse-keymap)))
                                (define-key map [mode-line mouse-1]
                                  #'grip-browse-preview)
                                (define-key map [mode-line mouse-2]
                                  #'grip-stop-preview)
                                (define-key map [mode-line mouse-3]
                                  #'grip-restart-preview)
                                map)))
     (doom-modeline-spc))))

;;
;; Follow mode
;;

(doom-modeline-def-segment follow
  (when (bound-and-true-p follow-mode)
    (let* ((windows (follow-all-followers))
           (nwindows (length windows))
           (nfollowing (- (length (memq (selected-window) windows))
                          1)))
      (concat
       (doom-modeline-spc)
       (propertize (format "Follow %d/%d" (- nwindows nfollowing) nwindows)
                   'face 'doom-modeline-buffer-minor-mode)))))

(provide 'doom-modeline-segments)

;;; doom-modeline-segments.el ends here
