;;; doom-modeline-segments.el --- The segments for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2025 Vincent Zhang

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
;; The segments for doom-modeline.
;; Use `doom-modeline-def-segment' to create a new segment.
;;

;;; Code:

(require 'doom-modeline-core)
(require 'doom-modeline-env)
(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'subr-x))


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
(defvar display-time-string)
(defvar edebug-execution-mode)
(defvar eglot--managed-mode)
(defvar eglot-menu)
(defvar eglot-menu-string)
(defvar eglot-server-menu)
(defvar erc-modified-channels-alist)
(defvar evil-ex-active-highlights-alist)
(defvar evil-ex-argument)
(defvar evil-ex-range)
(defvar evil-mc-frozen)
(defvar evil-state)
(defvar evil-visual-beginning)
(defvar evil-visual-end)
(defvar evil-visual-selection)
(defvar flycheck--automatically-enabled-checkers)
(defvar flycheck-current-errors)
(defvar flycheck-mode-menu-map)
(defvar flymake--mode-line-format)
(defvar flymake--state)
(defvar flymake-menu)
(defvar gnus-newsrc-alist)
(defvar gnus-newsrc-hashtb)
(defvar grip--process)
(defvar helm--mode-line-display-prefarg)
(defvar iedit-occurrences-overlays)
(defvar kele-menu-map)
(defvar meow--indicator)
(defvar minions-mode-line-lighter)
(defvar minions-mode-line-minor-modes-map)
(defvar mlscroll-right-align)
(defvar mu4e--modeline-item)
(defvar mu4e-alert-mode-line)
(defvar mu4e-alert-modeline-formatter)
(defvar mu4e-modeline-mode)
(defvar objed--obj-state)
(defvar objed--object)
(defvar objed-modeline-setup-func)
(defvar persp-nil-name)
(defvar phi-replace--mode-line-format)
(defvar phi-search--overlays)
(defvar phi-search--selection)
(defvar phi-search-mode-line-format)
(defvar projectile-mode-map)
(defvar rcirc-activity)
(defvar symbol-overlay-keywords-alist)
(defvar symbol-overlay-temp-symbol)
(defvar text-scale-mode-amount)
(defvar tracking-buffers)
(defvar visual-replace--calling-buffer)
(defvar visual-replace--match-ovs)
(defvar winum-auto-setup-mode-line)
(defvar xah-fly-insert-state-p)

(declare-function anzu--reset-status "ext:anzu")
(declare-function anzu--where-is-here "ext:anzu")
(declare-function async-inject-variables "ext:async")
(declare-function async-start "ext:async")
(declare-function avy-traverse "ext:avy")
(declare-function avy-tree "ext:avy")
(declare-function aw-update "ext:ace-window")
(declare-function aw-window-list "ext:ace-window")
(declare-function battery-format "battery")
(declare-function battery-update "battery")
(declare-function boon-modeline-string "ext:boon")
(declare-function boon-state-string "ext:boon")
(declare-function cider--connection-info "ext:cider")
(declare-function cider-connected-p "ext:cider")
(declare-function cider-current-repl "ext:cider")
(declare-function cider-jack-in "ext:cider")
(declare-function cider-quit "ext:cider")
(declare-function citre-mode "ext:citre-basic-tools")
(declare-function compilation-goto-in-progress-buffer "compile")
(declare-function dap--cur-session "ext:dap-mode")
(declare-function dap--debug-session-name "ext:dap-mode")
(declare-function dap--debug-session-state "ext:dap-mode")
(declare-function dap--session-running "ext:dap-mode")
(declare-function dap-debug-recent "ext:dap-mode")
(declare-function dap-disconnect "ext:dap-mode")
(declare-function dap-hydra "ext:dap-hydra")
(declare-function edebug-help "edebug")
(declare-function edebug-next-mode "edebug")
(declare-function edebug-stop "edebug")
(declare-function eglot--major-modes "eglot")
(declare-function eglot--server-info "eglot" t t)
(declare-function eglot-current-server "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function eglot-project-nickname "eglot" t t)
(declare-function erc-switch-to-buffer "erc")
(declare-function erc-track-switch-buffer "erc-track")
(declare-function evil-delimited-arguments "ext:evil-common")
(declare-function evil-emacs-state-p "ext:evil-states" t t)
(declare-function evil-force-normal-state "ext:evil-commands" t t)
(declare-function evil-insert-state-p "ext:evil-states" t t)
(declare-function evil-motion-state-p "ext:evil-states" t t)
(declare-function evil-normal-state-p "ext:evil-states" t t)
(declare-function evil-operator-state-p "ext:evil-states" t t)
(declare-function evil-replace-state-p "ext:evil-states" t t)
(declare-function evil-state-property "ext:evil-common")
(declare-function evil-visual-state-p "ext:evil-states" t t)
(declare-function eyebrowse--get "ext:eyebrowse")
(declare-function face-remap-remove-relative "face-remap")
(declare-function fancy-narrow-active-p "ext:fancy-narrow")
(declare-function flycheck-buffer "ext:flycheck")
(declare-function flycheck-count-errors "ext:flycheck")
(declare-function flycheck-error-level-compilation-level "ext:flycheck")
(declare-function flycheck-list-errors "ext:flycheck")
(declare-function flycheck-next-error "ext:flycheck")
(declare-function flycheck-previous-error "ext:flycheck")
(declare-function flymake--diag-type "flymake" t t)
(declare-function flymake--handle-report "flymake")
(declare-function flymake--lookup-type-property "flymake")
(declare-function flymake--state-diags "flymake" t t)
(declare-function flymake-disabled-backends "flymake")
(declare-function flymake-goto-next-error "flymake")
(declare-function flymake-goto-prev-error "flymake")
(declare-function flymake-reporting-backends "flymake")
(declare-function flymake-running-backends "flymake")
(declare-function flymake-show-buffer-diagnostics "flymake")
(declare-function flymake-show-buffer-diagnostics "flymake")
(declare-function flymake-start "flymake")
(declare-function follow-all-followers "follow")
(declare-function gnus-demon-add-handler "gnus-demon")
(declare-function grip--preview-url "ext:grip-mode")
(declare-function grip-browse-preview "ext:grip-mode")
(declare-function grip-restart-preview "ext:grip-mode")
(declare-function grip-stop-preview "ext:grip-mode")
(declare-function helm-candidate-number-at-point "ext:helm-core")
(declare-function helm-get-candidate-number "ext:helm-core")
(declare-function iedit-find-current-occurrence-overlay "ext:iedit-lib")
(declare-function iedit-prev-occurrence "ext:iedit-lib")
(declare-function image-compute-scaling-factor "image")
(declare-function image-get-display-property "image-mode")
(declare-function jsonrpc--request-continuations "jsonrpc" t t)
(declare-function jsonrpc-last-error "jsonrpc" t t)
(declare-function jsonrpc-name "jsonrpc" t t)
(declare-function kele-current-context-name "ext:kele")
(declare-function kele-current-namespace "ext:kele")
(declare-function lsp--workspace-print "ext:lsp-mode")
(declare-function lsp-describe-session "ext:lsp-mode")
(declare-function lsp-workspace-folders-open "ext:lsp-mode")
(declare-function lsp-workspace-restart "ext:lsp-mode")
(declare-function lsp-workspace-shutdown "ext:lsp-mode")
(declare-function lsp-workspaces "ext:lsp-mode")
(declare-function lv-message "ext:lv")
(declare-function mc/num-cursors "ext:multiple-cursors-core")
(declare-function meow--current-state "ext:meow")
(declare-function meow-beacon-mode-p "ext:meow")
(declare-function meow-insert-mode-p "ext:meow")
(declare-function meow-keypad-mode-p "ext:meow")
(declare-function meow-motion-mode-p "ext:meow")
(declare-function meow-normal-mode-p "ext:meow")
(declare-function minions--prominent-modes "ext:minions")
(declare-function mlscroll-mode-line "ext:mlscroll")
(declare-function mu4e--modeline-string "ext:mu4e-modeline")
(declare-function mu4e-alert-default-mode-line-formatter "ext:mu4e-alert")
(declare-function mu4e-alert-enable-mode-line-display "ext:mu4e-alert")
(declare-function mu4e-alert-view-unread-mails "ext:mu4e")
(declare-function mu4e-bookmark-favorite "ext:mu4e-bookmarks")
(declare-function mu4e-search "ext:mu4e")
(declare-function nyan-create "ext:nyan-mode")
(declare-function org-edit-src-save "org-src")
(declare-function parrot-create "ext:parrot")
(declare-function pdf-cache-number-of-pages "ext:pdf-cache" t t)
(declare-function persp-add-buffer "ext:persp-mode")
(declare-function persp-contain-buffer-p "ext:persp-mode")
(declare-function persp-switch "ext:persp-mode")
(declare-function phi-search--initialize "ext:phi-search")
(declare-function poke-line-create "ext:poke-line")
(declare-function popup-create "ext:popup")
(declare-function popup-delete "ext:popup")
(declare-function project-name "project")
(declare-function projectile-project-name "ext:projectile")
(declare-function rcirc-next-active-buffer "rcirc")
(declare-function rcirc-short-buffer-name "rcirc")
(declare-function rcirc-switch-to-server-buffer "rcirc")
(declare-function rcirc-window-configuration-change "rcirc")
(declare-function rime--should-enable-p "ext:rime")
(declare-function rime--should-inline-ascii-p "ext:rime")
(declare-function sml-modeline-create "ext:sml-modeline")
(declare-function svg-circle "svg")
(declare-function svg-create "svg")
(declare-function svg-image "svg")
(declare-function svg-line "svg")
(declare-function symbol-overlay-assoc "ext:symbol-overlay")
(declare-function symbol-overlay-get-list "ext:symbol-overlay")
(declare-function symbol-overlay-get-symbol "ext:symbol-overlay")
(declare-function symbol-overlay-rename "ext:symbol-overlay")
(declare-function tab-bar--current-tab "tab-bar")
(declare-function tab-bar--current-tab-index "tab-bar")
(declare-function tracking-next-buffer "ext:tracking")
(declare-function tracking-previous-buffer "ext:tracking")
(declare-function tracking-shorten "ext:tracking")
(declare-function warning-numeric-level "warnings")
(declare-function window-numbering-clear-mode-line "ext:window-numbering")
(declare-function window-numbering-get-number-string "ext:window-numbering")
(declare-function window-numbering-install-mode-line "ext:window-numbering")
(declare-function winum--clear-mode-line "ext:winum")
(declare-function winum--install-mode-line "ext:winum")
(declare-function winum-get-number-string "ext:winum")



;;
;; Buffer information
;;

(defvar-local doom-modeline--buffer-file-icon nil)
(defun doom-modeline-update-buffer-file-icon (&rest _)
  "Update file icon in mode-line."
  (setq doom-modeline--buffer-file-icon
        (when (and doom-modeline-major-mode-icon
                   (doom-modeline-icon-displayable-p))
          (let ((icon (doom-modeline-icon-for-buffer)))
            (propertize (if (or (null icon) (symbolp icon))
                            (doom-modeline-icon 'faicon "nf-fa-file_o" nil nil
                                                :face 'nerd-icons-dsilver)
                          (doom-modeline-propertize-icon icon))
                        'help-echo (format "Major-mode: %s" (format-mode-line mode-name)))))))
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
Uses `nerd-icons-mdicon' to fetch the icon."
  (doom-modeline-icon 'mdicon icon unicode text :face face))

(defvar-local doom-modeline--buffer-file-state-icon nil)
(defun doom-modeline-update-buffer-file-state-icon (&rest _)
  "Update the buffer or file state in mode-line."
  (setq doom-modeline--buffer-file-state-icon
        (when doom-modeline-buffer-state-icon
          (ignore-errors
            (concat
             (cond ((not (or (and (buffer-file-name) (file-remote-p buffer-file-name))
                             (verify-visited-file-modtime (current-buffer))))
                    (doom-modeline-buffer-file-state-icon
                     "nf-md-reload_alert" "⟳" "%1*"
                     'doom-modeline-warning))
                   (buffer-read-only
                    (doom-modeline-buffer-file-state-icon
                     "nf-md-lock" "🔒" "%1*"
                     'doom-modeline-warning))
                   ((and buffer-file-name (buffer-modified-p)
                         doom-modeline-buffer-modification-icon)
                    (doom-modeline-buffer-file-state-icon
                     "nf-md-content_save_edit" "💾" "%1*"
                     'doom-modeline-warning))
                   ((and buffer-file-name
                         ;; Avoid freezing while connection is lost
                         (not (file-remote-p buffer-file-name))
                         (not (file-exists-p buffer-file-name)))
                    (doom-modeline-buffer-file-state-icon
                     "nf-md-cancel" "🚫" "!"
                     'doom-modeline-urgent))
                   (t ""))
             (when (or (buffer-narrowed-p)
                       (and (bound-and-true-p fancy-narrow-mode)
                            (fancy-narrow-active-p))
                       (bound-and-true-p dired-narrow-mode))
               (doom-modeline-buffer-file-state-icon
                "nf-md-unfold_less_horizontal" "↕" "><"
                'doom-modeline-warning)))))))

(defvar-local doom-modeline--buffer-file-name nil)
(defun doom-modeline-update-buffer-file-name (&rest _)
  "Update buffer file name in mode-line."
  (setq doom-modeline--buffer-file-name
        (ignore-errors
          (save-match-data
            (if buffer-file-name
                (doom-modeline-buffer-file-name)
              (propertize (buffer-name)
                          'face 'doom-modeline-buffer-file
                          'mouse-face 'doom-modeline-highlight
                          'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                          'local-map mode-line-buffer-identification-keymap))))))
(add-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'after-save-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'evil-insert-state-exit-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'Info-selection-hook #'doom-modeline-update-buffer-file-name)
(advice-add #'rename-buffer :after #'doom-modeline-update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'doom-modeline-update-buffer-file-name)
(advice-add #'pop-to-buffer :after #'doom-modeline-update-buffer-file-name)
(advice-add #'popup-create :after #'doom-modeline-update-buffer-file-name)
(advice-add #'popup-delete :after #'doom-modeline-update-buffer-file-name)
;; (advice-add #'primitive-undo :after #'doom-modeline-update-buffer-file-name)
;; (advice-add #'set-buffer-modified-p :after #'doom-modeline-update-buffer-file-name)

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (advice-add #'handle-switch-frame :after #'doom-modeline-update-buffer-file-name)
        (add-function :after after-focus-change-function #'doom-modeline-update-buffer-file-name))
    (progn
      (add-hook 'focus-in-hook #'doom-modeline-update-buffer-file-name)
      (add-hook 'focus-out-hook #'doom-modeline-update-buffer-file-name))))

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
    (when-let* ((icon (or doom-modeline--buffer-file-icon
                          (doom-modeline-update-buffer-file-icon))))
      (unless (string-empty-p icon)
        (concat
         (if doom-modeline-major-mode-color-icon
             (doom-modeline-display-icon icon)
           (doom-modeline-propertize-icon
            icon
            (doom-modeline-face)))
         (doom-modeline-vspc))))))

(defsubst doom-modeline--buffer-state-icon ()
  "The icon of the current buffer state."
  (when doom-modeline-buffer-state-icon
    (when-let* ((icon (doom-modeline-update-buffer-file-state-icon)))
      (unless (string-empty-p icon)
        (concat
         (doom-modeline-display-icon icon)
         (doom-modeline-vspc))))))

(defsubst doom-modeline--buffer-simple-name ()
  "The buffer simple name."
  (propertize (buffer-name)
              'face (doom-modeline-face
                     (if (and doom-modeline-highlight-modified-buffer-name
                              (buffer-modified-p))
                         'doom-modeline-buffer-modified
                       'doom-modeline-buffer-file))
              'mouse-face 'doom-modeline-highlight
              'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
              'local-map mode-line-buffer-identification-keymap))

(defsubst doom-modeline--buffer-name ()
  "The current buffer name."
  (when doom-modeline-buffer-name
    (if (and (not (eq doom-modeline-buffer-file-name-style 'file-name))
             doom-modeline--limited-width-p)
        ;; Only display the buffer name if the window is small, and doesn't
        ;; need to respect file-name style.
        (doom-modeline--buffer-simple-name)
      (when-let* ((name (or doom-modeline--buffer-file-name
                            (doom-modeline-update-buffer-file-name))))
        ;; Check if the buffer is modified
        (if (and doom-modeline-highlight-modified-buffer-name
                 (buffer-modified-p))
            (propertize name 'face (doom-modeline-face 'doom-modeline-buffer-modified))
          (doom-modeline-display-text name))))))

(doom-modeline-def-segment buffer-info
  "Combined information about the current buffer.

Including the current working directory, the file name, and its state (modified,
read-only or non-existent)."
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
   (doom-modeline--buffer-simple-name)))

(doom-modeline-def-segment calc
  "Display calculator icons and info."
  (concat
   (doom-modeline-spc)
   (when-let* ((icon (doom-modeline-icon 'faicon "nf-fa-calculator" "🖩" "")))
     (concat
      (doom-modeline-display-icon icon)
      (doom-modeline-vspc)))
   (doom-modeline--buffer-simple-name)))

(doom-modeline-def-segment buffer-default-directory
  "Displays `default-directory' with the icon and state.

This is for special buffers like the scratch buffer where knowing the current
project directory is important."
  (let ((face (doom-modeline-face
               (if (and buffer-file-name (buffer-modified-p))
                   'doom-modeline-buffer-modified
                 'doom-modeline-buffer-path))))
    (concat
     (doom-modeline-spc)
     (and doom-modeline-major-mode-icon
          (concat
           (doom-modeline-icon
            'octicon "nf-oct-file_directory_fill" "🖿" "" :face face)
           (doom-modeline-vspc)))
     (doom-modeline--buffer-state-icon)
     (propertize (abbreviate-file-name default-directory) 'face face))))

(doom-modeline-def-segment buffer-default-directory-simple
  "Displays `default-directory'.

This is for special buffers like the scratch buffer where knowing the current
project directory is important."
  (let ((face (doom-modeline-face 'doom-modeline-buffer-path)))
    (concat
     (doom-modeline-spc)
     (and doom-modeline-major-mode-icon
          (concat
           (doom-modeline-icon
            'octicon "nf-oct-file_directory_fill" "🖿" "" :face face)
           (doom-modeline-vspc)))
     (propertize (abbreviate-file-name default-directory) 'face face))))


;;
;; Encoding
;;

(doom-modeline-def-segment buffer-encoding
  "Displays the eol and the encoding style of the buffer."
  (when doom-modeline-buffer-encoding
    (let ((sep (doom-modeline-spc))
          (face (doom-modeline-face))
          (mouse-face 'doom-modeline-highlight))
      (concat
       sep

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

       sep))))


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
              'face (doom-modeline-face)))))
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
    (when-let* ((host (file-remote-p default-directory 'host)))
      (propertize
       (concat "@" host)
       'face (doom-modeline-face 'doom-modeline-host)))))


;;
;; Major mode
;;

(doom-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (let ((sep (doom-modeline-spc))
        (face (doom-modeline-face 'doom-modeline-buffer-major-mode)))
    (concat
     sep
     (propertize (concat
                  (format-mode-line
                   (or (and (boundp 'delighted-modes)
                            (cadr (assq major-mode delighted-modes)))
                       mode-name))
                  (when (and doom-modeline-env-version doom-modeline-env--version)
                    (format " %s" doom-modeline-env--version)))
                 'help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                 'face face
                 'mouse-face 'doom-modeline-highlight
                 'local-map mode-line-major-mode-keymap)
     (and (boundp 'text-scale-mode-amount)
          (/= text-scale-mode-amount 0)
          (propertize
           (format
            (if (> text-scale-mode-amount 0) " (%+d)" " (%-d)")
            text-scale-mode-amount)
           'face face))
     sep)))


;;
;; Process
;;

(doom-modeline-def-segment process
  "The process info."
  (doom-modeline-display-text
   (format-mode-line mode-line-process)))


;;
;; Minor modes
;;

(doom-modeline-def-segment minor-modes
  (when doom-modeline-minor-modes
    (let ((sep (doom-modeline-spc))
          (face (doom-modeline-face 'doom-modeline-buffer-minor-mode))
          (mouse-face 'doom-modeline-highlight)
          (help-echo "Minor mode
  mouse-1: Display minor mode menu
  mouse-2: Show help for minor mode
  mouse-3: Toggle minor modes"))
      (if (bound-and-true-p minions-mode)
          `((:propertize ("" ,(minions--prominent-modes))
             face ,face
		     mouse-face ,mouse-face
		     help-echo ,help-echo
		     local-map ,mode-line-minor-mode-keymap)
            ,sep
            (:propertize ("" ,(doom-modeline-icon 'octicon "nf-oct-gear" "⚙"
                                                  minions-mode-line-lighter
                                                  :face face))
             mouse-face ,mouse-face
             help-echo "Minions
mouse-1: Display minor modes menu"
             local-map ,minions-mode-line-minor-modes-map)
            ,sep)
        `((:propertize ("" minor-mode-alist)
           face ,face
           mouse-face ,mouse-face
           help-echo ,help-echo
           local-map ,mode-line-minor-mode-keymap)
          ,sep)))))


;;
;; VCS
;;

(defun doom-modeline-vcs-icon (icon &optional unicode text face)
  "Displays the vcs ICON with FACE and VOFFSET.

UNICODE and TEXT are fallbacks.
Uses `nerd-icons-octicon' to fetch the icon."
  (doom-modeline-icon 'devicon (and doom-modeline-vcs-icon icon)
                      unicode text :face face))

(defvar-local doom-modeline--vcs nil)
(defun doom-modeline-update-vcs (&rest _)
  "Update vcs state in mode-line."
  (setq doom-modeline--vcs
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend))
                 (icon (cond ((memq state '(edited added))
                              (doom-modeline-vcs-icon "nf-dev-git_compare" "🔃" "*" 'doom-modeline-info))
                             ((eq state 'needs-merge)
                              (doom-modeline-vcs-icon "nf-dev-git_merge" "🔀" "?" 'doom-modeline-info))
                             ((eq state 'needs-update)
                              (doom-modeline-vcs-icon "nf-dev-git_pull_request" "⬇" "!" 'doom-modeline-warning))
                             ((memq state '(removed conflict unregistered))
                              (doom-modeline-icon 'octicon "nf-oct-alert" "⚠" "!" :face 'doom-modeline-urgent))
                             (t (doom-modeline-vcs-icon "nf-dev-git_branch" "" "@" 'doom-modeline-info))))
                 (str (or (and vc-display-status
                               (functionp doom-modeline-vcs-display-function)
                               (funcall doom-modeline-vcs-display-function))
                          ""))
                 (face (or (cdr (assq state doom-modeline-vcs-state-faces-alist))
                           'doom-modeline-vcs-default))
                 (text (propertize (if (length> str doom-modeline-vcs-max-length)
                                       (concat
                                        (substring str 0 (- doom-modeline-vcs-max-length 3))
                                        doom-modeline-ellipsis)
                                     str)
                                   'face face)))
            `((icon . ,icon) (text . ,text))))))
(add-hook 'find-file-hook #'doom-modeline-update-vcs)
(add-hook 'after-save-hook #'doom-modeline-update-vcs)
(advice-add #'vc-refresh-state :after #'doom-modeline-update-vcs)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-vcs-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-vcs-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs))))))

(doom-modeline-add-variable-watcher
 'vc-display-status
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq vc-display-status val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs))))))

(doom-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (when doom-modeline--vcs
    (let-alist doom-modeline--vcs
      (let ((sep (doom-modeline-spc))
            (vsep (doom-modeline-vspc)))
        (concat sep
                (propertize (concat
                             (doom-modeline-display-icon .icon)
                             vsep
                             (doom-modeline-display-text .text))
                            'help-echo (get-text-property 1 'help-echo vc-mode)
                            'mouse-face 'doom-modeline-highlight
                            'local-map (get-text-property 1 'local-map vc-mode))
                sep)))))


;;
;; Check
;;

(defun doom-modeline-check-icon (icon unicode text face)
  "Displays the check ICON with FACE.

UNICODE and TEXT are fallbacks.
Uses `nerd-icons-mdicon' to fetch the icon."
  (doom-modeline-icon 'mdicon (and doom-modeline-check-icon icon)
                      unicode text :face face))

(defun doom-modeline-check-text (text &optional face)
  "Displays the check TEXT with FACE."
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

(defvar-local doom-modeline--flycheck nil)
(defun doom-modeline-update-flycheck (&optional status)
  "Update flycheck via STATUS."
  (setq doom-modeline--flycheck
        (let-alist (doom-modeline--flycheck-count-errors)
          (let* ((vsep (doom-modeline-vspc))
                 (seg (if doom-modeline-check-simple-format
                          (let ((count (+ .error .warning .info)))
                            (pcase status
                              ('finished    (if (> count 0)
                                                (let ((face (if (> .error 0) 'doom-modeline-urgent 'doom-modeline-warning)))
                                                  (concat
                                                   (doom-modeline-check-icon "nf-md-alert_circle_outline" "⚠" "!" face)
                                                   vsep
                                                   (doom-modeline-check-text (number-to-string count) face)))
                                              (doom-modeline-check-icon "nf-md-check_circle_outline" "✔" "*" 'doom-modeline-info)))
                              ('running     (concat
                                             (doom-modeline-check-icon "nf-md-timer_sand" "⏳" "*" 'doom-modeline-debug)
                                             (when (> count 0)
                                               (concat
                                                vsep
                                                (doom-modeline-check-text (number-to-string count) 'doom-modeline-debug)))))
                              ('no-checker  (doom-modeline-check-icon "nf-md-alert_box_outline" "⚠" "-" 'doom-modeline-debug))
                              ('errored     (doom-modeline-check-icon "nf-md-alert_box_outline" "⚠" "!" 'doom-modeline-urgent))
                              ('interrupted (doom-modeline-check-icon "nf-md-pause_circle_outline" "⦷" "." 'doom-modeline-debug))
                              ('suspicious  (doom-modeline-check-icon "nf-md-file_question_outline" "❓" "?" 'doom-modeline-debug))
                              (_ "")))
                        (concat (doom-modeline-check-icon "nf-md-close_circle_outline" "⮾" "!" 'doom-modeline-urgent)
                                vsep
                                (doom-modeline-check-text (number-to-string .error) 'doom-modeline-urgent)
                                vsep
                                (doom-modeline-check-icon "nf-md-alert_outline" "⚠" "!" 'doom-modeline-warning)
                                vsep
                                (doom-modeline-check-text (number-to-string .warning) 'doom-modeline-warning)
                                vsep
                                (doom-modeline-check-icon "nf-md-information_outline" "🛈" "!" 'doom-modeline-info)
                                vsep
                                (doom-modeline-check-text (number-to-string .info) 'doom-modeline-info)))))
            (propertize seg
                        'help-echo (concat "Flycheck\n"
                                           (pcase status
                                             ('finished (format "error: %d, warning: %d, info: %d" .error .warning .info))
                                             ('running "Checking...")
                                             ('no-checker "No Checker")
                                             ('errored "Error")
                                             ('interrupted "Interrupted")
                                             ('suspicious "Suspicious"))
                                           "\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode")
                        'mouse-face 'doom-modeline-highlight
                        'local-map (let ((map (make-sparse-keymap)))
                                     (define-key map [mode-line down-mouse-1]
                                       flycheck-mode-menu-map)
                                     (define-key map [mode-line mouse-2]
                                       (lambda ()
                                         (interactive)
                                         (describe-function 'flycheck-mode)))
                                     map))))))
(add-hook 'flycheck-status-changed-functions #'doom-modeline-update-flycheck)
(add-hook 'flycheck-mode-hook #'doom-modeline-update-flycheck)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flycheck-mode)
           (doom-modeline-update-flycheck)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-check-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-check-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flycheck-mode)
           (doom-modeline-update-flycheck)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flycheck-mode)
           (doom-modeline-update-flycheck)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-check-simple-format
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-check-simple-format val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flycheck-mode)
           (doom-modeline-update-flycheck)))))))

;; Flymake

;; Compatibility
;; @see https://github.com/emacs-mirror/emacs/commit/6e100869012da9244679696634cab6b9cac96303.
(with-eval-after-load 'flymake
  (unless (boundp 'flymake--state)
    (defvaralias 'flymake--state 'flymake--backend-state))
  (unless (fboundp 'flymake--state-diags)
    (defalias 'flymake--state-diags 'flymake--backend-state-diags)))

(defun doom-modeline--flymake-count-errors ()
  "Count the number of ERRORS, grouped by level."
  (let ((warning-level (warning-numeric-level :warning))
        (note-level (warning-numeric-level :debug))
        (note 0) (warning 0) (error 0))
    (maphash (lambda (_b state)
               (cl-loop
                with diags = (flymake--state-diags state)
                for diag in diags do
                (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                               (warning-numeric-level :error))))
                  (cond ((> severity warning-level) (cl-incf error))
                        ((> severity note-level) (cl-incf warning))
                        (t (cl-incf note))))))
             flymake--state)
    `((note . ,note) (warning . ,warning) (error . ,error))))

(defvar-local doom-modeline--flymake nil)
(defun doom-modeline-update-flymake (&rest _)
  "Update flymake."
  (setq doom-modeline--flymake
        (let* ((known (hash-table-keys flymake--state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported)))
          (let-alist (doom-modeline--flymake-count-errors)
            (let* ((vsep (doom-modeline-vspc))
                   (seg (if doom-modeline-check-simple-format
                            (let ((count (+ .error .warning .note)))
                              (cond
                               (some-waiting (concat
                                              (doom-modeline-check-icon "nf-md-timer_sand" "⏳" "*" 'doom-modeline-debug)
                                              (when (> count 0)
                                                (concat
                                                 vsep
                                                 (doom-modeline-check-text (number-to-string count) 'doom-modeline-debug)))))
                               ((null known) (doom-modeline-check-icon "nf-md-alert_box_outline" "⚠" "!" 'doom-modeline-urgent))
                               (all-disabled (doom-modeline-check-icon "nf-md-alert_box_outline" "⚠" "!" 'doom-modeline-warning))
                               (t (if (> count 0)
                                      (let ((face (cond ((> .error 0) 'doom-modeline-urgent)
                                                        ((> .warning 0) 'doom-modeline-warning)
                                                        (t 'doom-modeline-info))))
                                        (concat
                                         (doom-modeline-check-icon "nf-md-alert_circle_outline" "⚠" "!" face)
                                         vsep
                                         (doom-modeline-check-text (number-to-string count) face)))
                                    (doom-modeline-check-icon "nf-md-check_circle_outline" "✔" "*" 'doom-modeline-info)))))
                          (concat
                           (doom-modeline-check-icon "nf-md-close_circle_outline" "⮾" "!" 'doom-modeline-urgent)
                           vsep
                           (doom-modeline-check-text (number-to-string .error) 'doom-modeline-urgent)
                           vsep
                           (doom-modeline-check-icon "nf-md-alert_outline" "⚠" "!" 'doom-modeline-warning)
                           vsep
                           (doom-modeline-check-text (number-to-string .warning) 'doom-modeline-warning)
                           vsep
                           (doom-modeline-check-icon "nf-md-information_outline" "🛈" "!" 'doom-modeline-info)
                           vsep
                           (doom-modeline-check-text (number-to-string .note) 'doom-modeline-info)))))
              (propertize
               seg
               'help-echo (concat
                           "Flymake\n"
                           (cond (some-waiting "Checking...")
                                 ((null known) "No Checker")
                                 (all-disabled "All Checkers Disabled")
                                 (t (format "%d/%d backends running\nerror: %d, warning: %d, note: %d"
                                            (length running) (length known) .error .warning .note)))
                           "\nmouse-1: Display minor mode menu\nmouse-2: Show help for minor mode")
               'mouse-face 'doom-modeline-highlight
               'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line down-mouse-1]
                              flymake-menu)
                            (define-key map [mode-line mouse-2]
                              (lambda ()
                                (interactive)
                                (describe-function 'flymake-mode)))
                            map)))))))
(advice-add #'flymake--handle-report :after #'doom-modeline-update-flymake)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (doom-modeline-update-flymake)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-check-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-check-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (doom-modeline-update-flymake)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (doom-modeline-update-flymake)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-check-simple-format
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-check-simple-format val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (doom-modeline-update-flymake)))))))

(doom-modeline-def-segment check
  "Displays color-coded error status in the current buffer with pretty icons."
  (when-let* ((sep (doom-modeline-spc))
              (vsep (doom-modeline-vspc))
              (seg (cond
                    ((and (bound-and-true-p flymake-mode)
                          (bound-and-true-p flymake--state)) ; only support 26+
                     doom-modeline--flymake)
                    ((and (bound-and-true-p flycheck-mode)
                          (bound-and-true-p flycheck--automatically-enabled-checkers))
                     doom-modeline--flycheck))))
    (concat
     sep
     (let ((str))
       (dolist (s (split-string seg " "))
         (setq str
               (concat str
                       (if (string-match-p "^[0-9]+$" s)
                           (concat vsep
                                   (doom-modeline-display-text s)
                                   vsep)
                         (doom-modeline-display-icon s)))))
       (propertize str
                   'help-echo (get-text-property 0 'help-echo seg)
                   'mouse-face 'doom-modeline-highlight
                   'local-map (get-text-property 0 'local-map seg)))
     sep)))


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
                'face (doom-modeline-face))))


;;
;; Selection
;;

(defsubst doom-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(doom-modeline-def-segment selection-info
  "Information about the current selection.

Such as how many characters and lines are selected, or the NxM dimensions of a
block selection."
  (when (and (or mark-active (and (bound-and-true-p evil-local-mode)
                                  (eq evil-state 'visual)))
             (doom-modeline--active))
    (cl-destructuring-bind (beg . end)
      (if (and (bound-and-true-p evil-local-mode) (eq evil-state 'visual))
          (cons evil-visual-beginning evil-visual-end)
        (cons (region-beginning) (region-end)))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat
          " "
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
          " "))
       'face 'doom-modeline-emphasis))))


;;
;; Matches (macro, anzu, evil-substitute, iedit, symbol-overlay and multi-cursors)
;;

(defsubst doom-modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (doom-modeline--active)
             (or defining-kbd-macro executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-panel))
          (vsep (propertize " " 'face
                            '(:inherit (doom-modeline-panel variable-pitch))))
          (macro-name (if (bound-and-true-p evil-this-macro)
                          (format " @%s "
                                  (char-to-string evil-this-macro))
                        "Macro")))
      (concat
       sep
       (if doom-modeline-always-show-macro-register
           (propertize macro-name 'face 'doom-modeline-panel)
         (concat
          (doom-modeline-icon 'mdicon "nf-md-record" "●"
                              macro-name
                              :face '(:inherit (doom-modeline-urgent doom-modeline-panel))
                              :v-adjust 0.15)
          vsep
          (doom-modeline-icon 'mdicon "nf-md-menu_right" "▶" ">"
                              :face 'doom-modeline-panel
                              :v-adjust 0.15)))
       sep))))

;; `anzu' and `evil-anzu' expose current/total state that can be displayed in the
;; mode-line.
(defun doom-modeline-fix-anzu-count (positions here)
  "Calulate anzu count via POSITIONS and HERE."
  (cl-loop with i = 0
           for (start . end) in positions
           do (cl-incf i)
           when (and (>= here start) (<= here end))
           return i
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
     'face (doom-modeline-face 'doom-modeline-panel))))

(defsubst doom-modeline--visual-replace ()
  "Show the match index and total number of `visual-replace'.
It respects `visual-replace-display-total'."
  (when (and (bound-and-true-p visual-replace--match-ovs)
             (not (bound-and-true-p iedit-mode))
             (fboundp 'visual-replace--preview-is-complete)
             (visual-replace--preview-is-complete))
    (propertize
     (let ((total (length visual-replace--match-ovs)))
       (format " %s "
               (if-let* ((ov
                          (seq-find
                           (lambda (ov) (overlay-get ov 'visual-replace-idx))
                           (with-current-buffer visual-replace--calling-buffer
                             (overlays-at (point))))))
                   (format "%d/%d" (1+ (overlay-get ov 'visual-replace-idx)) total)
                 (number-to-string total))))
     'face (doom-modeline-face 'doom-modeline-panel))))

(defsubst doom-modeline--evil-substitute ()
  "Show number of matches for `evil-ex' in real time.
The number of matches contains substitutions and highlightings."
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
     'face (doom-modeline-face 'doom-modeline-panel))))

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
     'face (doom-modeline-face 'doom-modeline-panel))))

(defsubst doom-modeline--symbol-overlay ()
  "Show the number of matches for symbol overlay."
  (when (and (doom-modeline--active)
             (bound-and-true-p symbol-overlay-keywords-alist)
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
           'face (doom-modeline-face 'doom-modeline-panel))))))

(defsubst doom-modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (cl-destructuring-bind (count . face)
    (cond ((bound-and-true-p multiple-cursors-mode)
           (cons (mc/num-cursors)
                 (doom-modeline-face 'doom-modeline-panel)))
          ((bound-and-true-p evil-mc-cursor-list)
           (cons (length evil-mc-cursor-list)
                 (doom-modeline-face (if evil-mc-frozen
                                         'doom-modeline-bar
                                       'doom-modeline-panel))))
          ((cons nil nil)))
    (when count
      (concat (propertize " " 'face face)
              (if (doom-modeline-icon-displayable-p)
                  (doom-modeline-icon 'faicon "nf-fa-i_cursor" "" "" :face face)
                (propertize "I"
                            'face `(:inherit ,face :height 1.4 :weight normal)
                            'display '(raise -0.1)))
              (propertize " "
                          'face `(:inherit (variable-pitch ,face)))
              (propertize (format "%d " count)
                          'face face)))))

(defsubst doom-modeline--phi-search ()
  "Show the number of matches for `phi-search' and `phi-replace'."
  (when (and (doom-modeline--active)
             (bound-and-true-p phi-search--overlays))
    (let ((total (length phi-search--overlays))
          (selection phi-search--selection))
      (when selection
        (propertize
         (format " %d/%d " (1+ selection) total)
         'face (doom-modeline-face 'doom-modeline-panel))))))

(defun doom-modeline--override-phi-search (orig-fun &rest args)
  "Override the mode-line of `phi-search' and `phi-replace'.
Apply ORIG-FUN with ARGS."
  (if (bound-and-true-p doom-modeline-mode)
      (apply orig-fun mode-line-format (cdr args))
    (apply orig-fun args)))
(advice-add #'phi-search--initialize :around #'doom-modeline--override-phi-search)

(defsubst doom-modeline--buffer-size ()
  "Show buffer size."
  (when size-indication-mode
    (let ((sep (doom-modeline-spc)))
      (concat sep
              (propertize "%I"
                          'face (doom-modeline-face)
                          'help-echo "Buffer size
mouse-1: Display Line and Column Mode Menu"
                          'mouse-face 'doom-modeline-highlight
                          'local-map mode-line-column-line-number-mode-map)
              sep))))

(doom-modeline-def-segment matches
  "Displays matches.

Including:
1. the currently recording macro, 2. A current/total for the
current search term (with `anzu'), 3. The number of substitutions being
conducted with `evil-ex-substitute', and/or 4. The number of active `iedit'
regions, 5. The current/total for the highlight term (with `symbol-overlay'),
6. The number of active `multiple-cursors'."
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--anzu)
                      (doom-modeline--visual-replace)
                      (doom-modeline--phi-search)
                      (doom-modeline--evil-substitute)
                      (doom-modeline--iedit)
                      (doom-modeline--symbol-overlay)
                      (doom-modeline--multiple-cursors))))
    (or (and (not (string-empty-p meta)) meta)
        (doom-modeline--buffer-size))))

(doom-modeline-def-segment buffer-size
  "Display buffer size."
  (doom-modeline--buffer-size))

;;
;; Media
;;

(doom-modeline-def-segment media-info
  "Metadata regarding the current file, such as dimensions for images."
  ;; TODO: Include other information
  (cond ((eq major-mode 'image-mode)
         (cl-destructuring-bind (width . height)
           (when (fboundp 'image-size)
             (image-size (image-get-display-property) :pixels))
           (format "  %dx%d  " width height)))))


;;
;; Bars
;;

(defvar doom-modeline--bar-active nil)
(defvar doom-modeline--bar-inactive nil)

(defsubst doom-modeline--bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and doom-modeline--bar-active doom-modeline--bar-inactive)
    (let ((width doom-modeline-bar-width)
          (height (max doom-modeline-height (doom-modeline--font-height))))
      (setq doom-modeline--bar-active
            (doom-modeline--create-bar-image 'doom-modeline-bar width height)
            doom-modeline--bar-inactive
            (doom-modeline--create-bar-image
             'doom-modeline-bar-inactive width height))))
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
         (height (max doom-modeline-height (doom-modeline--font-height)))
         (top-margin (if (zerop bs)
                         0
                       (/ (* height (1- ws)) bs)))
         (bottom-margin (if (zerop bs)
                            0
                          (max 0 (/ (* height (- bs we 1)) bs))))
         (cache (or (window-parameter nil 'doom-modeline--hud-cache)
                    (set-window-parameter
                     nil
                     'doom-modeline--hud-cache
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

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym _val op _where)
   (when (eq op 'set)
     (doom-modeline-refresh-bars)
     (doom-modeline-invalidate-huds))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym _val op _where)
   (when (eq op 'set)
     (doom-modeline-refresh-bars)
     (doom-modeline-invalidate-huds))))

(add-hook 'window-configuration-change-hook #'doom-modeline-refresh-bars)
(add-hook 'window-configuration-change-hook #'doom-modeline-invalidate-huds)

(doom-modeline-def-segment bar
  "The bar regulates the height of the `doom-modeline' in GUI."
  (when (display-graphic-p)
    (concat
     (if doom-modeline-hud
         (doom-modeline--hud)
       (doom-modeline--bar))
     (doom-modeline-spc))))

(doom-modeline-def-segment hud
  "Powerline's hud segment reimplemented in the style of bar segment."
  (when (display-graphic-p)
    (concat
     (doom-modeline--hud)
     (doom-modeline-spc))))


;;
;; Window state
;;

;; Port from Emacs 30
(unless (fboundp 'toggle-window-dedicated)
  (defun toggle-window-dedicated (&optional window flag interactive)
    "Toggle whether WINDOW is dedicated to its current buffer.

WINDOW must be a live window and defaults to the selected one.
If FLAG is t (interactively, the prefix argument), make the window
\"strongly\" dedicated to its buffer.  FLAG defaults to a non-nil,
non-t value, and is passed to `set-window-dedicated-p', which see.
If INTERACTIVE is non-nil, print a message describing the dedication
status of WINDOW, after toggling it.  Interactively, this argument is
always non-nil.

When a window is dedicated to its buffer, `display-buffer' will avoid
displaying another buffer in it, if possible.  When a window is
strongly dedicated to its buffer, changing the buffer shown in the
window will usually signal an error.

You can control the default of FLAG with
`toggle-window-dedicated-flag'.  Consequently, if you set that
variable to t, strong dedication will be used by default and
\\[universal-argument] will make the window weakly dedicated.

See the info node `(elisp)Dedicated Windows' for more details."
    (interactive "i\nP\np")
    (setq window (window-normalize-window window))
    (setq flag (cond
                ((consp flag)
                 (if (eq toggle-window-dedicated-flag t)
                     'interactive
                   t))
                ((null flag) toggle-window-dedicated-flag)
                (t flag)))
    (if (window-dedicated-p window)
        (set-window-dedicated-p window nil)
      (set-window-dedicated-p window flag))
    (when interactive
      (message "Window is %s dedicated to buffer %s"
               (let ((status (window-dedicated-p window)))
                 (cond
                  ((null status) "no longer")
                  ((eq status t) "now strongly")
                  (t "now")))
               (current-buffer))
      (force-mode-line-update)))

  (defvar mode-line-window-dedicated-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line mouse-1] #'toggle-window-dedicated)
      (purecopy map)) "\
Keymap for what is displayed by `mode-line-window-dedicated'."))

(doom-modeline-def-segment window-state
  (let ((face (if (doom-modeline--active)
                  'doom-modeline-emphasis
                'doom-modeline)))
    (cond
     ((eq (window-dedicated-p) t)
      (propertize
       (format " %s " (doom-modeline-icon 'mdicon "nf-md-pin" "⍑" "D" :face face))
       'help-echo "Window strongly dedicated to its buffer\nmouse-1: Toggle"
       'local-map mode-line-window-dedicated-keymap
       'mouse-face 'mode-line-highlight))
     ((window-dedicated-p)
      (propertize
       (format " %s " (doom-modeline-icon 'mdicon "nf-md-pin_outline" "⊤" "d" :face face))
       'help-echo "Window dedicated to its buffer\nmouse-1: Toggle"
       'local-map mode-line-window-dedicated-keymap
       'mouse-face 'mode-line-highlight))
     (t ""))))


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
    (when (and (length> num 0)
               (length> (cl-mapcan
                         (lambda (frame)
                           ;; Exclude minibuffer, tooltip and child frames
                           (unless (or (and (fboundp 'frame-parent) (frame-parent frame))
                                       (string= (frame-parameter frame 'name)
                                                (alist-get 'name (bound-and-true-p tooltip-frame-parameters))))
                             (window-list frame 'never)))
                         (visible-frame-list))
                        1))
      (propertize (format " %s " num)
                  'face (doom-modeline-face 'doom-modeline-buffer-major-mode)))))

;;
;; Project
;;

(defvar doom-modeline-project-map
  (cond
   ((and (memq doom-modeline-project-detection '(auto projectile))
         (bound-and-true-p projectile-mode))
    projectile-mode-map)
   ((and (memq doom-modeline-project-detection '(auto project))
         (fboundp 'project-current))
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1]
        (bound-and-true-p menu-bar-project-item))
      map))))

(defvar-local doom-modeline--project-name nil)
(defun doom-modeline-project-name ()
  "Get the project name."
  (or doom-modeline--project-name
      (setq doom-modeline--project-name
            (let ((name (cond
                         ((and (memq doom-modeline-project-detection '(auto projectile))
                               (bound-and-true-p projectile-mode))
                          (projectile-project-name))
                         ((and (memq doom-modeline-project-detection '(auto project))
                               (fboundp 'project-current))
                          (when-let* ((project (project-current)))
                            (project-name project))))))
              (if (and name (not (string-empty-p name)))
                  (format " [%s] " name)
                "")))))

(doom-modeline-add-variable-watcher
 'doom-modeline-project-detection
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-project-detection val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (setq doom-modeline--project-name nil)
         (and buffer-file-name (revert-buffer t t)))))))

(doom-modeline-def-segment project-name
  "The current perspective name."
  (when (and doom-modeline-project-name
             (doom-modeline--segment-visible 'project-name))
    (propertize (doom-modeline-project-name)
                'face (doom-modeline-face 'doom-modeline-project-name)
                'mouse-face 'mode-line-highlight
                'help-echo "mouse-1: Project menu"
                'local-map doom-modeline-project-map)))


;;
;; Workspace
;;

(doom-modeline-def-segment workspace-name
  "The current workspace name or number.
Requires `eyebrowse-mode' to be enabled or `tab-bar-mode' tabs to be created."
  (when doom-modeline-workspace-name
    (when-let*
        ((name (cond
                ((and (bound-and-true-p eyebrowse-mode)
                      (length> (eyebrowse--get 'window-configs) 1))
                 (setq mode-line-misc-info
                       (assq-delete-all 'eyebrowse-mode mode-line-misc-info))
                 (when-let*
                     ((num (eyebrowse--get 'current-slot))
                      (tag (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                   (if (length> tag 0) tag (int-to-string num))))
                ((and (fboundp 'tab-bar-mode)
                      (length> (frame-parameter nil 'tabs) 1))
                 (let* ((current-tab (tab-bar--current-tab))
                        (tab-index (tab-bar--current-tab-index))
                        (explicit-name (alist-get 'explicit-name current-tab))
                        (tab-name (alist-get 'name current-tab)))
                   (if explicit-name tab-name (+ 1 tab-index)))))))
      (propertize (format " %s " name)
                  'face (doom-modeline-face 'doom-modeline-workspace-name)))))


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
                 (icon (doom-modeline-icon 'octicon "nf-oct-repo" "🖿" "#"
                                           :face `(:inherit ,face :slant normal))))
            (when (or doom-modeline-display-default-persp-name
                      (not (string-equal persp-nil-name name)))
              (concat " "
                      (propertize (concat (and doom-modeline-persp-icon
                                               (concat icon (doom-modeline-vspc)))
                                          (propertize name 'face face))
                                  'help-echo "mouse-1: Switch perspective
mouse-2: Show help for minor mode"
                                  'mouse-face 'doom-modeline-highlight
                                  'local-map (let ((map (make-sparse-keymap)))
                                               (define-key map [mode-line mouse-1]
                                                 #'persp-switch)
                                               (define-key map [mode-line mouse-2]
                                                 (lambda ()
                                                   (interactive)
                                                   (describe-function 'persp-mode)))
                                               map))
                      " "))))))

(add-hook 'buffer-list-update-hook #'doom-modeline-update-persp-name)
(add-hook 'find-file-hook #'doom-modeline-update-persp-name)
(add-hook 'persp-activated-functions #'doom-modeline-update-persp-name)
(add-hook 'persp-renamed-functions #'doom-modeline-update-persp-name)
(advice-add #'lv-message :after #'doom-modeline-update-persp-name)

(doom-modeline-def-segment persp-name
  "The current perspective name."
  (when (doom-modeline--segment-visible 'persp-name)
    doom-modeline--persp-name))


;;
;; Misc info
;;

(doom-modeline-def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (when (or doom-modeline-display-misc-in-all-mode-lines
            (doom-modeline--segment-visible 'misc-info))
    (doom-modeline-display-text (format-mode-line mode-line-misc-info))))


;;
;; Position
;;

(doom-modeline-def-segment buffer-position
  "The buffer position information."
  (let ((visible (doom-modeline--segment-visible 'buffer-position))
        (sep (doom-modeline-spc))
        (wsep (doom-modeline-wspc))
        (face (doom-modeline-face))
        (help-echo "Buffer percentage\n\
mouse-1: Display Line and Column Mode Menu")
        (mouse-face 'doom-modeline-highlight)
        (local-map mode-line-column-line-number-mode-map))
    `(,wsep

      ;; Line and column
      (:propertize
       ((line-number-mode
         (column-number-mode
          (doom-modeline-column-zero-based
           doom-modeline-position-column-line-format
           ,(string-replace
             "%c" "%C" (car doom-modeline-position-column-line-format)))
          doom-modeline-position-line-format)
         (column-number-mode
          (doom-modeline-column-zero-based
           doom-modeline-position-column-format
           ,(string-replace
             "%c" "%C" (car doom-modeline-position-column-format)))))
        (doom-modeline-total-line-number
         ,(and doom-modeline-total-line-number
               (format "/%d" (line-number-at-pos (point-max))))))
       face ,face
       help-echo ,help-echo
       mouse-face ,mouse-face
       local-map ,local-map)

      ((or line-number-mode column-number-mode)
       ,sep)

      ;; Position
      (,visible
       ,(cond
         ((bound-and-true-p nyan-mode)
          (concat sep (nyan-create) sep))
         ((bound-and-true-p poke-line-mode)
          (concat sep (poke-line-create) sep))
         ((bound-and-true-p mlscroll-mode)
          (concat sep
                  (let ((mlscroll-right-align nil))
                    (format-mode-line (mlscroll-mode-line)))
                  sep))
         ((bound-and-true-p sml-modeline-mode)
          (concat sep (sml-modeline-create) sep))
         (t "")))

      ;; Percent position
      (doom-modeline-percent-position
       ((:propertize ("" doom-modeline-percent-position)
         face ,face
         help-echo ,help-echo
         mouse-face ,mouse-face
         local-map ,local-map)
        ,sep)))))

;;
;; Party parrot
;;
(doom-modeline-def-segment parrot
  "The party parrot animated icon. Requires `parrot-mode' to be enabled."
  (when (and (doom-modeline--segment-visible 'parrot)
             (bound-and-true-p parrot-mode))
    (concat (doom-modeline-wspc)
            (parrot-create)
            (doom-modeline-spc))))

;;
;; Modals (evil, overwrite, god, ryo and xah-fly-keys, etc.)
;;

(defun doom-modeline--modal-icon (text face help-echo &optional icon unicode)
  "Display the model icon with FACE and HELP-ECHO.
TEXT is alternative if icon is not available."
  (propertize (doom-modeline-icon
               'mdicon
               (and doom-modeline-modal-icon
                    (or (and doom-modeline-modal-modern-icon icon)
                        "nf-md-record"))
               (or (and doom-modeline-modal-modern-icon unicode) "●")
               text
               :face (doom-modeline-face face))
              'help-echo help-echo))

(defsubst doom-modeline--evil ()
  "The current evil state. Requires `evil-mode' to be enabled."
  (when (bound-and-true-p evil-local-mode)
    (let-alist (cond
                ((evil-normal-state-p)   '((face    . doom-modeline-evil-normal-state)
                                           (icon    . "nf-md-alpha_n_circle")
                                           (unicode . "🅝")))
                ((evil-emacs-state-p)    '((face    . doom-modeline-evil-emacs-state)
                                           (icon    . "nf-md-alpha_e_circle")
                                           (unicode . "🅔")))
                ((evil-insert-state-p)   '((face    . doom-modeline-evil-insert-state)
                                           (icon    . "nf-md-alpha_i_circle")
                                           (unicode . "🅘")))
                ((evil-motion-state-p)   '((face    . doom-modeline-evil-motion-state)
                                           (icon    . "nf-md-alpha_m_circle")
                                           (unicode . "🅜")))
                ((evil-visual-state-p)   '((face    . doom-modeline-evil-visual-state)
                                           (icon    . "nf-md-alpha_v_circle")
                                           (unicode . "🅥")))
                ((evil-operator-state-p) '((face    . doom-modeline-evil-operator-state)
                                           (icon    . "nf-md-alpha_o_circle")
                                           (unicode . "🅞")))
                ((evil-replace-state-p)  '((face    . doom-modeline-evil-replace-state)
                                           (icon    . "nf-md-alpha_r_circle")
                                           (unicode . "🅡")))
                (t                       '((face    . doom-modeline-evil-user-state)
                                           (icon    . "nf-md-alpha_u_circle")
                                           (unicode . "🅤"))))
      (doom-modeline--modal-icon
       (let ((tag (evil-state-property evil-state :tag t)))
         (if (stringp tag) tag (funcall tag)))
       .face
       (evil-state-property evil-state :name t)
       .icon
       .unicode))))

(defsubst doom-modeline--overwrite ()
  "The current overwrite state which is enabled by command `overwrite-mode'."
  (when (and (bound-and-true-p overwrite-mode)
             (not (bound-and-true-p evil-local-mode)))
    (doom-modeline--modal-icon
     "<W>" 'doom-modeline-overwrite "Overwrite mode"
     "nf-md-marker" "🅦")))

(defsubst doom-modeline--god ()
  "The current god state which is enabled by the command `god-mode'."
  (when (bound-and-true-p god-local-mode)
    (doom-modeline--modal-icon
     "<G>" 'doom-modeline-god "God mode"
     "nf-md-account_circle" "🅖")))

(defsubst doom-modeline--ryo ()
  "The current ryo-modal state which is enabled by the command `ryo-modal-mode'."
  (when (bound-and-true-p ryo-modal-mode)
    (doom-modeline--modal-icon
     "<R>" 'doom-modeline-ryo "Ryo modal"
     "nf-md-star_circle" "✪")))

(defsubst doom-modeline--xah-fly-keys ()
  "The current `xah-fly-keys' state."
  (when (bound-and-true-p xah-fly-keys)
    (if xah-fly-insert-state-p
        (doom-modeline--modal-icon
         "<I>" 'doom-modeline-fly-insert-state "Xah-fly insert mode"
         "nf-md-airplane_edit" "🛧")
      (doom-modeline--modal-icon
       "<C>" 'doom-modeline-fly-normal-state "Xah-fly command mode"
       "nf-md-airplane_cog" "🛧"))))

(defsubst doom-modeline--boon ()
  "The current Boon state. Requires `boon-mode' to be enabled."
  (when (bound-and-true-p boon-local-mode)
    (doom-modeline--modal-icon
     (boon-state-string)
     (cond
      (boon-command-state 'doom-modeline-boon-command-state)
      (boon-insert-state 'doom-modeline-boon-insert-state)
      (boon-special-state 'doom-modeline-boon-special-state)
      (boon-off-state 'doom-modeline-boon-off-state)
      (t 'doom-modeline-boon-off-state))
     (boon-modeline-string)
     "nf-md-coffee" "🍵")))

(defsubst doom-modeline--meow ()
  "The current Meow state. Requires `meow-mode' to be enabled."
  (when (bound-and-true-p meow-mode)
    (let-alist (cond
                ((meow-normal-mode-p) '((face    . doom-modeline-meow-normal-state)
                                        (icon    . "nf-md-alpha_n_circle")
                                        (unicode . "🅝")))
                ((meow-insert-mode-p) '((face    . doom-modeline-meow-insert-state)
                                        (icon    . "nf-md-alpha_i_circle")
                                        (unicode . "🅘")))
                ((meow-beacon-mode-p) '((face    . doom-modeline-meow-beacon-state)
                                        (icon    . "nf-md-alpha_b_circle")
                                        (unicode . "🅑")))
                ((meow-motion-mode-p) '((face    . doom-modeline-meow-motion-state)
                                        (icon    . "nf-md-alpha_m_circle")
                                        (unicode . "🅜")))
                ((meow-keypad-mode-p) '((face    . doom-modeline-meow-keypad-state)
                                        (icon    . "nf-md-alpha_k_circle")
                                        (unicode . "🅚")))
                (t                    '((face    . doom-modeline-meow-normal-state)
                                        (icon    . "nf-md-alpha_n_circle")
                                        (unicode . "🅝"))))
      (doom-modeline--modal-icon
       (substring-no-properties meow--indicator)
       .face
       (symbol-name (meow--current-state))
       .icon
       .unicode))))

(doom-modeline-def-segment modals
  "Displays modal editing states.

Including `evil', `overwrite', `god', `ryo' and `xha-fly-kyes', etc."
  (when doom-modeline-modal
    (let* ((evil (doom-modeline--evil))
           (ow (doom-modeline--overwrite))
           (god (doom-modeline--god))
           (ryo (doom-modeline--ryo))
           (xf (doom-modeline--xah-fly-keys))
           (boon (doom-modeline--boon))
           (meow (doom-modeline--meow))
           (vsep (doom-modeline-vspc))
           (sep (and (or evil ow god ryo xf boon meow) (doom-modeline-spc))))
      (concat sep
              (and evil (concat evil (and (or ow god ryo xf boon meow) vsep)))
              (and ow (concat ow (and (or god ryo xf boon meow) vsep)))
              (and god (concat god (and (or ryo xf boon meow) vsep)))
              (and ryo (concat ryo (and (or xf boon meow) vsep)))
              (and xf (concat xf (and (or boon meow) vsep)))
              (and boon (concat boon (and meow vsep)))
              meow
              sep))))

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
  (when-let* ((im (cond
                   (current-input-method
                    current-input-method-title)
                   ((and (bound-and-true-p evil-local-mode)
                         (bound-and-true-p evil-input-method))
                    (nth 3 (assoc default-input-method input-method-alist)))
                   (t nil)))
              (sep (doom-modeline-spc)))
    (concat
     sep
     (propertize im
                 'face (doom-modeline-face
                        (if (and (bound-and-true-p rime-mode)
                                 (equal current-input-method "rime"))
                            (if (and (rime--should-enable-p)
                                     (not (rime--should-inline-ascii-p)))
                                'doom-modeline-input-method
                              'doom-modeline-input-method-alt)
                          'doom-modeline-input-method))
                 'help-echo (concat
                             "Current input method: "
                             current-input-method
                             "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
                 'mouse-face 'doom-modeline-highlight
                 'local-map mode-line-input-method-map)
     sep)))


;;
;; Info
;;

(doom-modeline-def-segment info-nodes
  "The topic and nodes in the Info buffer."
  (concat
   " ("
   ;; topic
   (propertize (if (stringp Info-current-file)
                   (replace-regexp-in-string
                    "%" "%%"
                    (file-name-sans-extension
                     (file-name-nondirectory Info-current-file)))
                 (format "*%S*" Info-current-file))
               'face (doom-modeline-face 'doom-modeline-info))
   ") "
   ;; node
   (when Info-current-node
     (propertize (replace-regexp-in-string
                  "%" "%%" Info-current-node)
                 'face (doom-modeline-face 'doom-modeline-buffer-path)
                 'help-echo
                 "mouse-1: scroll forward, mouse-3: scroll back"
                 'mouse-face 'doom-modeline-highlight
                 'local-map Info-mode-line-node-keymap))))


;;
;; REPL
;;

(defun doom-modeline-repl-icon (text face)
  "Display REPL icon (or TEXT in terminal) with FACE."
  (doom-modeline-icon 'faicon "nf-fa-terminal" "$" text :face face))

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
                      'mouse-face 'doom-modeline-highlight
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
    (when-let* ((icon (when (bound-and-true-p cider-mode)
                        doom-modeline--cider))
                (sep (doom-modeline-spc)))
      (concat
       sep
       (doom-modeline-display-icon icon)
       sep))))


;;
;; LSP
;;

(defun doom-modeline-lsp-icon (text face)
  "Display LSP icon (or TEXT in terminal) with FACE."
  (if doom-modeline-lsp-icon
      (doom-modeline-icon 'octicon "nf-oct-rocket" "🚀" text :face face)
    (propertize text 'face face)))

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
                          (concat "LSP connected "
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
                      'mouse-face 'doom-modeline-highlight
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

(defun doom-modeline--eglot-pending-count (server)
  "Get count of pending eglot requests to SERVER."
  (if (fboundp 'jsonrpc-continuation-count)
      (jsonrpc-continuation-count server)
    (hash-table-count (jsonrpc--request-continuations server))))

(defvar-local doom-modeline--eglot nil)
(defun doom-modeline-update-eglot ()
  "Update eglot state."
  (setq doom-modeline--eglot
        (let* ((server (and (eglot-managed-p) (eglot-current-server)))
               (nick (and server (eglot-project-nickname server)))
               (pending (and server (doom-modeline--eglot-pending-count server)))
               (last-error (and server (jsonrpc-last-error server)))
               (face (cond (last-error 'doom-modeline-lsp-error)
                           ((and pending (cl-plusp pending)) 'doom-modeline-lsp-warning)
                           (nick 'doom-modeline-lsp-success)
                           (t 'doom-modeline-lsp-warning)))
               (server-info (and server (eglot--server-info server)))
               (server-name (or (plist-get server-info :name)
                                (and server (jsonrpc-name server)) ""))
               (major-modes (or (and server (eglot--major-modes server)) ""))
               (icon (doom-modeline-lsp-icon eglot-menu-string face)))
          (propertize icon
                      'help-echo (format "Eglot connected [%s]\n%s %s
mouse-1: Display minor mode menu
mouse-3: LSP server control menu"
                                         nick server-name major-modes)
                      'mouse-face 'doom-modeline-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1] eglot-menu)
                                   (define-key map [mode-line mouse-3] eglot-server-menu)
                                   map)))))
(add-hook 'eglot-managed-mode-hook #'doom-modeline-update-eglot)

(defvar-local doom-modeline--tags nil)
(defun doom-modeline-update-tags ()
  "Update tags state."
  (setq doom-modeline--tags
        (propertize
         (doom-modeline-lsp-icon "Tags" 'doom-modeline-lsp-success)
         'help-echo "Tags: Citre mode
mouse-1: Toggle citre mode"
         'mouse-face 'doom-modeline-highlight
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
 'doom-modeline-lsp-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-lsp-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-lsp-icon))))))

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
  (when doom-modeline-lsp
    (when-let* ((icon (cond ((bound-and-true-p lsp-mode)
                             doom-modeline--lsp)
                            ((bound-and-true-p eglot--managed-mode)
                             doom-modeline--eglot)
                            ((bound-and-true-p citre-mode)
                             doom-modeline--tags)))
                (sep (doom-modeline-spc)))
      (concat
       sep
       (doom-modeline-display-icon icon)
       sep))))

(defun doom-modeline-override-eglot ()
  "Override `eglot' mode-line."
  (if (and doom-modeline-lsp
           (bound-and-true-p doom-modeline-mode))
      (setq mode-line-misc-info
            (delq (assq 'eglot--managed-mode mode-line-misc-info) mode-line-misc-info))
    (add-to-list 'mode-line-misc-info
                 `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))))
(add-hook 'eglot-managed-mode-hook #'doom-modeline-override-eglot)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-eglot)

(doom-modeline-add-variable-watcher
 'doom-modeline-battery
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-lsp val)
     (doom-modeline-override-eglot))))


;;
;; GitHub
;;

(defvar doom-modeline--github-notification-number 0)
(defvar doom-modeline-before-github-fetch-notification-hook nil
  "Hooks before fetching GitHub notifications.
Example:
  (add-hook \\='doom-modeline-before-github-fetch-notification-hook
          #\\='auth-source-pass-enable)")

(defvar doom-modeline-after-github-fetch-notification-hook nil
  "Hooks after fetching GitHub notifications.")

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
                          (token (or (ghub--token ghub-default-host username 'forge t)
                                     (ghub--token ghub-default-host username 'ghub t))))
                (ghub-get "/notifications"
                          '((notifications . t))
                          :host ghub-default-host
                          :username username
                          :auth token
                          :unpaginate t
                          :noerror t))))))
     (lambda (result)
       (message "")                     ; suppress message
       (setq doom-modeline--github-notification-number (length result))
       (run-hooks 'doom-modeline-after-github-fetch-notification-hook)))))

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
             (doom-modeline--segment-visible 'github)
             (numberp doom-modeline--github-notification-number))
    (let ((sep (doom-modeline-spc)))
      (concat
       sep
       (propertize
        (concat
         (doom-modeline-icon 'octicon "nf-oct-mark_github" "🔔" "&"
                             :face 'doom-modeline-notification)
         (and (> doom-modeline--github-notification-number 0) (doom-modeline-vspc))
         (propertize
          (cond
           ((<= doom-modeline--github-notification-number 0) "")
           ((> doom-modeline--github-notification-number 99) "99+")
           (t (number-to-string doom-modeline--github-notification-number)))
          'face '(:inherit
                  (doom-modeline-unread-number doom-modeline-notification))))
        'help-echo "Github Notifications
mouse-1: Show notifications
mouse-3: Fetch notifications"
        'mouse-face 'doom-modeline-highlight
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
       sep))))


;;
;; Debug states
;;

;; Highlight the doom-modeline while debugging.
(defvar-local doom-modeline--debug-cookie nil)
(defun doom-modeline--debug-visual (&rest _)
  "Update the face of mode-line for debugging."
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (setq doom-modeline--debug-cookie
                  (face-remap-add-relative 'doom-modeline 'doom-modeline-debug-visual))
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

(defun doom-modeline-debug-icon (face)
  "Display debug icon with FACE and ARGS."
  (doom-modeline-icon 'codicon "nf-cod-debug" "🐛" "!" :face face))

(defun doom-modeline--debug-dap ()
  "The current `dap-mode' state."
  (when (and (bound-and-true-p dap-mode)
             (bound-and-true-p lsp-mode))
    (when-let* ((session (dap--cur-session)))
      (when (dap--session-running session)
        (propertize (doom-modeline-debug-icon 'doom-modeline-info)
                    'help-echo (format "DAP (%s - %s)
mouse-1: Display debug hydra
mouse-2: Display recent configurations
mouse-3: Disconnect session"
                                       (dap--debug-session-name session)
                                       (dap--debug-session-state session))
                    'mouse-face 'doom-modeline-highlight
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
                'mouse-face 'doom-modeline-highlight
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
                'mouse-face 'doom-modeline-highlight
                'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-error))))

(defsubst doom-modeline--debug-on-quit ()
  "The current `debug-on-quit' state."
  (when debug-on-quit
    (propertize (doom-modeline-debug-icon 'doom-modeline-warning)
                'help-echo "Debug on Quit
mouse-1: Toggle Debug on Quit"
                'mouse-face 'doom-modeline-highlight
                'local-map (make-mode-line-mouse-map 'mouse-1 #'toggle-debug-on-quit))))

(doom-modeline-def-segment debug
  "The current debug state."
  (when (doom-modeline--segment-visible 'debug)
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
  doom-modeline--pdf-pages)


;;
;; `mu4e' notifications
;;

(doom-modeline-def-segment mu4e
  "Show notifications of any unread emails in `mu4e'."
  (when (and doom-modeline-mu4e
             (doom-modeline--segment-visible 'mu4e))
    (when-let* ((sep (doom-modeline-spc))
                (vsep (doom-modeline-vspc))
                (icon (doom-modeline-icon 'mdicon "nf-md-email" "📧" "#"
                                          :face 'doom-modeline-notification))
                (values (cond ((and (bound-and-true-p mu4e-alert-mode-line)
                                    (numberp mu4e-alert-mode-line))
                               `(,mu4e-alert-mode-line ,#'mu4e-alert-view-unread-mails))
                              ((and (bound-and-true-p mu4e-modeline-mode)
                                    (fboundp 'mu4e-bookmark-favorite))
                               `(,(plist-get (mu4e-bookmark-favorite) :unread)
                                 ,(lambda ()
                                    (interactive)
                                    (mu4e-search (plist-get (mu4e-bookmark-favorite) :query)))))))
                (unread-count (nth 0 values))
                (open-fun (nth 1 values)))
      (when (> unread-count 0)
        (concat
         sep
         (propertize
          (concat
           icon
           vsep
           (propertize
            (if (> unread-count doom-modeline-number-limit)
                (format "%d+" doom-modeline-number-limit)
              (number-to-string unread-count))
            'face '(:inherit
                    (doom-modeline-unread-number doom-modeline-notification))))
          'mouse-face 'doom-modeline-highlight
          'keymap `(mode-line keymap
                              (mouse-1 . ,open-fun)
                              (mouse-2 . ,open-fun)
                              (mouse-3 . ,open-fun))
          'help-echo (concat (if (= unread-count 1)
                                 "You have an unread email"
                               (format "You have %s unread emails" unread-count))
                             "\nClick here to view "
                             (if (= unread-count 1) "it" "them")))
         sep)))))

(defun doom-modeline-override-mu4e-alert (&rest _)
  "Delete `mu4e-alert-mode-line' from global modeline string."
  (when (and (featurep 'mu4e-alert)
             (bound-and-true-p mu4e-alert-mode-line))
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
            :after #'doom-modeline-override-mu4e-alert)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-mu4e-alert)

(defun doom-modeline-override-mu4e-modeline (&rest _)
  "Delete `mu4e-alert-mode-line' from global modeline string."
  (when (bound-and-true-p mu4e-modeline-mode)
    (if (and doom-modeline-mu4e
             (bound-and-true-p doom-modeline-mode))
        ;; Delete original modeline
        (setq global-mode-string
              (delete mu4e--modeline-item global-mode-string))
      ;; Recover default settings
      (add-to-list 'global-mode-string mu4e--modeline-item))))
(add-hook 'mu4e-modeline-mode-hook #'doom-modeline-override-mu4e-modeline)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-mu4e-modeline)

(doom-modeline-add-variable-watcher
 'doom-modeline-mu4e
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-mu4e val)
     (doom-modeline-override-mu4e-alert)
     (doom-modeline-override-mu4e-modeline))))


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
        (gnus-demon-add-handler 'gnus-demon-scan-news doom-modeline-gnus-timer doom-modeline-gnus-idle))))
(add-hook 'gnus-started-hook #'doom-modeline-start-gnus-listener)

;; Stop the listener if gnus isn't running
(defun doom-modeline-stop-gnus-listener ()
  "Stop GNUS listener."
  (setq doom-modeline--gnus-started nil))
(add-hook 'gnus-exit-gnus-hook #'doom-modeline-stop-gnus-listener)

(doom-modeline-def-segment gnus
  "Show notifications of any unread emails in `gnus'."
  (when (and (doom-modeline--segment-visible 'gnus)
             doom-modeline-gnus
             doom-modeline--gnus-started
             ;; Don't display if the unread mails count is zero
             (numberp doom-modeline--gnus-unread-mail)
             (> doom-modeline--gnus-unread-mail 0))
    (let ((sep (doom-modeline-spc))
          (vsep (doom-modeline-vspc)))
      (concat
       sep
       (propertize
        (concat
         (doom-modeline-icon 'mdicon "nf-md-email" "📧" "#"
                             :face 'doom-modeline-notification)
         vsep
         (propertize
          (if (> doom-modeline--gnus-unread-mail doom-modeline-number-limit)
              (format "%d+" doom-modeline-number-limit)
            (number-to-string doom-modeline--gnus-unread-mail))
          'face '(:inherit
                  (doom-modeline-unread-number doom-modeline-notification))))
        'mouse-face 'doom-modeline-highlight
        'help-echo (if (= doom-modeline--gnus-unread-mail 1)
                       "You have an unread email"
                     (format "You have %s unread emails" doom-modeline--gnus-unread-mail)))
       sep))))


;;
;; IRC notifications
;;

(defun doom-modeline-shorten-irc (name)
  "Shorten IRC buffer `name' according to IRC mode.

Calls the mode specific function to return the shortened
version of `NAME' if applicable:
- Circe: `tracking-shorten'
- ERC: `erc-track-shorten-function'
- rcirc: `rcirc-shorten-buffer-name'

The specific function will decide how to stylize the buffer name,
read the individual functions documentation for more."
  (or (and (fboundp 'tracking-shorten)
           (car (tracking-shorten (list name))))
      (and (boundp 'erc-track-shorten-function)
           (functionp erc-track-shorten-function)
	       (car (funcall erc-track-shorten-function (list name))))
      (and (fboundp 'rcirc-short-buffer-name)
           (rcirc-short-buffer-name name))
      name))

(defun doom-modeline--tracking-buffers (buffers)
  "Logic to convert some irc BUFFERS to their font-awesome icon."
  (mapconcat
   (lambda (b)
     (propertize
      (funcall doom-modeline-irc-stylize b)
      'face '(:inherit (doom-modeline-unread-number doom-modeline-notification))
      'help-echo (format "IRC Notification: %s\nmouse-1: Switch to buffer" b)
      'mouse-face 'doom-modeline-highlight
      'local-map (make-mode-line-mouse-map
                  'mouse-1
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
             (doom-modeline--segment-visible 'irc-buffers))
    (let* ((buffers (doom-modeline--get-buffers))
           (number (length buffers))
           (sep (doom-modeline-spc)))
      (when (> number 0)
        (concat
         sep
         (doom-modeline--tracking-buffers buffers)
         sep)))))

(doom-modeline-def-segment irc
  "A notification icon for any unread irc buffer."
  (when (and doom-modeline-irc
             (doom-modeline--segment-visible 'irc))
    (let* ((buffers (doom-modeline--get-buffers))
           (number (length buffers))
           (sep (doom-modeline-spc))
           (vsep (doom-modeline-vspc)))
      (when (> number 0)
        (concat
         sep

         (propertize (concat
                      (doom-modeline-icon 'mdicon "nf-md-message_processing" "🗊" "#"
                                          :face 'doom-modeline-notification)
                      vsep
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
                     'mouse-face 'doom-modeline-highlight
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
           (concat sep (doom-modeline--tracking-buffers buffers)))

         sep)))))

(defun doom-modeline-override-rcirc ()
  "Override default `rcirc' mode-line."
  (if (and doom-modeline-irc
           (bound-and-true-p doom-modeline-mode))
      (setq global-mode-string
		    (delq 'rcirc-activity-string global-mode-string))
    (when (and rcirc-track-minor-mode
               (not (memq 'rcirc-activity-string global-mode-string)))
	  (setq global-mode-string
		    (append global-mode-string '(rcirc-activity-string))))))
(add-hook 'rcirc-track-minor-mode-hook #'doom-modeline-override-rcirc)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-rcirc)

(doom-modeline-add-variable-watcher
 'doom-modeline-irc
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-irc val)
     (doom-modeline-override-rcirc))))


;;
;; Battery status
;;

(defun doom-modeline-battery-icon (icon unicode text face)
  "Displays the battery ICON with FACE.

UNICODE and TEXT are fallbacks.
Uses `nerd-icons-mdicon' to fetch the icon."
  (doom-modeline-icon 'mdicon icon unicode text :face face))

(defvar doom-modeline--battery-status nil)
(defun doom-modeline-update-battery-status ()
  "Update battery status."
  (setq doom-modeline--battery-status
        (when (and doom-modeline-battery
                   (bound-and-true-p display-battery-mode))
          (let* ((data (and battery-status-function
                            (functionp battery-status-function)
                            (funcall battery-status-function)))
                 (status (cdr (assoc ?L data)))
                 (charging? (or (string-equal "AC" status)
                                (string-equal "on-line" status)))
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
                           (cond
                            ((>= percentage 100)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_100"
                                                           "nf-md-battery")
                                                         "🔋" "-" face))
                            ((>= percentage 90)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_90"
                                                           "nf-md-battery_90")
                                                         "🔋" "-" face))
                            ((>= percentage 80)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_80"
                                                           "nf-md-battery_80")
                                                         "🔋" "-" face))
                            ((>= percentage 70)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_70"
                                                           "nf-md-battery_70")
                                                         "🔋" "-" face))
                            ((>= percentage 60)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_60"
                                                           "nf-md-battery_60")
                                                         "🔋" "-" face))
                            ((>= percentage 50)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_50"
                                                           "nf-md-battery_50")
                                                         "🔋" "-" face))
                            ((>= percentage 40)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_40"
                                                           "nf-md-battery_40")
                                                         "🔋" "-" face))
                            ((>= percentage 30)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_30"
                                                           "nf-md-battery_30")
                                                         "🔋" "-" face))
                            ((>= percentage 20)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_20"
                                                           "nf-md-battery_20")
                                                         "🔋" "-" face))
                            ((>= percentage 10)
                             (doom-modeline-battery-icon (if charging?
                                                             "nf-md-battery_charging_10"
                                                           "nf-md-battery_10")
                                                         "🪫" "-" face))
                            (t (doom-modeline-battery-icon (if charging?
                                                               "nf-md-battery_charging_outline"
                                                             "nf-md-battery_outline")
                                                           "🪫" "!" face)))
                         (doom-modeline-battery-icon "nf-md-battery_alert" "⚠" "N/A" face)))
                 (text (if valid-percentage? (format "%d%s" percentage "%%") ""))
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
     (doom-modeline-update-battery-status))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (doom-modeline-update-battery-status))))

(doom-modeline-def-segment battery
  "Display battery status."
  (when (and doom-modeline-battery
             (bound-and-true-p display-battery-mode)
             (doom-modeline--segment-visible 'battery))
    (let ((sep (doom-modeline-spc))
          (vsep (doom-modeline-vspc)))
      (concat sep
              (car doom-modeline--battery-status)
              vsep
              (cdr doom-modeline--battery-status)
              sep))))

(defun doom-modeline-override-battery ()
  "Override default battery mode-line."
  (if (and doom-modeline-battery
           (bound-and-true-p doom-modeline-mode))
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
(add-hook 'display-battery-mode-hook #'doom-modeline-override-battery)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-battery)

(doom-modeline-add-variable-watcher
 'doom-modeline-battery
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-battery val)
     (doom-modeline-override-battery))))


;;
;; Package information
;;

(doom-modeline-def-segment package
  "Show package information via `paradox'."
  (concat
   (doom-modeline-display-text
    (format-mode-line 'mode-line-front-space))

   (when (and doom-modeline-icon doom-modeline-major-mode-icon)
     (concat
      (doom-modeline-spc)
      (doom-modeline-icon 'faicon "nf-fa-archive" nil nil
                          :face (doom-modeline-face
                                 (if doom-modeline-major-mode-color-icon
                                     'nerd-icons-silver
                                   'mode-line)))))
   (doom-modeline-display-text
    (format-mode-line 'mode-line-buffer-identification))))


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
    (let ((sep (doom-modeline-spc)))
      (concat
       sep
       (when doom-modeline-icon
         (concat
          (doom-modeline-icon 'sucicon "nf-custom-emacs" nil nil
                              :face (doom-modeline-face
                                     (and doom-modeline-major-mode-color-icon
                                          'nerd-icons-blue)))
          sep))
       (propertize
        (let ((custom (cdr (assoc (buffer-name) doom-modeline--helm-buffer-ids)))
              (case-fold-search t)
              (name (replace-regexp-in-string "-" " " (buffer-name))))
          (cond ((stringp custom) custom)
                ((functionp custom) (funcall custom))
                (t
                 (string-match "\\*helm:? \\(mode \\)?\\([^\\*]+\\)\\*" name)
                 (concat "HELM " (capitalize (match-string 2 name))))))
        'face (doom-modeline-face 'doom-modeline-buffer-file))
       sep))))

(doom-modeline-def-segment helm-number
  "Number of helm candidates."
  (when (bound-and-true-p helm-alive-p)
    (concat
     (propertize (format " %d/%d"
                         (helm-candidate-number-at-point)
                         (helm-get-candidate-number t))
                 'face (doom-modeline-face 'doom-modeline-buffer-path))
     (propertize (format " (%d total) " (helm-get-candidate-number))
                 'face (doom-modeline-face 'doom-modeline-info)))))

(doom-modeline-def-segment helm-help
  "Helm keybindings help."
  (when (bound-and-true-p helm-alive-p)
    (mapcar
     (lambda (s)
       (if (string-prefix-p "\\<" s)
           (propertize (substitute-command-keys s)
                       'face (doom-modeline-face
                              'doom-modeline-buffer-file))
         s))
     '("\\<helm-map>\\[helm-help]" "(help) "
       "\\<helm-map>\\[helm-select-action]" "(actions) "
       "\\<helm-map>\\[helm-maybe-exit-minibuffer]/F1/F2..." "(action) "))))

(doom-modeline-def-segment helm-prefix-argument
  "Helm prefix argument."
  (when (and (bound-and-true-p helm-alive-p)
             helm--mode-line-display-prefarg)
    (let ((arg (prefix-numeric-value (or prefix-arg current-prefix-arg))))
      (unless (= arg 1)
        (propertize (format "C-u %s" arg)
                    'face (doom-modeline-face 'doom-modeline-info))))))

(defvar doom-modeline--helm-current-source nil
  "The currently active helm source.")
(doom-modeline-def-segment helm-follow
  "Helm follow indicator."
  (and (bound-and-true-p helm-alive-p)
       doom-modeline--helm-current-source
       (eq 1 (cdr (assq 'follow doom-modeline--helm-current-source)))
       "HF"))

;;
;; Git timemachine
;;

(doom-modeline-def-segment git-timemachine
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-mode-icon)
   (doom-modeline--buffer-state-icon)
   (propertize
    "*%b*"
    'face (doom-modeline-face 'doom-modeline-buffer-timemachine))))

;;
;; Markdown/Org preview
;;

(doom-modeline-def-segment grip
  (when (bound-and-true-p grip-mode)
    (let ((sep (doom-modeline-spc)))
      (concat
       sep
       (let ((face (doom-modeline-face
                    (if grip--process
                        (pcase (process-status grip--process)
                          ('run 'doom-modeline-info)
                          ('exit 'doom-modeline-warning)
                          (_ 'doom-modeline-urgent))
                      'doom-modeline-urgent))))
         (propertize
          (doom-modeline-icon 'codicon "nf-cod-open_preview" "🗐" "@" :face face)
          'help-echo (format "Preview on %s
mouse-1: Preview in browser
mouse-2: Stop preview
mouse-3: Restart preview"
                             (grip--preview-url))
          'mouse-face 'doom-modeline-highlight
          'local-map (let ((map (make-sparse-keymap)))
                       (define-key map [mode-line mouse-1]
                         #'grip-browse-preview)
                       (define-key map [mode-line mouse-2]
                         #'grip-stop-preview)
                       (define-key map [mode-line mouse-3]
                         #'grip-restart-preview)
                       map)))
       sep))))

;;
;; Follow mode
;;

(doom-modeline-def-segment follow
  (when (bound-and-true-p follow-mode)
    (let* ((windows (follow-all-followers))
           (nwindows (length windows))
           (nfollowing (- (length (memq (selected-window) windows)) 1)))
      (concat
       (doom-modeline-spc)
       (propertize (format "Follow %d/%d" (- nwindows nfollowing) nwindows)
                   'face 'doom-modeline-buffer-minor-mode)))))

;;
;; Display time
;;

(defconst doom-modeline--clock-hour-hand-ratio 0.45
  "Length of the hour hand as a proportion of the radius.")

(defconst doom-modeline--clock-minute-hand-ratio 0.7
  "Length of the minute hand as a proportion of the radius.")

(defun doom-modeline--create-clock-svg (hour minute radius color)
  "Construct an SVG clock showing the time HOUR:MINUTE.
The clock will be of the specified RADIUS and COLOR."
  (let ((thickness-factor (image-compute-scaling-factor 'auto))
        (hour-x (* radius (sin (* (- 6 hour (/ minute 60.0)) (/ float-pi 6)))
                   doom-modeline--clock-hour-hand-ratio))
        (hour-y (* radius (cos (* (- 6 hour (/ minute 60.0)) (/ float-pi 6)))
                   doom-modeline--clock-hour-hand-ratio))
        (minute-x (* radius (sin (* (- 30 minute) (/ float-pi 30)))
                     doom-modeline--clock-minute-hand-ratio))
        (minute-y (* radius (cos (* (- 30 minute) (/ float-pi 30)))
                     doom-modeline--clock-minute-hand-ratio))
        (svg (svg-create (* 2 radius) (* 2 radius) :stroke color)))
    (svg-circle svg radius radius (- radius thickness-factor)
                :fill "none" :stroke-width (* 2 thickness-factor))
    (svg-circle svg radius radius thickness-factor
                :fill color :stroke "none")
    (svg-line svg radius radius (+ radius hour-x) (+ radius hour-y)
              :stroke-width (* 2 thickness-factor))
    (svg-line svg radius radius (+ radius minute-x) (+ radius minute-y)
              :stroke-width (* 1.5 thickness-factor))
    svg))

(defvar doom-modeline--clock-cache nil
  "The last result of `doom-modeline--generate-clock'.")

(defun doom-modeline--generate-clock ()
  "Return a string containing the current time as an analogue clock svg.
When the svg library is not available, return nil."
  (cdr
   (or (and (equal (truncate (float-time)
                             (* doom-modeline-time-clock-minute-resolution 60))
                   (car doom-modeline--clock-cache))
            doom-modeline--clock-cache)
       (and (require 'svg nil t)
            (setq doom-modeline--clock-cache
                  (cons (truncate (float-time)
                                  (* doom-modeline-time-clock-minute-resolution 60))
                        (propertize
                         " "
                         'display
                         (svg-image
                          (doom-modeline--create-clock-svg
                           (string-to-number (format-time-string "%-I")) ; hour
                           (* (truncate (string-to-number (format-time-string "%-M"))
                                        doom-modeline-time-clock-minute-resolution)
                              doom-modeline-time-clock-minute-resolution) ; minute
                           (if (integerp doom-modeline-time-clock-size) ; radius
                               doom-modeline-time-clock-size
                             (* doom-modeline-height 0.5 doom-modeline-time-clock-size))
                           "currentColor")
                          :scale 1 :ascent 'center)
                         'face 'doom-modeline-time
                         'help-echo (lambda (_window _object _pos)
                                      (format-time-string "%c")))))))))

(defun doom-modeline-time-icon ()
  "Displays the time icon."
  (or (and doom-modeline-time-live-icon
           doom-modeline-time-analogue-clock
           (display-graphic-p)
           (doom-modeline--generate-clock))
      (doom-modeline-icon
       'mdicon
       (if doom-modeline-time-live-icon
           (pcase (% (caddr (decode-time)) 12)
             (0 "nf-md-clock_time_twelve_outline")
             (1 "nf-md-clock_time_one_outline")
             (2 "nf-md-clock_time_two_outline")
             (3 "nf-md-clock_time_three_outline")
             (4 "nf-md-clock_time_four_outline")
             (5 "nf-md-clock_time_five_outline")
             (6 "nf-md-clock_time_six_outline")
             (7 "nf-md-clock_time_seven_outline")
             (8 "nf-md-clock_time_eight_outline")
             (9 "nf-md-clock_time_nine_outline")
             (10 "nf-md-clock_time_ten_outline")
             (11 "nf-md-clock_time_eleven_outline"))
         "nf-md-clock_outline")
       "⏰"
       ""
       :face '(:inherit doom-modeline-time :weight normal))))

(doom-modeline-def-segment time
  (when (and doom-modeline-time
             (bound-and-true-p display-time-mode)
             (doom-modeline--segment-visible 'time))
    (concat
     (doom-modeline-spc)
     (when doom-modeline-time-icon
       (concat
        (doom-modeline-time-icon)
        (and (or doom-modeline-icon doom-modeline-unicode-fallback)
             (doom-modeline-vspc))))
     (propertize display-time-string
                 'face (doom-modeline-face 'doom-modeline-time)))))

(defun doom-modeline-override-time ()
  "Override default `display-time' mode-line."
  (or global-mode-string (setq global-mode-string '("")))
  (if (and doom-modeline-time
           (bound-and-true-p doom-modeline-mode))
      (setq global-mode-string (delq 'display-time-string global-mode-string))
    (or (memq 'display-time-string global-mode-string)
	    (setq global-mode-string
		      (append global-mode-string '(display-time-string))))))
(add-hook 'display-time-mode-hook #'doom-modeline-override-time)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-time)

(doom-modeline-add-variable-watcher
 'doom-modeline-time
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-time val)
     (doom-modeline-override-time))))

;;
;; Compilation
;;

(doom-modeline-def-segment compilation
  (and (bound-and-true-p compilation-in-progress)
       (propertize "[Compiling] "
                   'face (doom-modeline-face 'doom-modeline-compilation)
	               'help-echo "Compiling; mouse-2: Goto Buffer"
                   'mouse-face 'doom-modeline-highlight
                   'local-map
                   (make-mode-line-mouse-map
                    'mouse-2
                    #'compilation-goto-in-progress-buffer))))

;;
;; Eldoc
;;

(doom-modeline-def-segment eldoc
  (and (bound-and-true-p eldoc-mode)
       '(eldoc-mode-line-string
		 (" " eldoc-mode-line-string " "))))

(defun doom-modeline-eldoc-minibuffer-message (format-string &rest args)
  "Display message specified by FORMAT-STRING and ARGS on the mode-line as needed.
This function displays the message produced by formatting ARGS
with FORMAT-STRING on the mode line when the current buffer is a minibuffer.
Otherwise, it displays the message like `message' would."
  (if (minibufferp)
      (progn
	    (add-hook 'minibuffer-exit-hook
		          (lambda () (setq eldoc-mode-line-string nil
			                  ;; https://debbugs.gnu.org/16920
			                  eldoc-last-message nil))
		          nil t)
	    (with-current-buffer
	        (window-buffer
	         (or (window-in-direction 'above (minibuffer-window))
                 (minibuffer-selected-window)
		         (get-largest-window)))
          (setq eldoc-mode-line-string
                (when (stringp format-string)
                  (apply #'format-message format-string args)))
          (force-mode-line-update)))
    (apply #'message format-string args)))

;;
;; Kubernetes
;;

(doom-modeline-def-segment k8s
  (when (and (bound-and-true-p kele-mode) (doom-modeline--segment-visible 'k8s))
    (let* ((ctx (kele-current-context-name :wait nil))
           (ns (kele-current-namespace :wait nil))
           (icon (doom-modeline-icon 'mdicon "nf-md-kubernetes" "K8s:" "K8s:"))
           (sep (doom-modeline-spc))
           (help-msg (let ((msgs (list (format "Current context: %s" ctx))))
                       (when ns
                         (setq msgs (append msgs (list (format "Current namespace: %s" ns)))))
                       (string-join msgs "\n"))))
      (propertize (concat
                   icon sep ctx
                   (when (and doom-modeline-k8s-show-namespace ns) (format "(%s)" ns))
                   sep)
                  'local-map (let ((map (make-sparse-keymap)))
                               (define-key map [mode-line down-mouse-1] kele-menu-map)
                               map)
                  'mouse-face 'doom-modeline-highlight
                  'help-echo help-msg))))

(provide 'doom-modeline-segments)

;;; doom-modeline-segments.el ends here
