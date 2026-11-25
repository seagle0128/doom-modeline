;;; doom-modeline-segments-perf-test.el --- Tests for P0 segment caching -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  Regression tests for the caching / throttling of segments whose data is
;;  expensive to compute (`word-count', the total-line count of
;;  `buffer-position', and the buffer state icon).  The contract under test:
;;  the expensive computation is bounded on the redisplay path -- either by
;;  a change-keyed cache, or by a time throttle when every edit invalidates
;;  the result (`word-count').
;;
;;  Run with:
;;    emacs -Q --batch -L . -l test/doom-modeline-segments-perf-test.el \
;;      -f ert-run-tests-batch-and-exit
;;
;;; Code:

(require 'cl-lib)
(require 'doom-modeline-core)
(require 'doom-modeline-segments)

(defun doom-modeline-test--reset-word-count-throttle ()
  "Reset the shared `timeout-throttle' state on the scan helper.
Each test must start with a fresh advice closure: the timer and cached
result live in that closure, not in a buffer-local variable."
  (advice-remove #'doom-modeline--scan-word-count 'throttle)
  (timeout-throttle #'doom-modeline--scan-word-count
                    'doom-modeline-word-count-throttle))

(ert-deftest doom-modeline-word-count/throttled-across-edits ()
  "Continuous edits reuse the last count until the throttle interval elapses."
  (let ((doom-modeline-enable-word-count t)
        (doom-modeline-continuous-word-count-modes '(text-mode))
        (doom-modeline-word-count-throttle 100.0)
        (calls 0))
    (doom-modeline-test--reset-word-count-throttle)
    (with-temp-buffer
      (text-mode)
      (insert "one two three four five")
      (setq doom-modeline--word-count nil)
      (cl-letf* ((orig (symbol-function 'count-words))
                 ((symbol-function 'count-words)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        ;; First render scans.
        (doom-modeline-segment--word-count)
        (should (= calls 1))
        (should (= doom-modeline--word-count 5))
        ;; Edits inside the throttle window do not trigger another scan.
        (insert " six")
        (doom-modeline-segment--word-count)
        (insert " seven")
        (doom-modeline-segment--word-count)
        (should (= calls 1))
        (should (= doom-modeline--word-count 5))))))

(ert-deftest doom-modeline-word-count/zero-is-throttled ()
  "A zero word count is a valid throttled value, not a permanent miss."
  (let ((doom-modeline-enable-word-count t)
        (doom-modeline-continuous-word-count-modes '(text-mode))
        (doom-modeline-word-count-throttle 100.0)
        (calls 0))
    (doom-modeline-test--reset-word-count-throttle)
    (with-temp-buffer
      (text-mode)
      (setq doom-modeline--word-count nil)
      (cl-letf* ((orig (symbol-function 'count-words))
                 ((symbol-function 'count-words)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        (doom-modeline-segment--word-count)
        (doom-modeline-segment--word-count)
        (should (= calls 1))
        (should (= doom-modeline--word-count 0))))))

(ert-deftest doom-modeline-word-count/rescans-after-interval ()
  "A new throttle window rescans and adopts the fresh count.
The expiry of the timer is owned by `timeout-throttle'; simulate it by
rebuilding the advice instead of relying on wall-clock time, which is not
deterministic in batch mode across Emacs versions."
  (let ((doom-modeline-enable-word-count t)
        (doom-modeline-continuous-word-count-modes '(text-mode))
        (doom-modeline-word-count-throttle 100.0)
        (calls 0))
    (doom-modeline-test--reset-word-count-throttle)
    (with-temp-buffer
      (text-mode)
      (insert "one two")
      (setq doom-modeline--word-count nil)
      (cl-letf* ((orig (symbol-function 'count-words))
                 ((symbol-function 'count-words)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        (doom-modeline-segment--word-count)
        (should (= calls 1))
        ;; Still inside the window: an edit does not trigger a rescan.
        (insert " three")
        (doom-modeline-segment--word-count)
        (should (= calls 1))
        ;; A fresh window rescans and adopts the new count.
        (doom-modeline-test--reset-word-count-throttle)
        (doom-modeline-segment--word-count)
        (should (= calls 2))
        (should (= doom-modeline--word-count 3))))))

(ert-deftest doom-modeline-word-count/no-cross-buffer-leak ()
  "A cached result from another buffer is never used for this buffer."
  (let ((doom-modeline-enable-word-count t)
        (doom-modeline-continuous-word-count-modes '(text-mode))
        (doom-modeline-word-count-throttle 100.0)
        (calls 0))
    (doom-modeline-test--reset-word-count-throttle)
    (cl-letf* ((orig (symbol-function 'count-words))
               ((symbol-function 'count-words)
                (lambda (&rest args)
                  (setq calls (1+ calls))
                  (apply orig args))))
      (let ((buf-a (generate-new-buffer " *dm-wc-a*"))
            (buf-b (generate-new-buffer " *dm-wc-b*")))
        (unwind-protect
            (progn
              (with-current-buffer buf-a
                (text-mode)
                (insert "one two three four five")
                (setq doom-modeline--word-count nil)
                (doom-modeline-segment--word-count)
                (should (= doom-modeline--word-count 5)))
              ;; Buffer B renders inside the same throttle window.  The
              ;; cached result belongs to A, so B must scan once on its own
              ;; first render rather than inheriting 5.
              (with-current-buffer buf-b
                (text-mode)
                (insert "x y")
                (setq doom-modeline--word-count nil)
                (doom-modeline-segment--word-count)
                (should (= doom-modeline--word-count 2))
                ;; Still inside the window: reuse B's own value, no new scan.
                (insert " z")
                (doom-modeline-segment--word-count)
                (should (= doom-modeline--word-count 2))
                (should (= calls 2)))
          (kill-buffer buf-a)
          (kill-buffer buf-b)))))))

(ert-deftest doom-modeline-total-lines/cached-until-change ()
  "The full-buffer `line-number-at-pos' scan runs once until a change."
  (let ((doom-modeline-enable-buffer-position t)
        (doom-modeline-total-line-number t)
        (calls 0))
    (with-temp-buffer
      (insert "a\nb\nc\n")
      (setq doom-modeline--total-lines nil)
      (cl-letf* ((orig (symbol-function 'line-number-at-pos))
                 ((symbol-function 'line-number-at-pos)
                  (lambda (&rest args)
                    (setq calls (1+ calls))
                    (apply orig args))))
        (doom-modeline-segment--buffer-position)
        (doom-modeline-segment--buffer-position)
        (should (= calls 1))
        (should (= doom-modeline--total-lines 4))
        (insert "d\n")
        (doom-modeline-segment--buffer-position)
        (should (= calls 2))
        (should (= doom-modeline--total-lines 5))))))

(ert-deftest doom-modeline-state-icon/reads-cache-on-redisplay ()
  "The buffer state icon is computed lazily and reused across redisplays."
  (with-temp-buffer
    (setq-local buffer-read-only t)
    (setq doom-modeline--buffer-state-icon-cache nil)
    (doom-modeline--buffer-state-icon)
    (let ((cached doom-modeline--buffer-state-icon-cache))
      (should cached)
      ;; A redisplay must not recompute: the cached object is reused as-is.
      (doom-modeline--buffer-state-icon)
      (should (eq cached doom-modeline--buffer-state-icon-cache)))))

(ert-deftest doom-modeline-state-icon/update-refreshes-cache ()
  "The update function writes the cache for the current buffer."
  (with-temp-buffer
    (setq doom-modeline--buffer-state-icon-cache nil)
    (doom-modeline-update-buffer-file-state-icon)
    (should doom-modeline--buffer-state-icon-cache)))

(ert-deftest doom-modeline-state-icon/remote-check-reuses-dir-cache ()
  "The remote state icon takes its host from the shared directory cache."
  (let ((calls 0)
        (dir "/ssh:user@example.com:/home/user/"))
    ;; Count only our direct `(file-remote-p DIR 'host)` lookups: TRAMP's
    ;; handler recurses with extra args, and the ERT harness uses temp dirs.
    (cl-letf* ((orig-remote (symbol-function 'file-remote-p))
               ((symbol-function 'file-remote-p)
                (lambda (&rest args)
                  (when (and (= (length args) 2)
                             (eq (cadr args) 'host)
                             (string-prefix-p "/ssh:user@" (car args)))
                    (setq calls (1+ calls)))
                  (apply orig-remote args))))
      (with-temp-buffer
        (setq default-directory dir
              doom-modeline--default-directory-cache nil
              doom-modeline--buffer-state-icon-cache nil)
        (doom-modeline-update-buffer-file-state-icon)
        (should (= calls 1))
        (should doom-modeline--buffer-state-icon-cache)
        ;; A second post-command refresh must not re-parse the TRAMP prefix.
        (doom-modeline-update-buffer-file-state-icon)
        (should (= calls 1))
        ;; A different directory invalidates the cache once, as before.
        (setq default-directory "/ssh:user@other.example.com:/home/user/")
        (doom-modeline-update-buffer-file-state-icon)
        (should (= calls 2))))))

(defun doom-modeline-test--count-file-call (symbol file counter)
  "Return an ERT-friendly replacement for SYMBOL counting calls on FILE.
The real definition is delegated to; only direct one-argument calls with
exactly FILE are counted, excluding TRAMP handler recursion.  COUNTER is a
one-element list used as a mutable cell (lexical binding safe)."
  (let ((orig (symbol-function symbol)))
    (lambda (&rest args)
      (when (and (equal (car args) file) (= (length args) 1))
        (setcar counter (1+ (car counter))))
      (apply orig args))))

(ert-deftest doom-modeline-refresh-state/remote-file-checked-once ()
  "The idle state refresh asks `file-remote-p' once and skips remote I/O."
  (let ((remote-calls (list 0))
        (exists-calls (list 0))
        (file "/ssh:user@example.com:/home/user/f.el"))
    (cl-letf* (((symbol-function 'file-remote-p)
                (doom-modeline-test--count-file-call
                 'file-remote-p file remote-calls))
               ((symbol-function 'file-exists-p)
                (doom-modeline-test--count-file-call
                 'file-exists-p file exists-calls)))
      (with-temp-buffer
        (setq buffer-file-name file)
        (doom-mdeline-refresh-buffer-file-state)
        (should (= (car remote-calls) 1))
        (should (= (car exists-calls) 0))))))

(ert-deftest doom-modeline-refresh-state/local-missing-file-detected ()
  "Local files still get exactly one remote check and one existence check."
  (let ((remote-calls (list 0))
        (exists-calls (list 0))
        (file "/nonexistent/doom-modeline-remote-p-test.el"))
    (cl-letf* (((symbol-function 'file-remote-p)
                (doom-modeline-test--count-file-call
                 'file-remote-p file remote-calls))
               ((symbol-function 'file-exists-p)
                (doom-modeline-test--count-file-call
                 'file-exists-p file exists-calls)))
      (with-temp-buffer
        (setq buffer-file-name file)
        (doom-mdeline-refresh-buffer-file-state)
        (should (= (car remote-calls) 1))
        (should (= (car exists-calls) 1))
        (should doom-modeline--buffer-file-state-icon)))))

(ert-deftest doom-modeline-default-dir-info/cached-until-dir-change ()
  "Remote-host lookup and directory abbreviation run once per directory."
  (let ((calls 0))
    (cl-letf* ((orig-remote (symbol-function 'file-remote-p))
               ((symbol-function 'file-remote-p)
                (lambda (&rest args)
                  (setq calls (1+ calls))
                  (apply orig-remote args))))
      (with-temp-buffer
        (setq doom-modeline--default-directory-cache nil)
        (let ((dir default-directory))
          (doom-modeline--default-directory-info)
          (doom-modeline--default-directory-info)
          (should (= calls 1))
          ;; Changing the directory invalidates via the DIR mismatch.
          (setq default-directory (expand-file-name "subdir/" dir))
          (doom-modeline--default-directory-info)
          (should (= calls 2)))))))

(ert-deftest doom-modeline-default-dir-info/local-dir-returns-nil-host ()
  "A local directory yields a nil remote host and a non-nil abbreviation."
  (with-temp-buffer
    (setq doom-modeline--default-directory-cache nil)
    (let ((info (doom-modeline--default-directory-info)))
      (should (null (car info)))
      (should (stringp (cdr info))))))

(ert-deftest doom-modeline-selection-stats/cached-until-region-change ()
  "Region scans run at most once per distinct region and word-count option."
  (let ((word-calls 0))
    (cl-letf* ((orig-words (symbol-function 'count-words))
               ((symbol-function 'count-words)
                (lambda (&rest args)
                  (setq word-calls (1+ word-calls))
                  (apply orig-words args))))
      (with-temp-buffer
        (insert "alpha beta gamma\ndelta epsilon\n")
        (setq doom-modeline--selection-cache nil)
        (let ((beg (point-min))
              (end (point-max)))
          (doom-modeline--selection-stats beg end t)
          (doom-modeline--selection-stats beg end t)
          (should (= word-calls 1))
          ;; Region bounds changed: recompute once.
          (doom-modeline--selection-stats beg (1- end) t)
          (should (= word-calls 2))
          ;; Word-count option changed: recompute once (and skip scan when nil).
          (doom-modeline--selection-stats beg (1- end) nil)
          (should (= word-calls 2))
          ;; Same bounds but buffer content changed: the tick invalidates.
          (goto-char (point-max))
          (insert "!")
          (doom-modeline--selection-stats beg (1- end) nil)
          (should (= word-calls 2))             ; nil option: no word scan
          (doom-modeline--selection-stats beg (point-max) t)
          (should (= word-calls 3)))))))

(ert-deftest doom-modeline-selection-stats/narrowing-invalidates-cache ()
  "Narrowing and widening invalidate the cache even when bounds and tick do not.
`count-lines' clamps to `point-max', so the narrowing bounds must be part of
the cache key."
  (let ((line-calls 0))
    (cl-letf* ((orig-lines (symbol-function 'count-lines))
               ((symbol-function 'count-lines)
                (lambda (&rest args)
                  (setq line-calls (1+ line-calls))
                  (apply orig-lines args))))
      (with-temp-buffer
        (dotimes (_ 8) (insert "alpha beta gamma\n"))
        (setq doom-modeline--selection-cache nil)
        (let ((beg (point-min))
              (end (1- (point-max))))
          ;; Warm the cache in the wide buffer.
          (doom-modeline--selection-stats beg end nil)
          (should (= line-calls 1))
          (doom-modeline--selection-stats beg end nil)
          (should (= line-calls 1))
          ;; Narrowing changes point-max but not beg/end or the tick.
          (narrow-to-region (point-min) 30)
          (doom-modeline--selection-stats beg end nil)
          (should (= line-calls 2))
          ;; Widen restores the original accessible portion: rescan again.
          (widen)
          (doom-modeline--selection-stats beg end nil)
          (should (= line-calls 3))
          ;; Stable narrowing still hits the cache.
          (doom-modeline--selection-stats beg end nil)
          (should (= line-calls 3)))))))

(ert-deftest doom-modeline-selection-stats/marker-bounds-with-narrowing ()
  "Marker bounds and narrowing together invalidate the cache correctly."
  (with-temp-buffer
    (dotimes (_ 8) (insert "alpha beta gamma\n"))
    (goto-char (point-min))
    (let ((b (point-marker))
          (e (copy-marker (point-max))))
      (setq doom-modeline--selection-cache nil)
      (let ((wide (doom-modeline--selection-stats b e t)))
        (narrow-to-region (point-min) 30)
        (let ((narrow (doom-modeline--selection-stats b e t)))
          ;; chars/words stay the same, line count must reflect the narrowing.
          (should (< (car narrow) (car wide)))
          (should (= (cadr narrow) (cadr wide)))
          (should (= (cddr narrow) (cddr wide))))))))

(ert-deftest doom-modeline-selection-info/evil-narrowing-updates-lines ()
  "The selection-info segment shows fresh line counts after narrowing in evil.
Evil's visual bounds are plain buffer-local values that narrowing does not
clamp, so the narrowing bounds must participate in the cache key."
  (let ((doom-modeline-selection-info t)
        (doom-modeline-enable-word-count nil))
    (cl-letf (((symbol-function 'doom-modeline--active) (lambda () t)))
      (with-temp-buffer
        (dotimes (_ 8) (insert "alpha beta gamma\n"))
        (setq-local evil-local-mode t)
        (setq-local evil-state 'visual)
        (setq-local evil-visual-selection 'line)
        (setq-local evil-visual-beginning (point-min))
        (setq-local evil-visual-end (1- (point-max)))
        (setq doom-modeline--selection-cache nil)
        (let ((wide (substring-no-properties
                     (doom-modeline-segment--selection-info))))
          (narrow-to-region (point-min) 30)
          (let ((narrow (substring-no-properties
                         (doom-modeline-segment--selection-info))))
            (should (string-match-p "L" wide))
            (should (string-match-p "L" narrow))
            (should-not (string= wide narrow))))))))

(ert-deftest doom-modeline-selection-stats/returns-lines-chars-words ()
  "The stats tuple carries lines, chars and words in the documented shape."
  (with-temp-buffer
    (insert "one two\nthree")
    (setq doom-modeline--selection-cache nil)
    (let* ((stats (doom-modeline--selection-stats (point-min) (point-max) t)))
      (should (= (car stats) 2))        ; lines
      (should (= (cadr stats) 13))      ; chars: "one two\nthree" = 13
      (should (= (cddr stats) 3)))))    ; words

(ert-deftest doom-modeline-workspace-keymap/cached-per-backend ()
  "The workspace keymap is built once per backend and reused."
  (let ((calls 0))
    (cl-letf* ((orig-map (symbol-function 'make-sparse-keymap))
               ((symbol-function 'make-sparse-keymap)
                (lambda (&rest args)
                  (setq calls (1+ calls))
                  (apply orig-map args))))
      (setq doom-modeline--workspace-keymap-cache nil)
      (let ((m1 (doom-modeline--workspace-keymap 'eyebrowse)))
        (should (keymapp m1))
        (should (eq m1 (doom-modeline--workspace-keymap 'eyebrowse)))
        (should (= calls 1))
        ;; Switching backend rebuilds once.
        (should (keymapp (doom-modeline--workspace-keymap 'tab-bar)))
        (should (= calls 2))))))

(ert-deftest doom-modeline-vcs/remote-skips-vc-io ()
  "Remote buffers keep the branch name without probing the working tree."
  (let ((doom-modeline-vcs-remote-state-check nil)
        (backend-calls 0)
        (state-calls 0))
    (cl-letf* ((orig-backend (symbol-function 'vc-backend))
               ((symbol-function 'vc-backend)
                (lambda (&rest args)
                  (setq backend-calls (1+ backend-calls))
                  (apply orig-backend args)))
               (orig-state (symbol-function 'vc-state))
               ((symbol-function 'vc-state)
                (lambda (&rest args)
                  (setq state-calls (1+ state-calls))
                  (apply orig-state args))))
      (with-temp-buffer
        (setq-local vc-mode "Git:main")
        (setq buffer-file-name "/ssh:user@example.com:/home/user/f.el")
        (doom-modeline-update-vcs)
        (should (= backend-calls 0))
        (should (= state-calls 0))
        (should doom-modeline--vcs)
        (should (equal (substring-no-properties
                        (alist-get 'text doom-modeline--vcs))
                       "main"))))))

(ert-deftest doom-modeline-vcs/local-still-queries-state ()
  "Local buffers keep the original live VCS state lookup."
  (let ((backend-calls 0)
        (state-calls 0))
    (cl-letf* (((symbol-function 'vc-backend)
                (lambda (&rest _) (setq backend-calls (1+ backend-calls)) 'Git))
               ((symbol-function 'vc-state)
                (lambda (&rest _) (setq state-calls (1+ state-calls)) 'edited))
               ((symbol-function 'locate-dominating-file)
                (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq-local vc-mode "Git:main")
        (setq buffer-file-name "/tmp/doom-modeline-vcs-test.el")
        (doom-modeline-update-vcs)
        (should (= backend-calls 1))
        (should (= state-calls 1))
        (should doom-modeline--vcs)))))

(ert-deftest doom-modeline-vcs/opt-in-restores-remote-query ()
  "Setting `doom-modeline-vcs-remote-state-check' restores live state."
  (let ((doom-modeline-vcs-remote-state-check t)
        (state-calls 0))
    (cl-letf* (((symbol-function 'vc-backend) (lambda (&rest _) 'Git))
               ((symbol-function 'vc-state)
                (lambda (&rest _) (setq state-calls (1+ state-calls)) 'edited))
               ((symbol-function 'locate-dominating-file)
                (lambda (&rest _) nil)))
      (with-temp-buffer
        (setq-local vc-mode "Git:main")
        (setq buffer-file-name "/ssh:user@example.com:/home/user/f.el")
        (doom-modeline-update-vcs)
        (should (= state-calls 1))))))

(provide 'doom-modeline-segments-perf-test)
;;; doom-modeline-segments-perf-test.el ends here
