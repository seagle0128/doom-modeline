# doom-modeline AGENTS.md

This file provides guidelines for agentic coding tools operating in the doom-modeline repository.

## Project Overview

doom-modeline is an Emacs mode-line package providing a minimal and modern modeline.
Pure Elisp project — no external build system beyond Cask. Version 4.3.0.

**Package-Requires**: `(emacs "25.1") (compat "30.1.0.0") (nerd-icons "0.1.0") (shrink-path "0.3.1")`

## Build, Lint, and Test Commands

### Prerequisites
- Emacs 27.2+ (development target; CI tests 27.2, 28.2, 29.4, 30.2, and snapshot)
- Cask (`brew install cask` or https://cask.readthedocs.io/)

### Commands
```bash
# Install dependencies
cask install

# Run all tests
cask exec ert-runner

# Run a single test (ERT selector)
cask exec ert-runner -l "doom-modeline-project-root/auto"

# Build the package
cask build

# Package for distribution
cask package
```

### Running Tests in Emacs Manually
```elisp
(require 'doom-modeline-core)
(ert-run-tests-interactively "doom-modeline-project-root/auto")
```

## Architecture

### Key Files
| File | Purpose |
|------|---------|
| `doom-modeline.el` | Main entry point, mode definition, modeline format definitions |
| `doom-modeline-core.el` | Core utilities, segment/modeline macros, faces, `defcustom`s |
| `doom-modeline-segments.el` | All built-in segment implementations |
| `doom-modeline-env.el` | Environment version detection (Python, Ruby, Go, etc.) |
| `test/` | Test files (`test-helper.el` provides mock functions) |

### File Header Format
Every `.el` file starts with:
```elisp
;;; <filename>.el --- <description> -*- lexical-binding: t; -*-
```
and ends with:
```elisp
(provide '<module>)
;;; <filename>.el ends here
```

## Code Style Conventions

### Required
- **`lexical-binding: t`** at the top of every `.el` file (the `-*-` cookie line)
- **Public API**: `doom-modeline-*` prefix
- **Internals**: `doom-modeline--*` (double dash for both functions and variables)
- **Segments**: defined via `doom-modeline-def-segment`, named as bare symbols (`buffer-info`, `vcs`)
- **Modelines**: defined via `doom-modeline-def-modeline`, named as quoted symbols (`'main`, `'minimal`)
- **Environments**: defined via `doom-modeline-def-env` (see `doom-modeline-env.el`)

### Imports
```elisp
(require 'compat)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))
```
- `cl-lib` prefixed only (`cl-letf`, `cl-case`, `cl-pushnew`). Never `cl` (deprecated).
- `compat` for cross-version compatibility (`if-let`, `when-let`, etc.)

### `defcustom` Group
All user options belong to the `doom-modeline` group (or `doom-modeline-env` subgroup).

## Testing

- Tests live in `test/` with a `.nosearch` marker file to prevent recursive scanning.
- Use `ert-deftest` with slash-separated descriptive names: `doom-modeline-feature/scenario`
- Use `skip-unless` for version-dependent tests
- **`test-helper.el`** mocks these functions so tests don't need real projects:
  - `ffip-get-project-root-directory` → returns `default-directory`
  - `projectile-project-root` → returns `default-directory`
  - `project-current` → returns `(vc . ,default-directory)`
- Test patterns to follow:
  ```elisp
  (ert-deftest doom-modeline-project-root/projectile ()
    (let ((default-directory "/home/user/project/")
          (doom-modeline-project-detection 'projectile)
          (doom-modeline--project-root nil))
      (cl-flet ((projectile-project-root () default-directory))
        (should (string= (doom-modeline-project-root) default-directory)))))
  ```

## Key Patterns

### Defining Segments
```elisp
(doom-modeline-def-segment segment-name
  "Docstring."
  ;; Body returns the segment string
  (when some-condition
    (format "output: %s" value)))
```
Segment functions are **auto-byte-compiled** by the macro. The segment runs on every modeline update — avoid expensive operations.

### Defining Modelines
```elisp
(doom-modeline-def-modeline 'modeline-name
  '(lhs-segments...)   ; left side
  '(rhs-segments...))  ; right side

;; Redefining the same NAME updates it in-place (no duplicate)
```

### Variable Watchers (pervasive pattern)
Many `defcustom`s have watchers that auto-update visible segments when the value changes:
```elisp
(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs))))))
```
Always add a variable watcher when a `defcustom` controls segment appearance.

### Segment Auto-Update via Hooks
Segments typically register hooks to stay current. Common hooks used:
- `find-file-hook`, `after-save-hook` — for file/buffer state
- `window-state-change-functions` — for width-dependent formatting
- `post-command-hook` — for selection info
- `flycheck-status-changed-functions` / `advice` on `flymake--handle-report` — for check segments
- `after-change-major-mode-hook` — for mode-specific info

### Modeline-to-Major-Mode Mapping
`doom-modeline-mode-alist` in `doom-modeline.el` maps major modes to modeline variants. Add entries here for new special modelines:
```elisp
(defvar doom-modeline-mode-alist
  '((message-mode         . message)
    (Info-mode            . info)
    (pdf-view-mode        . pdf)
    ...))
```

### Runtime Feature Checks
```elisp
(when (featurep 'some-feature)
  (require 'some-feature))

(when (>= emacs-major-version 28)
  (some-new-function))
```

### Environment Version Detection
Uses `doom-modeline-def-env` macro — define a command, a parser function, and hooks:
```elisp
(doom-modeline-def-env python
  :hooks '(python-mode-hook python-ts-mode-hook)
  :command (lambda () (list "python" "--version"))
  :parser (lambda (line) (cadr (split-string line))))
```
The macro auto-generates `doom-modeline-env-setup-<name>` and `doom-modeline-env-update-<name>` functions.

### Adding/Removing Segments at Runtime
Users can add/remove segments from existing modelines without redefining them:
```elisp
;; Add after anchor (in all modelines, or a specific one)
(doom-modeline-add-segment 'my-seg 'buffer-info :after 'main)
(doom-modeline-add-segment 'my-seg 'buffer-info :before)  ;; or :before

;; Remove
(doom-modeline-remove-segment 'my-seg 'main)

;; Exclude certain modelines from these operations
(setq doom-modeline-excluded-modelines '(speedbar))
```

## Performance

- Segment functions run on EVERY modeline update. Cache aggressively.
- Use `doom-modeline--limited-width-p` to skip details in narrow windows.
- Use hooks (`doom-modeline-before-update-env-hook`, `doom-modeline-after-update-env-hook`) for deferred/async work.
- Avoid file I/O or subprocess calls in segment functions (use cached values updated by hooks).

## Development Tips

```elisp
;; Reload package during development
(require 'doom-modeline nil t)

;; Enable debug on errors
(setq debug-on-error t)
```
