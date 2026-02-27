# doom-modeline AGENTS.md

This file provides guidelines for agentic coding tools operating in the doom-modeline repository.

## Project Overview

doom-modeline is an Emacs mode-line package that provides a minimal and modern modeline. It's a pure Elisp project with no external build system beyond Cask.

## Build, Lint, and Test Commands

### Prerequisites
- Emacs 27.2+ (tested against 27.2, 28.2, 29.4, 30.1, and snapshot)
- Cask (`brew install cask` or see https://cask.readthedocs.io/)

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

## Code Style Guidelines

### General Conventions
- **Always use `lexical-binding: t`** at the top of every `.el` file
- **Emacs version**: Target Emacs 27.2+, use `compat` package for compatibility shims
- **License**: GPL-3 (see standard package header below)

### File Header Format
Every `.el` file must have this header:
```elisp
;;; <filename>.el --- <description> -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2026 Vincent Zhang
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; <description>

;;; Code:

<content>

;;; <filename>.el ends here
```

### Imports and Dependencies
```elisp
;; Required at compile time
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; Runtime dependencies
(require 'nerd-icons)
(require 'shrink-path)
(require 'compat)  ; For compatibility across Emacs versions
```

### Naming Conventions
- **Public functions/variables**: `doom-modeline-*` prefix
- **Internal functions**: `doom-modeline--*` (double dash)
- **Private variables**: `doom-modeline--*` (double dash)
- **Segments**: Defined via `doom-modeline-def-segment`, named as symbols (e.g., `buffer-info`, `vcs`)
- **Modelines**: Defined via `doom-modeline-def-modeline`, named as symbols (e.g., `'main`, `'minimal`)

### Defining Segments
Use the `doom-modeline-def-segment` macro:
```elisp
(doom-modeline-def-segment segment-name
  "Docstring for the segment."
  ;; Body that returns the segment string
  (when some-condition
    (format "output: %s" value)))
```

### Defining Modelines
```elisp
(doom-modeline-def-modeline 'modeline-name
  '(left-hand-sideright-hand-side segments segments)
  '())
```

### Defining Environments (for version display)
```elisp
(doom-modeline-def-env python
  :command '("python" "--version")
  :parser (lambda () ...))
```

### Error Handling
- Use `error` for signaling errors: `(error "Format string %s" arg)`
- Use `user-error` for user-facing errors
- Avoid silent failures; prefer explicit error messages

### Custom Variables (defcustom)
```elisp
(defcustom doom-modeline-example-variable t
  "Description of the variable.
More details on separate lines."
  :type 'boolean
  :group 'doom-modeline)
```

### Use of Packages
- `cl-lib`: Use `cl-letf`, `cl-case`, `cl-pushnew`, etc. (prefixed)
- `subr-x`: Use `thread-first`, `thread-last`, `hash-table-keys`
- `compat`: Use `if-let`, `when-let`, `save-buffer` compatibility functions
- Avoid `cl` namespace (deprecated); use `cl-lib`

### Byte Compilation
- The `doom-modeline-def-segment` macro auto-byte-compiles segment functions
- Suppress warnings in generated code when appropriate:
  ```elisp
  (let (byte-compile-warnings)
    (byte-compile #'some-function))
  ```

### Testing Conventions
- Tests live in `test/` directory
- Test file naming: `doom-modeline-<module>-test.el`
- Use `ert-deftest` with descriptive names using `/` separator:
  ```elisp
  (ert-deftest doom-modeline-feature/scenario ()
    (let ((some-var t))
      (should (some-predicate))))
  ```
- Use `skip-unless` for version-dependent tests:
  ```elisp
  (skip-unless (>= emacs-major-version 26))
  ```
- Test helper file: `test-helper.el` provides mock functions for project detection

### Performance Considerations
- Avoid expensive operations in segment functions (they run on every modeline update)
- Use caching where appropriate
- Consider `doom-modeline-before-update-env-hook` and `doom-modeline-after-update-env-hook` for deferred work

### Common Patterns

#### Checking for features at runtime
```elisp
;; Check if feature is available
(when (featurep 'some-feature)
  (require 'some-feature))

;; Check Emacs version
(when (>= emacs-major-version 28)
  (some-new-function))
```

#### Adding to mode-line
```elisp
(add-hook 'doom-modeline-mode-hook
          (lambda ()
            (doom-modeline-set-modeline 'my-custom-modeline t)))
```

### Key Files
| File | Purpose |
|------|---------|
| `doom-modeline.el` | Main entry point, mode definition |
| `doom-modeline-core.el` | Core utilities, segment/modeline macros |
| `doom-modeline-segments.el` | All built-in segments |
| `doom-modeline-env.el` | Environment version detection |
| `test/` | Test files |

### Useful Development Commands
```elisp
;; Reload package during development
(require 'doom-modeline nil t)

;; Enable debug on errors
(setq debug-on-error t)

;; Check package compatibility
(package-lint-current-buffer)
```
