# AGENTS.md

Pure Elisp Emacs package (a mode-line replacement). Four root source files, no Cask, no build step beyond Eask.
The real version lives in the `doom-modeline.el` header (4.3.0); `Eask` still says `0.1.0`, so every eask command warns about the mismatch.

## Commands

```bash
eask test ert-runner                                   # full suite; green = "108 tests, 1 expected failures"
eask test ert-runner test/doom-modeline-env-test.el    # one test file
eask compile                                           # byte-compiles the 4 root *.el only
eask package                                           # dist/doom-modeline-<ver>.tar, root *.el only
emacs -Q --batch -l scripts/lint-segments.el -- doom-modeline-segments.el
```

- `eask exec ert-runner` fails (`ert-runner: command not found`). Use `eask test ert-runner`.
- ert-runner has no `-s/--selector`, and eask rejects unknown flags. For a single test, load it and pass an ERT selector:
  ```bash
  eask eval '(progn (require (quote ert)) (require (quote eieio))
                    (load "test/test-helper.el")
                    (load "test/doom-modeline-check-test.el")
                    (ert-run-tests-batch-and-exit "doom-modeline-flymake"))'
  ```
  `eieio` is needed because the check tests use `oset`; ert-runner happens to load it, a bare runner does not.
- `eask compile` leaves root `*.elc` behind, and `load` prefers `.elc` even when the `.el` is newer (`load-prefer-newer` is nil). Delete `*.elc` before testing edits made after a compile.
- Harmless noise on every eask invocation: `Error loading autoloads: (void-function defmacro*)`, "Missing `lexical-binding` cookie in Eask", the `0.1.0` warning.

## CI (`.github/workflows/ci.yml`)

Emacs 29.4 / 30.2 / 31.1 plus snapshot (experimental): `eask package` → `eask install` → `eask test ert-runner` → `eask compile`, then the segment lint.
`Package-Requires` still declares Emacs 25.1 as the floor, so gate newer APIs with `featurep` / `(when (>= emacs-major-version N) ...)` instead of assuming the CI versions.

## Redisplay-path contract (the repo's main design rule)

Segments run on **every** redisplay, so a segment body may only read caches:

- Expensive work lives in a `doom-modeline-update-*` function invoked from hooks/advice; it fills a `doom-modeline--*` cache plus a render key, and the segment rebuilds only when that key changes.
- The key must include every option the render depends on (`doom-modeline-icon`, `doom-modeline-check-icon`, `doom-modeline-unicode-fallback`, `doom-modeline--limited-width-p`, `(display-graphic-p)`, `doom-modeline--active`, ...), otherwise the cache cannot self-invalidate.
- Invalidate on real changes only: Flymake uses a buffer-local dirty flag set by advice on `flymake--handle-report`; Flycheck compares `flycheck-current-errors` / `flycheck-last-status-change` by `eq` identity (never deep `equal` over the error list).
- When every edit genuinely changes the result (e.g. `word-count`), a per-edit cache degenerates into a scan per keystroke. Throttle the scan with `timeout-throttle` (GNU ELPA `timeout`, min version 2.1.6), passing the option as a symbol so runtime changes take effect. Because the advice closure caches one global result, tag the result with `(BUFFER . COUNT)` and keep the displayed value buffer-local; never reuse another buffer's cached count.
- `scripts/lint-segments.el` fails CI when a segment body calls a whole-buffer, file, subprocess or project function (`count-words`, `line-number-at-pos`, `file-exists-p`, `project-current`, `call-process`, ...). Wrap it in a cached helper; calls guarded by `(unless doom-modeline--cache ...)` are allowed, and the `check` / `lsp` segments are exempt.

## Tests

- ert-runner auto-loads `test/test-helper.el`, which mocks `project-current`, `projectile-project-root` and `ffip-get-project-root-directory` to return `default-directory`; tests never need a real project. `test/.nosearch` stops package.el from recursing into the directory.
- Name tests `doom-modeline-feature/scenario`. Segments are exercised through their generated internals: `(doom-modeline-def-segment foo ...)` defines `doom-modeline-segment--foo`.
- Flycheck is **not** a dependency. Tests stub `flycheck-count-errors`, `flycheck-error-level-compilation-level` and set `flycheck-mode-menu-map` (see `doom-modeline-test--with-flycheck`).
- Flymake's backend-state constructor was renamed: Emacs 31 has `flymake--make-backend-state`, older versions `make-flymake--backend-state`. The helper probes both and `ert-skip`s otherwise.
- One test is deliberately `:expected-result :failed` (`doom-modeline-buffer-file-name/invalid`); "1 expected failures" is the passing baseline, do not chase it.

## Benchmarks

`bench/check-segment.el` (check segment, Flymake and Flycheck) and `bench/segments-perf.el` (cached segments, hot vs cold):

```bash
emacs -Q --batch -l bench/check-segment.el
DM_DIR=/path/to/tree DM_ITERATIONS=5000 emacs -Q --batch -l bench/check-segment.el
```

`DM_DIR` and `DM_ITERATIONS` are shared; `check-segment.el` adds `DM_N` (diagnostics), `segments-perf.el` adds `DM_LINES`, `DM_FORMAT`, `DM_SMOKE`. Both call `package-initialize` and drop any installed `doom-modeline-<ver>` from `load-path`, so local sources win. For before/after numbers, point `DM_DIR` at two byte-compiled copies of the sources (`load` picks `.elc` when present). `bench/` and `test/` fall outside `files "*.el"`, so they are never packaged or compiled; keep experiments there, not in the root.

## Conventions worth knowing

- `doom-modeline-def-segment` byte-compiles its body at load time (unless already native-compiled) and registers it in `doom-modeline--fn-alist`.
- `doom-modeline-def-modeline` is a **function**, not a macro: pass a quoted name (`'main`), and redefining the same name updates it in place.
- `defcustom`s that change rendering get a `doom-modeline-add-variable-watcher` that re-runs the matching `doom-modeline-update-*` for live buffers, so option changes apply without a restart.
- New special modelines need an entry in `doom-modeline-mode-alist` (`doom-modeline.el`).
- `doom-modeline--active` is t in batch (it compares `doom-modeline-current-window` with `frame-selected-window`), so bind it to nil to exercise the inactive path in tests or benchmarks.
