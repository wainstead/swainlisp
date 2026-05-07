# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A personal Emacs configuration. The primary working language is Emacs Lisp. There is no build step, no test suite, and no package manifest — changes take effect by reloading Emacs or evaluating the relevant buffer (`M-x eval-buffer`).

## How Configuration Loads

1. `init.el` — entry point; sets up MELPA, installs key-chord, configures macOS key modifiers, then calls:
2. `personal-lisp/swainlib.el` — the main library (~870 lines); loads all other modules from `personal-lisp/`
3. `personal-lisp/org-mode-mods.el` — GTD/org-mode setup (capture templates, agenda, refile, TODO keywords)
4. Language-specific files: `python-settings.el`, `ruby-rails.el`, `java-custom.el`, `php-custom.el`
5. Feature files: `desktop-auto-save.el`, `tail-logs.el`, `sw-sql-map.el`, `compilation-buffer-extras.el`

## Package Management

- **No use-package or straight.el.** Packages are installed via the native ELPA system and then `require`d directly.
- The active `elpa/` directory is gitignored and contains only key-chord. Most previously used packages are archived in `elpa.bak/`.
- External packages that aren't on MELPA live in `external-packages/` and are loaded manually.
- To add a package: install it via `M-x package-install`, add a `(require ...)` in the appropriate `personal-lisp/` file, and commit.

## Key Architectural Patterns

**Key-chord bindings**: The config uses key-chord extensively (two-key simultaneous presses). Chord definitions live in `swainlib.el`. Examples: `jv` → next-buffer, `jc` → open CLI shell buffer, `jk` → kill-buffer. When adding keybindings, prefer chords or the existing `M-a`/`M-s` prefix maps over bare global keys.

**god-mode**: Modal editing is enabled via god-mode. The mode-line background changes color to indicate the current mode. god-mode config is in `swainlib.el`.

**Shell buffer management**: Custom functions (`sw-cli`, `sw-sql`, `sw-tail`, etc.) spawn and name shell buffers. Named buffers (cli, sql, tail, root) are persisted across sessions via `desktop-auto-save.el`.

**GTD org system**: Org files live outside this repo at `~/Documents/GTD/`. Capture templates and agenda config are entirely in `personal-lisp/org-mode-mods.el`.

**Machine-specific customization**: Per-machine overrides go in `customization-files/emacs-<hostname>-custom.el`. These are loaded conditionally by hostname.

## Making Changes

- Most changes belong in `personal-lisp/swainlib.el` (core keybindings, UI, general utilities) or the relevant language/feature file.
- New standalone features should be a new file in `personal-lisp/` with a `(provide 'feature-name)` at the bottom, then `(require 'feature-name)` added in `swainlib.el`.
- `swainlib.historical.el` is a backup of a previous version — do not modify it.
- Files in `old-elisp/` are historical artifacts — do not load or reference them.
