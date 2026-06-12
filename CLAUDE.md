# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

A personal Emacs configuration. The primary working language is Emacs Lisp. There is no build step, no test suite, and no package manifest — changes take effect by reloading Emacs or evaluating the relevant buffer (`M-x eval-buffer`).

## How Configuration Loads

1. `init.el` — entry point; bootstraps straight.el, configures macOS key modifiers, then calls:
2. `personal-lisp/packages-core.el` — straight.el bootstrap, god-mode, key-chord, alfred-buffers
3. `personal-lisp/swainlib.el` — the main library; loads all other modules, then at the end auto-loads `customization-files/emacs-<hostname>-custom.el` if present
4. `personal-lisp/org-mode-mods.el` — GTD/org-mode base config (portable capture templates, agenda, refile, TODO keywords)
5. `personal-lisp/packages-langs.el` — language modes (markdown, yaml, json, terraform, etc.)
6. `personal-lisp/packages-tools.el` — dev tools (magit, highlight-indentation, llama)
7. `personal-lisp/packages-ai.el` — claude-code-ide and vterm
8. Feature files: `desktop-auto-save.el`, `tail-logs.el`, `sw-sql-map.el`

## Package Management

- **straight.el** is the package manager. It bootstraps itself on first launch and clones packages from git.
- To add a package: add `(straight-use-package 'package-name)` to the appropriate `personal-lisp/packages-*.el` file and restart Emacs. straight.el will install it automatically.
- `external-packages/alfred-buffers.el` is the only package not on MELPA; it is loaded directly from `packages-core.el`.
- The `straight/` directory is gitignored (managed by straight.el, not committed).

## Machine-Specific Configuration

Per-machine overrides live in `customization-files/emacs-<hostname>-custom.el` and are loaded automatically at the end of `swainlib.el`. The file is optional — if absent, Emacs loads cleanly with base config only.

To find the hostname for a new machine: `hostname -s` in a terminal.

The work laptop file (`customization-files/emacs-REM-MAC-19585-custom.el`) adds:
- `~/Documents/workfiles/lisp.el`
- Work org-agenda files (jira-tickets.org, kanban-project.org, team-building.org)
- Work org-capture templates (morning checklist, Friday checklist)

## New Machine Setup (Personal Laptop)

After cloning or pulling `master` on a new machine:

1. **Install prerequisites**: Emacs 28+, CMake (`brew install cmake`), and the Claude Code CLI (`npm install -g @anthropic-ai/claude-code`)
2. **Launch Emacs** — straight.el will bootstrap itself and install all packages on first run. This takes a minute; watch `*Messages*` for progress.
3. **Verify** no errors in `*Messages*` (`M-x view-echo-area-messages`)
4. **Get the hostname**: run `hostname -s` in a terminal
5. **Create a machine customization file**: `customization-files/emacs-<hostname>-custom.el` — can be empty or contain any machine-specific overrides (fonts, paths, org files for personal projects, etc.)
6. **Test key-chord**: type `jv` quickly — should call `next-buffer`
7. **Test god-mode**: type `jg` — background should change to dark navy/goldenrod
8. **Test magit**: `M-x magit-status`
9. Commit the new customization file to the repo

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
