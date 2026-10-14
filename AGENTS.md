# AGENTS.md

Guide for AI coding agents working in this repository. The workspace-wide guide is
`../oxcaml-claude-skills/AGENTS.md`.

## Purpose

`statements` builds customer-facing account statements from the journal of `../ledger`
and renders them as CSV and PDF. This file grows with the library.

## Build

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @statements/all
opam exec --switch 5.2.0+ox -- dune build --root .. @statements/runtest
```

Parallel work: prefix with `DUNE_BUILD_DIR=_build-statements`.
