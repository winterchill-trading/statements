# .github/

`workflows/ci.yml`: infra `ox-setup` with sibling pins `fixed-decimal`, `money`,
`ledger` (dependency order; add a new sibling dependency here and in `dune-project`
together), then build, tests, the quickstart, `scripts/check-pdf.sh` (installs `qpdf`
and `poppler-utils` from apt), odoc with warnings as errors, and the shared `obj-magic`
workflow.

Check a run with `gh run list --limit 1` and at most one `gh run watch <id> --exit-status`.
