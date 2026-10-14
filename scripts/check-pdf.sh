#!/usr/bin/env bash
# Renders the demo statement in every language and on both bases, then checks the files
# with external PDF tools: `qpdf --check` (structure) and `pdftotext` (the text is
# extractable, which needs a correct ToUnicode map when the font is embedded). A tool
# that is not installed is skipped with a note.
#
# Usage: scripts/check-pdf.sh <path to the statements executable>
set -euo pipefail

exe=${1:?usage: scripts/check-pdf.sh <statements executable>}
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT

have() { command -v "$1" >/dev/null 2>&1; }
have qpdf || echo "qpdf not found: structure check skipped"
have pdftotext || echo "pdftotext not found: text check skipped"

# The title each language must show on page one (a function, not an associative array,
# so the script also runs on the bash 3 that macOS ships).
title() {
  case "$1" in
    en) echo "Account statement" ;;
    de) echo "Kontoauszug" ;;
    fr) echo "Relevé de compte" ;;
    es) echo "Extracto de cuenta" ;;
    ru) echo "Выписка по счёту" ;;
    ky) echo "Эсептен көчүрмө" ;;
  esac
}

status=0
for locale in en de fr es ru ky; do
  for basis in netted posting; do
    pdf="$out/$locale-$basis.pdf"
    "$exe" demo -locale "$locale" -basis "$basis" -output "$pdf"
    if have qpdf; then
      qpdf --check "$pdf" >/dev/null || { echo "qpdf rejects $locale/$basis"; status=1; }
    fi
    if have pdftotext; then
      text=$(pdftotext "$pdf" -)
      for expected in "$(title "$locale")" "Example Bank" "Ужин / Dinner" "980"; do
        if [[ "$text" != *"$expected"* ]]; then
          echo "$locale/$basis: text '$expected' not extracted"; status=1
        fi
      done
    fi
  done
done
[[ $status -eq 0 ]] && echo "all PDFs passed"
exit $status
