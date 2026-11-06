#!/usr/bin/env bash
# Keeps the two long-lived branches in step. dev-only (not on main).
#
#   main = package code + docs + minimal CI. This is what the upstream PR shows.
#   dev  = main + test/CI infrastructure (list below). Nothing else may differ.
#
# Usage: tools/branches.sh check | sync | main-check
#   check       fail if dev and main differ outside the infrastructure list, if main
#               holds any infrastructure, or if dev lacks a commit that is on main
#   sync        on dev: merge main into dev, then run check
#   main-check  run R CMD check on main in a throwaway worktree (no tests there)
set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

ref() { git rev-parse --verify -q "$1" >/dev/null && echo "$1" || echo "origin/$1"; }
MAIN=$(ref main); DEV=$(ref dev)

# Infrastructure that exists only on dev (whole files / directories) ...
INFRA='^(tests/|tools/|\.lintr$|\.github/workflows/(check|r)\.yml$)'
# ... and files that exist on both but may differ only in these lines.
# Normalised before comparing: drop testthat/withr/.lintr/tools lines, trailing commas.
MIXED='^(DESCRIPTION|\.Rbuildignore)$'
norm() { grep -vE 'testthat|^[[:space:]]+withr|^\^\\\.lintr|^\^tools' | sed -E 's/,[[:space:]]*$//'; }

fail=0
bad() { echo "FAIL: $*" >&2; fail=1; }

check() {
  # 1. dev vs main differ only in infrastructure
  while IFS= read -r f; do
    [[ $f =~ $INFRA ]] && continue
    if [[ $f =~ $MIXED ]]; then
      if ! diff -q <(git show "$MAIN:$f" | norm) <(git show "$DEV:$f" | norm) >/dev/null; then
        bad "$f differs beyond the allowed infrastructure lines"
      fi
      continue
    fi
    bad "$f differs between main and dev: package changes belong on main, then 'tools/branches.sh sync'"
  done < <(git diff --name-only "$MAIN" "$DEV")

  # 2. main carries no infrastructure
  while IFS= read -r f; do
    [[ $f =~ $INFRA && $f != .github/workflows/r.yml ]] && bad "main contains infrastructure file $f"
  done < <(git ls-tree -r --name-only "$MAIN")
  git show "$MAIN:DESCRIPTION" | grep -qE 'testthat|withr' && bad "main DESCRIPTION mentions testthat/withr"

  # 3. dev has everything on main
  git merge-base --is-ancestor "$MAIN" "$DEV" || bad "dev is behind main: run tools/branches.sh sync"

  [[ $fail == 0 ]] && echo "OK: dev = main + infrastructure only"
  return $fail
}

case "${1:-}" in
  check) check ;;
  sync)
    [[ $(git branch --show-current) == dev ]] || { echo "switch to dev first" >&2; exit 1; }
    git merge --no-edit main
    check ;;
  main-check)
    tmp=$(mktemp -d); trap 'git worktree remove --force "$tmp/w" 2>/dev/null; rm -rf "$tmp"' EXIT
    git worktree add -q --detach "$tmp/w" "$MAIN"
    (cd "$tmp" && R CMD build w >/dev/null && _R_CHECK_FORCE_SUGGESTS_=false R CMD check --no-manual alchemeR_*.tar.gz | tail -3) ;;
  *) sed -n '2,13p' "$0"; exit 2 ;;
esac
