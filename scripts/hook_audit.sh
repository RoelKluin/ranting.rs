#!/usr/bin/env bash
# Re-runnable unused-hook audit (ROADMAP.md Phase 7 item 1).
#
# Answers two questions the `Ranting` trait cannot answer about itself:
#   1. Which trait methods has a *real fork* ever overridden?
#   2. Within the hooks that are overridden, which *parameters* are ignored?
#
# A hook that ships with an English-preserving default always compiles and
# always passes its tests, whether or not its shape matches a real need. The
# only evidence that a hook's shape is right is a fork that had to use it.
#
# Scope: the two downstream falsifier crates, which depend on `ranting` alone
# and are the repo's only public-API-only consumers. `src/` and
# `tests/ranting/` are deliberately excluded -- a main-crate test exercising a
# hook proves the plumbing works, not that a language needed it.
#
# Discovers sibling crates by globbing, like overnight_loop.sh's gate_dirs, so
# a third falsifier is audited without editing this script.
set -uo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

# Every fork directory: has its own Cargo.toml, depends on ranting, and is
# neither the root crate nor part of the library itself.
FORKS=()
for manifest in "$REPO_ROOT"/*/Cargo.toml; do
    dir="$(dirname "$manifest")"
    base="$(basename "$dir")"
    case "$base" in
        ranting_core | ranting_derive) continue ;;
    esac
    grep -q '^ranting = ' "$manifest" 2>/dev/null && FORKS+=("$dir")
done

if [ ${#FORKS[@]} -eq 0 ]; then
    echo "No falsifier crates found." >&2
    exit 1
fi

# The authoritative method list, read from the trait rather than hardcoded.
mapfile -t METHODS < <(awk '/^pub trait Ranting/,/^\}/' "$REPO_ROOT/src/lib.rs" |
    grep -oP '^\s+fn \K\w+' | sort)

echo "Ranting trait methods: ${#METHODS[@]}"
echo "Forks audited: ${FORKS[*]##*/}"
echo
printf '%-45s' "METHOD"
for f in "${FORKS[@]}"; do printf '%-14s' "${f##*/}"; done
echo

never=()
for m in "${METHODS[@]}"; do
    printf '%-45s' "$m"
    total=0
    for f in "${FORKS[@]}"; do
        # Count only *lexicon* overrides: src/, not the crate's own tests/,
        # where a hook may appear in a probe asserting it is never called.
        n=$(grep -rlE "fn $m\s*\(" "$f/src/" 2>/dev/null | wc -l)
        total=$((total + n))
        printf '%-14s' "$n"
    done
    [ "$total" -eq 0 ] && never+=("$m")
    echo
done

echo
echo "Never overridden by any fork (${#never[@]}):"
for m in "${never[@]}"; do echo "  $m"; done

echo
echo "Ignored parameters in overridden hooks (leading underscore == declared, unused):"
for p in count sentence_start case class as_plural degree uc style; do
    ign=0
    use=0
    for f in "${FORKS[@]}"; do
        ign=$((ign + $(grep -rhE "^[[:space:]]*_$p:" "$f/src/" 2>/dev/null | wc -l)))
        use=$((use + $(grep -rhE "^[[:space:]]*$p:" "$f/src/" 2>/dev/null | wc -l)))
    done
    [ $((ign + use)) -eq 0 ] && continue
    printf '  %-16s declared=%-3s ignored=%-3s used=%s\n' "$p" "$((ign + use))" "$ign" "$use"
done
