#!/usr/bin/env bash
# Runs each remaining line in tasks.txt through 'claude -p', gated by
# cargo fmt/clippy/test, committing successes to an isolated overnight
# branch. Never pushes. Completed tasks move to tasks_done.txt so a
# rerun resumes where it left off; failed tasks stay in tasks.txt for
# retry on the next run.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

TASKS_FILE="${TASKS_FILE:-tasks.txt}"
DONE_FILE="${DONE_FILE:-tasks_done.txt}"
# Per-repo, not a shared $HOME/logs: every overnight loop on the machine wrote
# to the same log and the same failures file, so entries from other repos
# interleaved with this one's and neither file could be trusted to describe a
# given run. Kept outside the repo deliberately -- a log directory inside it
# would be swept up by the per-task 'git add -A' below, and would trip the
# working-tree-clean pre-flight check in any repo that had not gitignored it.
LOG_DIR="${LOG_DIR:-$HOME/logs/$(basename "$REPO_ROOT")}"
DATE_TAG="$(date +%F)"
LOG_FILE="$LOG_DIR/$DATE_TAG.log"
FAIL_FILE="$LOG_DIR/failures"
MAX_TASKS="${MAX_TASKS:-0}" # 0 = unlimited
DRY_RUN=0
ALLOWED_TOOLS="Read Edit Write Bash(cargo *) Bash(git *)"

# This repo is not a cargo workspace -- ranting, ranting_core, ranting_derive
# and ranting_i18n each have their own Cargo.toml/Cargo.lock -- so a gate that
# only ran at the repo root never compiled ranting_i18n at all. Discovered by
# listing every directory with its own Cargo.toml, so a future sibling crate
# is picked up automatically without editing this script again.
gate_dirs() {
  local f
  for f in "$REPO_ROOT"/Cargo.toml "$REPO_ROOT"/*/Cargo.toml; do
    [[ -f "$f" ]] && dirname "$f"
  done
}

# Runs fmt/clippy/test in one crate directory, logging which directory failed
# so a gate failure points straight at the offending crate instead of forcing
# a manual re-run per directory to find it.
run_gate_in() {
  local dir="$1"
  echo "--- gate: $dir ---" | tee -a "$LOG_FILE"
  (
    cd "$dir" &&
      cargo fmt --check &&
      cargo clippy -- -D warnings &&
      cargo test
  ) >>"$LOG_FILE" 2>&1
}

# Runs the gate in every sibling crate directory; fails (and reports which
# directory) on the first failure.
run_gate() {
  local dir
  while IFS= read -r dir; do
    if ! run_gate_in "$dir"; then
      echo "Gate failed in $dir" | tee -a "$LOG_FILE" >&2
      return 1
    fi
  done < <(gate_dirs)
  return 0
}

usage() {
  cat <<EOF
Usage: $(basename "$0") [--dry-run] [--max-tasks N]

  --dry-run        Print what would run without invoking claude or git.
  --max-tasks N    Only process the first N remaining tasks this run.
EOF
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run) DRY_RUN=1; shift ;;
    --max-tasks) MAX_TASKS="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
done

mkdir -p "$LOG_DIR"
touch "$DONE_FILE"

if [[ ! -s "$TASKS_FILE" ]]; then
  echo "No tasks remaining in $TASKS_FILE." | tee -a "$LOG_FILE"
  exit 0
fi

if [[ -n "$(git status --porcelain)" ]]; then
  echo "Working tree not clean; commit or stash before running." >&2
  exit 1
fi

BRANCH="overnight/$DATE_TAG"
if git rev-parse --verify --quiet "$BRANCH" >/dev/null; then
  echo "Branch $BRANCH already exists; refusing to reuse it. Delete it or rerun tomorrow." >&2
  exit 1
fi

if [[ "$DRY_RUN" -eq 0 ]]; then
  git checkout -b "$BRANCH"
else
  echo "[dry-run] would create branch $BRANCH"
fi

# Pre-flight: if the gate itself is already broken on the starting commit,
# every task below is unwinnable regardless of what it changes -- fail fast
# instead of burning the whole run stashing 12 doomed attempts.
if [[ "$DRY_RUN" -eq 0 ]]; then
  echo "=== Pre-flight gate check ===" | tee -a "$LOG_FILE"
  if ! run_gate; then
    echo "Pre-flight gate failed on the starting commit -- fix that first, then rerun." | tee -a "$LOG_FILE" >&2
    echo "See $LOG_FILE for details." >&2
    exit 1
  fi
fi

n_done=0
n_failed=0
task_count=0

# Tasks are consumed one at a time, re-reading the file each iteration, so new
# lines appended to it *while the run is in progress* get picked up. The file is
# never rewritten wholesale -- a completed task is deleted from it individually,
# the moment it lands -- so an append can never be clobbered by a stale in-memory
# copy. (The previous mapfile-up-front + rewrite-at-the-end design silently
# discarded anything appended mid-run.)
declare -A attempted=()

# First non-blank line not already tried this run. Failed tasks stay in the file
# for the next run but are marked attempted, so one run never retries them --
# without that, a failing task would be picked forever.
next_task() {
  local line
  while IFS= read -r line || [[ -n "$line" ]]; do
    [[ -z "$line" ]] && continue
    [[ -n "${attempted["$line"]:-}" ]] && continue
    printf '%s' "$line"
    return 0
  done <"$TASKS_FILE"
  return 1
}

# Delete one exact line, preserving everything else -- including lines appended
# since the run started. awk compares the whole line as a literal string (no
# regex), and writes via a temp file in the same directory so a concurrent
# appender never sees a half-written file.
drop_task() {
  local tmp
  tmp="$(mktemp "$TASKS_FILE.XXXXXX")"
  TASK_TO_DROP="$1" awk 'BEGIN { t = ENVIRON["TASK_TO_DROP"] } $0 != t' \
    "$TASKS_FILE" >"$tmp"
  mv "$tmp" "$TASKS_FILE"
}

while :; do
  [[ -f "$TASKS_FILE" ]] || break

  if [[ "$MAX_TASKS" -gt 0 && "$task_count" -ge "$MAX_TASKS" ]]; then
    echo "Reached --max-tasks $MAX_TASKS; stopping with tasks still queued." |
      tee -a "$LOG_FILE"
    break
  fi

  task="$(next_task)" || break
  attempted["$task"]=1
  task_count=$((task_count + 1))

  echo "=== TASK: $task ===" | tee -a "$LOG_FILE"

  if [[ "$DRY_RUN" -eq 1 ]]; then
    echo "[dry-run] would run: claude -p \"$task\" --permission-mode acceptEdits --allowedTools \"$ALLOWED_TOOLS\""
    continue
  fi

  set +e
  claude -p "$task" \
    --permission-mode acceptEdits \
    --allowedTools "$ALLOWED_TOOLS" \
    >>"$LOG_FILE" 2>&1
  claude_exit=$?
  set -e
  if [[ "$claude_exit" -ne 0 ]]; then
    echo "claude -p exited $claude_exit" >>"$LOG_FILE"
  fi

  if run_gate; then
    # Dequeue *before* committing, so the queue update is part of this task's
    # own commit. If it were left uncommitted, a later task's gate failure
    # would 'git stash push -u' it away along with that task's work, and the
    # finished task would reappear in the queue (verified: it did).
    drop_task "$task"
    if [[ -n "$(git status --porcelain)" ]]; then
      git add -A
      git commit -m "auto: ${task:0:72}" >>"$LOG_FILE" 2>&1
    else
      echo "No changes produced; nothing to commit." >>"$LOG_FILE"
    fi
    echo "$task" >>"$DONE_FILE"
    n_done=$((n_done + 1))
    echo "DONE: $task" | tee -a "$LOG_FILE"
  else
    git stash push -u -m "failed: ${task:0:72}" >>"$LOG_FILE" 2>&1
    echo "FAILED: $task" >>"$FAIL_FILE"
    n_failed=$((n_failed + 1))
    echo "FAILED: $task" | tee -a "$LOG_FILE"
  fi
done

# Only remove the file once nothing is left in it -- a failed task, or one
# appended mid-run and not reached, must survive for the next run.
if [[ "$DRY_RUN" -eq 0 && -f "$TASKS_FILE" ]]; then
  if ! grep -q '[^[:space:]]' "$TASKS_FILE"; then
    rm -f "$TASKS_FILE"
  fi
fi

echo "=== Summary: $n_done done, $n_failed failed, branch $BRANCH ===" | tee -a "$LOG_FILE"
if [[ "$n_failed" -gt 0 ]]; then
  echo "See $FAIL_FILE and 'git stash list' for failed attempts." | tee -a "$LOG_FILE"
fi
echo "Nothing was pushed. Review with: git log master..$BRANCH" | tee -a "$LOG_FILE"
