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
LOG_DIR="${LOG_DIR:-$HOME/logs}"
DATE_TAG="$(date +%F)"
LOG_FILE="$LOG_DIR/$DATE_TAG.log"
FAIL_FILE="$LOG_DIR/failures"
MAX_TASKS="${MAX_TASKS:-0}" # 0 = unlimited
DRY_RUN=0
ALLOWED_TOOLS="Read Edit Write Bash(cargo *) Bash(git *)"

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

n_done=0
n_failed=0
task_count=0
mapfile -t TASKS < "$TASKS_FILE"
remaining=()

for task in "${TASKS[@]}"; do
  [[ -z "$task" ]] && continue

  if [[ "$MAX_TASKS" -gt 0 && "$task_count" -ge "$MAX_TASKS" ]]; then
    remaining+=("$task")
    continue
  fi
  task_count=$((task_count + 1))

  echo "=== TASK: $task ===" | tee -a "$LOG_FILE"

  if [[ "$DRY_RUN" -eq 1 ]]; then
    echo "[dry-run] would run: claude -p \"$task\" --permission-mode acceptEdits --allowedTools \"$ALLOWED_TOOLS\""
    remaining+=("$task")
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

  if cargo fmt --check >>"$LOG_FILE" 2>&1 \
    && cargo clippy -- -D warnings >>"$LOG_FILE" 2>&1 \
    && cargo test >>"$LOG_FILE" 2>&1; then
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
    remaining+=("$task")
  fi
done

if [[ "$DRY_RUN" -eq 0 ]]; then
  if [[ "${#remaining[@]}" -eq 0 ]]; then
    rm -f "$TASKS_FILE"
  else
    printf '%s\n' "${remaining[@]}" >"$TASKS_FILE"
  fi
fi

echo "=== Summary: $n_done done, $n_failed failed, branch $BRANCH ===" | tee -a "$LOG_FILE"
if [[ "$n_failed" -gt 0 ]]; then
  echo "See $FAIL_FILE and 'git stash list' for failed attempts." | tee -a "$LOG_FILE"
fi
echo "Nothing was pushed. Review with: git log master..$BRANCH" | tee -a "$LOG_FILE"
