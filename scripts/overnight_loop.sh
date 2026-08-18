#!/usr/bin/env bash
# Generic unattended task-runner loop: runs each remaining line in tasks.txt
# through 'claude -p', gated by a quality check, committing successes to an
# isolated overnight/<date> branch. Never pushes. Completed tasks are logged
# to tasks_done.txt; failed tasks stay in tasks.txt for retry on a later run.
#
# Synthesized from four repos' independent copies of this script (ranting,
# brain-forge, safecommit, reckoning), each of which had hardened it against
# a real overnight incident the others hadn't hit yet. See SKILL.md in this
# same directory for setup instructions and the incident history behind each
# design choice below, condensed.
set -euo pipefail

# --- Self-modification hardening --------------------------------------------
# A task editing *this file* while a run is executing it has two bad
# consequences: bash parses the whole script (including this while loop) up
# front and then reads the already-open file descriptor by byte offset, so an
# in-flight run keeps executing pre-edit logic regardless of what changed on
# disk; and if the edit changed the file's length, bash's saved "resume after
# the loop" offset can land mid-body in the new file, skipping or corrupting
# the summary/cleanup code. Fix: copy this script to a private path and exec
# that copy, so no task running later in the same invocation can affect it --
# an edit to the script on disk only ever changes the *next* invocation.
if [[ -z "${OVERNIGHT_LOOP_REEXEC:-}" ]]; then
  export OVERNIGHT_LOOP_REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
  tmp_self="$(mktemp "${TMPDIR:-/tmp}/overnight_loop.XXXXXX.sh")"
  cp "${BASH_SOURCE[0]}" "$tmp_self"
  chmod +x "$tmp_self"
  export OVERNIGHT_LOOP_REEXEC=1
  exec "$tmp_self" "$@"
fi
# Only reached inside the re-exec'd copy -- a trap set before `exec` above
# would never fire, since exec replaces the process image, traps and all.
trap 'rm -f "${BASH_SOURCE[0]}"' EXIT

REPO_ROOT="${OVERNIGHT_LOOP_REPO_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
cd "$REPO_ROOT"

# Optional per-repo config, sourced before any ${VAR:-default} below picks up
# its value -- lets a repo with e.g. a nested-gitlink crate layout
# (NESTED_REPO_GLOB) or a non-cargo gate (GATE_CMD) pin that config in the
# repo itself, committed, rather than relying on every invocation's caller to
# export the right env vars by hand. Kept as a separate file rather than
# edits to this script so the script itself stays identical across repos.
[[ -f "$REPO_ROOT/scripts/overnight_loop.local.env" ]] && source "$REPO_ROOT/scripts/overnight_loop.local.env"

TASKS_FILE="${TASKS_FILE:-tasks.txt}"
DONE_FILE="${DONE_FILE:-tasks_done.txt}"
# Per-repo, not a shared $HOME/logs: unqualified, every repo's loop on the
# same machine interleaves into the same log/failures file and neither can be
# trusted to describe one run. Kept outside the repo deliberately -- a log
# dir inside it would get swept into a task's commit and trip the
# working-tree-clean pre-flight check.
LOG_DIR="${LOG_DIR:-$HOME/logs/$(basename "$REPO_ROOT")}"
DATE_TAG="$(date +%F)"
LOG_FILE="$LOG_DIR/$DATE_TAG.log"
FAIL_FILE="$LOG_DIR/failures"
# Outside the repo for the same reason as LOG_DIR above: it also ends in
# ".lock", which STAGE_EXTENSIONS below recognizes (to allow staging real
# dependency lockfiles like Cargo.lock) -- an in-repo tasks.txt.lock would
# get git-added as a side effect of that and end up committed as bookkeeping.
LOCK_FILE="$LOG_DIR/$(basename "$TASKS_FILE").lock"
# Where declined untracked files get moved instead of left in the tree (see
# quarantine_declined() below) -- leaving them in place would otherwise
# permanently wedge every later invocation's clean-tree pre-flight check.
DECLINED_DIR="${DECLINED_DIR:-$LOG_DIR/declined}"
MAX_TASKS="${MAX_TASKS:-0}" # 0 = unlimited
# Nothing else bounds how long one `claude -p` call can block (network stall,
# an unattended permission prompt, a wedged child process). Generous default;
# override per repo/task mix.
TASK_TIMEOUT="${TASK_TIMEOUT:-3600}"
# `timeout` alone sends one SIGTERM at expiry and then just waits -- a
# process (or child) that ignores SIGTERM defeats TASK_TIMEOUT entirely.
# `-k` escalates to SIGKILL this many seconds later.
TASK_KILL_AFTER="${TASK_KILL_AFTER:-20}"
DRY_RUN=0
# GATE_CMD, if set, is run verbatim (via `bash -c`) from $REPO_ROOT as the
# sole quality gate -- set this for non-Rust repos, or Rust repos with a
# different check set. Left unset, this script auto-detects a Cargo
# workspace-of-independent-crates layout (see gate_dirs()) and runs
# fmt/clippy/test in each crate directory, which is what all four source
# repos actually needed (none of them were a single Cargo workspace).
GATE_CMD="${GATE_CMD:-}"
# Space-separated glob(s), relative to $REPO_ROOT, for crate/package
# directories that are *nested independent git repos* (gitlink entries, not
# submodules) rather than plain subdirectories -- e.g. "crates/*". A bare
# `git add -A && git commit` at the top level only stages a gitlink pointer
# bump, and only if the nested repo's own HEAD already moved, so a task that
# edits code inside one of these must have that nested repo committed
# separately or the "real" change never lands in either repo's history.
# Leave empty (the default) if the repo has no such layout.
NESTED_REPO_GLOB="${NESTED_REPO_GLOB:-}"
# Extensions stage_one_untracked_file() is willing to auto-stage. An
# allowlist, not a denylist: a denylist only blocks scratch/output types
# already seen going wrong, so the next unanticipated one sails through.
STAGE_EXTENSIONS="${STAGE_EXTENSIONS:-.rs .toml .md .txt .lock .sh .stderr .gitignore}"
# Read-only git tools only: a task's `claude -p` invocation must never be
# able to create git history itself -- only this loop, after the task
# returns and the gate passes, stages and commits. (Verified necessary: an
# unrestricted `Bash(git *)` grant let a task self-commit its own scratch
# files, bypassing every staging guard below.) EXTRA_ALLOWED_TOOLS defaults
# to a cargo grant for the auto-detected gate -- named subcommands only
# (check/build/test/fmt/clippy: what a task needs to verify its own edit
# compiles and passes the same checks the post-task gate below re-runs
# independently), not a blanket `cargo *`, which would also hand a task
# `cargo publish`/`add`/`remove`/`login`/`install`/arbitrary `cargo run` --
# none of which a task doing a single queued fix has a legitimate reason to
# need, and a broader grant only invites an unattended task to wander off
# task exploring instead of doing the narrow thing it was queued for. Set
# EXTRA_ALLOWED_TOOLS yourself alongside a custom GATE_CMD.
EXTRA_ALLOWED_TOOLS="${EXTRA_ALLOWED_TOOLS:-$([[ -z "$GATE_CMD" ]] && echo "Bash(cargo check *) Bash(cargo build *) Bash(cargo test *) Bash(cargo fmt *) Bash(cargo clippy *)")}"
ALLOWED_TOOLS="Read Edit Write Bash(git status *) Bash(git diff *) Bash(git log *) Bash(git show *) ${EXTRA_ALLOWED_TOOLS}"

# Commit/stash subjects embed a task's own text, which can run well past a
# reasonable subject-line length. A bare `${task:0:72}` cuts wherever the
# 72nd byte lands, mid-word as often as not (e.g. "...Phase 9 item 3, d" --
# sliced out of "derive"). Back off to the last whitespace inside the limit
# so the subject always ends on a word boundary; a single word longer than
# the limit (no whitespace to back off to) still falls back to the hard cut.
truncate_task_summary() {
  local task="$1" limit=72 truncated trimmed
  if [[ ${#task} -le $limit ]]; then
    printf '%s' "$task"
    return
  fi
  truncated="${task:0:$limit}"
  trimmed="${truncated% *}"
  if [[ -n "$trimmed" && "$trimmed" != "$truncated" ]]; then
    printf '%s' "$trimmed"
  else
    printf '%s' "$truncated"
  fi
}

# Commits the repo root's own staged changes with subject $1 and, when $2 is
# non-empty, that as a separate body paragraph -- the git equivalent of
# `git commit -m "$1" -m "$2"`, factored out so the one call site doesn't
# have to branch on whether a body exists.
commit_task_head() {
  local subject="$1" body="${2:-}"
  if [[ -n "$body" ]]; then
    git commit -m "$subject" -m "$body" >>"$LOG_FILE" 2>&1
  else
    git commit -m "$subject" >>"$LOG_FILE" 2>&1
  fi
}

# --- Gate ---------------------------------------------------------------

# Directories to run the default cargo gate in: $REPO_ROOT itself and every
# immediate subdirectory with its own Cargo.toml. A repo that is not a single
# Cargo workspace (each crate has its own Cargo.toml/Cargo.lock) needs this --
# a gate that only ran at the root would never compile the other crates at
# all. Auto-discovered so a new sibling crate is picked up with no edit here.
# Always returns 0, even when nothing matches: this is captured via command
# substitution under `set -e` everywhere it's called, and a "no crates found"
# result is not itself an error.
gate_dirs() {
  local f
  for f in "$REPO_ROOT"/Cargo.toml "$REPO_ROOT"/*/Cargo.toml; do
    [[ -f "$f" ]] && dirname "$f"
  done
  return 0
}

run_gate_in() {
  local dir="$1"
  echo "--- gate: $dir ---" | tee -a "$LOG_FILE"
  ( cd "$dir" && cargo fmt --check && cargo clippy -- -D warnings && cargo test ) >>"$LOG_FILE" 2>&1
}

# $1 (optional): newline-separated gate_dirs() output, precomputed by the
# caller so it isn't reglobbed once per call site per task iteration.
run_gate() {
  if [[ -n "$GATE_CMD" ]]; then
    echo "--- gate: $REPO_ROOT ($GATE_CMD) ---" | tee -a "$LOG_FILE"
    ( cd "$REPO_ROOT" && bash -c "$GATE_CMD" ) >>"$LOG_FILE" 2>&1
    return $?
  fi
  local dirs="${1:-}" dir
  [[ -z "$dirs" ]] && dirs="$(gate_dirs)"
  while IFS= read -r dir; do
    [[ -z "$dir" ]] && continue
    if ! run_gate_in "$dir"; then
      echo "Gate failed in $dir" | tee -a "$LOG_FILE" >&2
      return 1
    fi
  done <<<"$dirs"
  return 0
}

# --- CLI ------------------------------------------------------------------

usage() {
  cat <<EOF
Usage: $(basename "$0") [--dry-run] [--max-tasks N]
       $(basename "$0") --queue "task text"

  --dry-run        Print what would run without invoking claude or git.
  --max-tasks N    Only process the first N remaining tasks this run.
  --queue TEXT     Safely append TEXT as a new task line to $TASKS_FILE
                    (lock-protected, safe to run while a loop is in progress)
                    and exit. Does not start a run.

Config is via environment variables -- see the comment block at the top of
this script (GATE_CMD, NESTED_REPO_GLOB, TASK_TIMEOUT, MAX_TASKS, ...).
EOF
}

queue_task() {
  local text="$1"
  [[ -z "$text" ]] && { echo "Refusing to queue an empty task." >&2; exit 1; }
  if [[ "$text" == *$'\n'* ]]; then
    echo "Task text must be a single line (tasks.txt is one task per line)." >&2
    exit 1
  fi
  mkdir -p "$LOG_DIR"
  exec {lockfd}>"$LOCK_FILE"
  flock "$lockfd"
  printf '%s\n' "$text" >>"$TASKS_FILE"
  flock -u "$lockfd"
  exec {lockfd}>&-
  echo "Queued: $text"
}

QUEUE_TEXT=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run) DRY_RUN=1; shift ;;
    --max-tasks) MAX_TASKS="$2"; shift 2 ;;
    --queue) QUEUE_TEXT="$2"; shift 2 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage; exit 1 ;;
  esac
done

[[ -n "$QUEUE_TEXT" ]] && { queue_task "$QUEUE_TEXT"; exit 0; }

mkdir -p "$LOG_DIR" "$DECLINED_DIR"
# Not `touch "$DONE_FILE"` here: on a repo's very first run, an empty,
# untracked $DONE_FILE created before the clean-tree check below would
# itself trip that check, permanently failing the first invocation. It's
# created lazily by `echo ... >>"$DONE_FILE"` on the first real success
# instead, which is also the point it first needs to exist.

# --- Concurrency lock -------------------------------------------------------
# Nothing else here stops two instances racing on the same repo's tasks.txt
# or duplicating a commit. Key the lock on a hash of the full $REPO_ROOT path
# (not e.g. `basename`, which collides for two differently-located checkouts
# that share a leaf directory name) so it's genuinely per-checkout.
if ! command -v flock >/dev/null 2>&1; then
  echo "flock is required but not installed." >&2
  exit 1
fi
# Off the shared claude-home volume deliberately: in claude-box, every repo's
# container sees the same $REPO_ROOT ("/work"), so a REPO_KEY hashed from it
# would collide across repos if LOCK_DIR lived on that shared volume. /tmp is
# a per-repo volume in claude-box (claude-scratch-<slug>-<hash>), so this
# naturally partitions by repo there; elsewhere it's just the host's /tmp.
LOCK_DIR="${LOCK_DIR:-${TMPDIR:-/tmp}/.overnight_loop_locks}"
mkdir -p "$LOCK_DIR"
REPO_KEY="$(printf '%s' "$REPO_ROOT" | sha256sum | cut -c1-16)"
RUN_LOCK_FILE="$LOCK_DIR/${REPO_KEY}.lock"
exec 200>"$RUN_LOCK_FILE"
set +e
flock -n 200
flock_status=$?
set -e
if [[ $flock_status -eq 1 ]]; then
  echo "Another instance of this script is already running against $REPO_ROOT (lock held: $RUN_LOCK_FILE)." >&2
  exit 1
elif [[ $flock_status -ne 0 ]]; then
  echo "flock exited with unexpected status $flock_status; not a lock conflict, investigate directly." >&2
  exit 1
fi

if [[ ! -s "$TASKS_FILE" ]]; then
  echo "No tasks remaining in $TASKS_FILE." | tee -a "$LOG_FILE"
  exit 0
fi

if [[ -n "$(git status --porcelain)" ]]; then
  echo "Working tree not clean; commit or stash before running." >&2
  exit 1
fi

BRANCH="overnight/$DATE_TAG"
CURRENT_BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if [[ "$CURRENT_BRANCH" == "$BRANCH" ]]; then
  # A second run the same day (e.g. after --queue'ing more tasks) continues
  # on the branch it's already sitting on rather than refusing.
  echo "Already on branch $BRANCH; continuing on it." | tee -a "$LOG_FILE"
elif git rev-parse --verify --quiet "$BRANCH" >/dev/null; then
  echo "Branch $BRANCH already exists; refusing to reuse it. Delete it or rerun tomorrow." >&2
  exit 1
elif [[ "$DRY_RUN" -eq 0 ]]; then
  git checkout -b "$BRANCH"
else
  echo "[dry-run] would create branch $BRANCH"
fi

# Pre-flight: if the gate is already broken on the starting commit, every
# task below is unwinnable regardless of what it changes -- fail fast
# instead of burning the whole run on doomed attempts.
if [[ "$DRY_RUN" -eq 0 ]]; then
  echo "=== Pre-flight gate check ===" | tee -a "$LOG_FILE"
  if ! run_gate; then
    echo "Pre-flight gate failed on the starting commit -- fix that first, then rerun. See $LOG_FILE." | tee -a "$LOG_FILE" >&2
    exit 1
  fi
fi

# --- Nested repo (gitlink) support -----------------------------------------

nested_dirty_repos() {
  [[ -z "$NESTED_REPO_GLOB" ]] && return 0
  local glob d
  for glob in $NESTED_REPO_GLOB; do
    for d in $glob; do
      d="${d%/}"
      [[ -d "$d/.git" ]] || continue
      [[ -n "$(git -C "$d" status --porcelain)" ]] && echo "$d"
    done
  done
}

commit_nested_repos() {
  local msg="$1" body="${2:-}" d
  while IFS= read -r d; do
    [[ -z "$d" ]] && continue
    git -C "$d" add -A
    if [[ -n "$body" ]]; then
      git -C "$d" commit -m "$msg" -m "$body" >>"$LOG_FILE" 2>&1
    else
      git -C "$d" commit -m "$msg" >>"$LOG_FILE" 2>&1
    fi
  done < <(nested_dirty_repos)
}

stash_nested_repos() {
  local msg="$1" d
  while IFS= read -r d; do
    [[ -z "$d" ]] && continue
    git -C "$d" stash push -u -m "$msg" >>"$LOG_FILE" 2>&1
  done < <(nested_dirty_repos)
}

# --- Task queue (lock-protected, position-based) ----------------------------
# Pops and returns the first non-blank line of $TASKS_FILE, atomically with
# respect to a concurrent `--queue` append -- both take $LOCK_FILE. Re-reading
# the file each call (rather than snapshotting it once at the top of the
# script) is what lets tasks appended *while this loop is running* get picked
# up in the same run. Echoes nothing once the file is exhausted.
pop_task() {
  local out=""
  exec {lockfd}>"$LOCK_FILE"
  flock "$lockfd"
  if [[ -s "$TASKS_FILE" ]]; then
    rm -f "$TASKS_FILE.tmp"
    while IFS= read -r line || [[ -n "$line" ]]; do
      if [[ -z "$out" && -n "$line" ]]; then
        out="$line"
        continue
      fi
      printf '%s\n' "$line" >>"$TASKS_FILE.tmp"
    done <"$TASKS_FILE"
    mv -f "$TASKS_FILE.tmp" "$TASKS_FILE" 2>/dev/null || : >"$TASKS_FILE"
  fi
  flock -u "$lockfd"
  exec {lockfd}>&-
  printf '%s' "$out"
}

# Appends a task back to the end of $TASKS_FILE under the same lock, without
# clobbering anything queued concurrently.
requeue_task() {
  local text="$1"
  exec {lockfd}>"$LOCK_FILE"
  flock "$lockfd"
  printf '%s\n' "$text" >>"$TASKS_FILE"
  flock -u "$lockfd"
  exec {lockfd}>&-
}

# --dry-run must not pop-and-requeue through pop_task/requeue_task: with
# MAX_TASKS=0 that would cycle through the same tasks forever. Snapshot and
# print without touching $TASKS_FILE.
if [[ "$DRY_RUN" -eq 1 ]]; then
  mapfile -t PREVIEW_TASKS <"$TASKS_FILE"
  for task in "${PREVIEW_TASKS[@]}"; do
    [[ -z "$task" ]] && continue
    echo "=== TASK: $task ==="
    echo "[dry-run] would run: claude -p \"$task\" --permission-mode acceptEdits --allowedTools \"$ALLOWED_TOOLS\""
  done
  exit 0
fi

# --- Staging: what a task is expected to have produced ---------------------
# A bare `git add -A` can sweep an untracked scratch file a task left behind
# straight into a commit -- extension alone can't distinguish scratch from
# genuine, so filenames matching common scratch/backup patterns are refused
# regardless of extension, on top of only allowing a recognized doc/source
# extension through at all. Anything declined is quarantined (moved out of
# the tree, not left in place -- see quarantine_declined()) so it's neither
# silently committed nor silently lost, and doesn't wedge the next run's
# clean-tree pre-flight check.

is_scratch_name() {
  case "$1" in
    tmp_*|*_tmp|*_tmp.*|scratch_*|*.orig|*.bak|*.rej|*.swp|*~|core|core.*) return 0 ;;
    *) return 1 ;;
  esac
}

# Untracked directories worth recursing into per-file: every gate_dirs()
# crate's own root and its tests/ subtree (or, with no cargo layout, just
# "."), derived rather than hardcoded so a new crate/module is covered the
# moment gate_dirs() picks it up.
untracked_dir_allowlist() {
  local dirs="${1:-}" dir rel
  [[ -z "$dirs" ]] && dirs="$(gate_dirs)"
  if [[ -z "$dirs" ]]; then
    echo "./"
    return
  fi
  while IFS= read -r dir; do
    [[ -z "$dir" ]] && continue
    rel="${dir#"$REPO_ROOT"/}"
    if [[ "$rel" == "$dir" ]]; then
      echo "tests/"
    else
      echo "$rel/tests/"
      echo "$rel/"
    fi
  done <<<"$dirs"
}

is_allowed_prefix() {
  local path="$1" allowlist="$2" prefix
  while IFS= read -r prefix; do
    [[ "$path" == "$prefix"* ]] && return 0
  done <<<"$allowlist"
  return 1
}

# Moves a declined untracked path out of the tree into its own
# `mktemp -d` under $DECLINED_DIR (so two declined paths with the same
# basename never collide), with a PROVENANCE file recording where it came
# from and why. Leaving a declined path untracked in place -- the original,
# simpler design -- means it sits as a permanent '??' entry that the next
# invocation's clean-tree pre-flight refuses to start against: the mechanism
# meant to preserve a questionable file for review ends up permanently
# blocking the unattended loop instead. A failure here (permission,
# cross-device mv, the path vanishing) must not abort the whole script --
# this can run mid-cleanup after bookkeeping already changed -- so it's
# logged and left in place rather than propagated.
quarantine_declined() {
  local path="$1" reason="$2" dest
  local relpath="${path%/}"
  if ! dest="$(mktemp -d "$DECLINED_DIR/XXXXXX" 2>>"$LOG_FILE")"; then
    echo "Failed to quarantine ($reason): $path -- could not create quarantine dir, left in place." | tee -a "$LOG_FILE"
    return
  fi
  if ! mv -- "$path" "$dest/" 2>>"$LOG_FILE"; then
    if cp -a -- "$path" "$dest/" 2>>"$LOG_FILE" && rm -rf -- "$path" 2>>"$LOG_FILE"; then
      :
    else
      echo "Failed to quarantine ($reason): $path -- left in place, needs manual review." | tee -a "$LOG_FILE"
      rmdir -- "$dest" 2>/dev/null || true
      return
    fi
  fi
  printf 'original path: %s\nreason: %s\n' "$relpath" "$reason" >"$dest/PROVENANCE"
  echo "Declined to stage ($reason): $path -- moved to $dest/$(basename -- "$relpath")." | tee -a "$LOG_FILE"
}

stage_one_untracked_file() {
  local f="$1" base dir segment parent
  base="${f##*/}"
  dir="${f%/*}"
  [[ "$dir" == "$f" ]] && dir="."
  # No untracked top-level dotfile is auto-staged: ad hoc scratch/verification
  # scripts have repeatedly used a leading-dot name instead of a
  # tmp_/scratch_ prefix, slipping past is_scratch_name() entirely.
  if [[ "$dir" == "." && "$base" == .* ]]; then
    quarantine_declined "$f" "untracked top-level dotfile, needs manual review"
    return
  fi
  if is_scratch_name "$base"; then
    quarantine_declined "$f" "looks like scratch/backup output"
    return
  fi
  while [[ "$dir" != "." && "$dir" != "/" ]]; do
    segment="${dir##*/}"
    if is_scratch_name "$segment"; then
      quarantine_declined "$f" "inside scratch/backup-named directory"
      return
    fi
    parent="${dir%/*}"
    [[ "$parent" == "$dir" ]] && parent="."
    dir="$parent"
  done
  local ext matched=0 e
  for e in $STAGE_EXTENSIONS; do
    [[ "$f" == *"$e" ]] && { matched=1; break; }
  done
  if [[ "$matched" -eq 1 ]]; then
    git add -- "$f"
  else
    quarantine_declined "$f" "not a recognized source/doc type"
  fi
}

# $1 (optional): precomputed gate_dirs() output, threaded through so it's
# glob'd once per task iteration rather than once per untracked path.
stage_task_changes() {
  local dirs="${1:-}"
  git add -u

  local allowlist
  allowlist="$(untracked_dir_allowlist "$dirs")"

  local entry status f dir
  # -z: NUL-terminated, unquoted paths. The human-readable format
  # C-quotes/octal-escapes any path with non-ASCII bytes, which breaks a
  # naive '?? '-prefix string match for such a path.
  while IFS= read -r -d '' entry; do
    [[ -z "$entry" ]] && continue
    status="${entry:0:2}"
    f="${entry:3}"
    [[ "$status" == "??" ]] || continue
    if [[ "$f" == */ ]]; then
      dir="$f"
      if ! is_allowed_prefix "$dir" "$allowlist"; then
        quarantine_declined "$dir" "untracked directory not in allowlist, skipping recursive walk"
        continue
      fi
      while IFS= read -r -d '' entry; do
        [[ -z "$entry" ]] && continue
        f="${entry:3}"
        if ! is_allowed_prefix "$f" "$allowlist"; then
          quarantine_declined "$f" "file outside any allowlisted directory"
          continue
        fi
        stage_one_untracked_file "$f"
      done < <(git status --porcelain -z --untracked-files=all -- "$dir")
      continue
    fi
    stage_one_untracked_file "$f"
  done < <(git status --porcelain -z)
}

# Records a task as failed: stage+stash whatever it left (tracked changes
# only -- declined untracked paths were already quarantined by
# stage_task_changes, so a plain `git stash push` without `-u` doesn't need
# to sweep them, and deliberately doesn't, so a declined file is never hidden
# inside an opaque stash instead of sitting in $DECLINED_DIR for review).
#
# $2: pass "skip-stage" when the caller already ran stage_task_changes
# immediately before this (the gate-passed-but-commit-failed path) so
# nothing has changed in the tree since -- re-scanning would be wasted work.
record_task_failure() {
  local task="$1" dirs="${3:-}"
  if [[ "${2:-}" != "skip-stage" ]]; then
    stage_task_changes "$dirs"
  fi
  stash_nested_repos "failed: $(truncate_task_summary "$task")"
  # pop_task() already removed $task from $TASKS_FILE on disk, as an
  # uncommitted modification to an otherwise-tracked file, *before* claude -p
  # ran -- deliberately, so a concurrent --queue append is never lost. A
  # plain `git stash push` here would revert every tracked modification back
  # to HEAD, $TASKS_FILE included, silently undoing that pop and putting the
  # just-failed task straight back in front of pop_task() -- looping on the
  # same failing task within this run instead of deferring it to the next
  # one (deferred[]/requeue_task() below is what's supposed to own that).
  # Shield $TASKS_FILE's live queue state from the stash by saving and
  # restoring it around the call.
  # $() strips trailing newlines, so a plain `printf '%s'` restore below
  # would silently drop $TASKS_FILE's final newline -- harmless on its own,
  # but the next requeue_task() append (a bare `>>`, no leading newline of
  # its own) then lands directly after the last character of the previous
  # line instead of on a new one, concatenating two task lines into one
  # (confirmed: corrupted a real queue this way). Restore with the newline
  # put back, unless the file was genuinely empty to begin with.
  local tasks_snapshot
  tasks_snapshot="$(cat "$TASKS_FILE" 2>/dev/null || true)"
  git stash push -m "failed: $(truncate_task_summary "$task")" >>"$LOG_FILE" 2>&1
  if [[ -n "$tasks_snapshot" ]]; then
    printf '%s\n' "$tasks_snapshot" >"$TASKS_FILE"
  else
    : >"$TASKS_FILE"
  fi
  echo "FAILED: $task" >>"$FAIL_FILE"
  n_failed=$((n_failed + 1))
  echo "FAILED: $task" | tee -a "$LOG_FILE"
  deferred+=("$task")
}

# --- Main loop --------------------------------------------------------------

n_done=0
n_failed=0
task_count=0
deferred=() # failed-this-run tasks, requeued only after the loop exits so a
            # failing task isn't immediately re-popped within the same run

while :; do
  if [[ "$MAX_TASKS" -gt 0 && "$task_count" -ge "$MAX_TASKS" ]]; then
    echo "Reached --max-tasks limit; remaining tasks stay queued in $TASKS_FILE." | tee -a "$LOG_FILE"
    break
  fi

  task="$(pop_task)"
  [[ -z "$task" ]] && break
  task_count=$((task_count + 1))

  echo "=== TASK: $task ===" | tee -a "$LOG_FILE"

  # Standing instruction ahead of the task's own text: keep scratch/
  # verification work out of the tracked tree entirely, so there's no
  # filename left for is_scratch_name()'s ever-growing pattern list to need
  # to catch. Defense in depth, not a replacement for it -- effectiveness is
  # only observable by watching whether this incident class recurs.
  task_prompt="Before starting: any scratch or verification files you create while working on this task (throwaway scripts, sample data, manual-verification harnesses) must be written under a system temp directory (e.g. /tmp or a mktemp -d output), never inside this repository's working tree. Do not leave such files staged, committed, or untracked in the repo.

$task"

  pre_task_status="$(git status --porcelain -- .)"

  # Inside a claude-box container, `claude` here resolves to the
  # /usr/local/bin/claude shim (see that repo's claude-cwd-shim.sh), which
  # redirects to CLAUDE_BOX_UNIQUE_WORKDIR before handing off to the real
  # binary -- this loop's own git staging/commit and $REPO_ROOT-relative
  # paths stay on /work throughout, unaffected. A no-op outside claude-box.
  set +e
  timeout -k "$TASK_KILL_AFTER" "$TASK_TIMEOUT" claude -p "$task_prompt" \
    --permission-mode acceptEdits \
    --allowedTools "$ALLOWED_TOOLS" \
    >>"$LOG_FILE" 2>&1
  claude_exit=$?
  set -e
  if [[ "$claude_exit" -eq 124 ]]; then
    echo "claude -p timed out after ${TASK_TIMEOUT}s for: $task" >>"$LOG_FILE"
  elif [[ "$claude_exit" -eq 137 ]]; then
    echo "claude -p timed out after ${TASK_TIMEOUT}s and had to be killed for: $task" >>"$LOG_FILE"
  elif [[ "$claude_exit" -ne 0 ]]; then
    echo "claude -p exited $claude_exit" >>"$LOG_FILE"
  fi

  # A nonzero exit must not fall through to the gate: if claude made no edit
  # at all, the gate trivially passes against unchanged code and the task
  # would be wrongly marked DONE with zero work performed. Treat any nonzero
  # exit as failure unconditionally, regardless of what the gate would say.
  if [[ "$claude_exit" -ne 0 ]]; then
    record_task_failure "$task"
    continue
  fi

  # claude -p can also exit 0 while making no source changes (task already
  # satisfied, or hit a tool-permission wall) -- exit code alone doesn't tell
  # you real work happened. Diff pre/post `git status` instead of trusting
  # it. Deliberately not excluding $TASKS_FILE from this snapshot: a task
  # whose entire legitimate output is queuing new tasks (e.g. a review pass)
  # must not be treated as a no-op just because it never touched source.
  post_task_status="$(git status --porcelain -- .)"
  if [[ "$pre_task_status" == "$post_task_status" ]]; then
    echo "claude -p made no source changes for: $task" | tee -a "$LOG_FILE"
    record_task_failure "$task"
    continue
  fi

  task_gate_dirs="$(gate_dirs)"
  if run_gate "$task_gate_dirs"; then
    task_summary="$(truncate_task_summary "$task")"
    # A truncated subject silently discarded the rest of the task text --
    # nowhere else in the commit records it, since $DONE_FILE is gitignored.
    # Carry the untruncated task as the commit body whenever the subject
    # doesn't already say it in full.
    task_body=""
    [[ "$task_summary" != "$task" ]] && task_body="$task"
    commit_nested_repos "auto: $task_summary" "$task_body"
    # $DONE_FILE is written *before* staging/committing, so the bookkeeping
    # entry rides along inside this task's own commit rather than being left
    # untracked afterward -- an untracked $DONE_FILE would trip the *next*
    # run's clean-tree pre-flight check. If the commit below fails,
    # record_task_failure's stash (via stage_task_changes having just staged
    # this same edit) reverts it along with everything else, so it never
    # goes stale relative to git history.
    echo "$task" >>"$DONE_FILE"
    stage_task_changes "$task_gate_dirs"
    if commit_task_head "auto: $task_summary" "$task_body"; then
      n_done=$((n_done + 1))
      echo "DONE: $task" | tee -a "$LOG_FILE"
    else
      echo "git commit failed for: $task -- treating as failed" | tee -a "$LOG_FILE" >&2
      record_task_failure "$task" "skip-stage" "$task_gate_dirs"
    fi
  else
    record_task_failure "$task" "" "$task_gate_dirs"
  fi
done

# Put failed tasks back for a later run, appended under lock so anything
# queued concurrently during this run (via --queue) is preserved.
for task in "${deferred[@]+"${deferred[@]}"}"; do
  requeue_task "$task"
done

# The requeue above (and, on an all-failed run, pop_task's own removals) can
# leave $TASKS_FILE as an uncommitted modification -- committing it here
# keeps the working tree clean at rest, which the next invocation's
# pre-flight check requires to start at all.
if [[ -n "$(git status --porcelain -- "$TASKS_FILE")" ]]; then
  git add -- "$TASKS_FILE"
  git commit -m "chore: requeue $n_failed failed task(s)" >>"$LOG_FILE" 2>&1
fi

echo "=== Summary: $n_done done, $n_failed failed, branch $BRANCH ===" | tee -a "$LOG_FILE"
if [[ "$n_failed" -gt 0 ]]; then
  echo "See $FAIL_FILE and 'git stash list' for failed attempts." | tee -a "$LOG_FILE"
fi
echo "Nothing was pushed. Review with: git log master..$BRANCH" | tee -a "$LOG_FILE"
