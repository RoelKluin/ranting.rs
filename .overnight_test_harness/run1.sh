#!/usr/bin/env bash
set -uo pipefail
H=/work/.overnight_test_harness
R="$H/repo"
PATH="$H/bin:$PATH"
export PATH
LOG_DIR="$H/logs"
export LOG_DIR
TMPDIR="$H/tmp"
export TMPDIR
mkdir -p "$TMPDIR"
bash "$R/scripts/overnight_loop.sh"
echo "EXIT=$?"
