#!/bin/sh
# Golden-file regression test for pTeX-ng
# Compares .log output against known-good reference files.
#
# Usage: sh run.sh [--update]
#   --update: regenerate golden files from current output

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TEST_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
APTEX="${TEST_ROOT}/../src/aptex"

export TEXMFCNF="${TEST_ROOT}/cnf"

if [ ! -x "$APTEX" ]; then
  echo "ERROR: aptex binary not found at $APTEX"
  echo "Build it first: blade build //src:aptex"
  exit 1
fi

GOLDEN_DIR="$SCRIPT_DIR/golden"
INPUT_DIR="$SCRIPT_DIR/inputs"
WORK_DIR="$SCRIPT_DIR/_work"
UPDATE_MODE=0
FAILED=0
PASSED=0

if [ "$1" = "--update" ]; then
  UPDATE_MODE=1
  echo "=== UPDATE MODE: regenerating golden files ==="
fi

mkdir -p "$WORK_DIR" "$GOLDEN_DIR"

# Build format if needed
if [ ! -f "$TEST_ROOT/fmt/plain.fmt" ]; then
  echo "Building plain.fmt..."
  cd "$TEST_ROOT"
  $APTEX -ini plain.ini >/dev/null 2>&1
  mv plain.fmt fmt/
  mv plain.log fmt/
  cd "$SCRIPT_DIR"
fi

# Run each test .tex file
run_test() {
  local tex_file="$1"
  local base="$(basename "$tex_file" .tex)"
  local golden="$GOLDEN_DIR/${base}.log"

  echo -n "  $base ... "

  cd "$WORK_DIR"
  rm -f "${base}."*

  $APTEX +plain "$tex_file" >/dev/null 2>&1 || true

  if [ ! -f "${base}.log" ]; then
    echo "FAIL (no log output)"
    FAILED=$((FAILED + 1))
    return
  fi

  # Normalize: remove timestamps, version strings, file paths that vary
  sed -e 's/^This is.*$/This is aptex/g' \
      -e '/^Output written on/d' \
      -e '/^Transcript written on/d' \
      -e 's/ [0-9]* bytes//' \
      "${base}.log" > "${base}.log.normalized"

  if [ "$UPDATE_MODE" = "1" ]; then
    cp "${base}.log.normalized" "$golden"
    echo "UPDATED"
    PASSED=$((PASSED + 1))
  elif [ ! -f "$golden" ]; then
    echo "SKIP (no golden file; run with --update to create)"
    return
  else
    if diff -q "$golden" "${base}.log.normalized" >/dev/null 2>&1; then
      echo "PASS"
      PASSED=$((PASSED + 1))
    else
      echo "FAIL"
      echo "    diff: $golden vs ${base}.log.normalized"
      diff -u "$golden" "${base}.log.normalized" | head -30
      FAILED=$((FAILED + 1))
    fi
  fi
}

# Copy existing test inputs for regression
for f in "$TEST_ROOT"/test*.tex; do
  if [ -f "$f" ]; then
    cp "$f" "$INPUT_DIR/" 2>/dev/null || true
  fi
done

echo "=== Running regression tests ==="
for f in "$INPUT_DIR"/*.tex "$TEST_ROOT"/test*.tex; do
  [ -f "$f" ] && run_test "$f"
done

echo ""
echo "=== Results: $PASSED passed, $FAILED failed ==="

rm -rf "$WORK_DIR"

if [ "$FAILED" -gt 0 ]; then
  exit 1
fi
