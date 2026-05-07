#!/bin/sh
# End-to-end benchmark for pTeX-ng using hyperfine
#
# Prerequisites: hyperfine (https://github.com/sharkdp/hyperfine)
#   Install: cargo install hyperfine  OR  apt install hyperfine
#
# Usage: sh bench_compile.sh [warmup_count] [run_count]

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
TEST_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PROJECT_ROOT="$(cd "$TEST_ROOT/.." && pwd)"
APTEX="${PROJECT_ROOT}/src/aptex"

WARMUP="${1:-3}"
RUNS="${2:-10}"

export TEXMFCNF="${TEST_ROOT}/cnf"

if [ ! -x "$APTEX" ]; then
  echo "ERROR: aptex binary not found at $APTEX"
  exit 1
fi

if ! command -v hyperfine >/dev/null 2>&1; then
  echo "ERROR: hyperfine not installed"
  echo "Install: cargo install hyperfine  OR  apt install hyperfine"
  exit 1
fi

# Ensure format exists
if [ ! -f "$TEST_ROOT/fmt/plain.fmt" ]; then
  echo "Building plain.fmt..."
  cd "$TEST_ROOT"
  $APTEX -ini plain.ini >/dev/null 2>&1
  mv plain.fmt fmt/
  mv plain.log fmt/
fi

WORK_DIR="$SCRIPT_DIR/_work"
mkdir -p "$WORK_DIR"

# Generate test documents of varying sizes
cat > "$WORK_DIR/bench_small.tex" <<'EOF'
Hello, World!
\bye
EOF

cat > "$WORK_DIR/bench_medium.tex" <<'EOF'
\def\lipsum{Lorem ipsum dolor sit amet, consectetur adipiscing elit.
Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat. }
\lipsum\lipsum\lipsum\lipsum\lipsum
\lipsum\lipsum\lipsum\lipsum\lipsum
\lipsum\lipsum\lipsum\lipsum\lipsum
\lipsum\lipsum\lipsum\lipsum\lipsum
\bye
EOF

cat > "$WORK_DIR/bench_math.tex" <<'EOF'
The quadratic formula: $x = {-b \pm \sqrt{b^2 - 4ac} \over 2a}$

Display math:
$$\int_0^\infty e^{-x^2} dx = {\sqrt\pi \over 2}$$

$$\sum_{n=1}^{\infty} {1 \over n^2} = {\pi^2 \over 6}$$

$$\prod_{k=1}^{n} k = n!$$

\bye
EOF

cat > "$WORK_DIR/bench_linebreak.tex" <<'EOF'
\hsize=3in
\tolerance=200
\def\text{The quick brown fox jumps over the lazy dog. Pack my box with five dozen liquor jugs. How vexingly quick daft zebras jump. }
\text\text\text\text\text\text\text\text\text\text
\text\text\text\text\text\text\text\text\text\text
\text\text\text\text\text\text\text\text\text\text
\bye
EOF

echo "=== pTeX-ng End-to-End Benchmark ==="
echo "Binary: $APTEX"
echo "Warmup: $WARMUP, Runs: $RUNS"
echo ""

cd "$WORK_DIR"

echo "--- Format loading (minimal document) ---"
hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  "$APTEX +plain bench_small.tex" \
  --export-json bench_small.json 2>/dev/null || \
hyperfine --warmup "$WARMUP" --min-runs "$RUNS" \
  "$APTEX +plain bench_small.tex"

echo ""
echo "--- Medium text (paragraph building + line breaking) ---"
hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  "$APTEX +plain bench_medium.tex" \
  --export-json bench_medium.json 2>/dev/null || \
hyperfine --warmup "$WARMUP" --min-runs "$RUNS" \
  "$APTEX +plain bench_medium.tex"

echo ""
echo "--- Math typesetting ---"
hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  "$APTEX +plain bench_math.tex" \
  --export-json bench_math.json 2>/dev/null || \
hyperfine --warmup "$WARMUP" --min-runs "$RUNS" \
  "$APTEX +plain bench_math.tex"

echo ""
echo "--- Line breaking stress test ---"
hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  "$APTEX +plain bench_linebreak.tex" \
  --export-json bench_linebreak.json 2>/dev/null || \
hyperfine --warmup "$WARMUP" --min-runs "$RUNS" \
  "$APTEX +plain bench_linebreak.tex"

echo ""
echo "=== Benchmark complete ==="
echo "JSON results in: $WORK_DIR/bench_*.json"
