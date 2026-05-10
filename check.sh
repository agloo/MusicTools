#!/bin/bash
# Usage: ./check.sh /tmp/musescore_check.mxl
# Extracts the MXL, runs musescore-check, writes /tmp/violations.json.
# Then run the plugin in MuseScore to apply colors.

set -e

MXL="${1:-/tmp/musescore_check.mxl}"
XML="/tmp/musescore_check_extracted.xml"
OUT="/tmp/violations.json"
BIN="$(dirname "$0")/lean/MusicTools/.lake/build/bin/musescore-check"

unzip -p "$MXL" score.xml > "$XML"
"$BIN" "$XML" > "$OUT"
echo "$(cat "$OUT" | python3 -c 'import sys,json; v=json.load(sys.stdin); print(len(v), "violation(s)")')"
echo "Run the plugin in MuseScore to apply colors."
