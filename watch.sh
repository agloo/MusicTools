#!/bin/bash
# Polling watcher: when /tmp/musescore_check.json changes, run musescore-check
# and write the result to /tmp/musescore_check_violations.json.
# Run this in a terminal alongside MuseScore.

set -u

IN="/tmp/musescore_check.json"
OUT="/tmp/musescore_check_violations.json"
BIN="$(dirname "$0")/lean/MusicTools/.lake/build/bin/musescore-check"

if [ ! -x "$BIN" ]; then
    echo "missing binary: $BIN" >&2
    exit 1
fi

LAST=""
echo "watching $IN"

while true; do
    if [ -f "$IN" ]; then
        CUR=$(stat -f %m "$IN" 2>/dev/null || echo "")
        if [ -n "$CUR" ] && [ "$CUR" != "$LAST" ]; then
            LAST="$CUR"
            if "$BIN" "$IN" > "$OUT.tmp" 2> /tmp/musescore_check.err; then
                mv "$OUT.tmp" "$OUT"
                echo "$(date +%H:%M:%S) checked"
            else
                echo "$(date +%H:%M:%S) check failed (see /tmp/musescore_check.err)"
                rm -f "$OUT.tmp"
            fi
        fi
    fi
    sleep 0.15
done
