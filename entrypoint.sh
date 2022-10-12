#!/bin/sh
: "${RESTYLER_CANCEL_SIGNAL:=""}"

if [ -z "$RESTYLER_CANCEL_SIGNAL" ]; then
  exec /bin/restyler "$@"
fi

: "${RESTYLER_CANCEL_SIGNAL:=QUIT}"

for signal in $(kill -l); do
  if [ "$signal" != "$RESTYLER_CANCEL_SIGNAL" ]; then
    # Best-effort forward all signals we don't handle
    trap 'kill -'"$signal"' "$restyler_pid" 2>/dev/null' "$signal"
  fi
done

trap 'echo "Build canceled."; exit 0' "$RESTYLER_CANCEL_SIGNAL"

/bin/restyler "$@" &
restyler_pid=$!
wait "$restyler_pid"
