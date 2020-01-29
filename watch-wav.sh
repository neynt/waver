#!/bin/bash

on_exit() {
  kill $(jobs -p) &> /dev/null
  exit 0
}

regen_files() {
  kill $(jobs -p) &> /dev/null
  while [[ ! -x _build/default/waver.exe ]]; do
    sleep 0.1
  done
  IN_FILE="terra.wav"
  OUT_FILE="output.wav"
  time _build/default/waver.exe trigger $IN_FILE $OUT_FILE
  mpv --loop $OUT_FILE &
}

trap on_exit SIGINT

regen_files
while true; do
  if [[ -e _build/default/waver.exe ]]; then
    inotifywait -e close_write _build/default/waver.exe &> /dev/null
    regen_files
  else
    echo '...'
    sleep 0.1
  fi
done
