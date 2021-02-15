#!/bin/bash
set -uo pipefail

on_exit() {
  kill $(jobs -p)
  exit 0
}

waver() {
  time ./waver "$@"
}


regen_all() {
  kill $(jobs -p) &> /dev/null

  #waver frame data/broken_moon.mid frame.png
  #waver frame-spectrum terra.wav frame.png output.wav -time 10

  waver render-workspace output.wav
  mpv --loop output.wav &
}

trap on_exit SIGINT

regen_all
#while true; do eog frame.png; done &
while true; do
  if [[ -e $(readlink ./waver) ]]; then
    inotifywait -e close_write $(readlink ./waver)
    while [[ ! -x $(readlink ./waver) ]]; do
      sleep 0.1
    done
    regen_all
  else
    echo '...'
    sleep 0.1
  fi
done
