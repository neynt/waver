#!/bin/bash

on_exit() {
  kill $(jobs -p)
  exit 0
}

waver() {
  time _build/default/waver.exe "$@"
}

frame_broken_moon() {
  waver frame data/broken_moon.mid frame.png
}

frame_terra() {
  waver frame-spectrum terra.wav frame.png output.wav -time 10
}

regen_all() {
  #kill $(jobs -p) &> /dev/null
  frame_terra
  #mpv --loop output.wav &
}

trap on_exit SIGINT

regen_all
while true; do eog frame.png; done &
while true; do
  if [[ -e _build/default/waver.exe ]]; then
    inotifywait -e close_write _build/default/waver.exe
    while [[ ! -x _build/default/waver.exe ]]; do
      sleep 0.1
    done
    regen_all
  else
    echo '...'
    sleep 0.1
  fi
done
