#!/bin/bash

on_exit() {
	kill $(jobs -p)
	exit 0
}

do_stuff() {
	kill $(jobs -p)
	while [[ ! -x _build/default/waver.exe ]]; do
		sleep 0.1
	done
	time _build/default/waver.exe trigger output.wav
	mpv output.wav &
}

trap on_exit SIGINT

do_stuff
while true; do
	if [[ -e _build/default/waver.exe ]]; then
		inotifywait -e close_write _build/default/waver.exe
		do_stuff
	else
		echo '...'
		sleep 0.1
	fi
done
