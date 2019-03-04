#!/bin/bash

on_exit() {
	kill $(jobs -p)
	exit 0
}

do_stuff() {
	time _build/default/waver.exe frame data/broken_moon.mid frame.png
}

trap on_exit SIGINT

do_stuff
while true; do eog frame.png; done &
while true; do
	if [[ -e _build/default/waver.exe ]]; then
		inotifywait -e close_write _build/default/waver.exe
		do_stuff
	else
		echo '...'
		sleep 0.1
	fi
done
