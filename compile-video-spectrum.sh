#!/bin/bash

SONG=terra
FPS=30

OUT_DIR=frames/$SONG
OUTPUT_MKV=$SONG.mkv

mpv "mf://$OUT_DIR/*.png" \
--mf-fps=$FPS \
--ovc libx264 \
--ovcopts=bitrate=1200:threads=2 \
--oac=ac3 \
--audio-file $SONG.wav \
--oacopts=bitrate=240 \
-o $OUTPUT_MKV
