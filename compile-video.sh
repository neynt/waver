#!/bin/bash

SONG=resurrections
FPS=30

INPUT=data/$SONG.mid
OUT_DIR=frames/$SONG
OUTPUT_WAV=$SONG.wav
OUTPUT_MKV=$SONG.mkv

rm -rf $OUT_DIR
mkdir -p $OUT_DIR
_build/default/waver.exe render $INPUT $OUTPUT_WAV
_build/default/waver.exe video -fps $FPS $INPUT $OUT_DIR

mpv "mf://$OUT_DIR/*.png" \
--mf-fps=$FPS \
--ovc libx264 \
--ovcopts=bitrate=1200:threads=2 \
--oac=ac3 \
--audio-file $OUTPUT_WAV \
--oacopts=bitrate=240 \
-o $OUTPUT_MKV
