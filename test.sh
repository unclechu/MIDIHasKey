#! /usr/bin/env bash
set -Eeuo pipefail

# export GDK_SCALE=2
procs=`nproc --all`

stack build -j"$procs" \
	&& (cd midiplayer-jack-cpp/ && make -j"$procs") \
	&& (
		cd test/
		rm -f ./midihaskey
		dir=`stack path --local-install-root 2>/dev/null`
		ln -s -- "${dir}/bin/midihaskey"
		./midihaskey /dev/input/by-path/platform-i8042-serio-0-event-kbd
	)
