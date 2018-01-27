#!/usr/bin/env bash
./env.sh stack build \
	&& (cd midiplayer/ && make) \
	&& env PATH="midiplayer/build:$PATH" GDK_SCALE=2 \
		stack exec midihaskey -- /dev/input/by-id/usb-04b4_6018-event-kbd
