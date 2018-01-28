#!/usr/bin/env bash
stack build \
	&& (cd midiplayer/ && make) \
	&& env PATH="midiplayer/build:$PATH" GDK_SCALE=1 \
		stack exec midihaskey -- /dev/input/by-id/usb-04b4_6018-event-kbd
