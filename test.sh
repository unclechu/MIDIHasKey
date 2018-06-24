#!/usr/bin/env bash
# env GDK_SCALE=2
stack build \
	&& (cd midiplayer-jack-cpp/ && make) \
	&& (cd test/ && ./midihaskey /dev/input/by-id/usb-04b4_6018-event-kbd)
