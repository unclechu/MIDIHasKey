#!/usr/bin/env bash
stack build \
	&& (cd midiplayer/ && make) \
	&& (cd test/ && \
		env GDK_SCALE=1 ./midihaskey /dev/input/by-id/usb-04b4_6018-event-kbd)
