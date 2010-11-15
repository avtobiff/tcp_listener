#!/usr/bin/make -f

BEAMFILES := $(wildcard ebin/*)

all: build

build:
	erl -make

clean:
	rm -rf $(BEAMFILES)

dialyzer:
	dialyzer -c $(BEAMFILES)

.PHONY: all build clean dialyzer
