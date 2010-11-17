#!/usr/bin/make -f

BEAMFILES := $(wildcard ebin/*)

all: build

build:
	erl -make

clean:
	rm -f ebin/*

dialyzer:
	dialyzer -c $(BEAMFILES)

.PHONY: all build clean dialyzer
