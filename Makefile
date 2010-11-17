#!/usr/bin/make -f

VERSION := 0.1.0

PREFIX    ?= /usr
ERL_ROOT  := $(PREFIX)/lib/erlang
LIBDIR    := /lib
DISTDIR   := tcp_listener-$(VERSION)

BEAMFILES := $(wildcard ebin/*)

all: build

build:
	erl -make

clean:
	rm -f ebin/*

dialyzer:
	dialyzer -c $(BEAMFILES)

install: build
	# create dist directory and install files
	mkdir -p $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin
	install -m0644 $(BEAMFILES) $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin

uninstall:
	rm -rf $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/tcp_listener-[0-9][0-9.]*

.PHONY: all build clean dialyzer
