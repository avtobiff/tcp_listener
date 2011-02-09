#!/usr/bin/make -f

APPFILE := tcp_listener.app
VERSION := $(shell sed -n '/vsn/ {s/.*,\s*"\([0-9][0-9.]*\)".*/\1/; p}' \
                       src/$(APPFILE).src)

PREFIX    ?= /usr
ERL_ROOT  := $(PREFIX)/lib/erlang
LIBDIR    := /lib
DISTDIR   := tcp_listener-$(VERSION)

BEAMFILES := $(wildcard ebin/*)

all: build

build: ebin/$(APPFILE)
	erl -make

ebin/$(APPFILE): src/$(APPFILE).src
	cp $< $@

clean:
	rm -f ebin/*

dialyzer:
	dialyzer -c $(BEAMFILES)

install: build
	# create dist directory and install files
	mkdir -p $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin
	install -m0644 ebin/* $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/$(DISTDIR)/ebin

uninstall:
	rm -rf $(DESTDIR)$(ERL_ROOT)$(LIBDIR)/tcp_listener-[0-9][0-9.]*

.PHONY: all build clean dialyzer
