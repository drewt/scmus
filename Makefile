
prefix = /usr/local
bindir = $(prefix)/bin
datadir = $(prefix)/share
mandir = $(prefix)/share/man

prelude = '"(define *scmus-dir* \"$(datadir)/scmus\")"'

CSC      = csc
CSCFLAGS = -scrutinize -C -Wno-int-to-pointer-cast -uses lib -prologue prologue.scm
LD       = csc
LDFLAGS  =
INSTALL  = @scripts/install

eggs = ncurses sandbox utf8

objects = config.o command-line.o editable.o eval-mode.o format.o keys.o \
	  lib.o library-view.o main.o mpd-client.o option.o options-view.o \
	  search-view.o scmus-client.o ui-curses.o window.o

clean = $(objects) scmus

all: scmus

include rules.mk

config.o: CSCFLAGS += -prelude $(prelude)

scmus: $(objects)
	$(call cmd,ld,-lncursesw)

eggs:
	chicken-install $(eggs)

install: all
	$(INSTALL) -m755 $(bindir) scmus
	$(INSTALL) -m644 $(datadir)/scmus data/scmusrc.scm
	$(INSTALL) -m644 $(datadir)/scmus/colors $(wildcard data/colors/*)
	$(INSTALL) -m644 $(mandir)/man1 $(wildcard doc/*.1)
