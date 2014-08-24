prefix = /usr/local
bindir = $(prefix)/bin
datadir = $(prefix)/share
mandir = $(prefix)/share/man

version := $(shell git rev-parse --verify --short HEAD 2>/dev/null)-git

prelude = '"(define *scmus-dir* \"$(datadir)/scmus\")\
	    (define *version* \"$(version)\")"'

include config.mk
features-$(CONFIG_WIDE_CHAR) += -feature wide-char

LIBS = $(NCURSES_LIBS)

CSC      = csc
CSCFLAGS = -scrutinize -C -Wno-int-to-pointer-cast -uses lib \
	   -prologue prologue.scm $(features-y)
LD       = csc
LDFLAGS  =
INSTALL  = @scripts/install

eggs = easyffi sandbox utf8

objects = browser-view.o config.o command-line.o editable.o eval-mode.o \
	  format.o getopt.o keys.o lib.o library-view.o main.o mpd-client.o \
	  ncurses.o option.o options-view.o search-view.o scmus-client.o \
	  ui-curses.o window.o
target = scmus

clean = $(objects) $(target)

all: $(target)

include rules.mk

config.o: CSCFLAGS += -prelude $(prelude)

$(target): $(objects)
	$(call cmd,ld,$(LIBS))

eggs:
	chicken-install $(eggs)

install: all
	$(INSTALL) -m755 $(bindir) scmus
	$(INSTALL) -m644 $(datadir)/scmus data/scmusrc.scm
	$(INSTALL) -m644 $(datadir)/scmus/colors $(wildcard data/colors/*)
	$(INSTALL) -m644 $(mandir)/man1 $(wildcard doc/*.1)
