CSC      = csc
CSCFLAGS = -C -Wno-int-to-pointer-cast -uses lib
LD       = csc
LDFLAGS  =

eggs = ncurses sandbox

objects = config.o command-line.o editable.o eval-mode.o format.o keys.o \
	  lib.o main.o mpd-client.o option.o search-mode.o scmus-client.o \
	  ui-curses.o

clean = $(objects) scmus

all: scmus

include rules.mk

libmpdclient.scm: libmpdclient.h
	chicken-bind libmpdclient.h

scmus: $(objects)
	$(call cmd,ld,-lmpdclient)

eggs:
	chicken-install $(eggs)
