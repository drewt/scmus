CSC      = csc
CSCFLAGS =
LD       = csc
LDFLAGS  =

objects = config.o command-line.o command-mode.o editable.o format.o \
	  normal-mode.o main.o mpd-client.o search-mode.o scmus-client.o \
	  ui-curses.o
clean = $(objects) scmus

all: scmus

include rules.mk

libmpdclient.scm: libmpdclient.h
	chicken-bind libmpdclient.h

scmus: $(objects)
	$(call cmd,ld,-lmpdclient)
