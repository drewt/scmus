CSC      = csc
CSCFLAGS =
LD       = csc
LDFLAGS  =

objects = config.o command-mode.o editable.o normal-mode.o main.o mpd-client.o \
	  search-mode.o scmus-client.o ui-curses.o
clean = $(objects) scmus

all: scmus

include rules.mk

scmus: $(objects)
	$(call cmd,ld)
