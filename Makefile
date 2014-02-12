CSC      = csc
CSCFLAGS =
LD       = csc
LDFLAGS  =

objects = mpd-client.o scmus-client.o ui-curses.o
clean = $(objects) scmus

all: scmus

include rules.mk

scmus: $(objects)
	$(call cmd,ld)
