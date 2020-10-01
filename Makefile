UNAME_S=$(shell uname -s)
EMACS_ROOT ?= ../..
ifeq ($(UNAME_S),Darwin)
	EMACS ?= /Applications/Emacs.app/Contents/MacOS/Emacs
else
	EMACS ?= emacs
endif

CC      = clang
LD      = clang
CPPFLAGS = -I$(EMACS_ROOT)/src
CFLAGS = -O2 -std=gnu99 -ggdb3 -Wall -fPIC $(CPPFLAGS)

.PHONY : clean

all: signpost-core.so

signpost-core.so: signpost-core.o
	$(LD) -shared $(LDFLAGS) -o $@ $^

signpost-core.o: signpost-core.c
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	-rm -f *.so *.o
