#  Copyright (c) 2001 Dan Gudmundsson and Patrik Nyblom
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
#     $Id$
#

ifeq ($(OS_FLAG), mingw)
include ../../win32_conf/mingw_vars.mk

ERLC = erlc 

TARGETDIR = ../../ebin

# Files

ERL_SOURCES = woggle.erl 
ERL_OBJECTS = $(ERL_SOURCES:%.erl=$(TARGETDIR)/%.beam)
ERL_HEADERS = ../../include/gl.hrl ../include/woggle.hrl \
	woggle_ops.hrl

all:  $(ERL_OBJECTS)
	@echo Build done

clean:  
	rm -f $(ERL_OBJECTS)
	rm -f *~

$(TARGETDIR)/%.beam: %.erl $(ERL_HEADERS)
	$(ERLC) -I../../include -I../include -o $(TARGETDIR) $<

else

target clean: 
	@echo Only windows for now.

endif




