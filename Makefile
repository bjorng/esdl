#  Copyright (c) 2001 Dan Gudmundsson
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
#     $Id$
#

SUBDIRS = c_src src test doc

DATE    = $(shell date +%m%d)
REL     = esdl-0.94.$(DATE)
RELDIR  = /tmp/$(REL)

ifeq ($(OS_FLAG),mingw)
TAR = tar
SDL_DLL_DIR = $(SDLROOT)/lib
else
TAR = gtar
SDL_DLL_DIR = priv
endif

-include vsn

ifeq ($(ESDL_VSN), )
ESDL_VSN = $(REL)
endif

target clean:
	@for d in $(SUBDIRS); do         \
           if test ! -d $$d ; then \
               echo "=== Skipping subdir $$d" ; \
           else                    \
               xflag="" ;      \
               if test -f $$d/ignore_config_record.inf; then \
                  xflag=$$tflag ; \
               fi ;                    \
              (cd $$d && $(MAKE) $$xflag $@) ; \
            fi ;                           \
        done

release: target relsrc relwin

## Install needs to find the erlang directory
ERL_DIR := $(shell echo 'io:format("~s~n",[code:root_dir()]),halt().' | erl | sed 's,^[0-9]*> *,,g' | tail +2)

INSTALLDIR = $(ERL_DIR)/lib/$(ESDL_VSN)

## I assume that make install are only made on unixes... :-)
install: 
	@echo Found erlang at $(ERL_DIR)
	@echo Installing $(ESDL_VSN) in $(INSTALLDIR)
	@if test ! -d $(INSTALLDIR) ; then \
		mkdir $(INSTALLDIR) ; \
	else \
		rm -rf $(INSTALLDIR)/* ; \
	fi ;
	mkdir $(INSTALLDIR)/src
	mkdir $(INSTALLDIR)/c_src
	mkdir $(INSTALLDIR)/include
	mkdir $(INSTALLDIR)/doc
	mkdir $(INSTALLDIR)/ebin
	mkdir $(INSTALLDIR)/priv
	@if test ! -f vsn ; then  \
		echo ESDL_VSN=$(ESDL_VER) > $(INSTALLDIR)/vsn ; \
	else \
		cp vsn $(INSTALLDIR) ; \
	fi;
	cp license.terms Readme* $(INSTALLDIR)
	cp src/*.?rl $(INSTALLDIR)/src
	cp c_src/*.[ch] $(INSTALLDIR)/c_src
	cp include/*.hrl $(INSTALLDIR)/include
	cp doc/*.html $(INSTALLDIR)/doc
	cp ebin/*beam $(INSTALLDIR)/ebin
	cp priv/*.* $(INSTALLDIR)/priv


## This is used by me only for making a new release...
relsrc: target
	@echo Making $(RELDIR).tgz
	@if test ! -d $(RELDIR) ; then \
		mkdir $(RELDIR) ; \
	else \
		rm -rf $(RELDIR)/* ; \
	fi ;
	cp Makefile* license.terms README-SDL.txt Readme Readme.MacOSX-Cocoa Readme.win32 esdl.pub $(RELDIR)
	cp Configure-cygwin-mingw.sh $(RELDIR)
	echo ESDL_VSN=$(REL) > $(RELDIR)/vsn
	mkdir $(RELDIR)/api_gen
	cp api_gen/conv.erl api_gen/gldefs api_gen/glfuncs \
	   api_gen/gludefs api_gen/glufuncs $(RELDIR)/api_gen
	mkdir $(RELDIR)/c_src
	cp c_src/*.h c_src/*.c c_src/SDLMain.m c_src/Makefile* $(RELDIR)/c_src
	mkdir $(RELDIR)/doc
	cp doc/Makefile* doc/makedoc.erl doc/*.html $(RELDIR)/doc
	mkdir $(RELDIR)/ebin
	cp ebin/*.beam $(RELDIR)/ebin
	mkdir $(RELDIR)/include
	cp include/*.hrl $(RELDIR)/include
	mkdir $(RELDIR)/priv
	mkdir $(RELDIR)/src
	cp src/Makefile* src/*.?rl $(RELDIR)/src
	mkdir $(RELDIR)/test
	cp test/Makefile* test/*.erl test/*.wav test/*.bmp $(RELDIR)/test
	(cd $(RELDIR)/..; $(TAR) zcfv $(REL).src.tar.gz $(REL) )

relwin: relsrc
	cp $(SDL_DLL_DIR)/SDL.dll $(RELDIR)/priv
	cp priv/sdl_driver.dll $(RELDIR)/priv
	cp README-SDL.txt $(RELDIR)
	(cd $(RELDIR)/..; zip -r $(REL).win.zip $(REL) )

