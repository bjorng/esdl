#  Copyright (c) 2001 Dan Gudmundsson
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
# 
#     $Id$
#
ESRC = .
EBIN = ../ebin
ERLC = erlc
ERLINC = ../include
ERL_FLAGS = -I$(ERLINC) +inline +warn_unused_vars

ErlMods		= \
		sdl sdl_video sdl_events \
		sdl_mouse sdl_joystick sdl_keyboard sdl_active sdl_audio \
		gl glu sdl_util

ErlSrc		= $(ErlMods:%=%.erl)
ErlTargets	= $(ErlMods:%=$(EBIN)/%.beam)

# Targets
target: $(ErlTargets)

clean:  
	rm -f $(ErlTargets) 
	rm -f *~

# Rules
$(EBIN)/%.beam: $(ESRC)/%.erl
	$(ERLC) -W -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<
