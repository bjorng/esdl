
ESRC = ../src
ERLC = erlc

DOCG = makedoc.beam
DOC  = gl.html glu.html sdl.html sdl_active.html \
	sdl_audio.html sdl_events.html sdl_keyboard.html sdl_mouse.html sdl_joystick.html \
	sdl_util.html sdl_video.html


target: $(DOCG) $(DOC) 
%.beam: %.erl
	$(ERLC) -W -bbeam $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) $<

clean:  
	rm -f $(DOCG) $(DOC) 
	rm -f *~

%.html: $(ESRC)/%.erl $(DOCG)
	erl -noshell -s makedoc start $< -s erlang halt
