%%%----------------------------------------------------------------------
%%% File    : makedoc.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 29 Nov 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(makedoc).
-author('dgud@erix.ericsson.se').

%%-compile(export_all).
-export([start/0, start/1]).

-record(f, {func, args, ret, capi, desc, ef}).

start() ->
    SdlWWW = "http://www.libsdl.org/docs/", %%video/functions.html#SDL_GetVideoSurface",
    make(sdl, SdlWWW),
    make(sdl_active, SdlWWW),     
    make(sdl_audio, SdlWWW),
    make(sdl_events, SdlWWW),
    make(sdl_keyboard, SdlWWW),
    make(sdl_mouse,  SdlWWW),
    make(sdl_video, SdlWWW),
    GLWWW = "http://www.sun.com/software/graphics/OpenGL/manpages/",
    make(gl,  GLWWW),
    make(glu, GLWWW),
    make(sdl_util, ""),
    ok.

start([File]) ->
    SdlWWW = "http://www.libsdl.org/docs/", %%video/functions.html#SDL_GetVideoSurface",
    GLWWW = "http://www.sun.com/software/graphics/OpenGL/manpages/",
%%    io:format("Makedoc: ~p~n", [File]),
    case File of 
	'../src/sdl.erl' ->
	    make(sdl, SdlWWW);
	'../src/sdl_active.erl' ->
	    make(sdl_active, SdlWWW);     
	'../src/sdl_audio.erl' ->
	    make(sdl_audio, SdlWWW);
	'../src/sdl_events.erl' ->
	    make(sdl_events, SdlWWW);
	'../src/sdl_keyboard.erl' ->
	    make(sdl_keyboard, SdlWWW);
	'../src/sdl_mouse.erl' ->
	    make(sdl_mouse,  SdlWWW);
	'../src/sdl_joystick.erl' ->
	    make(sdl_joystick,  SdlWWW);
	'../src/sdl_video.erl' ->
	    make(sdl_video, SdlWWW);
	'../src/gl.erl' ->
	    make(gl,  GLWWW);
	'../src/glu.erl' ->
	    make(glu, GLWWW);
	'../src/sdl_util.erl'  ->
	    make(sdl_util, "")
    end,

    ok.


make(Module, WWW) ->
    Mod = atom_to_list(Module),
    Infile = filename:join(["../src/", Mod ++ ".erl"]),
    Outfile = Mod ++ ".html",
    io:format("Making ~p => ~p ~n", [Infile, Outfile]),
			       
    {ok, In}  = file:open(Infile, [read]),
    {ok, Out} = file:open(Outfile, [write]),
    insert_header(Mod, Out),
    Next = fun() -> io:get_line(In, "[Reading]") end, 
    create_doc(Next(), Next, WWW, Out, undef, Module, 1, undef),
    insert_footer(Out),
    file:close(In),
    file:close(Out).

%%create_doc(Line, NextLineFun, WWWRef, OutFile, Acc)
create_doc(eof, N, W, O, Acc, M, L, FD) ->    
    ok;
create_doc("%% Func:" ++ Func, N, W, O, undef, M, L, undef) ->
    case strip(Func) of
	[] -> 
	    io:format("~s:~p: Empty func desc ~n", [M, L]),
	     create_doc(N(), N, W, O, func, M, L + 1, undef);
	F -> 
	    create_doc(N(), N, W, O, func, M, L + 1, #f{func = F})
    end;
create_doc("%% Args:" ++ Arg, N, W, O, func, M, L, FD) -> 
    case strip(Arg) of
	[] ->
	    create_doc(N(), N, W, O, args, M, L + 1, FD);
	Args ->
%%	    io:format(O, "Args: ~s ~n", [Args]),
	    create_doc(N(), N, W, O, args, M, L + 1, FD#f{args = Args})
    end;
create_doc("%% Returns:" ++ Value, N, W, O, args, M, L, FD) ->
    case strip(Value) of
	[] ->
	    create_doc(N(), N, W, O, ret, M, L + 1, FD);
	V ->
%%	    io:format(O, "Returns: ~s ~n", [V]),
	    create_doc(N(), N, W, O, ret, M, L + 1, FD#f{ret = V})
    end;
create_doc("%% Desc:" ++ Value, N, W, O, State, M, L, FD) ->
    case strip(Value) of
	[] ->
	    create_doc(N(), N, W, O, desc, M, L + 1, FD);
	V ->
%%	    io:format(O, "Description: ~s ~n", [V]),
	    create_doc(N(), N, W, O, desc, M, L + 1, FD#f{desc = V})
    end;
create_doc("%% C-API func:" ++ Value, N, W, O, ret, M, L, FD) ->
    case strip(Value) of
	[] ->
	    io:format("~s:~p: Missing Ref to C-API~n", [M, L]),
	    create_doc(N(), N, W, O, capi, M, L + 1, FD);
	V ->
%%	    io:format(O, "C_API REF: <a href=\"~s \"> ~s </a>~n", [W, V]),
	    create_doc(N(), N, W, O, capi, M, L + 1, FD#f{capi = V})
    end;
    
create_doc("%%" ++ Rest, N, W, O, undef, M, L, undef) ->
    create_doc(N(), N, W, O, undef, M, L + 1, undef);
create_doc("%%" ++ Value, N, W, O, State, M, L, FD) ->
    case strip(Value) of
	[] ->
	    create_doc(N(), N, W, O, State, M, L + 1, FD);
	V ->
%%	    io:format(O, "~s ~n", [V]),
	    create_doc(N(), N, W, O, State, M, L + 1, add_line(V, State, FD))
    end;
create_doc(Else, N, W, O, undef, M, L, undef) ->
    create_doc(N(), N, W, O, undef, M, L + 1, undef);
create_doc(Else, N, W, O, capi, M, L, FD) ->
    EF = strip(Else),
    NewL = write_doc(FD#f{ef = EF}, N, W, O, M, L),
    create_doc(N(), N, W, O, undef, M, NewL + 1, undef);
create_doc(Else, N, W, O, desc, M, L, FD) ->
    EF = strip(Else),
    NewL = write_doc(FD#f{ef = EF}, N, W, O, M, L),
    create_doc(N(), N, W, O, undef, M, NewL + 1, undef);
create_doc(Else, N, W, O, S, M, L, FD) ->
    io:format("~s:~p: Error in parsing got ~p ~p ~n", 
	      [M, L, Else, FD]),
    create_doc(N(), N, W, O, undef, M, L + 1, undef).

write_doc(FD, N, W, O, M, L) ->
    case get_func_clause(FD#f.ef, []) of
	false ->	 
	    NextLine = strip(N()),
%	    io:format("~s:~p: Incomplete func header ~p ~n", [M, L, FD#f.ef])
	    1 + write_doc(add_line(NextLine, ef, FD), N, W, O, M, L);
	FC when list(FC) ->
	    case strip(N()) of
		"exit(nyi)" ++ _IgnoreRest ->
		    io:format("~s:~p: Not implemented Ignoring ~p~n", 
			      [M,L, FD#f.func]),
		    L;
		Next ->
		    io:format(O, "<p><A NAME=\"~s\">~n",    [FD#f.func]),
		    io:format(O, "<strong> ~s </strong>", [FC]),
		    io:format(O, "<UL>", []),
		    write_type(O, "Args:", FD#f.args),
		    write_type(O, "Returns:", FD#f.ret),
		    write_type(O, gen_ext_ref(M, FD#f.capi, W), FD#f.capi),
		    write_type(O, "Desc:<br>", FD#f.desc),
		    io:format(O, "</UL>~n", []),	    
		    L + 1
	    end
    end.   

write_type(O, Desc, undefined) ->    ok;
write_type(O, Desc, Str) ->
    io:format(O, "~s ~s<br>~n", [Desc, Str]).    

add_line(V, func, FD) ->
    FD#f{func = FD#f.func ++ [$  | V]};
add_line(V, args, FD) ->
    FD#f{args = FD#f.args ++ [$  | V]};
add_line(V, ret, FD) ->
    FD#f{ret = FD#f.ret ++ [$  | V]};
add_line(V, capi, FD) ->
    FD#f{capi = FD#f.capi ++ [$  | V]};
add_line(V, desc, FD) ->
    FD#f{desc = FD#f.desc ++ [$  | V]};
add_line(V, ef, FD) ->
    FD#f{ef = FD#f.ef ++ [$  | V]}.

gen_ext_ref(gl, Func, W) ->
    "<a href=\"" ++ W ++ get_capi_funcname(Func) ++ ".html\">C-API:</a>";
gen_ext_ref(glu, Func, W) ->
    "<a href=\"" ++ W ++ get_capi_funcname(Func) ++ ".html\">C-API:</a>";
gen_ext_ref(_, Func, W) ->
    "<a href=\"" ++ W ++ lc(get_capi_funcname(Func)) ++ ".html\">C-API:</a>".

get_capi_funcname(Func) ->
    get_capi_funcname(Func, [$#]).

get_capi_funcname([], _)->   [];
get_capi_funcname(undefined, _)->   [];

get_capi_funcname([$( | Rest], Acc) ->
    New = strip_gl(Acc),
    lists:reverse(New);
get_capi_funcname([$  | Rest], Acc) ->
    get_capi_funcname(Rest, []);
get_capi_funcname([$* | Rest], Acc) ->
    get_capi_funcname(Rest, []);
get_capi_funcname([H|R], Acc) ->
    get_capi_funcname(R, [H |Acc]).


lc([]) ->
    [];
lc([$_|R]) ->
    lc(R);
lc([A|R]) when A > $A, A < $a ->
    [(A - $A + $a) | lc(R)];
lc([A|R]) ->
    [A|lc(R)].

get_func_clause([], _) -> false;
get_func_clause("when" ++ RFunc, Acc) ->
    lists:reverse(Acc);
get_func_clause("->" ++ Rest, Acc) ->
    lists:reverse(Acc);
get_func_clause([H|R], Acc) ->
    get_func_clause(R, [H|Acc]).

insert_header(Mod, Out) ->
    io:format(Out, 
	      "<HTML><HEAD><TITLE>~s</TITLE></HEAD>~n"
	      "<BODY BGCOLOR=\"#FFFFFF\">~n", [Mod]),
    io:format(Out, "<H3>Header File</H3>~n", []),
    io:format(Out, "The header file may include additional information "
	      "<a href=\"../include/~s.hrl\">~s.hrl</a>~n", [Mod, Mod]),    
    io:format(Out, "<H3>Exports</H3>~n", []).

insert_footer(Out) ->
    io:format(Out, "</html>", []).

strip(String) ->
    Temp = strip1(lists:reverse(String)),
    strip1(lists:reverse(Temp)).
		  
strip1([$  | Rest]) ->
    strip(Rest);
strip1([$\n |Rest]) ->
    strip(Rest);
strip1([$\t |Rest]) ->
    strip(Rest);
strip1([$\r |Rest]) ->
    strip(Rest);
strip1(Rest) ->
    Rest.

strip_gl("b1" ++ Rest) -> Rest;
strip_gl("b2" ++ Rest) -> Rest;
strip_gl("b3" ++ Rest) -> Rest;
strip_gl("b4" ++ Rest) -> Rest;
strip_gl("bu1" ++ Rest) -> Rest;
strip_gl("bu2" ++ Rest) -> Rest;
strip_gl("bu3" ++ Rest) -> Rest;
strip_gl("bu4" ++ Rest) -> Rest;
strip_gl("s1" ++ Rest) -> Rest;
strip_gl("s2" ++ Rest) -> Rest;
strip_gl("s3" ++ Rest) -> Rest;
strip_gl("s4" ++ Rest) -> Rest;
strip_gl("su1" ++ Rest) -> Rest;
strip_gl("su2" ++ Rest) -> Rest;
strip_gl("su3" ++ Rest) -> Rest;
strip_gl("su4" ++ Rest) -> Rest;
strip_gl("i1" ++ Rest) -> Rest;
strip_gl("i2" ++ Rest) -> Rest;
strip_gl("i3" ++ Rest) -> Rest;
strip_gl("i4" ++ Rest) -> Rest;
strip_gl("iu1" ++ Rest) -> Rest;
strip_gl("iu2" ++ Rest) -> Rest;
strip_gl("iu3" ++ Rest) -> Rest;
strip_gl("iu4" ++ Rest) -> Rest;
strip_gl("f1" ++ Rest) -> Rest;
strip_gl("f2" ++ Rest) -> Rest;
strip_gl("f3" ++ Rest) -> Rest;
strip_gl("f4" ++ Rest) -> Rest;
strip_gl("d1" ++ Rest) -> Rest;
strip_gl("d2" ++ Rest) -> Rest;
strip_gl("d3" ++ Rest) -> Rest;
strip_gl("d4" ++ Rest) -> Rest;
strip_gl("vb1" ++ Rest) -> Rest;
strip_gl("vb2" ++ Rest) -> Rest;
strip_gl("vb3" ++ Rest) -> Rest;
strip_gl("vb4" ++ Rest) -> Rest;
strip_gl("vbu1" ++ Rest) -> Rest;
strip_gl("vbu2" ++ Rest) -> Rest;
strip_gl("vbu3" ++ Rest) -> Rest;
strip_gl("vbu4" ++ Rest) -> Rest;
strip_gl("vs1" ++ Rest) -> Rest;
strip_gl("vs2" ++ Rest) -> Rest;
strip_gl("vs3" ++ Rest) -> Rest;
strip_gl("vs4" ++ Rest) -> Rest;
strip_gl("vsu1" ++ Rest) -> Rest;
strip_gl("vsu2" ++ Rest) -> Rest;
strip_gl("vsu3" ++ Rest) -> Rest;
strip_gl("vsu4" ++ Rest) -> Rest;
strip_gl("vi1" ++ Rest) -> Rest;
strip_gl("vi2" ++ Rest) -> Rest;
strip_gl("vi3" ++ Rest) -> Rest;
strip_gl("vi4" ++ Rest) -> Rest;
strip_gl("viu1" ++ Rest) -> Rest;
strip_gl("viu2" ++ Rest) -> Rest;
strip_gl("viu3" ++ Rest) -> Rest;
strip_gl("viu4" ++ Rest) -> Rest;
strip_gl("vf1" ++ Rest) -> Rest;
strip_gl("vf2" ++ Rest) -> Rest;
strip_gl("vf3" ++ Rest) -> Rest;
strip_gl("vf4" ++ Rest) -> Rest;
strip_gl("vd1" ++ Rest) -> Rest;
strip_gl("vd2" ++ Rest) -> Rest;
strip_gl("vd3" ++ Rest) -> Rest;
strip_gl("vd4" ++ Rest) -> Rest;
%strip_gl("b" ++ Rest) -> Rest;
strip_gl("bu" ++ Rest) -> Rest;
%strip_gl("s" ++ Rest) -> Rest;
strip_gl("su" ++ Rest) -> Rest;
%strip_gl("i" ++ Rest) -> Rest;
strip_gl("iu" ++ Rest) -> Rest;
%strip_gl("f" ++ Rest) -> Rest;
%strip_gl("d" ++ Rest) -> Rest;
strip_gl("vb" ++ Rest) -> Rest;
strip_gl("vbu" ++ Rest) -> Rest;
strip_gl("vs" ++ Rest) -> Rest;
strip_gl("vsu" ++ Rest) -> Rest;
strip_gl("vi" ++ Rest) -> Rest;
strip_gl("viu" ++ Rest) -> Rest;
strip_gl("vf" ++ Rest) -> Rest;
strip_gl("vd" ++ Rest) -> Rest;

strip_gl(NoStrip) -> NoStrip.

