-module(woggle).

-define(WOGGLE_PORT,esdl_port).
-define(DRIVER_NAME,"woggle_driver").

-include("woggle.hrl").
-include("woggle_ops.hrl").

-compile(export_all).

init() ->
    ok = erl_ddll:load_driver(priv_dir(),?DRIVER_NAME),
    Port = open_port({spawn,?DRIVER_NAME},[binary]),
    register(?WOGGLE_PORT, Port).

swapBuffers() ->    
    cast(?WOGGLE_SwapBuffers,[]).

listModes() ->
    listModesConvert(call(?WOGGLE_ListModes,[])).

listModesConvert(<<>>) ->
    [];
listModesConvert(<<D:8, F:32, W:16, H:16, Rest/binary>>) ->
    [#woggle_res{cdepth = D, freq=F, w=W, h=H} | listModesConvert(Rest)];
listModesConvert(<<Res:16>>) ->
    {error,Res}.

%% PLACEHOLDER_FOR_GENERATED_FUNCTIONS

%% Same as in sdl.erl, but we should be able to run without 
%% most of esdl eventually, so we keep local copies, and omit sdlmem.

cast(Op, Arg) ->
    erlang:port_control(?WOGGLE_PORT, Op, Arg),
    ok.

call(Op, Arg) ->
    erlang:port_control(?WOGGLE_PORT, Op, Arg).

send_bin(Bin) when is_binary(Bin) ->
    erlang:port_command(?WOGGLE_PORT, Bin).

send_bin(Bin, _, _) when is_binary(Bin) -> send_bin(Bin);
send_bin(Term, Mod, Line) -> erlang:fault({Mod,Line,unsupported_type,Term}).

% XXX Fixme
priv_dir() ->
    case code:priv_dir(esdl) of
	P when list(P) -> 
	    P;
	{error, _} ->
	    case code:is_loaded(?MODULE) of
		{file, SDLPath} -> 
		    strip(SDLPath, "ebin/" ++ atom_to_list(?MODULE) 
			  ++ ".beam") ++ "priv/";
		_ ->  
		    atom_to_list(c:pwd()) ++ "../../priv/"
	    end
    end.

strip(Src, Src) ->
    [];
strip([H|R], Src) ->
    [H| strip(R, Src)].
