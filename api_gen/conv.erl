%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Dan Gudmundsson.
%% Portions created by me are Copyright 2000 All Rights Reserved.''
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : conv.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : conv between C header files to generate C and Erl stub files
%%% Created : 29 Aug 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(conv).
-author('dgud@erix.ericsson.se').

-include("conv.hrl").

%%-compile(export_all).
-export([go/1]).
-export([is_matrixOp/1]).

-record(fs, {hrl, erl, c, h, sw, fhrl, last = 0, file}).

-define(W(Str,Args), io:format(Fd, Str, Args)).

-define(erl_comments, [Fd#fs.hrl]).
-define(c_comments, []).

-define(GLUTYPE(T), T=="GLUtesselator";T=="GLUnurbs";T=="GLUquadric").

-define(GL_BYTE_SIZE, 8).
-define(GL_UNSIGNED_BYTE_SIZE, 8).
-define(GL_SHORT_SIZE, 16).
-define(GL_UNSIGNED_SHORT_SIZE, 16).
-define(GL_INT_SIZE, 32).
-define(GL_UNSIGNED_INT_SIZE, 32).
-define(GL_FLOAT_SIZE, 32).
-define(GL_DOUBLE_SIZE, 64).

-define(SEARCH, "http://www.google.com/search?btnI=I%27m+Feeling+Lucky&amp;q=manual+pages+").

gl_type_size(TYPE) -> 
    case (TYPE) of 
	"boolean" ->            hd(io_lib:format("~p", [?GL_BYTE_SIZE div 8]));
	"?GL_BYTE" ->           hd(io_lib:format("~p", [?GL_BYTE_SIZE div 8]));
	"?GL_UNSIGNED_BYTE" ->  hd(io_lib:format("~p", [?GL_UNSIGNED_BYTE_SIZE div 8]));
	"?GL_SHORT" ->          hd(io_lib:format("~p", [?GL_SHORT_SIZE div 8]));
	"?GL_UNSIGNED_SHORT" -> hd(io_lib:format("~p", [?GL_UNSIGNED_SHORT_SIZE div 8]));
	"?GL_INT" ->            hd(io_lib:format("~p", [?GL_INT_SIZE div 8]));
	"?GL_UNSIGNED_INT" ->   hd(io_lib:format("~p", [?GL_UNSIGNED_INT_SIZE div 8]));
	"?GL_FLOAT" ->          hd(io_lib:format("~p", [?GL_FLOAT_SIZE div 8]));
	"?GL_DOUBLE" ->         hd(io_lib:format("~p", [?GL_DOUBLE_SIZE div 8]));
	ELsE -> ELsE
    end.            

go(File) ->
    {ok, Bin} = file:read_file(File),
    Prog_list = binary_to_list(Bin),
    Prog = tokens(Prog_list, " \t\n\r,();*"),
    FName = 
	if atom(File) ->
		atom_to_list(File) -- ".h";
	   list(File) ->
		File -- ".h"
	end,
    Dir = "generated/",
    {ok, Erl}  = file:open(Dir ++ FName ++ ".erl", [write]),
    {ok, Hrl}  = file:open(Dir ++ FName ++ ".hrl", [write]),
    {ok, FHrl} = file:open(Dir ++ FName ++ "_funcs.hrl", [write]),
    {ok, C}    = file:open(Dir ++ "esdl_" ++ FName  ++ ".c", [write]),
    {ok, H}    = file:open(Dir ++ "esdl_" ++ FName  ++ ".h", [write]),
    {ok, Sw}   = file:open(Dir ++ "esdl_" ++ FName  ++ "_fp.h", [write]),

    State0 = case FName of 
		 "gl"  -> #fs{file = "OPENGL"};
		 "glu" -> #fs{file = "OPENGLU"};
		 "glext" -> #fs{file = "OPENGL_EXTS"}
	     end,

    [write_cpyr_c(F) || F <- [C,H,Sw]],
    [write_cpyr_erl(F) || F <- [Erl, Hrl, FHrl]],

    State = State0#fs{hrl = Hrl, erl = Erl, c = C, h = H, sw = Sw, fhrl = FHrl},
    init_files(FName, State),
    Res = generate(Prog, State),
    post_files(FName, State),
    Files = [Erl, FHrl, Hrl, C, H, Sw],
    [file:close(F) || F <- Files],
    Res.


write_cpyr_c(Fd) ->
    ?W("/*  
 *  Copyright (c) 2003 Dan Gudmundsson
 *  See the file \"license.terms\" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 *
 */
", []).

write_cpyr_erl(Fd) ->
    ?W("%%
%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file \"license.terms\" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%
", []).

init_files("gl", State) ->
    erase(),
    GL = conv_gl:add(),
    lists:foreach(fun({F, D}) -> put(F,D) end, GL),
    conv_gl:init_hrl(State#fs.hrl),
    conv_gl:init_erl(State#fs.erl),    
    catch conv_gl:init_c(State#fs.c);
init_files("glu", State) ->
    erase(),
    GLU = conv_glu:add(),
    lists:foreach(fun({F, D}) -> put(F,D) end, GLU),
    conv_glu:init_hrl(State#fs.hrl),
    conv_glu:init_erl(State#fs.erl),    
    catch conv_glu:init_c(State#fs.c);
init_files("glext", State) ->
    erase(),
    GLU = conv_glext:add(),
    lists:foreach(fun({F, D}) -> put(F,D) end, GLU),    
    catch conv_glext:init_hrl(State#fs.hrl),
    catch conv_glext:init_erl(State#fs.erl),    
    catch conv_glext:init_c(State#fs.c),
    catch conv_glext:init_h(State#fs.h);
init_files(Which, _State) ->
    io:format("Don't no how to initialize ~s~n", [Which]).

post_files("gl", State) ->
    catch conv_gl:post_erl(State#fs.erl),    
    catch conv_gl:post_hrl(State#fs.hrl);
post_files("glu", State) ->
    catch conv_glu:post_erl(State#fs.erl),    
    catch conv_glu:post_hrl(State#fs.hrl);
post_files("glext", State) ->
    catch conv_glext:post_erl(State#fs.erl),    
    catch conv_glext:post_hrl(State#fs.hrl);
post_files(_, _) ->
    ok.

generate(["/", pointer |R], Fd) ->
    [io:format(F, "%%%% ", []) || F <- ?erl_comments],
    [io:format(F, "/* ", []) || F <-   ?c_comments  ],
    Rest = generate_comments(R, Fd),
    generate(Rest, Fd);
generate([[$/, pointer |R1]|R], Fd) ->
    [io:format(F, "%%%% ", []) || F <- ?erl_comments],
    [io:format(F, "/* ", []) || F <-   ?c_comments  ],
    Rest = generate_comments([R1|R], Fd),
    generate(Rest, Fd);
generate([What, "=", Value, eol | R], Fd) ->
    V = generate_value(Value),
    io:format(Fd#fs.hrl, "-define(~s, ~s).~n", [What, V]),
    generate(R, Fd);
generate(["#define", What, eol | R], Fd) ->
    io:format(Fd#fs.hrl, "-define(~s, 1).~n", [What]),
    generate(R, Fd);
generate(["#define", What, Value, eol | R], Fd) ->
    V = generate_value(Value),
    io:format(Fd#fs.hrl, "-define(~s, ~s).~n", [What, V]),
    {Arb,String} = arb(What),
    case Arb of
	false -> 
	    ignore;
	_ ->
	    io:format(Fd#fs.hrl, "-define(~s, ~s).~n", [String, V])
    end,
    generate(R, Fd);
generate(["#ifndef", What, eol | R], Fd) ->
    case skip_extensions(What, R) of
	false ->
	    io:format(Fd#fs.hrl, "-ifndef(~s).~n", [What]),
	    generate(R, Fd);
	NewR ->
	    generate(NewR, Fd)
    end;
generate(["#ifdef", "GL_GLEXT_PROTOTYPES", eol | R0], Fd) ->
    R=store_prototypes(R0),
    generate(R, Fd);
generate(["#ifdef", What, eol | R], Fd) ->
    io:format(Fd#fs.hrl, "-ifdef(~s).~n", [What]),
    generate(R, Fd);
generate(["#else", eol | R], Fd) ->
    io:format(Fd#fs.hrl, "-elseif.~n", []),
    generate(R, Fd);
generate(["#endif" | R], Fd) ->
    io:format(Fd#fs.hrl, "-endif.~n", []),
    generate(R, Fd);
generate(["enum"|R], Fd) ->
    generate(R, Fd);
generate(["extern", _, "{" |R], Fd) ->
    generate(R, Fd);
generate(["#pragma"|R0], Fd) ->
    [eol|R] = lists:dropwhile(fun(eol) -> false; (_) ->true end, R0),
    generate(R, Fd);
generate(["#include"|R0], Fd) ->
    [eol|R] = lists:dropwhile(fun(eol) -> false; (_) ->true end, R0),
    generate(R, Fd);
generate(["class"|R0], Fd) ->
    [eol|R] = lists:dropwhile(fun(eol) -> false; (_) ->true end, R0),
    generate(R, Fd);
generate(["{"|R], Fd) ->
    generate(R, Fd);
generate(["}"|R], Fd) ->
    generate(R, Fd);
generate(["};"|R], Fd) ->
    generate(R, Fd);
generate([eol|R], Fd) ->
    generate(R, Fd);
generate([], _Fd) ->
    ok;
generate([[]|R], Fd) ->
    generate(R, Fd);
generate([Func|R], Fd) 
  when Func == "WINGDIAPI"; Func == "GLAPI"; Func == "extern" ->
    case catch generate_funcs(R, Fd) of
	{'EXIT', What} -> 
	    display_error(What, R, Fd);
	{R2, Fd2} ->
	    generate(R2, Fd2)
    end;

generate(["typedef", Type, lpar, "APIENTRYP", Func, rpar|R], Fd) ->
    NewF = gb_trees:get(Func, get(glext_proto)),
    generate(["GLAPI",Type, "APIENTRY",NewF|R], Fd);

generate(["typedef"|R], Fd) ->
    io:format("skipped typedef ~p ~n", [string:substr(R, 1, 5)]),
    C = lists:dropwhile(fun(eol) -> false; 
			   (_A) -> true
			end, R),
    generate(C, Fd);

generate(R, Fd) ->
    io:format("%% ERROR: Generate couldn't handle '", []),
    C = lists:dropwhile(fun(eol) -> false; 
			   (A) -> io:format("~s ", [A]), true
			end, R),
    io:format("'~n"),
    generate(C, Fd).


display_error(What, R, Fd) ->    
    case What of 
	{Reason, [{M,F,[A1|AR]}| ER]} when list(A1) ->
	    io:format(" ERROR: Generate funcs couldn't handle ", []),
	    C = lists:dropwhile(fun(eol) -> false; 
				   (A) -> io:format("~s ", [A]), true
				end, R),
	    io:format("~n EXIT ~p  ~n", [{Reason, [{M,F,[lists:sublist(A1, 10)|AR]}|ER]}]),
	    generate(C, Fd);
	_ ->
	    io:format("%% ERROR: Generate funcs couldn't handle ~p ~n", 
		      [What])
    end.

generate_funcs(["const", Type, pointer | Rest], Files) ->
    generate_funcs([{Type, pointer}| Rest], Files);
generate_funcs([Type, pointer | Rest], Files) ->
    generate_funcs([{Type, pointer}| Rest], Files);
generate_funcs([Type, AE, Func, lpar | Rest], Files) 
  when AE == "APIENTRY"; AE == "GLAPIENTRY" ->
    generate_funcs([Type, Func, lpar | Rest], Files);
generate_funcs([Type, Func, lpar | Rest], Files) ->
    {Args, R2} = getArgs(Rest, []),
    Next = case catch genfuncs(Type, Func, Args, Files) of
		{'EXIT', Reason} ->
		    io:format("genfuncs crashed in ~p ~s(~p)~n ~p",
			      [Type,Func, Args,Reason]),
		    exit(Reason);
		Res ->
		    Res
	    end,
    {R2, Files#fs{last = Next}}.

generate_comments([pointer, "/"|R], Fd) ->
    [io:format(F, "~n", []) || F <-    ?erl_comments],
    [io:format(F, "*/~n ", []) || F <- ?c_comments  ],
    R;
generate_comments([eol|R], Fd) ->
    [io:format(F, "~n%%%", []) || F <- ?erl_comments],
    [io:format(F, "~n * ", []) || F <- ?c_comments  ],
    generate_comments(R, Fd);
generate_comments([W|R], Fd) ->
    [io:format(F, "~s ", [W]) || F <- ?erl_comments],
    [io:format(F, "~s ", [W]) || F <- ?c_comments],
    generate_comments(R, Fd).

generate_value([$0, $x | Rest]) ->
    "16#" ++ Rest;
generate_value("GL_" ++ _ = Val) ->
    "?" ++ Val;
generate_value(Val) ->
    Val.

getArgs([rpar, eol | Rest], Acc) -> 
    {lists:reverse(Acc), Rest};
getArgs(["void", rpar, eol | Rest], Acc) -> 
    {lists:reverse(Acc), Rest};
%getArgs(["const" | Rest], Acc) ->
%    getArgs(Rest, Acc);
%getArgs(["void", "(GLCALLBACK", "*fn)()"| Rest], Acc) ->
%    getArgs(Rest, [{"void", "(GLCALLBACK *fn)()"} | Acc]);
getArgs([Type, lpar, pointer, GLCALLBACK, rpar, lpar,rpar| Rest], Acc) ->
    getArgs(Rest, [{Type, callback, GLCALLBACK} | Acc]);
getArgs([Type, Var, rpar, eol | Rest], Acc) ->
    {lists:reverse([{Type, Var}|Acc]), Rest};
getArgs([Type, Var, separtor | Rest], Acc) ->
    getArgs(Rest, [{Type, Var} | Acc]);

getArgs(["const", Type, pointer, Var, separtor | Rest], Acc) ->
    getArgs(Rest, [{Type, const, pointer, Var} | Acc]);
getArgs(["const", Type, pointer, Var, rpar, eol | Rest], Acc) ->
    {lists:reverse([{Type, const, pointer, Var}|Acc]), Rest};

getArgs([Type, pointer, Var, separtor | Rest], Acc) ->
    getArgs(Rest, [{Type, pointer, Var} | Acc]);
getArgs([Type, pointer, Var, rpar, eol | Rest], Acc) ->
    {lists:reverse([{Type, pointer, Var}|Acc]), Rest};

getArgs([Type, pointer, pointer, Var, separtor | Rest], Acc) ->
    getArgs(Rest, [{Type, pointer, pointer, Var} | Acc]);
getArgs([Type, pointer, pointer, Var, rpar, eol | Rest], Acc) ->
    {lists:reverse([{Type, pointer, pointer, Var}|Acc]), Rest};
getArgs(["const", Type, pointer, pointer, Var, separtor | Rest], Acc) ->
    getArgs(Rest, [{Type, const, pointer, pointer, Var} | Acc]);
getArgs(["const", Type, const,pointer, pointer, Var, rpar, eol | Rest], Acc) ->
    {lists:reverse([{Type, const, pointer, pointer, Var}|Acc]), Rest};
getArgs([eol | Rest], Acc) ->
    getArgs(Rest, Acc).

genfuncs(Type, FuncName, Args, F) ->
    case skip(FuncName) of 
	true -> 
	    io:format("Skipped ~s ~n", [FuncName]),
	    F#fs.last;
	call_vector ->
	    Last = F#fs.last,
	    genf_erl(Type, FuncName, Args, Last, F#fs.erl),
	    Last;
	false ->
	    io:format("Generating:  {~p,{skip,[]}},~n", [FuncName]),
	    Last = F#fs.last,
	    genf_h(Type, FuncName, Args, Last, F#fs.file, F#fs.h),
	    genf_sw(Type, FuncName, Args, Last, F#fs.sw),
	    genf_hrl(Type, FuncName, Args, Last, F#fs.file, F#fs.fhrl),
	    genf_erl(Type, FuncName, Args, Last, F#fs.erl),
	    genf_c(Type, FuncName, Args, Last, F#fs.c),
	    Last+1
    end.
    
genf_hrl(_Type, FuncName0, _Args, Last, File, Fd) -> 
    case skip(FuncName0) of
	true -> ignore;
	_ ->
	    {_, FuncName} = arb(FuncName0),
	    ?W("-define(~s, ?SDL_~s_HRL + ~p).~n", [FuncName, File, Last])
    end.
genf_h(Type, FuncName, Args, Last, File, Fd) -> 
    case skip(FuncName) of
	true -> ignore;
	_ ->
	    F = FuncName ++ "Func",
	    ?W("enum { ~s = ~s_H + ~p };~n", [F, File, Last]),
	    case is_extension(FuncName) of
		true -> gen_extension(Type, FuncName, Args, Fd);
		false -> ignore
	    end,
	    ?W("void ~s (sdl_data *, int, char *); ~n", [format_func2c(FuncName)])
    end.
genf_sw(_Type, FN, _Args, _Last, Fd) -> 
    case skip(FN) of
	true -> ignore;
	_ ->
	    case is_extension(FN) of
		true ->
		    ?W("{ ~sFunc,  \"~s\", ~s, &esdl_~s},~n",
		       [FN,FN,format_func2c(FN),FN]);
		false ->
		    ?W("{ ~sFunc,  \"~s\", ~s },~n",[FN,FN,format_func2c(FN)])
	    end
    end.
genf_erl(Type, FuncName, Args0, _Last, Fd) -> 
    EFN = format_func2erl(FuncName),
    {Rets,Args1} = find_returns(Args0, FuncName),
    Args = fixArgsOrder(FuncName, Args1),
    ?W("%% @spec ~s(", [EFN]), write_args(FuncName, erlcom, Args1, Fd), 
    ?W(") -> ",[]), write_returns(FuncName, erlcom, Type, Rets, Fd),
%    ?W("%% Func:    ~s ~n", [EFN]),
%    ?W("%% Args:    ",[]), write_args(FuncName, erlcom, Args1, Fd),
%    ?W("~n%% Returns: ",[]), write_returns(FuncName, erlcom, Type, Rets, Fd),
    ?W("~n%% @doc <a href=\"~s~s\">External manpage: ~s</a>", [?SEARCH,strip_types(FuncName),EFN]),
    ?W("~n%% C-API func: ~s ~s(", 
       if element(2,Type) == pointer -> [element(1,Type)++"*", FuncName]; 
	  true -> [Type, FuncName] end),
    write_args(FuncName, c, Args0, Fd),
    Cfunc = case skip(FuncName) of call_vector -> call_vector(FuncName); _ -> FuncName end,
    ?W(")~n~s(", [EFN]), write_args(FuncName, erl, Args1, Fd), ?W(") -> ~n", []),
    case arb(FuncName) of 
	{false,_} ->
	    ignore;
	{{test,ReNamed},_} ->
	    ?W(" try ~s(", [ReNamed]), write_args(FuncName, erl, Args1, Fd), 
	    ?W(")~n catch error:_ -> ~s_fallback(", [FuncName]), 
	    write_args(FuncName, erl, Args1, Fd),
	    ?W(") end.~n",[]),
	    ?W("%% @hidden~n",[]),
	    ?W("~s_fallback(", [FuncName]), write_args(FuncName, erl, Args1, Fd),
	    ?W(") -> ~n", []);
	{true,NoArb0} ->
	    NoArb = format_func2erl(NoArb0),
	    ?W(" ~s(", [NoArb]), write_args(FuncName, erl, Args1, Fd), 
	    ?W(").~n~s(", [NoArb]), write_args(FuncName, erl, Args1, Fd),
	    ?W(") -> ~n", [])
    end,
    %%    io:format(Fd, "exit(nyi),~n", []),
    NewArgs = maybe_build_erlbinaries(FuncName, Args, Fd),
    case {Type, Rets} of
	{"void", []} ->
	    ?W(" cast(?~s, ", [element(2,arb(Cfunc))]),
	    write_args(FuncName, binerl, NewArgs, Fd),
	    ?W(").~n~n", []);
	_ -> 
	    ?W(" Bin = call(?~s, ", [element(2,arb(Cfunc))]),
	    write_args(FuncName, binerl, NewArgs, Fd),
	    ?W("), ~n",[]),
	    write_undef_rets(FuncName, Rets, Fd),
	    ?W(" case Bin of ~n", []),
	    write_returns(FuncName, erl, Type, Rets, Fd),
	    ?W("\tElse -> erlang:error({?MODULE, " 
		      "?LINE, badtype, Else})~n"
		      " end.~n~n", [])
    end.

gen_extension(Type, Func, Args, Fd) ->
    %% typedef void (APIENTRY * PFNGLPOINTPARAMETERFEXTPROC)(GLenum pname, GLfloat param);
    %% PFNGLPOINTPARAMETERFEXTPROC glPointParameterfEXT;
    case is_extension(Func) of
	true ->
	    Proto = "ESDL" ++ uppercase_all(Func) ++ "PROC", 
	    ?W("typedef ~s (APIENTRY * ~s)(", [Type, Proto]),
	    write_args(Func, c, Args, Fd),
	    ?W(");~n", []),
	    ?W("ESDL_EXTERN ~s esdl_~s;~n", [Proto, Func]),
%	    ?W("void ~s(sdl_data *, int, char *);~n",[format_func2c(Func)]),
%%%	    ?W("sdl_fun esdl_load_~s()~n{~n esdl_~s = (~s) SDL_GL_GetProcAddress(\"~s\");~n",
%%%	       [Func, Func, Proto, Func]),	    
%%%	    ?W(" if(NULL != esdl_~s )~n   return (sdl_fun) ~s; ~n", [Func,format_func2c(Func)]),
%%%	    ?W(" else return NULL; ~n}~n~n", []);
	    ok;
	false ->
	    ignore
    end.

genf_c(Type, FuncName, Args0, _Last, Fd) -> 
    CallFunc =  case is_extension(FuncName) of
		    true -> "esdl_" ++ FuncName;
		    false -> FuncName
		end,
    ?W("void ~s(sdl_data *egl_sd, int egl_len, char *egl_buff) ~n", 
       [format_func2c(FuncName)]),
    ?W("{~n char * bp; ~n", []),
    {Rets,Args} = find_returns(Args0, FuncName),
    case {Type, Rets} of
	{"void", []} ->
	    ok;
	{{RT,pointer},[]} ->
	    ?W(" char * egl_start; ~n", []),
	    ?W(" int egl_sendlen; ~n", []),
	    ?W(" const ~s *egl_res; ~n", [RT]);
	{Type,[]} ->
	    ?W(" char * egl_start; ~n", []),
	    ?W(" int egl_sendlen; ~n", []),
	    ?W(" ~s egl_res; ~n", [Type]);
	{"void", _} ->
	    ?W(" char * egl_start; ~n", []),
	    ?W(" int egl_sendlen; ~n", []);
%	    ?W(" int i; ~n", []);
	_ ->
	    ?W(" char * egl_start; ~n", []),
	    ?W(" int egl_sendlen; ~n", []),
%%	    ?W(" int i; ~n", []),
	    ?W(" ~s egl_res; ~n", [Type])
    end,
    write_args(FuncName, cdef, Args0, Fd),
    ?W(" bp = egl_buff;~n", []),
    OrderedArgs = fixArgsOrder(FuncName, Args0),
    write_args(FuncName, binc, OrderedArgs, Fd),
    case {Type, Rets} of
	{"void", []} ->
	    ?W(" ~s(", [CallFunc]),
	    write_args(FuncName, ccalls, Args0, Fd),
	    ?W(");~n", []),
	    free_mem(Args, FuncName, Fd),
	    ok;
	_ ->	
	    case Type of 
		"void" -> ignore ;
		_ -> 
		    ?W(" egl_res = ", [])
	    end,
	    ?W(" ~s(", [CallFunc]),
	    write_args(FuncName, ccalls, Args0, Fd),
	    ?W(");~n", []),
	    AllocSize = get_size(FuncName, Type, Rets, ""),
	    ?W(" bp = egl_start = sdl_get_temp_buff(egl_sd, ~s);~n",[AllocSize]),
	    case Type of 
		"void" -> ignore;
		{"GLubyte", pointer} ->
		    ?W(" strcpy((GLubyte *)bp, egl_res);~n", []),
		    ?W(" bp += strlen(egl_res);~n", []);
		{RType, pointer} when ?GLUTYPE(RType) ->
		    ?W(" * (~s **) bp = egl_res;~n"
		       " bp += sizeof(~s *);~n", [RType, RType]);
		_ -> 		    
		    ?W(" * (~s *) bp = egl_res;~n"
		       " bp += sizeof(~s);~n", [Type, Type])
	    end,
	    Forloop = fun({V1,T1}) ->
			      case getdef(FuncName, V1) of
				  1 ->
				      ?W(" * (~s *)bp = ~s[0]; bp += sizeof(~s);~n",
					 [T1,V1,T1]);
				  N when integer(N) ->
				      ?W(" memcpy(bp, ~s, sizeof(~s)*~w);~n"
					 " bp += sizeof(~s)*~w;~n",
					 [V1,T1,N,T1,N]);
				  {undefined, N, _} when integer(N) ->
				      ?W(" memcpy(bp, ~s, sizeof(~s)*~w);~n"
					 " bp += sizeof(~s)*~w;~n",
					 [V1,T1,N,T1,N]);
				  {Val,_} when is_list(Val) ->
				      ?W(" memcpy(bp, ~s, sizeof(~s)*(*~s));~n"
					 " bp += sizeof(~s)*(*~s);~n",
					 [V1,T1,Val,T1,Val]);
				  Val when is_list(Val) ->
				      ?W(" memcpy(bp, ~s, sizeof(~s)*(*~s));~n"
					 " bp += sizeof(~s)*(*~s);~n",
					 [V1,T1,Val,T1,Val])
			      end;
			 ({V1,pointer,_T1}) ->
			      ?W(" PUSHGLPTR(~s,bp);~n", [V1])
		      end,
	    lists:foreach(Forloop, Rets),
	    free_mem(Args0, FuncName, Fd),
	    ?W(" egl_sendlen = bp - egl_start;~n"
	       " sdl_send(egl_sd, egl_sendlen);~n", [])
    end,
    ?W("}~n~n~n", []).

free_mem([{_T, const, pointer, pointer, V1}|Args], FuncName, Fd) 
  when FuncName == "glShaderSourceARB" ->
    ?W(" free(~s);~n", [V1]),
    free_mem(Args, FuncName, Fd);
free_mem([{T, const, pointer, V1}|Args], FuncName, Fd) ->
    case getdef(FuncName, V1) of
	Str when list(Str), 
		 ((T == "GLdouble") or (T=="GLclampd")) ->
	    ?W(" free(~s);~n", [V1]);
	{undefined, Val,_} when ((T == "GLdouble") or (T == "GLclampd")), is_list(Val) ->
	    ?W(" free(~s);~n", [V1]);
	_  -> ignore
    end,
    free_mem(Args, FuncName, Fd);
free_mem([{T, pointer, V1}|Args], FuncName, Fd) ->
    case getdef(FuncName, V1) of
	_  when ?GLUTYPE(T) ->
	    ignore;
	Str when list(Str) ->
	    ?W(" free(~s);~n", [V1]);
	{Str,_} when list(Str) ->
	    ?W(" free(~s);~n", [V1]);
	_  -> ignore
    end,
    free_mem(Args, FuncName, Fd);
free_mem([_|Args], FuncName, Fd) ->
    free_mem(Args, FuncName, Fd);
free_mem([], _,Fd) ->
    case get(free_args) of
	undefined ->
	    ok;
	true ->
	    erase(free_args),
	    ?W(" sdl_free_binaries(egl_sd);~n", [])
    end.

maybe_build_erlbinaries(Func,[Arg={_,const,pointer,_}|R],Fd) ->
    case is_vector(Func) of
	N when is_number(N) ->
	    [Arg | maybe_build_erlbinaries(Func, R, Fd)];
	Vec ->
	    case build_erlbinaries(Func, Arg, Vec, R == [], Fd) of
		already_sent -> 
		    maybe_build_erlbinaries(Func, R, Fd);
		New ->
		    put({binary_arg, Func}, Arg),
		    [New | maybe_build_erlbinaries(Func, R, Fd)]
	    end
    end;    
maybe_build_erlbinaries(Func,[Arg={_,const,pointer,pointer,_}|R],Fd) ->
    case is_vector(Func) of
	N when is_integer(N) ->
	    [Arg | maybe_build_erlbinaries(Func, R, Fd)];
	Vector ->
	    case build_erlbinaries(Func, Arg, Vector, R == [], Fd) of
		already_sent -> 
		    maybe_build_erlbinaries(Func, R, Fd);
		New ->
		    put({binary_arg, Func}, Arg),
		    [New | maybe_build_erlbinaries(Func, R, Fd)]
	    end
    end;
maybe_build_erlbinaries(Func, [V={_T,pointer,Var}|R], Fd) ->
    case getdef(Func, Var) of
	pointer -> 
	    ?W(" sdl:send_bin(~s, ?MODULE, ?LINE),~n", [uppercase(Var)]),
	    maybe_build_erlbinaries(Func, R, Fd);
	sdlmem -> 
	    ?W(" sdl:send_bin(~s, ?MODULE, ?LINE),~n", [uppercase(Var)]),
	    maybe_build_erlbinaries(Func, R, Fd);
	_ ->
	    [V|maybe_build_erlbinaries(Func, R,Fd)]
    end;
maybe_build_erlbinaries(Func, [V|R], Fd) ->
    [V|maybe_build_erlbinaries(Func, R,Fd)];
maybe_build_erlbinaries(_F, [], _Fd) ->
    [].

build_erlbinaries(_Func, {_T, const, pointer, pointer,V}, _IsTuple, _IsLast, Fd) ->
    ?W(" lists:foreach(fun(Values) -> sdl:send_bin(list_to_binary([Values,0]), ?MODULE, ?LINE) end, ~s),~n", 
       [uppercase(V)]),
    already_sent;

build_erlbinaries(Func,{T,const,pointer,V},{tuplelist,1}, _IsLast, Fd) ->
    N = case getdef(Func, V) of 
	    What when is_list(What) -> uppercase(What);
	    What when is_integer(What) -> integer_to_list(What)
	end,
    ?W(" sdl:send_bin(list_to_binary(term2bin(~s,~s,~s)), ?MODULE, ?LINE),~n",
       [uppercase(V),N,type_to_enum(T)]),
    already_sent;
build_erlbinaries(_Func,{T,const,pointer,V},{tuplelist,N}, _IsLast, Fd) ->
    ?W(" sdl:send_bin(sdl_util:tuplelist2bin(~p,~s,~s), ?MODULE, ?LINE),~n",
       [N,type_to_enum(T),uppercase(V)]),
    already_sent;
build_erlbinaries(Func, {T, const, pointer,V}, _IsTuple, IsLast, Fd) ->
    Var = uppercase(V),
    Type = case type_to_enum(T) of
	       Unknown = "GeneraterUnknownType:" ++ _ ->
		   case catch uppercase(gettype(Func, V)) of
		       {'EXIT', _} ->
			   Unknown;
		       UppType -> 
			   UppType
		   end;
	       EnumType ->
		   EnumType
	   end,
    case getdef(Func, V) of
	pointer when T == "GLcharARB";T == "GLchar"  -> 
	    ?W(" sdl:send_bin(list_to_binary([~s,0]), ?MODULE, ?LINE),~n", [Var]),
	    already_sent;
	pointer ->
%%	    ?W(" sdl:send_bin(~s, ?MODULE, ?LINE),", [Var]),
	    ?W("%% Maybe NULL or offset sometimes~n",[]), 
	    ?W(" New~s =~n   if is_integer(~s) -> ~s;~n"
	       "      true ->~n",[Var,Var,Var]), 
	    ?W("        sdl:send_bin(~s, ?MODULE, ?LINE),~n",[Var]),
	    ?W("       0~n   end,~n", []),
	    {"GLint", "new" ++ Var};
	sdlmem ->
	    ?W(" sdl:send_bin(~s, ?MODULE, ?LINE),~n", [Var]),
	    already_sent;
	{index_or_list, Val} ->
	    ?W("%% Maybe NULL or offset sometimes2~n",[]),
	    ?W(" New~s = if is_integer(~s) -> ~s; ~n", [Var,Var,Var]),
	    ?W("\tis_list(~s) ; is_tuple(~s) -> ", [Var, Var]),
	    ?W("sdl:send_bin(list_to_binary(term2bin(~s, ~s, ~s)),?MODULE,?LINE),0;~n", 
	       [Var, uppercase(Val), Type]),
	    ?W("\tis_binary(~s) -> sdl:send_bin(~s, ?MODULE, ?LINE),0;~n",[Var,Var]),
	    ?W("\ttrue -> erlang:error({?MODULE, ?LINE, unsupported_type, ~s})~n", 
	       [Var]),
	    ?W(" end, ~n", []),
	    {"GLint", "new" ++ Var};
	Val when integer(Val) ->
	    case Val == 16 andalso is_matrixOp(Func) of
		true ->
		    ?W(" New~s = if~n", [Var]),
		    ?W("\tis_list(~s) ; is_tuple(~s) -> ", [Var, Var]),
		    ?W("matrix2bin(~s, ~s);~n", [Var, Type]),
		    ?W("\tbinary(~s) -> ~s;~n",[Var, Var]),
		    ?W("\ttrue -> erlang:error({?MODULE, ?LINE, unsupported_type, ~s})~n", 
		       [Var]),
		    ?W(" end, ~n", []),
		    {binary, "new" ++ Var};
		false ->
		    ?W(" New~s = if~n", [Var]),
		    ?W("\tis_list(~s) ; is_tuple(~s) -> ", [Var, Var]),
		    ?W("term2bin(~s, ~p, ~s);~n", [Var, Val, Type]),
		    %% 	    ?W("\tbinary(~s), size(~s) >= ~p * ~s -> ~s;~n",
		    %% 		      [Var, Var, Val, gl_type_size(Type), Var]);
		    ?W("\tbinary(~s) -> ~s;~n",[Var, Var]),
		    ?W("\ttrue -> erlang:error({?MODULE, ?LINE, unsupported_type, ~s})~n", 
		       [Var]),
		    ?W(" end, ~n", []),
		    {binary, "new" ++ Var}
	    end;
	{undefined, _,_} when IsLast -> %% Assert that XXXLen is only generated last 
	    ?W(" New~s = if~n", [Var]), %% the code for handling XXXLen has been removed
	    ?W("\tis_list(~s) -> ", [Var]), %% in the c-code (except for type GLdouble)
	    ?W(" ~sLen = length(~s), ~n"
	       "\t  [<<~sLen:32/native>>, term2bin(~s, ~sLen, ~s)];~n",
	       [Var, Var, Var, Var, Var, Type]),
	    ?W("\tis_tuple(~s) -> ", [Var]),
	    ?W(" ~sLen = size(~s), ~n"
	       "\t  [<<~sLen:32/native>>, term2bin(~s, ~sLen, ~s)];~n",
	       [Var, Var, Var, Var, Var, Type]),	    
	    ?W("\tis_binary(~s) -> "
	       "[<<(size(~s) div ~s):32/native>>,~s];~n", 
	       [Var, Var, gl_type_size(Type), Var]),
	    ?W("\ttrue -> erlang:error({?MODULE, ?LINE, unsupported_type, ~s})~n", 
	       [Var]),
	    ?W(" end, ~n", []),
	    {binary, "new" ++ Var};

	Val when list(Val) ->
	    ?W(" New~s = if~n", [Var]),
	    ?W("\tis_list(~s) ; is_tuple(~s) -> ", [Var, Var]),
	    ?W("term2bin(~s, ~s, ~s);~n", [Var, uppercase(Val), Type]),
	    %% 	    ?W("\tis_binary(~s), size(~s) >= ~s * ?gl_type_size(~s) -> ~s;~n",
	    %% 		      [Var, Var, uppercase(Val), gl_type_size(Type), Var])    
 	    ?W("\tis_binary(~s) -> ~s;~n",
	       [Var, Var]),
	    ?W("\ttrue -> erlang:error({?MODULE, ?LINE, unsupported_type, ~s})~n", 
	       [Var]),
	    ?W(" end, ~n", []),
	    {binary, "new" ++ Var}
    end.

get_size(Func, {"GLubyte",pointer}, R, _First) ->
    "strlen(egl_res) " ++ get_size(Func, "void", R, "+ ");
get_size(Func, {Type,pointer}, R, "") when ?GLUTYPE(Type) ->
    "sizeof(" ++ Type ++ "*) " ++ get_size(Func, "void", R, "+ ");
get_size(Func,"void", [{_V,pointer,T}|R], First) ->
    First ++ "sizeof(" ++ T ++ "*)" ++ get_size(Func,"void", R, "+ ");
get_size(Func,"void", [{V,T}|R], First) ->
    case getdef(Func, V) of
	N when integer(N) ->
	    First ++ "sizeof(" ++ T ++ ") *" ++ integer_to_list(N) ++ 
		get_size(Func,"void", R, "+ ");
	{undefined, Val, _} when integer(Val) ->
	    First ++ "sizeof(" ++ T ++ ") *" ++ integer_to_list(Val) ++ 
		get_size(Func,"void", R, "+ ");
	Val when is_list(Val) ->
	    First ++ "sizeof(" ++ T ++ ") * (*" ++ Val ++ ")" ++ 
		get_size(Func,"void", R, "+ ");
	{Val,_} when is_list(Val) ->
	    First ++ "sizeof(" ++ T ++ ") * (*" ++ Val ++ ")" ++ 
		get_size(Func,"void", R, "+ ")
% 	{undefined, Val, _} when is_list(Val)->
% 	    First ++ "sizeof(" ++ T ++ ") * (*" ++ Val ++ ")" ++ 
% 		get_size(Func,"void", R, "+ ")
    end;
get_size(Func,Type, R, "") ->
    "sizeof(" ++ Type ++ ") " ++ get_size(Func,"void", R, "+ ");
get_size(_Func,"void", [], "+ ") ->
    [].

bump_buff(false, Type, Len, Fd) when number(Len) ->
    ?W(" bp += sizeof(~s)*(~w); ~n", [Type, Len]);
bump_buff(false, Type, Len, Fd) when is_list(Len) ->
    ?W(" bp += sizeof(~s)*(*~s); ~n", [Type, Len]);
bump_buff(true, _,_,_) ->
    ok.

write_args(FuncName, T, L, Fd) ->
    put(arg_cnt, 0),
    case has_binary(FuncName) of 
	false -> skip;
	_ when T == binerl -> ?W("[", []);
	_ -> skip
    end,
    write_args(FuncName, T, L, Fd, first, 0).
write_args(FuncName, T, [H], Fd, Prev, Align0) ->
    Args = remap_const(FuncName, H),
    {Prev1,Align} = write_arg(FuncName, T, Args, Fd, Prev, true, Align0),
    case Prev1 of
	buildbin when T == binerl -> ?W(">>", []);
	_ -> skip
    end,
    case has_binary(FuncName) of 
	false -> skip;
	_ when T == binerl -> ?W("]", []);
	_ -> skip
    end,
    Align;
write_args(_FuncName, Type, [], Fd, _First0, _Align0) ->
    if Type == binerl -> 
	    ?W("[]", []);
       true -> skip
    end;
write_args(FuncName, Type, [H|R], Fd, Prev0, Align0) ->
    Args = remap_const(FuncName, H),
    {Prev,Align} = write_arg(FuncName, Type, Args, Fd, Prev0, false, Align0),
    write_args(FuncName, Type, R, Fd, Prev, Align).

write_arg(_FuncName, Type, {T,V}, Fd, Prev0, IsLast, Align0) ->
    Cont = case Prev0 of first -> ""; _Else -> ", "  end,
    Align = write_align(Type, Align0, T, Fd),
    Prev = 
	case Type of
	    c ->
		?W("~s~s ~s", [Cont, T, V]), 
		no;
	    cdef when ((T == "GLdouble") or (T == "GLclampd")) ->
		?W(" ~s ~s;~n", [T, V]),
		no;
	    cdef ->
		?W(" ~s * ~s;~n", [T, V]),
		no;
	    binc when ((T=="GLdouble") or (T=="GLclampd")), IsLast ->
		?W(" memcpy(&~s, bp, sizeof(~s)); ~n", 
 		   [V, T]), 	       
		no;
	    binc when ((T=="GLdouble") or (T=="GLclampd")) ->
 		?W(" memcpy(&~s, bp, sizeof(~s)); bp += sizeof(~s); ~n", 
 		   [V, T, T]), 
		no;
	    binc when IsLast ->
		?W(" ~s = (~s *) bp; ~n", 
		   [V, T]), 
		no;
	    binc ->
		?W(" ~s = (~s *) bp; bp += sizeof(~s); ~n", 
		   [V, T, T]), 
		no;
	    ccalls when ((T=="GLdouble") or (T=="GLclampd")) ->
		?W("~s~s", [Cont, remCarray(V)]), 
		no;
	    ccalls ->
		?W("~s*~s", [Cont, remCarray(V)]), 
		no;
	    erl ->
		?W("~s~s", [Cont, element(1, erlVar(V))]), 
		no;
	    erlcom ->
		?W("~s~s::~s", [Cont, element(1, erlVar(V)),erltype(T)]), 
		no;
	    binerl ->
		Next = write_binarg(Prev0, T, Fd),
		binary_arg(erlVar(V), T, Fd),
		Next
	end,
    {Prev, Align+typeSz(T)};

write_arg(FuncName, Type, {T,pointer, V}, Fd, Prev0, _IsLast, Align0) ->
    Cont = case Prev0 of first -> "";  _Else -> ", "  end,
    Align = write_align(Type, Align0, T, Fd),
    Prev = 
	case Type of
	    c ->
		?W("~s~s * ~s", [Cont, T, V]);
	    cdef ->
		case getdef(FuncName, V) of
		    Val when integer(Val) ->  
			?W(" ~s ~s[~p]; ~n", 
			   [T, V, Val]);
		    _ when ?GLUTYPE(T) ->
			case T of
			    "GLUtesselator" ->
				?W(" eglu_tessobj * ~s;~n", [V]);
			    _ ->
				?W(" ~s * ~s;~n", [T,V])
			end;
		    {undefined, Val,_} ->
			?W(" ~s ~s[~p];~n", 
			   [T,V,Val]);
%		    pointer -> 
%			?W(" char ~sMemT = 0;~n", [T,V]);
%			?W(" ~s * ~s = NULL;~n", [T,V]);
		    sdlmem ->
			?W(" GLvoid * ~s = NULL;~n", [V]);
		    Variable when list(Variable) ->
			?W(" ~s * ~s = NULL;~n", [T,V]);
		    {Variable,_Max} when list(Variable) ->
			?W(" ~s * ~s = NULL;~n", [T,V]);
		    Error ->
			erlang:error({?MODULE, ?LINE, Error})
		end;
	    binc ->
		case getdef(FuncName, V) of 
		    sdlmem ->
			Cnt = get(arg_cnt),
			?W(" ~s = (~s *) egl_sd->bin[~w].base; ~n", [V,T,Cnt]),
			put(free_args, true),
			put(arg_cnt, Cnt+1);
		    _ when ?GLUTYPE(T) ->
			case T of
			    "GLUtesselator" ->
				?W(" GetNativePtr(~s, (eglu_tessobj *),bp);~n",
				   [V]);
			    _ ->
				?W(" GetNativePtr(~s, (~s *),bp);~n",
				   [V,T])
			end;
		    Variable when list(Variable) ->
			?W(" ~s = (~s*) malloc(sizeof(~s)*(*~s));~n",
				  [V,T,T,Variable]);
		    {Variable,Max} when list(Variable) ->
			?W(" ~s = (~s*) malloc(sizeof(~s)*(*~s));~n",
			   [V,T,T,Max]);

		    Var when integer(Var) ->
			skip;
		    {undefined, _Var, _} ->
			skip;
		    Error ->
			erlang:error({?MODULE, ?LINE, Error})
		end;
	    ccalls ->
		case getdef(FuncName, V) of 
		    sdlmem ->
			?W("~s~s", [Cont, V]);
		    _ when T == "GLUtesselator" ->
			?W("~s~s->tess", [Cont,V]);
		    _ ->
			?W("~s~s", [Cont,remCarray(V)])
		end;
	    erlcom -> 
		case getdef(FuncName, V) of 
		    sdlmem ->
			?W("~s~s::~s", 
			   [Cont, element(1, erlVar(V)), erltype(sdlmem)]);
		    _ when ?GLUTYPE(T) ->
			?W("~s~s::~s", [Cont,element(1, erlVar(V)), erltype(T)]);
		    _ ->
			ignore
		end;
	    erl ->
		case getdef(FuncName, V) of 
		    sdlmem ->
			?W("~s#sdlmem{bin=~s}", 
			   [Cont, element(1, erlVar(V))]);
		    _ when ?GLUTYPE(T) ->
			?W("~s~s=#~s{}", 
			   [Cont,element(1, erlVar(V)),eglutype(T)]);
		    _ ->
			ignore
		end;
	    binerl ->
		case getdef(FuncName, V) of 
		    sdlmem ->
% 			write_binarg(Prev0, T, Fd),
% 			?W("(~s#sdlmem.ptr):32/big-unsigned",
% 			   [element(1, erlVar(V))]),
% 			buildbin;
			Prev0;
		    _ when ?GLUTYPE(T) ->
			write_binarg(Prev0, T, Fd),
			?W("(~s#~s.ptr)~s", 
			   [element(1, erlVar(V)),eglutype(T), bintype(T)]),
			buildbin;
		    _ -> 
			Prev0
		end
	end,
    {Prev, Align};

write_arg(FuncName, Type, {T, const, pointer, V}, Fd, Prev0, IsLast, Align0) ->
    Cont = case Prev0 of first -> ""; _Else -> ", "  end,
    Align1 = write_align(Type, Align0, T, Fd),
    Prev = 
	case Type of
	    c ->
		?W("~s const ~s * ~s", [Cont, T, V]);
	    cdef ->
		case getdef(FuncName, V) of
		    Val when integer(Val), ((T=="GLdouble") or (T=="GLclampd")) ->  
			?W(" ~s ~s[~w];~n", [T, V, Val]);
		    Val when integer(Val) ->  
			?W(" ~s * ~s;~n", [T, V]);
		    {undefined, Val,_} when ((T=="GLdouble") or (T=="GLclampd")) ->
			if is_list(Val) -> 
				?W(" ~s *~s;int * ~sLen;~n", [T,V,V]);
			   true ->
				?W(" ~s ~s[~w];int * ~sLen;~n", [T,V,Val,V])
			end;
		    {undefined, _Val,_}  ->
			?W(" ~s * ~s;~n", [T,V]);
		    pointer when ((T=="GLdouble") or (T=="GLclampd")) -> 
			?W("{not_implemented, ~p}", [?LINE]),
			?W(" int * ~sMemT = 0;~n", [V]),
			?W(" int * ~sLen = 0;~n", [V]),
			?W(" ~s * ~s = NULL;~n", [T,V]);
		    pointer -> 
			?W(" ~s * ~s = NULL;~n", [T,V]);
		    sdlmem ->
			?W(" GLvoid * ~s = NULL;~n", [V]);
		     %% ?W(" oglmem * ~s = NULL;~n", [V]);
		    Variable when list(Variable) ->
			?W(" ~s * ~s = NULL; ~n", [T,V]);
		    {tuplelist,_N} -> 
			?W(" ~s * ~s = NULL;~n", [T,V]);
		    {index_or_list,_} ->
			?W(" ~s * ~s = NULL;~n", [T,V]);
		    Error ->
			erlang:error({?MODULE, ?LINE, Error})
		end;
	    binc ->
		case getdef(FuncName, V) of
		    Val when integer(Val),((T=="GLdouble") or (T=="GLclampd")) ->
			?W(" memcpy(~s,bp,sizeof(~s)*~w); ~n", [V,T,Val]),
			bump_buff(IsLast, T, Val, Fd);
		    Val when integer(Val) ->
			?W(" ~s = (~s *) bp;~n",[V,T]),
			bump_buff(IsLast, T, Val, Fd);
		    {undefined, Val,_} when ((T=="GLdouble") or (T=="GLclampd")) ->
			?W(" ~sLen  = (int *) bp; bp += sizeof(int); ~n",
			   [V]),
			if is_list(Val) -> 
			    ?W(" ~s = (~s*) malloc(sizeof(~s)*(*~sLen));~n",
			       [V,T,T,V]);
			   true ->
				ignore
			end,
			?W(" memcpy(~s,bp,sizeof(~s)*(*~sLen));~n"
			   " bp += sizeof(~s)*(*~sLen); ~n", 
			   [V,T,V,T,V]);
		    {undefined, _Val,_} when IsLast -> 
			?W(" bp += sizeof(int); ~n",[]),
			?W(" ~s = (~s *) bp; ~n", [V,T]);
		    {undefined, Val,_} when is_list(Val) -> 
			?W(" bp += sizeof(int); ~n", []),
			?W(" ~s = (~s *) bp; bp += sizeof(~s)*(*~s); ~n", 
			   [V,T,T,Val]);
		    pointer when ((T=="GLdouble") or (T=="GLclampd")) ->
			?W("{not_implemented, ~p}", [?LINE]);
		    pointer -> 
			Cnt = get(arg_cnt),
 			?W(" if(egl_sd->next_bin == ~p) {~n  ~s = (~s *) *(GLint *)bp;~n",
 			   [Cnt,V,T]),
			?W(" } else {~n  ~s = (~s *) egl_sd->bin[~w].base;~n };~n",
			   [V,T,Cnt]),
			?W(" bp += sizeof(GLint);~n", []),
			put(free_args, true),
			put(arg_cnt, Cnt+1);
		    {index_or_list,_} -> 
			Cnt = get(arg_cnt),
 			?W(" if(egl_sd->next_bin == ~p) {~n  ~s = (~s *) *(GLint *)bp;~n",
 			   [Cnt,V,T]),
			?W(" } else {~n  ~s = (~s *) egl_sd->bin[~w].base;~n };~n",
			   [V,T,Cnt]),
			?W(" bp += sizeof(GLint);~n", []),
			put(free_args, true),
			put(arg_cnt, Cnt+1);
		    sdlmem -> 
			Cnt = get(arg_cnt),
			?W(" ~s = (~s *) egl_sd->bin[~w].base; ~n", [V,T,Cnt]),
			put(free_args, true),
			put(arg_cnt, Cnt+1);
		    {tuplelist,_N} -> 
			Cnt = get(arg_cnt),
			?W(" ~s = (~s *) egl_sd->bin[~w].base; ~n", [V,T,Cnt]),
			put(free_args, true),
			put(arg_cnt, Cnt+1);
		    Str when list(Str), ((T=="GLdouble") or (T=="GLclampd")) ->
			?W(" ~s = (~s*) malloc(sizeof(~s)*(*~s));~n",
			   [V,T,T,Str]),
			?W(" memcpy(~s,bp,sizeof(~s)*(*~s));~n", [V,T,Str]),
			bump_buff(IsLast, T,Str,Fd);
		    Str when list(Str) ->
			TT = if T == "GLvoid" -> gettype(FuncName, V); true -> T end,
			?W(" ~s = (~s *) bp;~n", [V,T]),
			bump_buff(IsLast, TT,Str,Fd);
		    Error -> 
			erlang:error({?MODULE,?LINE, Error})
		end;
	    
	    ccalls ->
		?W("~s~s", [Cont, remCarray(V)]);
	    erlcom ->
		case is_vector(FuncName) of
		    false ->
			?W("~s~s::binary() | [~s]", [Cont, element(1, erlVar(V)),erltype(T)]);
		    {tuplelist, 1} ->
			write_tuple(erlVar(V), 1, Cont++"[","::"++erltype(T),"]",Fd);
		    {tuplelist, Int} ->
			write_tuple(erlVar(V), Int, Cont++"[{","::"++erltype(T),"}]",Fd);
		    Int ->
			write_tuple(erlVar(V), Int, Cont++"{","::"++erltype(T),"}",Fd)
		end;
	    erl ->
		case is_vector(FuncName) of
		    Int when is_integer(Int) ->
			write_tuple(erlVar(V), Int, Cont++"{", "", "}",Fd);
		    _ ->
			?W("~s~s", [Cont, element(1, erlVar(V))])
		end;
	    binerl ->
		case is_vector(FuncName) of
		    Int when integer(Int) ->
			write_binarg(Prev0, T, Fd),
			write_tuple(erlVar(V), Int, "", bintype(T), "", Fd),
			buildbin;
		    {tuplelist, _Int} ->
			exit({?MODULE,?LINE});
		    _ ->
			Prev0
		end
	end,
    Align = case erlVar(V) of
		{_, 0} -> Align1 + typeSz(T);
		{_, N} when T /= binary -> 
		    Align1 + typeSz(T)*N
	    end,
    {Prev, Align};

write_arg(_FuncName, _Type, {T,pointer,pointer,_V}, _Fd, _First, _IsLast, _Align) 
  when T == ((T=="GLdouble") or (T=="GLclampd")) ->
    erlang:error({not_implemented, ?LINE});
write_arg(_FuncName, Type, {T, pointer, pointer, V}, Fd, First, _IsLast, Align) ->
    Cont = case First of first -> ""; {first,_} -> ""; _Else -> ", "  end,
    case Type of
	c ->
	    ?W("~s ~s* *~s", [Cont, T, V]);
	cdef ->
	    ?W(" ~s *~s = NULL;~n", [T,V]);
	binc ->
	    ignore;
	ccalls ->
	    ?W("~s&~s", [Cont, remCarray(V)]);
	erlcom ->
	    io:format("~p~p error", [?MODULE,?LINE]),
	    exit(error)
    end,
    {no, Align};

write_arg(FuncName,Type,{T,const,pointer,pointer,V},Fd,First, _IsLast,Align) 
  when FuncName == "glShaderSource" ->
    Cont = case First of first -> ""; {first,_} -> ""; _Else -> ", "  end,
    case Type of
	c ->
	    ?W("~s const ~s* *~s", [Cont, T, V]);
	cdef ->
	    ?W(" const ~s* *~s;~n", [T,V]),
	    ?W(" int index;~n", []);
	binc -> 
	    case getdef(FuncName, V) of 
		Variable when list(Variable) ->
		    ?W(" ~s = (const ~s* *) malloc(sizeof(~s*)*(*~s));~n",
		       [V,T,T,Variable]),
		    Cnt = get(arg_cnt),
		    ?W(" for(index=0; index < *~s; index++) ~n"
		       "    ~s[index] = (~s *) egl_sd->bin[index+~w].base;~n",
		       [Variable, V, T, Cnt]),
		    put(free_args, true),
		    put(arg_cnt, unusable)
	    end;
	ccalls ->
	    ?W("~s~s", [Cont, remCarray(V)]);
	erl ->
	    ?W("~s~s", [Cont, element(1, erlVar(V))]);
	erlcom ->
	    ?W("~s~s::[binary()]", [Cont, element(1, erlVar(V))]);
	_ ->
	    ignore
    end,
    {no, Align};


write_arg(_FuncName, _Type, {_T, callback, _V}, Fd, _First, _, Align) ->
    io:format("Error Dont know how to generate callbacks~n", []),
    io:format(Fd,"Error Dont know how to generate callbacks~n", []),
    {no, Align}.

write_tuple({V,0}, N, Start, Type, End, Fd) ->
    ?W("~s", [Start]),
    write_tuple2(V, 1, N, Type, End, Fd).
write_tuple2(V,N,N, Type, End,Fd) ->
    ?W("~s~w~s~s", [V, N, Type,End]);
write_tuple2(V,N,Max, Type, End,Fd) ->
    ?W("~s~w~s,", [V, N, Type]),
    write_tuple2(V,N+1,Max, Type, End,Fd).

write_binarg(Prev0, binary, Fd) ->    
    if Prev0 == first ->
	    ?W(" ",[]), 
	    binary;
       Prev0 == binary ->
	    ?W(", ",[]),
	    binary;
       true -> 
	    ?W(">>,",[]),
	    binary
    end;
write_binarg(Prev0, _, Fd) ->
    if 
	Prev0 == first ->
	    ?W("<<", []),
	    buildbin;
	Prev0 == binary ->
	    ?W(", <<", []),			
	    buildbin;
	true ->
	    ?W(", ", []),
	    buildbin		
    end.

write_undef_rets(_FuncName, [], _Fd) ->    ok;
write_undef_rets(FuncName, [{V,_T}|R], Fd) ->
    case getdef(FuncName,V) of
	{undefined, Max, Enum} ->
	    ?W( " ~sLen = ~sLen(~s),~n", 
		[uppercase(V),FuncName,uppercase(Enum)]),
	    ?W( " ~sBump = ~w - ~sLen,~n", 
		[uppercase(V),Max,uppercase(V)]);
	_ ->
	    skip
    end,
    write_undef_rets(FuncName,R,Fd);
write_undef_rets(FuncName, [_|R],Fd) ->
    write_undef_rets(FuncName,R,Fd).

write_returns(_FuncName, erlcom, Type, Rets, Fd) ->
    case {Type, Rets} of
	{"void", []} -> ?W("ok", []);
	{"void", [{D,_}]} ->
	    ?W("~s::[~s]", [uppercase(D),erltype(Type)]);
	{"void", [{D,pointer,_}]} ->
	    ?W("~s::sdlmem()", [uppercase(D)]);
	{"void", [{V1, T1}|R]} ->
	    ?W("{[~s::~s]", [uppercase(V1),erltype(T1)]),
	    [?W(", [~s::~s]", [uppercase(V),erltype(T2)]) || {V, T2} <- R],
	    ?W("}", []);	    
	{{RType, pointer}, []} when ?GLUTYPE(RType) ->
	    ?W("[~s]", [uppercase(eglutype(RType))]);
	{{RT,pointer}, []} -> 
	    ?W("[~s]", [erltype(RT)]);
	{Type, []} -> ?W("~s", [erltype(Type)]);	
	{Type, _} -> 
	    ?W("{~s", [erltype(Type)]),
	    [?W(", [~s::~s]", [uppercase(V),erltype(T)]) || {V,T} <- Rets],
	    ?W("}", [])
    end;
write_returns(FuncName, erl, Type, Rets, Fd) ->
    case {Type, Rets} of
	{"void", []} -> 
	    erlang:error({?MODULE, ?LINE, {erl, Type, Rets}});
	{{RType, pointer}, []} when ?GLUTYPE(RType) ->
	    ?W("\t<<", []),
	    binary_arg({"Ret", 0}, RType, Fd),
	    ?W(">> -> ~n  #~s{ptr=Ret};~n",
		      [eglutype(RType)]);
	{{RType,pointer}, []} ->
	    ?W("\tRet -> bin2list(undefined,~s,Ret);~n",
	       [type_to_enum(RType)]);
	{Type, []} ->
	    ?W("\t<<", []),
	    binary_arg({"Ret", 0}, Type, Fd),
	    ?W(">> -> ~n   ~s;~n",[boolret("Ret", Type)]);
	{"void", _} ->
	    write_rets(FuncName, Rets, Rets,Fd, 0, "void");
	{Type, _} -> 		 
	    ?W("\t<<", []),   
	    binary_arg({"Ret", 0}, Type, Fd),
	    write_rets(FuncName,Rets,Rets,Fd, 1, Type)
    end.

write_rets(Func, [Ret], _, Fd, 0,_T) ->
    ?W("\t<<",[]),
    write_ret(Func, Ret, Fd),
    ?W(">> -> ~n\t ",[]),
    write_ret2(Func, Ret, "", Fd),
    ?W(";~n",[]);
write_rets(Func, [Ret|R], Rets, Fd, N,T) ->
    if N == 0 -> ?W("\t<<",[]); true -> ?W(", ",[]) end,    
    write_ret(Func, Ret, Fd),
    write_rets(Func, R, Rets, Fd, N+1,T);
write_rets(Func, [], Rets, Fd, N,Type) ->
    if N == 1 ->
	    ?W(">> -> ~n\t ",[]);
       Type == "void" ->
	    ?W(">> -> ~n\t {",[]);
       true ->
	    ?W(">> -> ~n\t {~s, ",[boolret("Ret", Type)])
    end,
    write_ret2(Func, hd(Rets), "", Fd),
    [write_ret2(Func, Ret, ", ", Fd) || Ret <- tl(Rets)],
    if N == 1 ->
	    ?W(";~n ",[]);
       true ->
	    ?W("};~n",[])
    end.


write_ret(FuncName, {D,T}, Fd) ->
    UD = uppercase(D),
    case getdef(FuncName, D) of
	1 ->
	    ?W("~s~s", [UD, bintype(T)]);
	{undefined, _Max, _} ->
	    ?W("~s:~sLen/binary-unit:~s_SIZE,_:~sBump/binary-unit:~s_SIZE",
	       [UD, UD, type_to_enum(T), UD, type_to_enum(T)]);
	{Length,_Max} when list(Length) ->
	    ?W("~s:~s/binary-unit:~s_SIZE", 
	       [UD, uppercase(Length), type_to_enum(T)]);
	Length when list(Length) ->
	    ?W("~s:~s/binary-unit:~s_SIZE", 
	       [UD, uppercase(Length), type_to_enum(T)]);
	Length when integer(Length) ->
	    ?W("~s:~w/binary-unit:~s_SIZE", 
	       [UD, Length, type_to_enum(T)])
    end;
write_ret(_FuncName, {D,pointer,_T}, Fd) ->
    UD = uppercase(D),
    ?W("~s:32/big-unsigned", [UD]).

write_ret2(FuncName, {D,T}, Cont, Fd) ->
    UD = uppercase(D),
    case getdef(FuncName, D) of
	1 ->
	    ?W("~s~s", [Cont,UD]);
	{undefined, _Max, _} ->
	    ?W("~sbin2list(~sLen, ~s, ~s)", 
	       [Cont,UD,type_to_enum(T),UD]);
	Length when list(Length) -> 
	    ?W("~sbin2list(~s, ~s, ~s)", 
	       [Cont,uppercase(Length),type_to_enum(T),UD]);
	{Length,_Max} when list(Length) -> 
	    ?W("~sbin2list(~s, ~s, ~s)", 
	       [Cont,uppercase(Length),type_to_enum(T),UD]);
	Length when integer(Length) -> 
	    ?W("~sbin2list(~w, ~s, ~s)", 
	       [Cont,Length,type_to_enum(T),UD])
    end;
write_ret2(_FuncName, {_D,pointer,_T}, Cont, Fd) ->
    %%    ?W("~s#sdlmem{ptr=~s}", [Cont,UD]).
    ?W("~serlang:error({nyi, ?MODULE,?LINE})", [Cont]).

find_returns(Args,Funcs) ->
    find_returns(Args,Funcs,[],[]).
find_returns([{T, pointer, pointer, V}|R], Func, Rets, Args) ->
    find_returns(R, Func, [{V,pointer,T}|Rets],Args);
find_returns([W={T, pointer, V}|R], Func, Rets, Args) ->
    case getdef(Func, V) of
	sdlmem -> 
	    find_returns(R, Func, Rets, [W|Args]);
	_ when ?GLUTYPE(T) -> 
	    find_returns(R, Func, Rets, [W|Args]);
	_ ->
	    case remap_const(Func, W) of
		W -> 
		    find_returns(R, Func, [{V,T}|Rets],Args);
		New ->
		    find_returns(R, Func, Rets,[New|Args])
	    end	    
    end;
find_returns([H|R], Func,Rets,Args) ->
    find_returns(R, Func, Rets, [H|Args]);
find_returns([], _Func,Rets,Args) ->
    {lists:reverse(Rets),lists:reverse(Args)}.

boolret(Var, "GLboolean") ->
    Var ++ " /= ?GL_FALSE";
boolret(Var, _) ->
    Var.

write_align(binerl, A, T, Fd) when (A rem 4) /= 0 ->
    case typeSz(T) of 
	_ when T == binary ->
	    PadBytes =  4-(A rem 4),
	    ?W(", 0:~p", [PadBytes*8]), 
	    0;
	1 -> A;
	2 when (A rem 2) == 0 ->
	    A;
	2 -> %% Pad one byte
	    ?W(", 0:8", []),
	    A + 1;
	_ ->
	    PadBytes =  4 - (A rem 4),
	    ?W(", 0:~p", [PadBytes*8]), 
	    0
    end;

write_align(binc, A, T, Fd) when (A rem 4) /=0 ->
    case typeSz(T) of 
	_ when T == binary ->
	    PadBytes = 4 - (A rem 4),
	    ?W(" bp += ~p;~n", [PadBytes]),
	    0;
	1 -> A;
	2 when (A rem 2) == 0 ->
	    A;
	2 -> %% Pad one byte
	    ?W(" bp += 1;~n", []),
	    A + 1;
	_ -> %% Expecting that 64 bits can be word aligned
	    PadBytes = 4 - (A rem 4),
	    ?W(" bp += ~p;~n", [PadBytes]),
	    0
    end;

write_align(_,A,_,_) ->
    A.
binary_arg({Var, 0}, binary, Fd) ->
    ?W("~s", [Var]);
binary_arg({Var, 0}, Type, Fd) ->
    ?W("~s~s", [Var, bintype(Type)]);
binary_arg({Var, N}, Type, Fd) ->
    io:format("**Shouldn't be here in binary_arg ~p~n", [{{Var, N}, Type}]),
    ?W(">>,term2bin(~s, ~s, ?~s)),<<", [Var, N, Type]).

% bintype(binary) ->    %% Probably a pointer
%     "/binary";
bintype("GLvoid") ->  %% Probably a pointer
    ":32/?UN";
bintype("GLenum") ->
    ":32/?UN";
bintype("GLboolean") ->
    ":8/unsigned";
bintype("GLbyte") ->
    ":8/signed";
bintype("GLshort") ->
    ":16/?SN";
bintype("GLint") ->
    ":32/?SN";
bintype("GLubyte") ->
    ":8/unsigned";
bintype("GLushort") ->
    ":16/?UN";
bintype("GLuint") ->
    ":32/?UN";
bintype("GLsizei") ->
    ":32/?SN";
bintype("GLfloat") ->
    ":32/?FN";
bintype("GLclampf") ->
    ":32/?FN";
bintype("GLdouble") ->
    ":64/?FN";
bintype("GLclampd") ->
    ":64/?FN";
bintype("GLbitfield") ->
    ":32/?UN";
bintype("GLchar" ++ _) ->
    ":8/?UN";
bintype("GLhandle" ++ _) ->
    ":32/?UN";
bintype("GLsizeiptr") ->
    ":32/?UN";
bintype("GLintptr") ->
    ":32/?UN";
%% bintype("?_PTR") ->
%%     ":?_PTR";


%% GLU types
bintype("GLUnurbs") ->  %%  a pointer
    ":32/?UN";
bintype("GLUquadric") ->  %%  a pointer
    ":32/?UN";
bintype("GLUtesselator") ->  %%  a pointer
    ":32/?UN";
bintype(What) ->
    "GeneraterUnknownType:" ++ What.

eglutype("GLUtesselator") ->
    "tessPtr";
eglutype("GLUquadric") ->
    "quadricPtr";
eglutype("GLUnurbs") ->
    "nurbsPtr".

type_to_enum("GLboolean") ->    "?GL_BYTE";
type_to_enum("GLbyte") ->       "?GL_BYTE";
type_to_enum("GLshort") ->      "?GL_SHORT";
type_to_enum("GLint") ->        "?GL_INT";
type_to_enum("GLubyte") ->      "?GL_UNSIGNED_BYTE";
type_to_enum("GLcharARB") ->    "?GL_UNSIGNED_BYTE";
type_to_enum("GLchar") ->       "?GL_UNSIGNED_BYTE";
type_to_enum("GLushort") ->     "?GL_UNSIGNED_SHORT";
type_to_enum("GLuint") ->       "?GL_UNSIGNED_INT";
type_to_enum("GLsizei") ->      "?GL_UNSIGNED_INT";
type_to_enum("GLfloat") ->      "?GL_FLOAT";
type_to_enum("GLclampf") ->     "?GL_FLOAT";
type_to_enum("GLdouble") ->     "?GL_DOUBLE";
type_to_enum("GLclampd") ->     "?GL_DOUBLE";
type_to_enum("GLbitfield") ->   "?GL_INT";
type_to_enum("GLenum") ->       "?GL_INT";  %% hmm
type_to_enum("GLhandleARB") ->  "?GL_UNSIGNED_INT";
type_to_enum("GLsizeiptr") ->   "?GL_UNSIGNED_INT";  %% hmm
type_to_enum("GLintptr") ->     "?GL_UNSIGNED_INT";  %% hmm
    
type_to_enum(What) ->
    "GeneraterUnknownType: " ++ What.

erltype("GLboolean") ->    "bool()";
erltype("GLbyte") ->       "integer()";
erltype("GLshort") ->      "integer()";
erltype("GLint") ->        "integer()";
erltype("GLubyte") ->      "integer()";
erltype("GLcharARB") ->    "integer()";
erltype("GLchar") ->       "integer()";
erltype("GLushort") ->     "integer()";
erltype("GLuint") ->       "integer()";
erltype("GLsizei") ->      "integer()";
erltype("GLfloat") ->      "float()";
erltype("GLclampf") ->     "float()";
erltype("GLdouble") ->     "float()";
erltype("GLclampd") ->     "float()";
erltype("GLbitfield") ->   "integer()";
erltype("GLenum") ->       "integer()";  %% hmm
erltype("GLhandleARB") ->  "integer()";
erltype("GLsizeiptr") ->   "integer()";  %% hmm
erltype("GLintptr") ->     "integer()";  %% hmm
erltype("GLvoid") ->       "number()";      %% hmm
erltype(sdlmem) ->         "sdlmem()";  %% hmm
%%erltype({binORlist,T}) ->      "::binary()";  %% hmm
erltype(T) when ?GLUTYPE(T) ->  "::"++ eglutype(T) ++"()";  %% hmm

erltype(_T) -> "term()".
     

typeSz(T) ->
    case type_to_enum(T) of
	"GeneraterUnknownType" ++ _ ->
	    byteSz(T);
	Else ->
	    byteSz(Else)
    end.

byteSz(TYPE) -> 
    case TYPE of 
	"boolean" ->            ?GL_BYTE_SIZE div 8;
	"GLboolean" ->          ?GL_BYTE_SIZE div 8;
	"GLenum" ->             ?GL_INT_SIZE div 8;
	"?GL_BYTE" ->           ?GL_BYTE_SIZE div 8;
	"?GL_UNSIGNED_BYTE" ->  ?GL_UNSIGNED_BYTE_SIZE div 8;
	"?GL_SHORT" ->          ?GL_SHORT_SIZE div 8;
	"?GL_UNSIGNED_SHORT" -> ?GL_UNSIGNED_SHORT_SIZE div 8;
	"?GL_INT" ->            ?GL_INT_SIZE div 8;
	"?GL_UNSIGNED_INT" ->   ?GL_UNSIGNED_INT_SIZE div 8;
	"?GL_FLOAT" ->          ?GL_FLOAT_SIZE div 8;
	"?GL_DOUBLE" ->         ?GL_DOUBLE_SIZE div 8;
	"GLvoid" ->             ?GL_INT_SIZE div 8; % Pointer
	"void"   ->             ?GL_INT_SIZE div 8; % Pointer
	"GLintptr" ++ _ ->      ?GL_UNSIGNED_INT_SIZE div 8; % long ?
	"GLsizeiptr" ++ _ ->    ?GL_UNSIGNED_INT_SIZE div 8; % long ?
	"GLchar" ++ _ ->        ?GL_BYTE_SIZE div 8;
	"GLhandle" ++ _ ->      ?GL_UNSIGNED_INT_SIZE div 8;
	"?_PTR" ++ _ ->         8;  %% Always 8
	binary ->  4  %% Ignore binariers are always 32 aligned.
    end.            

remCarray(Arg) ->
    case string:tokens(Arg, "[]") of
	[Arg] ->
	    [Arg];
	[Arg1, _ArraySize] ->
	    [Arg1]
    end.
    
erlVar(Arg) ->
    case string:tokens(Arg, "[]") of
	[Arg] ->
	    {uppercase(Arg), 0};
	[Arg1, ArraySize] ->
	    {uppercase(Arg1), ArraySize}
    end.

lowercase([F|R]) ->
    [F+($a-$A)|R].

uppercase([F|R]) when F >= $a->
    [F+($A-$a)|R];
uppercase(Str) when list(Str) ->
    Str.

uppercase_all([F|R]) when F >= $a->
    [F+($A-$a)|uppercase_all(R)];
uppercase_all([A|R]) ->
    [A|uppercase_all(R)];
uppercase_all([]) ->
    [].

format_func2erl([$g,$l, $x, U|Rest]) ->
    lowercase([U|Rest]);
format_func2erl([$g,$l, $u, U|Rest]) ->
    lowercase([U|Rest]);
format_func2erl("glBegin") ->
    "\'begin\'";
format_func2erl("glEnd") ->
    "\'end\'";
format_func2erl([$g,$l, U|Rest]) ->
    lowercase([U|Rest]).

format_func2c([$g,$l, $x, U|Rest]) ->
    lowercase([U|Rest]);
format_func2c([$g,$l, $u, U|Rest]) ->
    "eglu_" ++ lowercase([U|Rest]);
format_func2c([$g,$l, U|Rest]) ->
    "egl_" ++ lowercase([U|Rest]).


% tokens(S) ->
%     tokens1(S, " \r\n", []).
tokens(S, Toks) ->
    tokens1(S, Toks, []).

tokens1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
        true -> tokens1(S, Seps, [C|Toks]);
        false -> tokens2(S, Seps, Toks, [C])
    end;
tokens1([], _Seps, Toks) ->
    replace_and_remove(Toks, []).

tokens2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
        true -> tokens1(S, Seps, [C, lists:reverse(Cs) |Toks]);
        false -> tokens2(S, Seps, Toks, [C|Cs])
    end;
tokens2([], _Seps, Toks, Cs) ->
    replace_and_remove([lists:reverse(Cs)|Toks], []).

replace_and_remove([E|R], Acc) when list(E) -> %% Keep everything that is a word
    replace_and_remove(R, [E|Acc]);
replace_and_remove([$\n | R], Acc) ->   %% It is line oriented so keep eol
    replace_and_remove(R, [eol|Acc]);
replace_and_remove([$( | R], Acc) ->
    replace_and_remove(R, [lpar|Acc]);
replace_and_remove([$) | R], Acc) ->
    replace_and_remove(R, [rpar|Acc]);
replace_and_remove([$* | R], Acc) ->
    replace_and_remove(R, [pointer|Acc]);
replace_and_remove([$, | R], Acc) ->
    replace_and_remove(R, [separtor|Acc]);

replace_and_remove([_E|R], Acc) ->       %% Ignore everthing else
    replace_and_remove(R, Acc);
replace_and_remove([], Acc) ->
    Acc.

%% The extensions we want to have..
skip_extensions("GL_ARB_vertex_blend",_R) ->    false;
skip_extensions("GL_ARB_imaging",_R) ->         false;
skip_extensions("GL_ARB_matrix_palette",_R) ->  false;
skip_extensions("GL_ARB_shadow_ambient",_R) ->  false;
skip_extensions("GL_ARB_vertex_program",_R) ->  false;
skip_extensions("GL_ARB_fragment_program",_R) ->  false;
skip_extensions("GL_NV_depth_clamp",_R) ->  false;
%% skip_extensions("GL_ARB_vertex_shader",_R) ->  false;
%% skip_extensions("GL_ARB_fragment_shader", _R) ->  false;
%% skip_extensions("GL_ARB_shading_language_100",_R) ->  false;
%% skip_extensions("GL_ARB_texture_non_power_of_two",_R) ->  false;
%% skip_extensions("GL_ARB_point_sprite",_R) ->  false;
skip_extensions("GL_ARB_shader_objects",_R) ->  false;
%% skip_extensions("GL_ARB_draw_buffers",_R) ->  false;
skip_extensions("GL_ABGR_EXT", _R) -> false;
skip_extensions("GL_ATI_separate_stencil",_R) -> false;    
%% skip_extensions("",_R) -> false;
skip_extensions("GL_NV_float_buffer",_R) -> false;
skip_extensions("GL_ATI_texture_float",_R) -> false;
skip_extensions("GL_EXT_texture_mirror_clamp",_R) -> false;
skip_extensions("GL_EXT_framebuffer_object",_) -> false;

%% skip the rest.
skip_extensions("GL_ARB_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_EXT_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_SGI" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_FfdMaskSGIX" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_NV_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_HP_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_IBM_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_WIN_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_PGI_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_INTEL_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_REND_screen_coordinates" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_APPLE_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_SUN_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_GREMEDY_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_SUNX_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_INGR_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_MESA_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_3DFX_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_ATI_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions("GL_OML_" ++ _, R) ->
    skip_to_endif(R,0);
skip_extensions(_What, _R) ->
    false.

skip_to_endif(["#ifdef", _What, eol | R],N) ->
    skip_to_endif(R,N+1);
skip_to_endif(["#endif", eol |R], N) ->
    if N == 0 -> R;
       true -> skip_to_endif(R, N-1)
    end;
skip_to_endif(["#endif", "/", pointer, _, pointer, "/", eol |R], N) ->
    if N == 0 -> R;
       true -> skip_to_endif(R, N-1)
    end;
skip_to_endif(R0, N) ->
    [eol|R] = lists:dropwhile(fun(eol) -> false; (_) ->true end, R0),
    skip_to_endif(R, N).

prototypes(Func) ->
    "PFN" ++ uppercase_all(Func) ++ "PROC".

store_prototypes([Pre, _Type|R0]) 
  when Pre == "WINGDIAPI"; Pre == "GLAPI"; Pre == "extern" ->
    {Func,R1} = 
	case R0 of
	    ["APIENTRY", F|Rest] -> {F,Rest};
	    [pointer, "APIENTRY", F|Rest] -> {F,Rest}
	end,
    
    Tree = case get(glext_proto) of
	       undefined -> 
		   gb_trees:empty();
	       Tx ->
		   Tx
	   end,
    NT = gb_trees:insert(prototypes(Func), Func, Tree),
    put(glext_proto, NT),
    [eol|R] = lists:dropwhile(fun(eol) -> false; (_) ->true end, R1),
    store_prototypes(R);
store_prototypes(["#endif"| R0]) ->
    [eol|R] = lists:dropwhile(fun(eol) -> false; (_) ->true end, R0),
    R.

is_extension(Func) ->
    case get(glext_proto) of
	undefined -> false;
	T -> gb_trees:is_defined(prototypes(Func), T)
    end.

skip(Func) ->
    case get(Func) of
	{skip, _Else} ->
	    true;
	_Else ->
	    has_vector(lists:reverse(Func))
    end.

arb(Func) ->
    case get(Func) of 
	{{use,Other}, _} -> 
	    {{test,format_func2erl(Other)}, Func};
	_What ->
	    case lists:reverse(Func) of
		"BRA_" ++ Define ->
		    {true,lists:reverse(Define)};
		"BRA" ++ Rfunc ->
		    {true,lists:reverse(Rfunc)};
		_ -> {false,Func}
	    end
    end.

call_vector(FuncName) ->
    call_vector(lists:reverse(FuncName), FuncName).
call_vector("BRA" ++ List, _Func) ->
    lists:reverse(List) ++ "vARB";
call_vector(_, Func) ->
    Func ++ "v".

has_vector("v" ++ _) ->
    false;
has_vector([_,_|"paMlg"]) ->  %% glMap1d
    false;
has_vector([_,_|"dirGpaMlg"]) -> %% glMapGrid1d
    false;
has_vector([_,_|"mrofinUlg"]) -> %% glUniform
    false;
has_vector([$b, N|_]) when N > 47, N < 58 ->
    call_vector;
has_vector([$i, N|_]) when N > 47, N < 58 ->
    call_vector;
has_vector([$s, N|_]) when N > 47, N < 58 ->
    call_vector;
has_vector([$b,$u,N|_]) when N > 47, N < 58 ->
    call_vector;
has_vector([$i,$u,N|_]) when N > 47, N < 58 ->
    call_vector;
has_vector([$s,$u,N|_]) when N > 47, N < 58 ->
    call_vector;
has_vector([$d, N|_]) when N > 47, N < 58 ->
    call_vector;
has_vector([$f, N|_]) when N > 47, N < 58 ->
    call_vector;
has_vector("BRA" ++ Rest) -> %% ARB
    has_vector(Rest);
has_vector(_) ->
    false.

strip_types(Func) ->
    RFunc = lists:reverse(Func),
    case (has_vector(RFunc) /= false) orelse is_vector(Func) /= false of
	false -> Func;
	true ->  strip_func(RFunc) 
    end.

strip_func("BRA" ++ Name) ->    strip_func(Name) ++ "ARB";
strip_func("TXE" ++ Name) ->    strip_func(Name) ++ "EXT";
strip_func("v" ++ Name) ->      strip_func(Name);
strip_func("f" ++ Name) ->      strip_func(Name);
strip_func("d" ++ Name) ->      strip_func(Name);
strip_func("s" ++ Name) ->      strip_func(Name);
strip_func("i" ++ Name) ->      strip_func(Name);
strip_func("u" ++ Name) ->      strip_func(Name);
strip_func("b" ++ Name) ->      strip_func(Name);
strip_func([N|Name]) when N > 47, N < 58 ->
    lists:reverse(Name);
strip_func(Name) -> lists:reverse(Name).
	    
is_vector(Func) ->
    case get({is_vector, Func}) of
	undefined ->
	    Res0 = is_vector2(lists:reverse(Func)),
	    Res = case Func of 
		      "glUniform" ++ _ when Res0 /= false -> 
			  {tuplelist, Res0};
		      _ -> Res0
		  end,
	    put({is_vector, Func}, Res),
	    Res;
	Else ->
	    Else
    end.

is_vector2([$v,$f,N,$x,$i,$r,$t,$a|_]) when N > 47, N < 58 ->
    I=N-$0, 
    I*I;
is_vector2([$v,$b, N|_]) when N > 47, N < 58 ->
    N-$0;
is_vector2([$v,$i, N|_]) when N > 47, N < 58 ->
    N-$0;
is_vector2([$v,$s, N|_]) when N > 47, N < 58  ->
    N-$0;
is_vector2([$v,$b,$u,N|_]) when N > 47, N < 58 ->
    N-$0;
is_vector2([$v,$i,$u,N|_]) when N > 47, N < 58 ->
    N-$0;
is_vector2([$v,$s,$u,N|_]) when N > 47, N < 58 ->
    N-$0;
is_vector2([$v,$d, N|_]) when N > 47, N < 58 ->
    N-$0;
is_vector2([$v,$f, N|_]) when N > 47, N < 58 ->
    N-$0;
is_vector2(_) ->
    false.

has_binary(Func) ->
    case get({binary_arg, Func}) of
 	undefined ->
 	    false;
 	E -> E
    end.

remap_const(FuncName, Arg = {T, pointer, V}) ->
    case get(FuncName) of
	undefined ->
	    Arg;
	{{use, TestFunc},_} -> 
	    remap_const(TestFunc,Arg);
	{List, _Spec} ->
	    case lists:keysearch(V, 1, List) of
		{value, {V, {const,_Value}}} ->
		    {T, const, pointer, V};
		{value, {V, {const,_Value}, _Type}} ->
		    {T, const, pointer, V};
		_ ->
		    Arg
	    end
    end;
remap_const(_FuncName, What) ->
    What.

fixArgsOrder(Func, Args) ->
    case get(Func) of
	undefined -> 
	    Args;
	{{use, TestFunc},_} -> 
	    fixArgsOrder(TestFunc,Args);
	{_, []} ->
	    Args;
	{_, Order} ->
	    fixArgsOrder(Order, Args, [])
    end.
fixArgsOrder([F|R], [A={_,F}|As], Ordered) ->
    fixArgsOrder(R,As,[A|Ordered]);
fixArgsOrder([F|R], [A={_,_,F}|As], Ordered) ->
    fixArgsOrder(R,As,[A|Ordered]);
fixArgsOrder([F|R], [A={_,_,_,F}|As], Ordered) ->
    fixArgsOrder(R,As,[A|Ordered]);
fixArgsOrder([F|R], [A={_,_,_,_,F}|As], Ordered) ->
    fixArgsOrder(R,As,[A|Ordered]);
fixArgsOrder(Next, [A|As], Ordered) ->
    fixArgsOrder(Next,As++[A],Ordered);
fixArgsOrder([], [], Ordered) ->
    lists:reverse(Ordered).

getdef(_Func, Var) when Var == "tess"; Var == "nurb"; Var == "quad" ->	
    Var;
getdef(Func, Var) ->			   
    case get(Func) of
	undefined ->
	    case is_vector(Func) of
		false ->
		    io:format("Undefined length {\"~s\", {[{\"~s\", XX}], []}},~n", 
			      [Func, Var]),
		    Var;
		Num ->
		    Num
	    end;
	{{use, TestFunc},_} -> 
	    getdef(TestFunc,Var);
	{List, Spec} ->
	    case lists:keysearch(Var, 1, List) of
		{value, {Var, {const,Value}}} ->
		    Value;
		{value, {Var, {const,Value}, _Type}} ->
		    Value;
		{value, {Var, Value}} ->
		    Value;
		{value, {Var, Value, _Type}} ->
		    Value;
		_Else ->
		    NewList = List ++ [{Var, "XXX"}],
		    io:format("Undefined length ~p ~n",
			      [{Func, Var, {NewList, Spec}}]),
		    Var
	    end
    end.


gettype(_Func, Var) when Var == "tess"; Var == "nurb"; Var == "quad" ->
    Var;
gettype(Func, Var) ->
    case get(Func) of
	undefined ->
	    io:format("Undefined type {\"~s\", {[{\"~s\", XX}], []}},~n", 
		      [Func, Var]),
	    Var;
	{{use, TestFunc},_} -> 
	    gettype(TestFunc,Var);
	{List, Spec} ->
	    case lists:keysearch(Var, 1, List) of
		{value, {Var, _Value}} ->
		    io:format("Undefined type ~p ~n", 
			      [{Func, Var, {List, Spec}}]),
		    undefined;
		{value, {Var, _Value, Type}} ->
		    Type;
		_Else ->
		    io:format("Undefined type ~p ~n", 
			      [{Func, Var, {List, Spec}}]),
		    undefined
	    end
    end.


is_matrixOp([]) -> false;
is_matrixOp("Matrix" ++ _) -> true;
is_matrixOp([_A|R]) -> is_matrixOp(R).


     
