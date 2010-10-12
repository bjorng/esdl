%%  Copyright (c) 2001 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%% 
%%     $Id$
%%
%%%----------------------------------------------------------------------
%%% File    : sdl_util.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Purpose : 
%%% Created : 13 Sep 2000 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(sdl_util).

-export([term2bin/3,matrix2bin/2,bin2list/3,
	 tuplelist2bin/3, tuplelist2bin2/2, tuplelist2bin3/2, tuplelist2bin4/2,
	 alloc/2,getBin/1,
	 read/2,write/2,write/3,readBin/2,
	 copySdlImage2GLArray/3,
	 debug/1,wb/1,whex/1]).

%% Obsolete - use alloc instead.
-export([malloc/2,free/1]).

-include("sdl_video.hrl").
-include("sdl_util.hrl").
-include("sdl.hrl").
-include("esdl.hrl").

-import(sdl, [call/2,cast/2]).
-import(lists, [reverse/1]).

-define(SDL_UTIL_copySdlImage2GLArray, (?SDL_UTIL_HRL+1)).
-define(SDL_UTIL_debug, (?SDL_UTIL_HRL+2)).

-define(MYGL_malloc, (?SDL_UTIL_HRL+3)).
-define(MYGL_write,  (?SDL_UTIL_HRL+4)).

%% Func:    alloc 
%% Args:    Size and type (?SDL_UNSIGNED_INT, ...)
%% Returns: A reference to the memory area
%% Desc:    Allocates Size unit memory of Type,
%%          where Type is one of the opengl types described in gl.hrl.
%%	    The memory will be garbage-collected away as any other
%%	    Erlang term; thus there is no need to worry about freeing
%%	    the memory.
alloc(Size0, Type) ->
    %% Make sure that the size is at least 65 bytes
    %% to guarantee that the binary will be allocated
    %% outside of the heap.
    Size = case mem_size(Type, Size0) of
	       Size1 when Size1 < 65 -> 65;
	       Size1 -> Size1
	   end,

    Bin = <<0:Size/unit:8>>,
    #sdlmem{type=Type,bin=Bin,size=Size0}.

%% Func:    getBin
%% Args:    References to a memory area allocated by alloc/2.
%% Returns: The memory allocated as a binary.
%% Desc:    Returns all of an allocated memory area as a binary.
%%          Note that if you do any write operation to the memory,
%%          the contents of the returned binary will change.
%%	    CAVEAT PROGRAMMOR.
getBin(#sdlmem{bin=Bin}) -> Bin.

%% Func:    malloc 
%% Args:    Size and type (?SDL_UNSIGNED_INT, ...)
%% Returns: A reference to the memory area
%% Desc:    Obsolete name. Use alloc/2 instead.
malloc(Size, Type) ->
    alloc(Size, Type).

%% Func:    free
%% Args:    Ref to memory
%% Desc:    Obsolete function. Does nothing as memory blocks
%%	    allocated by alloc/2 are garbage-collected.
free(_Ref) ->
    ok.

%% Func:    read/readBin
%% Args:    MemoryRef, Size
%% Returns: A list/Binary of Size elements of type Type
%% Desc:    Read allocated memory 
read(#sdlmem{type=Type,bin=Bin}, Size) ->
    bin2list(Size, Type, Bin).

readBin(#sdlmem{type=Type,bin=Bin}, Size) ->
    if
	Type =:= ?SDL_BYTE; Type =:= ?SDL_UNSIGNED_BYTE ->
	    <<Data:Size/binary,_/binary>> = Bin,
	    %% Force a copy to prevent the read binary to
	    %% change later.
	    list_to_binary([Data]);
	true ->
	    %% Non-byte type. No longer supported.
            erlang:error({non_byte_type,Type})
    end.

%% Func:    write
%% Args:    MemoryRef, List|Binary
%% Returns: ok
%% Desc:    Write to data memory 
write(#sdlmem{type=Type,size=Size,bin=Dest}, Data) ->
    do_write(Type, Size, Dest, Data).

%% Func:    write
%% Args:    MemoryRef, Offset, List|Binary
%% Returns: ok
%% Desc:    Write to memory, starting Offset unit into it.
write(#sdlmem{type=Type,size=Size0,bin=Dest0}, Offset, Data) when Offset < Size0 ->
    Size = Size0 - Offset,
    ByteOffset = mem_size(Type, Offset),
    <<_:ByteOffset/binary,Dest/binary>> = Dest0,
    do_write(Type, Size, Dest, Data).

do_write(Type, Size0, Dest, List) when is_list(List) ->
    Size = case length(List) of
	       ListLen when ListLen > Size0 -> Size0;
	       ListLen -> ListLen
	   end,
    TermBin = term2bin(List, Size, Type),
    sdl:send_bin(Dest),
    cast(?MYGL_write, TermBin);
do_write(_Type, _Size, Dest, Src) when is_binary(Src), size(Src) =< size(Dest) ->
    sdl:send_bin(Dest),
    sdl:send_bin(Src),
    cast(?MYGL_write, []).

%% Func:    term2bin
%% Args:    ListOfNumbers (or tuple), MaxNo, Type 
%% Returns: Deep list
%% Desc:    Converts the first MaxNo numbers of ListOfNumbers to a 
%%          binary of the type Type.
%%          Where Type is SDL_TYPE existing in sdl.hrl (e.g. ?SDL_DOUBLE)

term2bin(Bin, _, _) when is_binary(Bin) -> Bin;
term2bin(List, N, Type) when is_list(List) ->
    if
	length(List) =:= N ->
	    list2bin(List, Type);
	true ->
	    list2bin(lists:sublist(List, N), Type)
    end;
term2bin(Tuple, N, Type) when is_tuple(Tuple) ->
    case size(Tuple) of
	N when Type == ?SDL_BYTE; Type == ?SDL_UNSIGNED_BYTE ->
	    tuple_to_list(Tuple);
	N -> 
	    tuple2bin(Tuple, N, Type);
	_Sz ->
	    term2bin(tuple_to_list(Tuple), N, Type)
    end.

list2bin(List, ?SDL_BYTE) ->
    List;
list2bin(List,?SDL_UNSIGNED_BYTE) ->
    List;
list2bin(List, ?SDL_FLOAT) ->
    float_list2bin(List, 32, []);
list2bin(List, ?SDL_DOUBLE) ->
    float_list2bin(List, 64, []);
list2bin(List, ?SDL_2_BYTES) ->
    int_list2bin(List, 16, []);
list2bin(List, ?SDL_SHORT) ->
    int_list2bin(List, 16, []);
list2bin(List, ?SDL_UNSIGNED_SHORT) ->
    int_list2bin(List, 16, []);
list2bin(List, ?SDL_3_BYTES) ->
    int_list2bin(List, 24, []);
list2bin(List, ?SDL_4_BYTES) ->
    int_list2bin(List, 32, []);
list2bin(List, ?SDL_INT) ->
    int_list2bin(List, 32, []);
list2bin(List, ?SDL_UNSIGNED_INT) ->
    int_list2bin(List, 32, []);
list2bin(List, boolean) ->
    bool_list2bin(List, []).

float_list2bin([H|T], N, Acc) ->
    float_list2bin(T, N, [Acc|<<H:N/native-float>>]);
float_list2bin([], _, Acc) -> Acc.

int_list2bin([H|T], N, Acc) ->
    int_list2bin(T, N, [Acc|<<H:N/native>>]);
int_list2bin([], _, Acc) -> Acc.

bool_list2bin([false|T], Acc) ->
    bool_list2bin(T, [Acc,0]);
bool_list2bin([true|T], Acc) ->
    bool_list2bin(T, [Acc,1]);
bool_list2bin([H|T], Acc) ->
    bool_list2bin(T, [Acc,H,1]);
bool_list2bin([], Acc) -> Acc.

tuple2bin(Tuple, Max, ?SDL_FLOAT) ->
    float_tuple2bin(Tuple, Max, 32, []);
tuple2bin(Tuple, Max, ?SDL_DOUBLE) ->
    float_tuple2bin(Tuple, Max, 64, []);
tuple2bin(Tuple, Max, ?SDL_2_BYTES) ->
    int_tuple2bin(Tuple, Max, 16, []);
tuple2bin(Tuple, Max, ?SDL_SHORT) ->
    int_tuple2bin(Tuple, Max, 16, []);
tuple2bin(Tuple, Max, ?SDL_UNSIGNED_SHORT) ->
    int_tuple2bin(Tuple, Max, 16, []);
tuple2bin(Tuple, Max, ?SDL_3_BYTES) ->
    int_tuple2bin(Tuple, Max, 24, []);
tuple2bin(Tuple, Max, ?SDL_4_BYTES) ->
    int_tuple2bin(Tuple, Max, 32, []);
tuple2bin(Tuple, Max, ?SDL_INT) ->
    int_tuple2bin(Tuple, Max, 32, []);
tuple2bin(Tuple, Max, ?SDL_UNSIGNED_INT) ->
    int_tuple2bin(Tuple, Max, 32, []);
tuple2bin(Tuple, Max, boolean) ->
    bool_tuple2bin(Tuple, Max, []).

float_tuple2bin(_, 0, _, Acc) -> Acc;
float_tuple2bin(Tuple, I, N, Acc) ->
    float_tuple2bin(Tuple, I-1, N, [<<(element(I, Tuple)):N/native-float>>|Acc]).

int_tuple2bin(_, 0, _, Acc) -> Acc;
int_tuple2bin(Tuple, I, N, Acc) ->
    int_tuple2bin(Tuple, I-1, N, [<<(element(I, Tuple)):N/native>>|Acc]).

bool_tuple2bin(_, 0, Acc) -> Acc;
bool_tuple2bin(Tuple, I, Acc0) ->
    Acc = case element(I, Tuple) of
	      false -> [0|Acc0];
	      true -> [1|Acc0];
	      Bool -> [Bool|Acc0]
	  end,
    bool_tuple2bin(Tuple, I-1, Acc).

%% Func:    tuplelist2bin[TupleSize]
%% Args:    [TupleSize,] Type, List
%% Returns: Binary
%% Desc:    Converts a tupleList [{X,Y,..}..] to binary.

tuplelist2bin(2,Type,List) -> 
    tuplelist2bin2(Type,List);
tuplelist2bin(3,Type,List) -> 
    tuplelist2bin3(Type,List);
tuplelist2bin(4,Type,List) -> 
    tuplelist2bin4(Type,List);
tuplelist2bin(16,Type,List) ->  %% A special case.
    list_to_binary([matrix2bin(Tuple,Type)|| Tuple <- List]);
tuplelist2bin(Other,Type,List) ->
    list_to_binary([term2bin(Tuple,Other,Type)|| Tuple <- List]).

tuplelist2bin2(?SDL_FLOAT,List) ->
    tuplelist2bin2_float(List,32,[]);
tuplelist2bin2(?SDL_DOUBLE,List) ->
    tuplelist2bin2_float(List,64,[]);
tuplelist2bin2(?SDL_INT,List) ->
    tuplelist2bin2_int(List,32,[]);
tuplelist2bin2(?SDL_UNSIGNED_INT,List) ->
    tuplelist2bin2_int(List,32,[]).

tuplelist2bin3(?SDL_FLOAT,List) ->
    tuplelist2bin3_float(List,32,[]);
tuplelist2bin3(?SDL_DOUBLE,List) ->
    tuplelist2bin3_float(List,64,[]);
tuplelist2bin3(?SDL_INT,List) ->
    tuplelist2bin3_int(List,32,[]);
tuplelist2bin3(?SDL_UNSIGNED_INT,List) ->
    tuplelist2bin3_int(List,32,[]).

tuplelist2bin4(?SDL_FLOAT,List) ->
    tuplelist2bin4_float(List,32,[]);
tuplelist2bin4(?SDL_DOUBLE,List) ->
    tuplelist2bin4_float(List,64,[]);
tuplelist2bin4(?SDL_INT,List) ->
    tuplelist2bin4_int(List,32,[]);
tuplelist2bin4(?SDL_UNSIGNED_INT,List) ->
    tuplelist2bin4_int(List,32,[]).

tuplelist2bin2_float([{X,Y}|List],N,Acc) ->
    tuplelist2bin2_float(List,N,[<<X:N/float-native,Y:N/float-native>>|Acc]);
tuplelist2bin2_float([],_,Acc) -> list_to_binary(reverse(Acc)).
tuplelist2bin3_float([{X,Y,Z}|List],N,Acc) ->
    tuplelist2bin3_float(List,N,[<<X:N/float-native,Y:N/float-native,
				  Z:N/float-native>>|Acc]);
tuplelist2bin3_float([],_,Acc) -> list_to_binary(reverse(Acc)).
tuplelist2bin4_float([{X,Y,Z,W}|List],N,Acc) ->
    tuplelist2bin4_float(List,N,[<<X:N/float-native,Y:N/float-native,
				  Z:N/float-native,W:N/float-native>>|Acc]);
tuplelist2bin4_float([],_,Acc) -> list_to_binary(reverse(Acc)).

tuplelist2bin2_int([{X,Y}|List],N,Acc) ->
    tuplelist2bin2_int(List,N,[<<X:N/native,Y:N/native>>|Acc]);
tuplelist2bin2_int([],_,Acc) -> list_to_binary(reverse(Acc)).
tuplelist2bin3_int([{X,Y,Z}|List],N,Acc) ->
    tuplelist2bin3_int(List,N,[<<X:N/native,Y:N/native,
				Z:N/native>>|Acc]);
tuplelist2bin3_int([],_,Acc) -> list_to_binary(reverse(Acc)).
tuplelist2bin4_int([{X,Y,Z,W}|List],N,Acc) ->
    tuplelist2bin4_int(List,N,[<<X:N/native,Y:N/native,
				Z:N/native,W:N/native>>|Acc]);
tuplelist2bin4_int([],_,Acc) -> list_to_binary(reverse(Acc)).

%% Func:    bin2list
%% Args:    No, Type, Binary
%% Returns: ListOfNumbers
%% Desc:    Converts the first 'No' of numbers in the binary Binary  
%%          to a list of numbers of type Type.

bin2list(undefined, Type, Bin) ->
    bin2list(size(Bin), Type, Bin);
bin2list(N, ?SDL_UNSIGNED_BYTE, Bin0) ->
    Bin = if
	      N < size(Bin0) ->
		  <<B:N/binary,_/binary>> = Bin0,
		  B;
	      true -> Bin0
	  end,
    binary_to_list(Bin);
bin2list(N, ?SDL_UNSIGNED_SHORT, Bin) ->
    bin2unsigned(N, 2, Bin);
bin2list(N, ?SDL_UNSIGNED_INT, Bin) ->
    bin2unsigned(N, 4, Bin);
bin2list(N, ?SDL_BYTE, Bin) ->
    bin2int(N, 1, Bin);
bin2list(N, ?SDL_SHORT, Bin) ->
    bin2int(N, 2, Bin);
bin2list(N, ?SDL_INT, Bin) ->
    bin2int(N, 4, Bin);
bin2list(N, ?SDL_FLOAT, Bin) ->
    bin2float(N, 4, Bin);
bin2list(N, ?SDL_DOUBLE, Bin) ->
    bin2float(N, 8, Bin);
bin2list(_, boolean, Bin) ->
    [B =/= 0 || B <- binary_to_list(Bin)].

bin2float(N, Sz, Bin) ->
    bin2float_1(Sz*(N-1), Sz, Bin, []).

bin2float_1(N, _, _, Acc) when N < 0 -> Acc;
bin2float_1(N, Sz, Bin, Acc) ->
    <<_:N/binary,F:Sz/native-float-unit:8,_/binary>> = Bin,
    bin2float_1(N-Sz, Sz, Bin, [F|Acc]).

bin2unsigned(N, Sz, Bin) ->
    bin2unsigned_1(Sz*(N-1), Sz, Bin, []).

bin2unsigned_1(N, _, _, Acc) when N < 0 -> Acc;
bin2unsigned_1(N, Sz, Bin, Acc) ->
    <<_:N/binary,U:Sz/native-unsigned-unit:8,_/binary>> = Bin,
    bin2unsigned_1(N-Sz, Sz, Bin, [U|Acc]).

bin2int(N, Sz, Bin) ->
    bin2int_1(Sz*(N-1), Sz, Bin, []).

bin2int_1(N, _, _, Acc) when N < 0 -> Acc;
bin2int_1(N, Sz, Bin, Acc) ->
    <<_:N/binary,U:Sz/native-signed-unit:8,_/binary>> = Bin,
    bin2int_1(N-Sz, Sz, Bin, [U|Acc]).

matrix2bin(Matrix, ?SDL_FLOAT) ->
    matrix2bin_1(Matrix, 32);
matrix2bin(Matrix, ?SDL_DOUBLE) ->
    matrix2bin_1(Matrix, 64).

matrix2bin_1({A,B,C, E,F,G, I,J,K, M,N,O}, S) ->
    <<A:S/native-float,B:S/native-float,C:S/native-float,0.0:S/native-float,
     E:S/native-float,F:S/native-float,G:S/native-float,0.0:S/native-float,
     I:S/native-float,J:S/native-float,K:S/native-float,0.0:S/native-float,
     M:S/native-float,N:S/native-float,O:S/native-float,1.0:S/native-float>>;
matrix2bin_1({A,B,C,D, E,F,G,H, I,J,K,L, M,N,O,P}, S) ->
    <<A:S/native-float,B:S/native-float,C:S/native-float,D:S/native-float,
     E:S/native-float,F:S/native-float,G:S/native-float,H:S/native-float,
     I:S/native-float,J:S/native-float,K:S/native-float,L:S/native-float,
     M:S/native-float,N:S/native-float,O:S/native-float,P:S/native-float>>;
matrix2bin_1([A,B,C, E,F,G, I,J,K, M,N,O], S) ->
    <<A:S/native-float,B:S/native-float,C:S/native-float,0.0:S/native-float,
     E:S/native-float,F:S/native-float,G:S/native-float,0.0:S/native-float,
     I:S/native-float,J:S/native-float,K:S/native-float,0.0:S/native-float,
     M:S/native-float,N:S/native-float,O:S/native-float,1.0:S/native-float>>;
matrix2bin_1([A,B,C,D, E,F,G,H, I,J,K,L, M,N,O,P], S) ->
    <<A:S/native-float,B:S/native-float,C:S/native-float,D:S/native-float,
     E:S/native-float,F:S/native-float,G:S/native-float,H:S/native-float,
     I:S/native-float,J:S/native-float,K:S/native-float,L:S/native-float,
     M:S/native-float,N:S/native-float,O:S/native-float,P:S/native-float>>.

%% Func:    copySdlImage2GLArray
%% Args:    SdlImageRef, malloced Mem, BytesPerPixel
%% Returns: ok or exits
%% Desc:    Converts sdl_surface data to a byte RGB(A) buffer.
%%          sdl_surfaces are BGR and upside/down.
copySdlImage2GLArray(Image, Mem, Bpp) when is_record(Image, sdl_surface) ->
    copySdlImage2GLArray(Image#sdl_surface.self, Mem, Bpp);
copySdlImage2GLArray({surfacep, Image}, #sdlmem{bin=Mem}, Bpp) 
  when Bpp == 3; Bpp == 4 ->
    sdl:send_bin(Mem),
    case call(?SDL_UTIL_copySdlImage2GLArray, <<Image:?_PTR,Bpp:8>>) of
	<<1:8>> -> ok;
	_O -> 
	    error({error, _O})
    end.

mem_size(?SDL_BYTE, Size) -> Size;
mem_size(?SDL_UNSIGNED_BYTE, Size) -> Size;
mem_size(?SDL_UNSIGNED_SHORT, Size) -> 2*Size;
mem_size(?SDL_SHORT, Size) -> 2*Size;
mem_size(?SDL_UNSIGNED_INT, Size) -> 4*Size;
mem_size(?SDL_INT, Size) -> 4*Size;
mem_size(?SDL_FLOAT, Size) -> 4*Size;
mem_size(?SDL_DOUBLE, Size) -> 8*Size.

%% Func: Debug 
%% Args: Level (0 is off)
%% Returns: ok
%% Desc: Set ESDL debug level
debug(L) when is_integer(L) ->
    cast(?SDL_UTIL_debug, [L]).

%% Debug functions
wb([]) -> ok;
wb([A, B, C, D|R]) ->
    whex([A, B, C,D]),
    wb(R).

whex([]) -> io:format(" ~n");
whex([A|R]) ->
    A1 = A div 16,
    A2 = A rem 16,
    whex(A1),whex(A2),
    io:format(" "),
    whex(R);

whex(A) when A < 10 ->
    io:format("~p", [A]);
whex(A) when A == 10 ->
    io:format("A");
whex(A) when A == 11 ->
    io:format("B");
whex(A) when A == 12 ->
    io:format("C");
whex(A) when A == 13 ->
    io:format("D");
whex(A) when A == 14 ->
    io:format("E");
whex(A) when A == 15 ->
    io:format("F").
