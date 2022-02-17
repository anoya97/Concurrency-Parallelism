-module(hola).
-export([hola/0]).
-export([fact/1]).
-export([fact2/1]).
-export([fact3/1]).
-export([fact_aux/2]).

hola() ->
	io:format("Hola Mundo!~n").

fact(N) ->
	if N==0 -> 1;
		true -> N*fact(N-1)
	end.

fact2(0) -> 1;
fact2(N) when N > 0 ->
	N * fact(N - 1).


fact_aux(0,Acc) -> Acc;
fact_aux(N,Acc) -> fact_aux(N-1, N*Acc).

fact3(N) ->
	if
		N < 0 -> {error,badarg};
		true -> {ok, fact_aux(N, 1)}
	
	end.