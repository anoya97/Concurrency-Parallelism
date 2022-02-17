-module(hash).

-export([start/0, store/3, get/2, del/2, stop/1]). % Api

-export([init/0]).

%%% API %%%%

start() ->
	spawn(?MODULE, init, []).

store(Hash, Key, Value) ->
	Hash ! {store, Key, Value}.

get(Hash, Key) ->
	Hash ! {get, Key, self()},
	receive
		{hash_ok, V} ->
			{ok, V};
		{hash_error, Reason} -> 
			{error, Reason}
	end.

del(Hash, Key) ->
	Hash ! {del, Key}.
	
stop(Hash) ->
	Hash ! stop.

%%% Internal functions %%%%%

init() ->
	loop([]).

loop(D) ->
	receive
		{store, NK, NV} ->
			loop([{NK,NV} | [{K, V} || {K, V} <- D, K/=NK]]);
		{get, K, From} ->
			From ! find(K, D),
			loop(D);
		{del, KD} ->
			loop([{K, V} || {K,V} <- D, K/=KD]); 
		stop ->
			ok
	end.

find(_, []) -> {hash_error, not_found};
find(K, [{K,V} | _]) -> {hash_ok, V};
find(K, [_|T]) -> find(K, T).
