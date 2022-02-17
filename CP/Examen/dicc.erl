-module(dicc).

-export([start/1, init_store/0, get/2, put/3, sync_put/3, sync_update/4]).

%%% Arranque del sistema
start(N) -> % arranca N diccionarios
	L = start_stores(N), % arrancar diccionarios
	send_store_list(L, L), % enviar copia de la lista de diccionarios
	L.

start_stores(0) -> [];
start_stores(N) -> [spawn(?MODULE, init_store, []) | start_stores(N-1)].

send_store_list([], _) -> ok;
send_store_list([S|T], L) -> S ! {store_list, L}, send_store_list(T,L).

init_store() ->
	receive
		{store_list, L} -> loop_store([], L)
	end.

%%% API
get(K, S) ->
	S ! {get, K, self()},
	receive
		{get_reply, R} -> R
	end.

put(K, V, S) -> S ! {put, K, V}.


sync_put(K, V, S) ->
	S ! {sync_put, K, V, self()},
	receive
		sync_put_ok -> ok
	end.

%%% Loop y auxiliares
loop_store(Dic, L) ->
	receive
		{get, K, From} ->
			R = lookup(K, Dic),
			From ! {get_reply, R},
			loop_store(Dic, L);
		{store, K, V} ->
			loop_store(store(K, V, Dic), L);
		{sync_store, K, V, From} ->
			From ! {sync_store_done, self()},
			loop_store(store(K, V, Dic), L);
		{put, K, V} ->
			reenviar(K, V, L),
			loop_store(Dic, L);
		{sync_put, K, V, From} ->
			spawn(?MODULE, sync_update, [K, V, From, L]),
			loop_store(Dic, L)
	end.

sync_update(K, V, From, L) ->
	sync_reenviar(K, V, L),
	sync_wait(L),
	From ! sync_put_ok.

sync_wait([]) -> ok;
sync_wait(L) ->
	receive
		{sync_store_done, From} ->
			sync_wait(L -- [From])
	end.

sync_reenviar(_, _, []) -> ok;
sync_reenviar(K, V, [S|T]) ->
	S ! {sync_store, K, V, self()},
	sync_reenviar(K, V, T).

reenviar(_, _, []) -> ok;
reenviar(K, V, [S|T]) ->
	S ! {store, K, V},
	reenviar(K,V, T).

lookup(_, []) -> {error, not_found};
lookup(K, [{K, V} | _]) -> {ok, V};
lookup(K, [_|T]) -> lookup(K, T).

store(K, V, []) -> [{K,V}];
store(K, V, [{K,_} | T]) -> [{K,V} | T];
store(K, V, [{K2, V2}|T]) -> [{K2, V2} | store(K, V, T)].
