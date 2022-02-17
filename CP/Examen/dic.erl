-module(dic).

-export([start/1, init_store/0, get/2, put/3]).

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

%%%API

get(K, S) ->
  S ! {get , K, self ()},
  receive
    {get_reply , R} -> R
  end.

put(K, V, S) ->
  S ! {put, K, V}.



%sync_put(K, V, S) -> . . .

%%%Loop y auxiliares
loop_store(Dic, L) ->
  receive
    {get , K, From} ->
      R = lookup(K, Dic),
      From ! {get_reply, R},
      loop_store(Dic, L);
    {put, K, V, From} ->
      store(K, V, Dic),
      send_store_list(L,Dic),
      From ! {put, ok}
  end.

lookup(_,[]) -> not_found;
lookup(K,[{K, V}|_]) -> {ok, V};
lookup(K,[_|T]) -> lookup(K, T).

store(K, V, []) -> [{K,V}];
store(K, V, [{K,_}|T]) -> [{K,V} | T];
store(K, V, [{K2, V2}|T]) -> [{K2, V2} | store(K, V, T) ].
