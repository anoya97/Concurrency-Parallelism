-module(dict).
-export([ start/1, init store/0, get/2, put/3, sync put/3]).
%%% Arranque del sistema
start(N) - > % arranca N diccionarios
L = start stores(N) , % arrancar diccionarios
send store list(L, L) , % enviar copia de la lista de diccionarios
loop(L).
start stores(0) - > [ ] ;
start stores(N) - > [spawn(?MODULE, init store , [ ] ) | start stores(N -1)].
send store list ( [ ] , ) - > ok;
send store list ([S|T] , L) - > S ! {store list , L}, send store list(T,L).
init store() - >
receive
{store list , L} - > loop store ( [ ] , L)
end.
%%%API
get(K, S) - >
S ! {get , K, self ()},
receive
{get reply , R} - > R
end.
put(K, V, S) - > . . .
sync put(K, V, S) - > . . .
%%% Loop y auxiliares
loop store(Dic, L) - >
receive
{get , K, From} - >
R = lookup(K, Dic) ,
From ! {get reply , R},
loop(Dic, L)
end.
lookup( , [ ] ) - > not found;
lookup(K, [{K, V } | ]) - > {ok, V};
lookup(K, [ |T]) - > lookup(K, T).
store(K, V, [ ] ) - > [{K,V}];
store(K, V, [{K, } | T]) - > [{K,V } | T] ;
store(K, V, [{K2, V2}|T]) - > [{K2, V2} | store(K, V, T) ] .
