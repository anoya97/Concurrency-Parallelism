-module(recursos).

-export([start/1, avail/1, alloc/1, release/2]). % Api
-export([init/1]).




start(Lista) ->
	spawn(?MODULE, init, [Lista]).

avail(G) ->
	G ! {avail, self()},
  receive
    {avail,L} -> L
  end.

alloc(G) ->
  G ! {alloc, self()},
	receive
    {alloc,R} -> {alloc,R};
		{error, M} -> {error, M}
  end.

release(G,Recurso) ->
	G ! {release, self(), Recurso},
	receive
    {release,R} -> {release,R};
		{error, M} -> {error, M}
  end.


%%% Internal functions %%%%%

init(Lista) ->
	loop(Lista,[]).


loop(ListaLibres, ListaAsignados) ->
	receive
		{avail,From} -> From ! {avail,length(ListaLibres)},loop(ListaLibres,ListaAsignados);

		{alloc,From} ->
			case ListaLibres of
				[] -> From ! {error, "sin recursos"	}, loop(ListaLibres,ListaAsignados);
				[H | T] -> From ! {alloc, H}, loop(T,[{H,From} | ListaAsignados])
			end;

		{release, From, Recurso} ->
			 A = lists:keyfind(Recurso, 1,ListaAsignados),
			case A of
				{_ , Cliente} ->
					if From == Cliente -> From ! {release,ok},
																loop([Recurso | ListaLibres], lists:keydelete(Recurso, 1,ListaAsignados));
												true -> From ! {error, "recurso no reservado"}, loop(ListaLibres,ListaAsignados)
					end;
				false -> From ! {error, "recurso no reservado"},loop(ListaLibres,ListaAsignados)
			end
		end.
