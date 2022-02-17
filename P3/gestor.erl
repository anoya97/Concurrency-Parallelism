-module(gestor).
-export([start/1,stop/1,avail/1,alloc/1,release/2,add/2]).
-export([init/1]).


start(L)-> 
	spawn(?MODULE,init,[L]).
	
init(L)-> 
	process_flag(trap_exit, true),
	loop(L,[]).
 
stop(G)->
	G ! stop.

avail(G)->
	G ! {avail,self()},
	receive
		{avail,Len}->
		Len
	end.

add(G,List)->
	G ! {add,List}.	

alloc(G)->
	G ! {alloc,self()},
	receive
		{ok,Recurso}->
			{ok,Recurso};
		{error,sin_recursos}->
			{error,sin_recursos}	
	end.

release(G, Recurso)->
	G ! {release,Recurso,self()},
	receive
		ok -> ok;
		{error,recurso_no_reservado}-> 
			{error,recurso_no_reservado}
	end.				

intro(L,[])-> L;
intro(L,[H])-> [H|L];
intro(L,[H|T])-> intro([H|L],T).

		
del_repetidos(L,[]) -> L;
del_repetidos(L,[H]) -> lists:delete(H,L); %Elimina de la lista principal los elementos repetidos
del_repetidos(L,[H|T]) -> del_repetidos(lists:delete(H,L),T).

del_repetidosl2(List,[]) -> List;
del_repetidosl2(List,[{X,_}]) -> lists:delete(X,List);  %Elimina de la lista que vas a insertar los elementos que ya fueron reservados
del_repetidosl2(List,[{X,_}|T]) -> del_repetidosl2(lists:delete(X,List),T).

del_rec(L,L2,Pid) -> 
	Rec = lists:keyfind(Pid,2,L2),
		case Rec of
			{Recurso,Pid}-> 	 
				del_rec([Recurso|L],lists:keydelete(Recurso,1,L2),Pid);
			false-> unlink(Pid),loop(L,L2)	
		end.

loop(L,L2) -> 
	receive
		{avail,From}->
			From ! {avail,length(L)},
			loop(L,L2);

		{alloc,From}->
			case L of
				[] -> From ! {error,sin_recursos},
					  loop(L,L2);
				[H|T] -> From ! {ok,H},
						 link(From),
						 loop(T,[{H,From}|L2])
						 
			end;

		{add,List}->
			loop(intro(del_repetidos(L,del_repetidosl2(List,L2)),del_repetidosl2(List,L2)),L2);
					

		{release,Recurso,From}->
			Rec = lists:keyfind(Recurso,1,L2),
			case Rec of
				{Recurso,Id}-> 
					if Id == From -> From ! ok,
					   		loop([Recurso|L],lists:keydelete(Recurso,1,L2));
					   true -> From ! {error,recurso_no_reservado},
					   		loop(L,L2)
					end;

				false -> From ! {error,recurso_no_reservado},
					   		loop(L,L2)   		
			end;

		{'EXIT',Pid, _}-> 
			del_rec(L,L2,Pid);

		stop-> 
			ok
	end.