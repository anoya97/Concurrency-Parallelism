-module(server).
-export([start/0,init/0,echo/2]).


start()-> spawn(server,init,[]).

echo(Echo_Srv, Msg) ->
	Echo_Srv ! {echo,Msg,self()},
	receive
		{echo_reply, Reply} -> Reply
	end.
	
init() -> loop().

loop() -> 
	receive
		stop -> 
			ok;
		{echo, Msg, From} ->
			From ! {echo_reply, Msg},			
			loop()
	end.				