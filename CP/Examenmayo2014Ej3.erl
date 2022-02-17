-module(num server) .
-export( [ start/0, init/0, add/1, sub/1, reset/0]).

start() ->
  Pid = spawn(num server, init , [ ]) ,
  register(num server, Pid) .

add(N) ->
  num server ! {add, N,self()},
  receive
      {num_server , X} -> X;
  end.

sub(N) ->
  num server ! {sub, N,self()},
  receive
      {num_server , X} -> X;
  end.

get_ops() ->
  num_server ! {sub,N,self()},
  receive
      {get_ops , X} -> X;
  end.

reset() ->
  num server ! reset .

init() −>
  loop(0,0).

loop(N,OP) ->
  receive
    {add, X} ->
      From ! {num_server, N+X},
      loop(N + X,OP+1);
    {sub, X} ->
      From ! {num_server, N-X},
      loop(N- X,OP+1);
    {sub, X} ->
      From ! {get_ops, OP},
      loop(N,OP);
    reset ->
      loop(0,0)
  end.
