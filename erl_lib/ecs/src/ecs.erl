-module(ecs).
-behavior(application).
-export([start/2,
         stop/1]).

start(normal, _) ->
    rand:seed(exrop),
    ecs_master:start_link().

stop(Reason) ->
    io:format("Erlang cluster service stopping: ~s.~n", [Reason]).
