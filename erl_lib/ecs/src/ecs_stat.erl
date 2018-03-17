-module(ecs_stat).
-record(stat, {value, time, origin}).

-export([new/1,
         modify/2,
         overwrite/2,
         to_tuple/2]).

new(Value) ->
    #stat{
       value = Value,
       time = erlang:system_time(),
       origin = erlang:node()}.

modify(Stat, Value) ->
    #stat{
       value = Stat#stat.value + Value,
       time = erlang:system_time(),
       origin = Stat#stat.origin}.

overwrite(Stat, Value) ->
    #stat{
       value = Value,
       time = erlang:system_time(),
       origin = Stat#stat.origin}.

to_tuple(Name, Stat) ->
    {Name, Stat#stat.value, Stat#stat.time, Stat#stat.origin}.
