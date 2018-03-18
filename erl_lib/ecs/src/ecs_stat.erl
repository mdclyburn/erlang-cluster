-module(ecs_stat).
-record(stat, {name, value, time, origin, tags}).

-export([new/1,
         new/2,
         new/3,
         modify/2,
         overwrite/2,
         to_tuple/1]).

new(Name) -> new(Name, 0).

new(Name, Value) -> new(Name, Value, []).

new(Name, Value, Tags) ->
    #stat{
       name = Name,
       value = Value,
       time = erlang:system_time(),
       origin = erlang:node(),
       tags = Tags}.

modify(Value, Stat) ->
    #stat{
       name = Stat#stat.name,
       value = Stat#stat.value + Value,
       time = erlang:system_time(),
       origin = Stat#stat.origin,
       tags = Stat#stat.tags}.

overwrite(Value, Stat) ->
    #stat{
       name = Stat#stat.name,
       value = Value,
       time = erlang:system_time(),
       origin = Stat#stat.origin,
       tags = Stat#stat.tags}.

to_tuple(Stat) ->
    {Stat#stat.name, Stat#stat.value, Stat#stat.time, Stat#stat.origin, Stat#stat.tags}.
