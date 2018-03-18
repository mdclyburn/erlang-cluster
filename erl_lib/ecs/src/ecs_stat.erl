-module(ecs_stat).
-record(stat, {name, value, time, origin, tags, continuous = true}).

-export([new/4,
         modify/2,
         overwrite/2,
         to_tuple/1,
         continuous/1]).

new(Name, Value, Tags, IsContinuous) ->
    #stat{
       name = Name,
       value = Value,
       time = erlang:system_time(),
       origin = erlang:node(),
       tags = Tags,
       continuous = IsContinuous}.

modify(Value, Stat) ->
    #stat{
       name = Stat#stat.name,
       value = Stat#stat.value + Value,
       time = erlang:system_time(),
       origin = Stat#stat.origin,
       tags = Stat#stat.tags,
       continuous = Stat#stat.continuous}.

overwrite(Value, Stat) ->
    #stat{
       name = Stat#stat.name,
       value = Value,
       time = erlang:system_time(),
       origin = Stat#stat.origin,
       tags = Stat#stat.tags,
       continuous = Stat#stat.continuous}.

to_tuple(Stat) ->
    {Stat#stat.name,
     Stat#stat.value,
     Stat#stat.time,
     Stat#stat.origin,
     Stat#stat.tags,
     Stat#stat.continuous}.

continuous(Stat) -> Stat#stat.continuous.
