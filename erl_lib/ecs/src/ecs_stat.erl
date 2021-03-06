-module(ecs_stat).
-include("ecs_stat.hrl").

-export([new/1,
         new/2,
         new/3,
         new/4,
         set_opts/2,
         update/2,
         reset/1,
         continuous/1]).

% Create a new Stat record.
% Opts is a list of key-value pairs of options.
-spec new(Name, Value, Tags, Opts) -> Stat when
      Name :: nonempty_string(),
      Value :: number(),
      Tags :: [nonempty_string()],
      Opts :: [{atom(), term()}],
      Stat :: stat().

new(Name) -> new(Name, 0).
new(Name, Value) -> new(Name, Value, []).
new(Name, Value, Tags) -> new(Name, Value, Tags, []).
new(Name, Value, Tags, Opts) ->
    #stat{
       name = Name,
       value = Value,
       tags = Tags,
       time = erlang:system_time(),
       origin = erlang:node(),
       options = maps:from_list(Opts)}.

-spec set_opts(Stat, Options) -> Stat when
      Stat :: stat(),
      Options :: [{atom(), term()}].

% Available options:
%   {continuous, true|false}
%     whether the measurement is a continuous one
%   {aggregation, fn/2}
%     function used to aggregate values where the first
%     parameter is the new value and the second parameter is the old value
%   {reset, fn/1}
%     function used to reset the value of the statistic where the parameter
%     is the current value
set_opts(Stat, []) -> Stat;
set_opts(Stat, [{Option, Value}|Rest]) ->
    set_opts(Stat#stat{options = maps:put(Option, Value, Stat#stat.options)},
             Rest).

-spec update(Value, Stat) -> Stat when
      Value :: number(),
      Stat :: stat().

% Modify the stored value of a stat. If the stat does not have an aggregation
% function supplied, the value is simply overwritten.
update(Value, Stat) ->
    Stat#stat{
      time = erlang:system_time(),
      value = (maps:get(aggregation,
                        Stat#stat.options,
                        fun (New, _) -> New end))(Value, Stat#stat.value)}.

-spec reset(Stat) -> Stat when
      Stat :: stat().

% Apply the reset function to the stored value of a stat. If the stat does not
% have a reset function, then the value is set to zero.
reset(Stat) ->
    Stat#stat{
      time = erlang:system_time(),
      value = (maps:get(reset,
                        Stat#stat.options,
                        fun(_) -> 0 end))(Stat#stat.value)}.

-spec continuous(Stat) -> IsContinuous when
      Stat :: stat(),
      IsContinuous :: boolean().

continuous(Stat) -> maps:get(continuous, Stat#stat.options, true).
