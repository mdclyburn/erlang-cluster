-record(stat, {name = "" :: nonempty_string(),
               value = 0 :: number(),
               tags = [] :: [nonempty_string()],
               time = 0 :: integer(),
               origin = unknown :: atom(),
               options = #{} :: #{atom() => term()}}).

-type stat() :: #stat{}.
-type stat_option() :: {continuous, boolean()}
                     | {aggregation, fun((number()) -> number())}
                     | {reset, fun((number()) -> number())}.

-export_type([stat/0]).
