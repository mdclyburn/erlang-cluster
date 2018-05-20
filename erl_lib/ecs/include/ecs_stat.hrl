-record(stat, {name :: nonempty_string(),
               value :: number(),
               tags :: [nonempty_string()],
               time :: integer(),
               origin :: atom(),
               options :: #{atom() => term()}}).

-type stat() :: #stat{}.
-type stat_option() :: {continuous, boolean()}
                     | {aggregation, fun((number()) -> number())}
                     | {reset, fun((number()) -> number())}.

-export_type([stat/0]).
