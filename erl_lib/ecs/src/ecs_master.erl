-module(ecs_master).
-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok,
     {
       #{strategy => one_for_one,
         intensity => 5,
         period => 60},

       [
        #{id => connectivity,
          start => {ecs_connectivity, start_link, []},
          restart => permanent,
          shutdown => 500,
          type => worker,
          modules => [ecs_connectivity]}
       ]
     }}.
