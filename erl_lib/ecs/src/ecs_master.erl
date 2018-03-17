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
        #{id => config_manager,
          start => {ecs_config_manager, start_link, []},
          restart => permanent,
          shutdown => 1000,
          type => worker,
          modules => [ecs_config_manager]},

        #{id => statistics,
          start => {ecs_statistics, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [ecs_statistics]},

        #{id => connectivity,
          start => {ecs_connectivity, start_link, []},
          restart => permanent,
          shutdown => 500,
          type => worker,
          modules => [ecs_connectivity]}
       ]
       ++ configured_roles(ecs_config:roles(erlang:list_to_atom(ecs_util:host())))
     }}.

configured_roles(Roles) -> configured_roles(Roles, []).
configured_roles([], ChildSpecs) -> ChildSpecs;
configured_roles([Role|Rest], ChildSpecs) ->
    io:format("Configuring for ~w role.~n", [Role]),
    case Role of
        stat_forward ->
            configured_roles(Rest,
                             [#{id => stat_forwarder,
                                start => {ecs_statforwarder, start_link, []},
                                restart => permanent,
                                shutdown => 5000,
                                type => worker,
                                modules => [ecs_statforwarder]} | ChildSpecs]);
        appm ->
            configured_roles(Rest,
                             [#{id => app_manager,
                                start => {ecs_app, start_link, []},
                                restart => permanent,
                                shutdown => 5000,
                                type => worker,
                                modules => [ecs_app]} | ChildSpecs]);
        UnknownRole ->
            io:format("Unconfigurable role: ~w~n", UnknownRole),
            configured_roles(Rest, ChildSpecs)
    end.
