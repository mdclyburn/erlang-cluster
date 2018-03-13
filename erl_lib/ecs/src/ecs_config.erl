-module(ecs_config).
-export([cluster/0,
         nodes/0,
         roles/1]).

cluster() ->
    case file:consult(config_directory() ++ "/nodes") of
        {ok, [Nodes]} -> Nodes
    end.

nodes() -> lists:map(fun ({Name, _}) -> Name end, cluster()).

roles(Name) ->
    case lists:filter(fun ({N, _}) -> N == Name end, cluster()) of
        [] -> [];
        Result -> lists:nth(1, Result)
    end.

% ===== Private

config_directory() ->
    case application:get_env(config_dir) of
        {ok, Dir} -> Dir;
        undefined -> throw({unspecified, config_dir})
    end.
