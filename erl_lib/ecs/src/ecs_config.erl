-module(ecs_config).
-export([cluster_nodes/0]).

config_directory() ->
    case application:get_env(config_dir) of
        {ok, Dir} -> Dir;
        undefined -> throw({unspecified, config_dir})
    end.

cluster_nodes() ->
    case file:consult(config_directory() ++ "/nodes") of
        {ok, [Nodes]} -> Nodes
    end.
