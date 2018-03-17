-module(ecs_config).
-export([cluster/0,
         nodes/0,
         roles/1,
         applications/1,
         nodes_of_role/1,
         nodes_of_application/1,
         nodes_of_application_r/1]).

cluster() ->
    case file:consult(config_directory() ++ "/nodes") of
        {ok, [Nodes]} -> Nodes
    end.

nodes() -> lists:map(fun ({Name, _, _}) -> Name end, cluster()).

roles(Name) ->
    case lists:filtermap(fun ({N, R, _}) ->
                                 case N == Name of
                                     true -> {true, R};
                                     false -> false
                                 end
                         end,
                         cluster())
    of
        [] -> [];
        Result -> lists:nth(1, Result)
    end.

applications(Name) ->
    case lists:filtermap(fun ({N, _, A}) ->
                                 case N == Name of
                                     true -> {true, A};
                                     false -> false
                                 end
                         end,
                         cluster())
    of
        [] -> [];
        Result -> lists:nth(1, Result)
    end.

nodes_of_role(Role) -> lists:filtermap(
                         fun ({Name, Roles, _}) ->
                                 case lists:member(Role, Roles) of
                                     true -> {true, Name};
                                     false -> false
                                 end
                         end,
                         cluster()).

nodes_of_application(Application) -> lists:filtermap(
                         fun ({Name, _, Applications}) ->
                                 case lists:member(Application, Applications) of
                                     true -> {true, Name};
                                     false -> false
                                 end
                         end,
                         cluster()).

nodes_of_application_r(Application) ->
    ecs_util:randomize(nodes_of_application(Application)).

% ===== Private

config_directory() ->
    case application:get_env(config_dir) of
        {ok, Dir} -> Dir;
        undefined -> throw({unspecified, config_dir})
    end.
