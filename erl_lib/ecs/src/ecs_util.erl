-module(ecs_util).

-export([name/0,
         host/0]).

% Returns the name of the node.
name() -> lists:takewhile(fun (C) -> C /= $@ end, erlang:atom_to_list(node())).

% Returns the host of the node.
host() -> lists:delete($@, lists:dropwhile(fun (C) -> C /= $@ end, erlang:atom_to_list(node()))).
