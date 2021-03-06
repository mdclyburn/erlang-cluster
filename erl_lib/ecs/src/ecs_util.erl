-module(ecs_util).

-export([name/0,
         name/1,
         host/0,
         host/1,
         host_a/0,
         randomize/1]).

% Returns the name of the node.
name() -> name(node()).
name(Node) -> lists:takewhile(fun (C) -> C /= $@ end, erlang:atom_to_list(Node)).

% Returns the host of the node.
host() -> host(node()).
host(Node) -> lists:delete($@, lists:dropwhile(fun (C) -> C /= $@ end, erlang:atom_to_list(Node))).

% Returns the host of the node as an atom.
host_a() -> erlang:list_to_atom(host()).

% Randomize a list.
randomize([]) -> [];
randomize(L) when is_list(L) ->
    lists:map(fun ({_, E}) -> E end,
              lists:sort(
                lists:map(fun (E) -> {rand:uniform(25 * erlang:length(L)), E} end, L))).
