-module(ecs_connectivity).
-behavior(gen_server).

-export([start_link/0,
         reload_nodes/0]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

% Connectivity Service
%
% Maintains clustering between Erlang nodes.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% Reload the node list from file.
reload_nodes() -> gen_server:cast(?MODULE, reload).

% Force a reconnect to happen.
% This will regenerate a new timeout.

% ===== gen_server Calls

init(_) ->
    io:format("Connectivity service started.~n"),
    Nodes = get_nodes(),
    reconnect(Nodes),
    {ok, Nodes, generate_timeout()}.

terminate(Reason, _) ->
    io:format("Connectivity service stopping: ~w.~n", [Reason]).

handle_call(_, _, Data) -> {reply, unknown, Data}.

handle_cast(reload, _) -> {noreply, ecs_config:nodes()};
handle_cast(_, Data) -> {noreply, Data}.

handle_info(timeout, Nodes) ->
    reconnect(Nodes),
    {noreply, Nodes, generate_timeout()};
handle_info(_, Data) -> {reply, unknown, Data}.

code_change(_, Data, _) -> {ok, Data}.

%% ===== Private

get_nodes() ->
    lists:map(fun (Host) -> erlang:list_to_atom(
                              ecs_util:name()
                              ++ "@"
                              ++ erlang:atom_to_list(Host)) end,
                            ecs_config:nodes()).

% Attempt to connect to nodes that are listed in the nodes file but are not
% currently connected. This function recognizes that multiple nodes may be
% connected to with a single net_kernel:connect.
reconnect(Known) -> reconnect(
                      lists:subtract(
                        lists:filter(fun (N) -> N /= erlang:node() end,
                                     Known),
                        nodes(connected)),
                      0).
reconnect([], ConnectionsMade) -> ecs_statistics:record("connects_per_reconnect", ConnectionsMade), ok;
reconnect([UnconnectedNode|Rest], ConnectionsMade) ->
    io:format("attempting to connect to ~w...~n", [UnconnectedNode]),
    case net_kernel:connect(UnconnectedNode) of
        true ->
            timer:sleep(1000),
            reconnect(lists:subtract(Rest, erlang:nodes(connected)),
                      ConnectionsMade + 1);
        false ->
            reconnect(Rest, ConnectionsMade)
    end.

% Generate a timeout in milliseconds depending on how large the cluster is to
% help reduce reconnect operations from every node happening at the same time.
generate_timeout() ->
    Wait = rand:uniform((erlang:length(erlang:nodes(connected)) + 5) * 60 * 1000),
    ecs_statistics:record("reconnect_attempt_wait", Wait),
    Wait.
