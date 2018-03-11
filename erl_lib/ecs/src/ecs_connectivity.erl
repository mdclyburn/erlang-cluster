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
    Nodes = load_node_list(),
    reconnect(Nodes),
    {ok, Nodes, generate_timeout()}.

terminate(Reason, _) ->
    io:format("Connectivity service stopping: ~w.~n", [Reason]).

handle_call(_, _, Data) -> {reply, unknown, Data}.

handle_cast(reload, Nodes) ->
    try load_node_list() of
        {ok, []} ->
            io:format("Connectivity: no nodes listed; ignoring.~n"),
            {noreply, Nodes}; % Do not accept empty node lists.
        {ok, NewNodes} ->
            io:format("Connectivity: reload found ~b nodes.~n", [erlang:length(NewNodes)]),
            {noreply, NewNodes}
    catch
        throw:bad_nodes_file -> io:format("Connectivity: bad nodes file.~n"),
                                {noreply, Nodes}
    end;
handle_cast(_, Data) -> {reply, unknown, Data}.

handle_info(timeout, Nodes) ->
    reconnect(Nodes),
    {noreply, Nodes, generate_timeout()};
handle_info(_, Data) -> {reply, unknown, Data}.

code_change(_, Data, _) -> {ok, Data}.

%% ===== Private

% Load all nodes listed in the nodes file.
load_node_list() ->
    case application:get_env(nodes_file) of
        {ok, Path} -> lists:delete(erlang:node(), load_node_list(Path))
    end.
load_node_list(Path) ->
    case file:open(Path, [read]) of
        {ok, File} -> load_node_list(File, []);
        E = {error, _} -> E
    end.
load_node_list(File, Nodes) ->
    case file:read_line(File) of
        {ok, Line} -> load_node_list(File, [hostname_to_node(ecs_util:name(), lists:droplast(Line))|Nodes]);
        eof -> Nodes
    end.

% Attempt to connect to nodes that are listed in the nodes file but are not
% currently connected. This function recognizes that multiple nodes may be
% connected to with a single net_kernel:connect.
reconnect(Known) -> reconnect(lists:subtract(Known, nodes(connected)), 0).
reconnect([], ConnectionsMade) -> ecs_statistics:record("connects_per_reconnect", ConnectionsMade), ok;
reconnect([UnconnectedNode|Rest], ConnectionsMade) ->
    case net_kernel:connect(UnconnectedNode) of
        true ->
            timer:sleep(1000),
            reconnect(lists:subtract(Rest, erlang:nodes(connected)),
                      ConnectionsMade + 1);
        false ->
            reconnect(Rest, ConnectionsMade)
    end.

% Prepends a node Name to the Host and returns it as an atom.
hostname_to_node(Name, Host) -> erlang:list_to_atom(Name ++ "@" ++ Host).

% Generate a timeout in milliseconds depending on how large the cluster is to
% help reduce reconnect operations from every node happening at the same time.
generate_timeout() ->
    Wait = rand:uniform((erlang:length(erlang:nodes(connected)) + 5) * 60 * 1000),
    ecs_statistics:record("reconnect_attempt_wait", Wait),
    Wait.
