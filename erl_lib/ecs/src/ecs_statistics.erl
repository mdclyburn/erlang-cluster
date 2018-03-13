-module(ecs_statistics).
-behavior(gen_server).

-export([start_link/0,
         record/2,
         queued/0]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-define(FORWARD_DELAY, 3000).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record(Name, Value) -> gen_server:cast(?MODULE, {record, new_stat(Name, Value)}).

queued() -> gen_server:call(?MODULE, get).

% ===== gen_server

init(_) ->
    io:format("Statistics service started.~n"),
    {ok, create_data()}.

terminate(Reason, Data) ->
    io:format("Statistics service flushing statistics and exiting: ~w.~n", [Reason]),
    flush(Data).

handle_call(get, _, Data) -> {reply, get_stats(Data), Data};
handle_call(_, _, Data) -> {reply, unknown, Data}.

handle_cast({record, Stat}, Data) -> {noreply, add_stat(Stat, Data)};
handle_cast(_, Data) -> {noreply, Data}.

handle_info(submit, Data) -> {noreply, flush(Data)};
handle_info(_, Data) -> {noreply, Data}.

code_change(_, Data, _) -> {ok, Data}.

% ===== Private

% Initialize internal data.
create_data() -> create_data(
                   lists:map(fun (Host) ->
                                     erlang:list_to_atom(ecs_util:name() ++ "@" ++ erlang:atom_to_list(Host)) end,
                             ecs_config:nodes_of_role(stat_forward))).
create_data(Forwarders) ->
    case lists:member(stat_forward, ecs_config:roles(ecs_util:host_a())) of
        true -> {[], [erlang:node()], setup_timer()};
        false -> {[], Forwarders, setup_timer()}
    end.

get_stats({S, _, _}) -> S.
get_forwarders({_, F, _}) -> F.

add_stat(Stat, {S, F, T}) -> {[Stat|S], F, T}.
clear_stats({_, F, T}) -> {[], F, T}.

% Create a new stat and record the time of this call as the generation time.
new_stat(Name, Value) -> {Name, Value, erlang:system_time(), erlang:node()}.

setup_timer() ->
    case timer:send_interval(?FORWARD_DELAY, submit) of
        {ok, Ref} -> Ref;
        {error, _} -> exit(timer_setup_failed)
    end.

% Add common data measurements.
add_basic(Stats) ->
    Stats ++
        [new_stat("process_count", erlang:length(erlang:processes()))].

flush(Data) ->
    case ecs_statforwarder:submit(add_basic(get_stats(Data)), randomize(get_forwarders(Data))) of
        ok -> clear_stats(Data);
        {error, no_forwarder} -> io:format("No forwarder available; retaining data.~n"), Data % this will grow without bound...
    end.

% Randomize a list.
randomize([]) -> [];
randomize(L) when is_list(L) ->
    lists:map(fun ({_, E}) -> E end,
              lists:sort(
                lists:map(fun (E) -> {rand:uniform(25 * erlang:length(L)), E} end, L))).
