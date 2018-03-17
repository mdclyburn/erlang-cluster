-module(ecs_statistics).
-behavior(gen_server).

-export([start_link/0,
         record/3,
         queued/0]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-define(FORWARD_DELAY, 3000).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record(update, Name, Value) -> gen_server:cast(?MODULE, {update, Name, Value});
record(overwrite, Name, Value) -> gen_server:cast(?MODULE, {overwrite, Name, Value}).

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

handle_cast({update, Name, Value}, Data) -> {noreply, update_stat(Name, Value, Data)};
handle_cast({overwrite, Name, Value}, Data) -> {noreply, overwrite_stat(Name, Value, Data)};
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
        true -> {dict:new(), [erlang:node()], setup_timer()};
        false -> {dict:new(), Forwarders, setup_timer()}
    end.

get_forwarders({_, F, _}) -> F.

update_stat(Name, Value, {Stats, F, T}) ->
    {dict:update(Name,
                 fun (Stat) -> ecs_stat:modify(Stat, Value) end,
                 ecs_stat:new(Value),
                 Stats),
     F,
     T}.

overwrite_stat(Name, Value, {Stats, F, T}) ->
    {dict:update(Name,
                 fun (Stat) -> ecs_stat:overwrite(Stat, Value) end,
                 ecs_stat:new(Value),
                 Stats),
     F,
     T}.

get_stats({S, _, _}) -> S.
clear_stats({_, F, T}) -> {dict:new(), F, T}.

setup_timer() ->
    case timer:send_interval(?FORWARD_DELAY, submit) of
        {ok, Ref} -> Ref;
        {error, _} -> exit(timer_setup_failed)
    end.

% Add common data measurements.
add_basic(Stats) ->
    add_basic(Stats,
              [{"process_count", erlang:length(erlang:processes())},
               {"memory_atom", erlang:memory(atom)},
               {"memory_atom_used", erlang:memory(atom_used)}]).
add_basic(Stats, []) -> Stats;
add_basic(Stats, [{Name, Value}|Rest]) ->
    record(overwrite, Name, Value),
    add_basic(Stats, Rest).

flush(Data) ->
    case ecs_statforwarder:submit(
           lists:map(
             fun ({Name, Stat}) -> ecs_stat:to_tuple(Name, Stat) end,
             dict:to_list(add_basic(get_stats(Data)))),
           ecs_util:randomize(get_forwarders(Data))) of
        ok -> clear_stats(Data);
        {error, no_forwarder} -> io:format("No forwarder available; retaining data.~n"), Data % this will grow without bound...
    end.
