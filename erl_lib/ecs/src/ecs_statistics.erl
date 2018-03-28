-module(ecs_statistics).
-behavior(gen_server).

-export([start_link/0,
         add/2,
         update/2,
         queued/0]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-define(FORWARD_DELAY, 3000).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(Id, Stat) -> gen_server:cast(?MODULE, {add, Id, Stat}).
update(Id, Value) -> gen_server:cast(?MODULE, {update, Id, Value}).

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

handle_cast({add, Id, Stat}, Data) -> {noreply, add_stat(Id, Stat, Data)};
handle_cast({update, Id, Value}, Data) -> {noreply, update_stat(Id, Value, Data)};
handle_cast(_, Data) -> {noreply, Data}.

handle_info(submit, Data) -> {noreply, flush(Data)};
handle_info(_, Data) -> {noreply, Data}.

code_change(_, Data, _) -> {ok, Data}.

% ===== Private

% Initialize internal data.
create_data() -> create_data(
                   lists:map(fun (Host) ->
                                     erlang:list_to_atom(ecs_util:name()
                                                         ++ "@"
                                                         ++ erlang:atom_to_list(Host)) end,
                             ecs_config:nodes_of_role(stat_forward))).
create_data(Forwarders) ->
    case lists:member(stat_forward, ecs_config:roles(ecs_util:host_a())) of
        true -> {add_basic(dict:new()), [erlang:node()], setup_timer()};
        false -> {add_basic(dict:new()), Forwarders, setup_timer()}
    end.

get_forwarders({_, F, _}) -> F.

add_stat(Id, Stat, {Stats, F, T}) ->
    case dict:is_key(Id, Stats) of
        true ->
            io:format("Duplicate stat ID ~w; refusing to add.~n", [Id]),
            {Stats, F, T};
        false ->
            {dict:store(Id, Stat, Stats), F, T}
    end.

update_stat(Id, Value, {Stats, F, T}) ->
    case dict:is_key(Id, Stats) of
        true ->
            {dict:update(Id,
                         fun (Stat) -> ecs_stat:update(Value, Stat) end,
                         Stats),
             F,
             T};
        false ->
            io:format("Stat ~w does not exist; cannot update.~n", [Id]),
            {Stats, F, T}
    end.

get_stats({S, _, _}) -> S.

clear_stats({S, F, T}) ->
    {dict:filter(fun (_, Stat) -> ecs_stat:continuous(Stat) == true end, S), F, T}.

reset_stats({S, F, T}) ->
    {dict:map(fun (_, Stat) -> ecs_stat:reset(Stat) end, S), F, T}.

setup_timer() ->
    case timer:send_interval(?FORWARD_DELAY, submit) of
        {ok, Ref} -> Ref;
        {error, _} -> exit(timer_setup_failed)
    end.

add_basic(Stats) ->
    add_basic(Stats,
              ["process_count",
               "memory_atom",
               "memory_atom_used"]).

add_basic(Stats, []) -> Stats;
add_basic(Stats, [Id|Rest]) ->
    add_basic(dict:store(Id, ecs_stat:new(Id), Stats), Rest).

% Update common data measurements.
update_basic(Stats) ->
    update_basic(Stats,
              [{"process_count", erlang:length(erlang:processes())},
               {"memory_atom", erlang:memory(atom)},
               {"memory_atom_used", erlang:memory(atom_used)}]).
update_basic(Stats, []) -> Stats;
update_basic(Stats, [{Id, Value}|Rest]) ->
    update_basic(dict:update(Id, fun (Stat) -> ecs_stat:update(Value, Stat) end, Stats), Rest).

flush(Data) ->
    case
        ecs_statforwarder:submit(
          lists:map(fun ({_, V}) -> V end, dict:to_list(update_basic(get_stats(Data)))),
          ecs_util:randomize(get_forwarders(Data)))
    of
        ok -> reset_stats(clear_stats(Data));
        {error, no_forwarder} -> io:format("No forwarder available; retaining data.~n"), Data % this will grow without bound...
    end.
