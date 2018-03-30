-module(ecs_statforwarder).
-behavior(gen_server).

-export([start_link/0,
         submit/2]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-define(FORWARD_DELAY, 10000).

% ===== Public

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

submit([], _) -> ok;
submit(_, []) -> {error, no_forwarder};
submit(Stats, [Forwarder|Rest]) when is_list(Stats) ->
    try
        case Forwarder == ecs_util:host_a() of
            true -> gen_server:call(?MODULE, {add, Stats});
            false -> gen_server:call({?MODULE, Forwarder}, {add, Stats})
        end
    catch
        exit:_ -> submit(Stats, Rest)
    end.

% ===== gen_server

init(_) ->
    io:format("Statistics forwarding service started.~n"),
    inets:start(),
    case get_forwarding_info() of
        ForwardingInfo = {influx, Uri, _} ->
            io:format("Data will be forwarded to Influx (~s).~n", [Uri]),
            {ok, create_data(ForwardingInfo)};
        none ->
            io:format("No data will be forwarded.~n"),
            {ok, create_data(nil)}
    end.

terminate(Reason, _) -> io:format("Statistics forwarder stopping: ~w.~n", [Reason]).

handle_call({add, Stats}, _, Data) -> {reply, ok, add_stats(Stats, Data)};
handle_call(Request, _, Data) -> io:format("Unhandled call: ~s.~n", [Request]), {reply, unknown, Data}.

handle_cast(Request, Data) -> io:format("Unhandled cast: ~w~n", [Request]), {noreply, Data}.

handle_info(forward, Data) ->
    case forward_data(get_stats(Data), get_forwarding_info(Data)) of
        ok -> {noreply, clear_stats(Data)};

        {error, {transient, Message}} ->
            io:format("Failed to forward data: ~s.~n", [Message]),
            {noreply, Data};
        {error, {permanent, Message}} ->
            io:format("Failed to forward data: ~s.~n", [Message]),
            {stop, fatal_error}
    end;
handle_info(Info, Data) -> io:format("Unhandled info: ~s.~n", [Info]), {noreply, Data}.

code_change(_, Data, _) -> {ok, Data}.

% ===== Private

% Create initial data.
create_data(ForwardingInfo) -> {ForwardingInfo, [], setup_timer()}.

get_forwarding_info({ForwardingInfo, _, _}) -> ForwardingInfo.

% Add a new stat to be reported.
add_stats(Stats, {F, S, T}) -> {F, Stats ++ S, T}.

get_stats({_, Stats, _}) -> Stats.

clear_stats({F, _, T}) -> {F, [], T}.

setup_timer() ->
    case timer:send_interval(?FORWARD_DELAY, forward) of
        {ok, Ref} -> Ref;
        {error, _} -> exit(timer_setup_failed)
    end.

get_forwarding_info() ->
    case application:get_env(statistics_forwarding) of
        {ok, {Method, Options}} -> get_forwarding_info(Method, dict:from_list(Options));
        {error, _} -> throw(bad_statf_config);
        _ -> none
    end.
get_forwarding_info(influx, Options) ->
    {influx,
     dict:fetch(uri, Options),
     base64:encode_to_string(dict:fetch(username, Options) ++ ":" ++ dict:fetch(password, Options))}.

% Forward stats to Influx for reporting.
forward_data(Stats, {influx, Uri, Authorization}) ->
    case ecs_influx:write(Stats, Uri, Authorization) of
        ok -> ok;
        {error, 401} -> {error, {permanent, "bad authentication credentials"}};
        {error, Info} -> {error, {transient, io_lib:format("~w", [Info])}}
    end;
% Dump data.
forward_data(_, nil) -> ok.
