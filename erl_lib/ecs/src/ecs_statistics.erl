-module(ecs_statistics).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-define(FORWARD_DELAY, 1000).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% ===== gen_server

init(_) ->
    io:format("Statistics service started.~n"),
    inets:start(),
    case get_forwarding_info() of
        ForwardingInfo = {influx, Uri, _} ->
            io:format("Data will be forwarded to Influx (~s).~n", [Uri]),
            {ok, {ForwardingInfo, []}, ?FORWARD_DELAY};
        none ->
            io:format("No data will be forwarded.~n"),
            {ok, nil, ?FORWARD_DELAY}
    end.

terminate(Reason, _) -> io:format("Statistics forwarder stopping: ~w.~n", [Reason]).

handle_call(_, _, Data) -> {reply, unknown, Data}.

handle_cast(_, Data) -> {noreply, Data}.

handle_info(timeout, {ForwardingInfo, Stats}) ->
    case forward_data(Stats, ForwardingInfo) of
        ok -> {noreply, {ForwardingInfo, []}, ?FORWARD_DELAY};

        {error, {temporary, Message}} ->
            io:format("Failed to forward data: ~s.~n", [Message]),
            {noreply, {ForwardingInfo, Stats, ?FORWARD_DELAY}};
        {error, {transient, Message}} ->
            io:format("Failed to forward data: ~s.~n", [Message]),
            {stop, fatal_error}
    end;
handle_info(_, Data) -> {noreply, Data}.

code_change(_, Data, _) -> {ok, Data}.

% ===== Private

get_forwarding_info() ->
    case application:get_env(statistics_forwarding) of
        {ok, {Method, Options}} -> get_forwarding_info(Method, dict:from_list(Options));
        {error, _} -> throw(bad_statf_config)
    end.
get_forwarding_info(influx, Options) ->
    {influx,
     dict:fetch(uri, Options),
     base64:encode_to_string(dict:fetch(username, Options) ++ ":" ++ dict:fetch(password, Options))}.

forward_data(Measurements, {influx, Uri, Authorization}) ->
    case ecs_influx:write_all(add_basic(influx, Measurements), Uri, Authorization) of
        ok -> ok;
        {error, 401} -> {error, {permanent, "bad authentication credentials"}};
        {error, Code} -> {error, {transient, io_lib:format("HTTP ~b", [Code])}}
    end.


% Add common data measurements.
add_basic(influx, Measurements) ->
    Measurements ++
        [ecs_influx:new_measurement("process_count", erlang:length(erlang:processes()))].
