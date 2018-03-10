-module(ecs_statistics).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

-define(FORWARD_DELAY, 500).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% ===== gen_server

init(_) ->
    io:format("Statistics service started.~n"),
    case get_forwarding_info() of
        Data = {influx, Uri, _} ->
            io:format("Data will be forwarded to Influx (~s).~n", [Uri]),
            {ok, Data};
        none ->
            io:format("No data will be forwarded.~n"),
            {ok, nil}
    end.


terminate(Reason, _) -> io:format("Statistics forwarder stopping: ~w.~n", [Reason]).

handle_call(_, _, Data) -> {reply, unknown, Data}.

handle_cast(_, Data) -> {noreply, Data}.

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
