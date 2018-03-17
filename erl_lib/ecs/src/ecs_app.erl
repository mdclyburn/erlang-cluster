-module(ecs_app).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% ===== gen_server

init(_) ->
    io:format("Application manager service started.~n"),
    case start_applications() of
        {ok, Started} -> {ok, Started};
        {error, Failed} -> {stop, {failed, Failed}}
    end.

terminate(_, _) -> io:format("Application manager service stopping.~n").

handle_call(_, _, Data) -> {reply, unknown, Data}.

handle_cast(_, Data) -> {noreply, Data}.

handle_info(_, Data) -> {noreply, Data}.

code_change(_, Data, _) -> {ok, Data}.

% ===== Private

start_applications() -> start_applications(
                          ecs_config:applications(
                            ecs_util:host_a()),
                         []).
start_applications([], Started) -> {ok, Started};
start_applications([Application|Rest], Started) ->
    case application:ensure_all_started(Application) of
        {ok, _} -> start_applications(Rest, [Application|Started]);
        {error, _} -> {error, Application}
    end.
