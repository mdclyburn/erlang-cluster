-module(ecs_config_manager).
-behavior(gen_server).

-export([start_link/0]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) -> {ok, nil}.

terminate(_, _) -> ok.

handle_call({get, Application}, _, nil) ->
    {reply,
     ecs_config:nodes_of_application_r(Application),
     nil};
handle_call(_, _, _) -> {reply, unknown, nil}.

handle_cast(_, _) -> {noreply, nil}.

handle_info(_, _) -> {noreply, nil}.

code_change(_, _, _) -> {ok, nil}.
