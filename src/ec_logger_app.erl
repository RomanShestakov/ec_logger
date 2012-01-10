-module(ec_logger_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-define(APPS, [log4erl, resource_discovery]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    [begin application:start(A), io:format("~p~n", [A]) end || A <- ?APPS].

start(_StartType, _StartArgs) ->
    log4erl:conf(ec_util:log4erl_config()),
    log4erl:info("starting ec_logger..."),
    case ec_logger_sup:start_link() of
	{ok, Pid} -> {ok, Pid};
	Error -> Error
    end.

stop(_State) ->
    ok.

stop() ->
    log4erl:info("stopping ec_logger"),
    [application:stop(A) || A <- ?APPS].
