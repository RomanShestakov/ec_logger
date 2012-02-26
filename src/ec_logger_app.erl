-module(ec_logger_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/0, stop/1]).

-define(APP, ec_logger).
-define(APPS, [log4erl, resource_discovery, ?APP]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    [begin application:start(A), io:format("~p~n", [A]) end || A <- ?APPS].

start(_StartType, _StartArgs) ->
    log4erl:conf(ec_logger_util:log4erl_config()),
    case ec_logger_sup:start_link() of
	{ok, Pid} -> {ok, Pid};
	Error -> Error
    end.

stop(_State) ->
    ok.

stop() ->
    log4erl:info("stopping ec_logger"),
    [application:stop(A) || A <- ?APPS].

