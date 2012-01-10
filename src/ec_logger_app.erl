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
    log4erl:conf(log4erl_config()),
    case ec_logger_sup:start_link() of
	{ok, Pid} -> {ok, Pid};
	Error -> Error
    end.

stop(_State) ->
    ok.

stop() ->
    log4erl:info("stopping ec_logger"),
    [application:stop(A) || A <- ?APPS].

%%--------------------------------------------------------------------
%% @doc
%% Get log config of the project
%% creates a full path name to the job file definition.
%% @end
%%--------------------------------------------------------------------
-spec log4erl_config() -> string() | no_return().
log4erl_config() ->
    case application:get_env(?APP, log4erl_config) of
	{ok, Value} -> Value;
	undefined -> throw({error, log4erl_config_not_defined})
    end.
