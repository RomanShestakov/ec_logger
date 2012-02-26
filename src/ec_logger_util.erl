-module(ec_logger_util).
-export([
	 log4erl_config/0,
	 get_log_dir/0,
	 get_reports_dir/0
	 %get_slots/0
	]).

-define(APP, ec_logger).

%%--------------------------------------------------------------------
%% @doc
%% Get log config of the project
%% @end
%%--------------------------------------------------------------------
-spec log4erl_config() -> string() | no_return().
log4erl_config() ->
    case application:get_env(?APP, log4erl_config) of
	{ok, Value} -> Value;
	undefined -> throw({error, log4erl_config_not_defined})
    end.

-spec get_log_dir() -> string() | no_return().
get_log_dir() ->
    case application:get_env(?APP, log) of
	{ok, Value} -> Value;
	undefined -> throw({error, log_not_defined})
    end.

-spec get_reports_dir() -> string() | no_return().
get_reports_dir() ->
    case application:get_env(?APP, reports) of
	{ok, Value} -> Value;
	undefined -> throw({error, reports_not_defined})
    end.
				   
	    
    
