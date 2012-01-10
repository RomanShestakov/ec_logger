
-module(ec_logger_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    EC_Logger = {'ec_logger', {'ec_logger', start_link, []}, permanent, 2000, supervisor, ['ec_logger']},
    {ok, {{one_for_one, 5, 10}, [EC_Logger]}}.

