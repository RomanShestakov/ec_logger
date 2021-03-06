%%%-------------------------------------------------------------------
%%% @author Roman Shestakov <>
%%% @copyright (C) 2012, Roman Shestakov
%%% @doc
%%%
%%% @end
%%% Created : 10 Jan 2012 by Roman Shestakov <>
%%%-------------------------------------------------------------------
-module(ec_logger).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(LOGGER, ec_logger).

-record(state, {logs = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    log4erl:info("starting ec_logger on: ~p", [node()]),
    %% publish logger through resource_discovery
    resource_discovery:add_local_resource_tuple({?LOGGER, self()}),
    %% synch resources
    resource_discovery:trade_resources(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({open_logger, Name, RunDate, Flag}, _From, #state{logs = Logs} = State) ->
    %% open logger for a given file
    LogName = log_name(Name, RunDate),
    case proplists:get_value(LogName, Logs) of
	undefined ->
	    %% if log for this file is not open - open it, otherwise do nothing
	    log4erl:debug("opening logger ~p", [LogName]),
	    {ok, Log} = disk_log:open([{name, LogName}, {format, external}, {repair, truncate},  {file, log_path(Name, RunDate, Flag)}]),
	    %% add new open log to a list of open logs
	    {reply, ok, State#state{logs = Logs ++ [{LogName, Log}]}};
	_Other -> 
	    {reply, ok, State}
    end;
handle_call({close_logger, Name, RunDate}, _From, #state{logs = Logs} = State) ->
    LogName = log_name(Name, RunDate),
    case proplists:get_value(LogName, Logs) of
	undefined ->
	    log4erl:error("can't close logger ~p", [LogName]),
	    {reply, {error, log_not_open, LogName}, State};
	Log -> 
	    log4erl:debug("closing logger ~p", [LogName]),
	    disk_log:close(Log),
	    {reply, ok, State#state{logs = proplists:delete(LogName, Logs)}}
    end;
handle_call({stdout, Name, RunDate, Data}, _From, #state{logs = Logs} = State) ->
    LogName = log_name(Name, RunDate),
    Log = proplists:get_value(LogName, Logs),
    log4erl:debug("~p, ~p, ~p",[Name, RunDate, Data]),
    disk_log:blog(Log, Data),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
%% handle_cast({open_logger, Name, RunDate}, State) ->
%%     %{ok, _Log} = disk_log:open([{name, log_name(Name, RunDate)}, {file, log_path(Name, RunDate)}]),
%%     {noreply, State};
%% handle_cast({stdout, Name, RunDate, Data}, State) ->
%%     log4erl:error("~p , ~p, ~p",[Name, RunDate, Data]),
%%     {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    log4erl:info("ec_logger is shutting down on node ~p", [node()]),
    %% make logger resource unavailable
    resource_discovery:delete_local_resource_tuple({?LOGGER, self()}),
    resource_discovery:trade_resources(),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
 
%%--------------------------------------------------------------------
%% @doc
%% get name of logger for a given job name and run_date
%% @end
%%--------------------------------------------------------------------
-spec log_name(binary(), atom()) -> atom().
log_name(Name, RunDate) ->
    list_to_atom(binary_to_list(Name) ++ "_" ++ ec_time_fns:date_to_string(RunDate)).

%%--------------------------------------------------------------------
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec log_path(binary(), atom(), atom()) -> string().
log_path(Name, RunDate, undefined) ->
    Dir = filename:join([ec_logger_util:get_log_dir(), ec_time_fns:date_to_string(RunDate), binary_to_list(Name)]),
    filelib:ensure_dir(Dir),
    Dir;
log_path(Name, RunDate, r) ->
    Dir = filename:join([ec_logger_util:get_reports_dir(), ec_time_fns:date_to_string(RunDate), binary_to_list(Name)]),
    filelib:ensure_dir(Dir),
    Dir.

