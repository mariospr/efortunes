%%----------------------------------------------------------------------
%%
%% Copyright (C) 2006 Mario Sánchez Prada (msanchez@igalia.com)
%%
%% This program is free software; you can redistribute it and/or
%% modify it under the terms of version 2 of the GNU General Public
%% License as published by the Free Software Foundation.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% General Public License for more details.
%%
%% You should have received a copy of the GNU General Public
%% License along with this program; if not, write to the
%% Free Software Foundation, Inc., 59 Temple Place - Suite 330,
%% Boston, MA 02111-1307, USA.
%%
%% Author: Mario Sánchez Prada (msanchez@igalia.com)
%%----------------------------------------------------------------------

%% @copyright 2006 Mario Sánchez Prada (msanchez@igalia.com)
%% @author Mario Sánchez Prada (msanchez@igalia.com)
%% @version 0.1 beta, {@date} {@time}.
%% @end

-module(efortunes_super).

-behaviour(supervisor).

-export([start_link/1,init/1]).

-define(SERVER, ?MODULE).

-include("efortunes_definitions.hrl").


%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Mode) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Mode]).


%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init(_) ->

    % Init crypto system
    io:format("~nStarting crypto system...~n"),
    crypto:start(),

    % Init Mnesia database system
    io:format("Trying to start mnesia...~n"),
    case ?EFORTUNES_DATABASE_MANAGER:create_schema() of
	{error, _Error} -> % Schema already created
	    ?EFORTUNES_DATABASE_MANAGER:startMnesiaNodes(),
	    io:format("Mnesia started~n");
	ok -> % Create a new schema
	    ?EFORTUNES_DATABASE_MANAGER:startMnesiaNodes(),
	    io:format("[WARNING] :: Mnesia started, but DB will be probably empty~n");
	Other ->
	    io:format("[ERROR] :: Mnesia NOT started: ~w~n",[Other])
    end,

    % Init logging system
    io:format("Initializing logging system...~n"),
    Logdir = getLogsDir(),
    LogPrefix = getLogPrefix(),
    {{Y,Mon,D},{H,Min,S}} = calendar:local_time(),
    Logfilename = [Logdir, io_lib:format("/~s-~p_~p_~p-~p_~p_~p.log", [LogPrefix, Y, Mon, D, H, Min, S])],
    error_logger:logfile({open, Logfilename}),
    error_logger:tty(getLogTTY()),

    % Init supervisor starting server facades
    io:format("Starting main supervisor...~n"),   

    EfortunesFacadeChild = 
	{?EFORTUNES_FACADE, 
	 {?EFORTUNES_FACADE, start_link, []},
	 permanent, brutal_kill, worker, [?EFORTUNES_FACADE]},

    MessagesTraderChild = 
	{?MESSAGES_TRADER,
	 {?MESSAGES_TRADER,start_link,[]},
	 permanent,2000,worker,[?MESSAGES_TRADER]},   


    {ok,{{one_for_one,1,60},
          [EfortunesFacadeChild, MessagesTraderChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================

getLogsDir() ->
    case application:get_env(logs_dir) of
	{ok, LogsDir} when is_list(LogsDir) -> LogsDir;
	_ -> ?DEFAULT_LOGS_DIR
    end.

getLogPrefix() ->
    case application:get_env(log_prefix) of
	{ok, LogPrefix} when is_list(LogPrefix) -> LogPrefix;
	_ -> ?DEFAULT_LOG_PREFIX
    end.

getLogTTY() ->
    case application:get_env(log_tty) of
	{ok, LogTTY} when is_atom(LogTTY) -> LogTTY;
	_ -> ?DEFAULT_LOG_TTY
    end.
