%%----------------------------------------------------------------------
%%
%% Copyright (C) 2006 Mario Sánchez Prada (mario@mariospr.org)
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
%% Author: Mario Sánchez Prada (mario@mariospr.org)
%%----------------------------------------------------------------------

%% @copyright 2006 Mario Sánchez Prada (mario@mariospr.org)
%% @author Mario Sánchez Prada (mario@mariospr.org)
%% @version 0.1 beta, {@date} {@time}.
%% @end

-module(efortunes_facade).

-behaviour(gen_server).

-include("efortunes_records.hrl").
-include("efortunes_definitions.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([login/2, logout/1, add_fortune/2, add_fortune/4, remove_fortune/1, update_fortune/1, find_fortune/1, find_all_fortunes/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% API
%% 
%% Exported functions:
%%    + login(Login, Password)
%%    + logout(Login)
%%    + add_fortune(Author, Content)
%%    + add_fortune(Author, Content, Date, Time)
%%    + remove_fortune(Id)
%%    + update_fortune(Updated_fortune)
%%    + find_fortune(Id)
%%    + find_all_fortunes()
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

% login (Login, Password)
login (Login, Password)  when is_list(Login), is_list(Password) ->    
    gen_server:call(?SERVER,
		   {login, {Login, Password}});
% Invalid params
login (_Login, _Password) ->
    {internal_error, incorrectParamsError("login(Login, Password)")}.

% logout(Login)
logout(Login)  when is_list(Login) ->    
    gen_server:call(?SERVER,
		   {logout, {Login}});
% Invalid params
logout(_Login) ->
    {internal_error, incorrectParamsError("logout(Login)")}.

% add_fortune(Author, Content)
add_fortune(Author, Content)  when is_list(Author), is_list(Content) ->    
    gen_server:call(?SERVER,
		   {add_fortune, {Author, Content, date(), time()}});
% Invalid params
add_fortune(_Author, _Content) ->
    {internal_error, incorrectParamsError("add_fortune(Author, Content)")}.


% add_fortune(Author, Content, Date, Time)
add_fortune(Author, Content, Date, Time)  when is_list(Author), is_list(Content) ->    
    gen_server:call(?SERVER,
		   {add_fortune, {Author, Content, Date, Time}});
% Invalid params
add_fortune(_Author, _Content, _Date, _Time) ->
    {internal_error, incorrectParamsError("add_fortune(Author, Content, Date, Time)")}.


% remove_fortune
remove_fortune(Id)  when is_integer(Id) ->    
    gen_server:call(?SERVER,
		   {remove_fortune, {Id}});
% Invalid params
remove_fortune(_Id) ->
    {internal_error, incorrectParamsError("remove_fortune(Id)")}.


% update_fortune
update_fortune(Updated_fortune)  when is_record(Updated_fortune, fortune) ->    
    gen_server:call(?SERVER,
		   {update_fortune, {Updated_fortune}});
% Invalid params
update_fortune(_Updated_fortune) ->
    {internal_error, incorrectParamsError("update_fortune(Fortune)")}.

% find fortune
find_fortune(Id)  when is_integer(Id) ->    
    gen_server:call(?SERVER,
		   {find_fortune, {Id}});
% Invalid params
find_fortune(_Id) ->
    {internal_error, incorrectParamsError("find_fortune(Id)")}.

% find all fortunes
find_all_fortunes() ->    
    gen_server:call(?SERVER,
		   {find_all_fortunes, {}}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

% login
handle_call({login, Params={_Login, _Password}}, _From, _State) ->

    Reply = ?MNESIA_ACTION_PROCESSOR:execute(transactional_action, 
	      efortunes_action_login, Params),
    {reply, Reply, _State};

% logout
handle_call({logout, Params={_Login}}, _From, _State) ->

    Reply = ?MNESIA_ACTION_PROCESSOR:execute(transactional_action, 
	      efortunes_action_logout, Params),
    {reply, Reply, _State};

% add fortune
handle_call({add_fortune, Params={_Author, _Content, _Date, _Time}}, _From, _State) ->

    Reply = ?MNESIA_ACTION_PROCESSOR:execute(transactional_action, 
	      efortunes_action_addFortune, Params),
    {reply, Reply, _State};


% remove fortune
handle_call({remove_fortune, Params={_Id}}, _From, _State) ->

    Reply = ?MNESIA_ACTION_PROCESSOR:execute(transactional_action, 
	      efortunes_action_removeFortune, Params),
    {reply, Reply, _State};


% update fortune
handle_call({update_fortune, Params={_Updated_fortune}}, _From, _State) ->

    Reply = ?MNESIA_ACTION_PROCESSOR:execute(transactional_action, 
	      efortunes_action_updateFortune, Params),
    {reply, Reply, _State};

% find fortune
handle_call({find_fortune, Params={_Id}}, _From, _State) ->

    Reply = ?MNESIA_ACTION_PROCESSOR:execute(non_transactional_action, 
	      efortunes_action_findFortune, Params),
    {reply, Reply, _State};

% find all fortunes
handle_call({find_all_fortunes, Params}, _From, _State) ->

    Reply = ?MNESIA_ACTION_PROCESSOR:execute(non_transactional_action, 
	      efortunes_action_findAllFortunes, Params),
    {reply, Reply, _State};

% Unknown request
handle_call(UnknownRequest, _From, _State) ->
    Reply = {internal_error, genericUserFacadeError(["Unknown request:  ", UnknownRequest])},
    {reply, Reply, _State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

genericUserFacadeError(Reason) ->
    error_logger:error_msg(["[efortunes_facade] :: ", Reason, "~n"]).

incorrectParamsError(FunctionSignature) ->
    genericUserFacadeError(["Incorrect params when calling ", FunctionSignature]).
