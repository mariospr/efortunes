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

%%%-------------------------------------------------------------------
%%% File    : efortunes_messagesTrader.erl
%%% Author  : Mariolo <mario@localhost.localdomain>
%%% Description : This server stores messages strings according to each
%%%    language which is supported by our system. In order to easily access
%%%    the localized strings for each message code, we've got an structure 
%%%    based on ets tables, in a way that we have an ets table with the strings
%%%    for each desired message in our system, indexed by a complex key built
%%%    from a language code (which references the desired language) and a
%%%     message code (which references the desired message). 
%%%    
%%%    And here is a visual representation of this structure:
%%%    
%%%                   MessagesTable 
%%%      ------------------------------------
%%%     |          KEY        |     VALUE    |
%%%      ------------------------------------
%%%     | {langCode, msgCode} |   msgString  |
%%%      ------------------------------------
%%%     | {langCode, msgCode} |   msgString  |
%%%      ------------------------------------
%%%     |          ...        |      ...     |
%%%      ------------------------------------
%%%
%%% Created : 18 Apr 2006 by Mariolo <mario@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(efortunes_messagesTrader).

-behaviour(gen_server).

%% API
-export([start_link/0, getMessage/1, getMessage/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("efortunes_definitions.hrl").

-record(state, {messagesTable}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, efortunes_messagesTrader}, 
			  ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Function: getMessage(MessageCode) -> 
%%                             {ok,MessageString} | {error, Reason}
%%
%% Description: Returns an string associated to a message code for the
%%   default language defined by the private function defaultLanguage/1
%%--------------------------------------------------------------------
getMessage(MessageCode) when is_atom(MessageCode) ->
    getMessage(?DEFAULT_MESSAGES_LANGUAGE, MessageCode);
getMessage(_MessageCode) ->
    {internal_error, incorrectParamsError("getMessage(MessageCode)")}.

%%--------------------------------------------------------------------
%% Function: getMessage(MessageCode, LanguageCode) -> 
%%                             {ok,MessageString} | {error, Reason}
%%
%% Description: Returns an string associated to a message code for the
%%              specified language code through the second parameter
%%--------------------------------------------------------------------
getMessage(LanguageCode, MessageCode) 
  when is_atom(LanguageCode), is_atom(MessageCode)  ->   
    gen_server:call(efortunes_messagesTrader,
		    {getMessage, {LanguageCode, MessageCode}});
getMessage(_LanguageCode, _MessageCode) ->
    {internal_error,
     incorrectParamsError("getMessage(LanguageCode, MessageCode)")}.

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

    io:format("Reading message files...~n"),
    MessagesDir = getMessagesDir(),

    % Retrieves a tuple of {LanguageCode, FilePath} tuples
    MessageFiles = 
    case file:list_dir(MessagesDir) of
	{ok, Filenames} ->
	    getMessageFiles(Filenames);
	{error, _Reason} ->
	    % If any error occurred, returns no files
	    []
    end,
    
    % Now, let's read all the language files caching their contents
    % into ets tables as commented above in the header of this file

    % Creates a new table...
    MessagesTable = ets:new(messagesTable, []),

    % ...and fills it with data
    fillMessagesTable(MessagesTable, MessageFiles),

    {ok, #state{messagesTable=MessagesTable}}.           

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({getMessage, _Params={LanguageCode, MessageCode}}, _From, State) ->
    
    % Get Messages table
    MessagesTable = State#state.messagesTable,

    % Get the string for this message code
    case ets:lookup(MessagesTable, {LanguageCode, MessageCode}) of
	[{{LanguageCode, MessageCode}, MessageString}] ->           
	    % Return correct value
	    Reply = MessageString,
	    {reply, Reply, State};

	_ -> % Return an empty string and log the error
	    error_logger:error_msg("Message '~p' not found for language ~p~n",
				   [MessageCode, LanguageCode]),
	    Reply = "",
	    {reply, Reply, State}
    end;

% Unknown request
handle_call(UnknownRequest, _From, State) ->
    Reply = {error, genericMessagesTraderError(
		      ["Unknown request:  ", UnknownRequest])},
    {reply, Reply, State}.

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

getMessagesDir() ->
    case application:get_env(messages_dir) of
	{ok, MessagesDir} when is_list(MessagesDir) -> MessagesDir;
	_ -> ?DEFAULT_MESSAGES_DIR
    end.

% Builds a list of {LanguageName, Filepath} tuples from Filenames list
getMessageFiles(Filenames) ->
    getMessageFiles(Filenames, []).

getMessageFiles([], Acc) ->
    Acc;
getMessageFiles([Filename|Tail], Acc) ->
    case string:tokens(Filename, ".") of % Get language of file
	["messages", Language] -> % Read files like "messages.EN", in example
	    getMessageFiles(Tail, [{list_to_atom(Language), 
				    [getMessagesDir(),"/",Filename]}|Acc]);
	_ ->
	    getMessageFiles(Tail, Acc)
    end.
    
% Fills an ets table with the messages string for each 
% combination of language and message code
fillMessagesTable(MessagesTable, MessageFiles) ->
    
    Fun = fun(MessageFile) ->
	    {LangCode, Filepath} = MessageFile,
        
            % insert messages for a concrete language
	    insertLanguageMessages(MessagesTable, LangCode, Filepath)
    end,
    lists:foreach(Fun, MessageFiles),

    % Return the messages table
    MessagesTable.

% Insert messages for a concrete language
insertLanguageMessages(MessagesTable, LangCode, Filepath) ->
    
    % Get messages tuples from file
    Messages = case file:consult(Filepath) of
	{ok, MessageList} ->
	    MessageList;
	{error, _Reason} ->
	    []
    end,
    
    % Fills the ets table of messages for a concrete language from a
    % list of tuples containing pairs {messageCode, messageString}
    Fun =  fun(Message) ->
            % Adds the messages for a language to the ets table
	    {MessageCode, MessageString} = Message,
	    ets:insert(MessagesTable, {{LangCode, MessageCode},
				       MessageString})
    end,
    lists:foreach(Fun, Messages),
    
    % ... and return it
    MessagesTable.

genericMessagesTraderError(Reason) ->
    error_logger:error_msg(["[MessagesTrader] :: ", Reason, "~n"]).

incorrectParamsError(FunctionSignature) ->
    genericMessagesTraderError(["Incorrect params when calling ", 
				FunctionSignature]).
