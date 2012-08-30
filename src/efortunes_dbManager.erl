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

-module(efortunes_dbManager).

-include("efortunes_records.hrl").
-include("efortunes_definitions.hrl").

%% API
-export([create_tables/0, delete_tables/0, insert_examples/0, 
	 startMnesiaNodes/0, stopMnesiaNodes/0, reset/0,
	 create_schema/0, delete_schema/0]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: create()
%% Description: Create database records
%%--------------------------------------------------------------------
create_tables() ->
    mnesia:create_table(user_profile,
			[{disc_copies, ?STORAGE_NODES}, {attributes, record_info(fields, user_profile)}]),
    mnesia:create_table(fortune,
			[{disc_copies, ?STORAGE_NODES}, {attributes, record_info(fields, fortune)}]),
    mnesia:create_table(counter,
			[{disc_copies, ?STORAGE_NODES}, {attributes, record_info(fields, counter)}]).


%%--------------------------------------------------------------------
%% Function: delete()
%% Description: Delete database records
%%--------------------------------------------------------------------
delete_tables() ->
    mnesia:delete_table(fortune),
    mnesia:delete_table(counter).

%%--------------------------------------------------------------------
%% Function: insert_examples()
%% Description: Insert some examples into the DB
%%--------------------------------------------------------------------
insert_examples() ->

    User = #user_profile{
      login="admin",
      name="Administrador",
      encryptedPassword=crypto:sha("admin")
     },
    
    Fortune1 = #fortune{
      author="Mario",
      date=date(),
      time=time(),
      content="Las aceitunas tienen aceite... pero solo un aceite cada aceit-UNA"
     },
    
    Fortune2 = #fortune{
      author="Mario",
      date=date(),
      time=time(),
      content="París es una marujilla de las nuevas tecnologías"
     },

    ?USER_PROFILE_DAO:create(User),
    ?FORTUNE_DAO:create(Fortune1),
    ?FORTUNE_DAO:create(Fortune2),

    io:format("Examples inserted").

%%--------------------------------------------------------------------
%% Function: reset()
%% Description: Restore database
%%--------------------------------------------------------------------
startMnesiaNodes() ->

    io:format("Starting mnesia...~n"),    

    % start mnesia
    mnesia:start(),
    io:format("   Mnesia node: ~p (schema)~n", [node()]),

    % if other storage nodes (different from node()) exists, start them
    lists:foreach(
      fun(Node) -> 
	      spawn(Node, mnesia, start, []),
	      io:format("   Mnesia node: ~p (storage)~n", [Node])
      end,
      lists:filter(fun(N) -> N =/= node() end, ?STORAGE_NODES)    
     ).

stopMnesiaNodes() ->
    
    io:format("Stopping mnesia...~n"),    

    % stop mnesia
    mnesia:stop(),
    io:format("   Mnesia node: ~p (schema)~n", [node()]),
    
    % if other storage nodes (different from node()) exists, stop them
    lists:foreach(
      fun(Node) -> 
	      spawn(Node, mnesia, stop, []),
	      io:format("   Mnesia node: ~p (storage)~n", [Node])
      end,
      lists:filter(fun(N) -> N =/= node() end, ?STORAGE_NODES)    
     ).

reset() ->
    stopMnesiaNodes(),
    delete_schema(),
    create_schema(),
    startMnesiaNodes(),
    timer:sleep(2000), %% FIXME -- time needed for letting mnesia nodes to properly start
    create_tables(),
    insert_examples().

create_schema() ->
    case lists:member(node(), ?STORAGE_NODES) of
	true ->	
	    mnesia:create_schema(?STORAGE_NODES);
	false ->
	    mnesia:create_schema([node()|?STORAGE_NODES])
    end.

%% @spec delete_schema() -> Output
%%
%% @doc This function deletes the schema.
delete_schema() ->
    case lists:member(node(), ?STORAGE_NODES) of
	true ->	
	    mnesia:delete_schema(?STORAGE_NODES);
	false ->
	    mnesia:delete_schema([node()|?STORAGE_NODES])
    end.


%%====================================================================
%% Internal functions
%%====================================================================
