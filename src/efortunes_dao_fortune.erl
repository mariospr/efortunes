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

-module(efortunes_dao_fortune).

-include("efortunes_definitions.hrl").
-include("efortunes_records.hrl").

%% API
-export([create/1, update/1, find/1, remove/1, find_all/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
create(Fortune) ->
    F = fun() ->		
		NewFortune = Fortune#fortune{id = ?MNESIA_UTILS:next_secuence_number(fortune)},
		?MNESIA_UTILS:check_not_exists(fortune, NewFortune#fortune.id),
		{mnesia:write(NewFortune), NewFortune}
        end,
    case mnesia:transaction(F) of
        {atomic, {ok, NewFortune}}      -> {ok, NewFortune};
        {aborted, {Reason, _}} -> {error, Reason}
    end.    

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
update(Fortune) ->
    F = fun() ->
		?MNESIA_UTILS:check_not_null(Fortune#fortune.id),
		?MNESIA_UTILS:check_exists(fortune, Fortune#fortune.id),
		mnesia:write(Fortune)
        end,
    case mnesia:transaction(F) of
        {atomic, ok}      -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
find(ID) ->
    F = fun() ->
                 mnesia:read({fortune, ID})
        end,
    case mnesia:transaction(F) of
        {atomic, []}      -> {error, not_found};
        {atomic, [Fortune]}  -> {ok, Fortune};
        {aborted, Reason} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
remove(ID) ->
    F = fun() ->
                 ?MNESIA_UTILS:check_exists(fortune, ID),
                 mnesia:delete({fortune, ID})
        end,
    case mnesia:transaction(F) of
        {atomic, ok}      -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
find_all() ->
    F = fun() ->
                 WP = mnesia:table_info(fortune, wild_pattern),
                 [Fortune || Fortune <- mnesia:match_object(WP)]
        end,
    case mnesia:transaction(F) of
        {atomic, Result}  -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.
