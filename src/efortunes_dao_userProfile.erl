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

-module(efortunes_dao_userProfile).

-include("efortunes_records.hrl").
-include("efortunes_definitions.hrl").

%% API
-export([create/1, update/1, find/1, remove/1, find_all/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
create(User_Profile) ->
    F = fun() ->		
		?MNESIA_UTILS:check_not_exists(user_profile, User_Profile#user_profile.login),
		{mnesia:write(User_Profile), User_Profile}
        end,
    case mnesia:transaction(F) of
        {atomic, {ok, User_Profile}}      -> {ok, User_Profile};
        {aborted, {Reason, _}} -> {error, Reason}
    end.    

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
update(User_Profile) ->
    F = fun() ->
		?MNESIA_UTILS:check_exists(user_profile, User_Profile#user_profile.login),
		mnesia:write(User_Profile)
        end,
    case mnesia:transaction(F) of
        {atomic, ok}      -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
find(Login) ->
    F = fun() ->
                 mnesia:read({user_profile, Login})
        end,
    case mnesia:transaction(F) of
        {atomic, []}      -> {error, not_found};
        {atomic, [User_Profile]}  -> {ok, User_Profile};
        {aborted, Reason} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
remove(Login) ->
    F = fun() ->
                 ?MNESIA_UTILS:check_exists(user_profile, Login),
                 mnesia:delete({user_profile, Login})
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
                 WP = mnesia:table_info(user_profile, wild_pattern),
                 [User_Profile || User_Profile <- mnesia:match_object(WP)]
        end,
    case mnesia:transaction(F) of
        {atomic, Result}  -> {ok, Result};
        {aborted, Reason} -> {error, Reason}
    end.
