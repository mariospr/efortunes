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

-module(efortunes_action_logout).

-include("efortunes_definitions.hrl").
-include("efortunes_records.hrl").

-export([execute/1]).

%%--------------------------------------------------------------------
%% Function: execute(Params) -> 
%%               ok
%%             | {model_error, user_not_found}
%%             | {internal_error, Reason}
%%
%% Parameters:
%%   Params: A tuple containing all needed parameters for the action 
%%
%% Description: executes bussiness logic for the action
%%--------------------------------------------------------------------
execute(_Params={Login}) ->
        
    error_logger:info_msg("Logging out user ~s~n", [Login]), % DEBUG INFO

    case ?USER_PROFILE_DAO:find(Login) of
	{ok, _User_profile} -> 
	    ok;
	{error, not_found} ->
	    {model_error, user_not_found};
	{error, Reason} -> % Unknown error
	    {internal_error, Reason}
    end.
	    
