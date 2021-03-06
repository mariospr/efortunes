%%----------------------------------------------------------------------
%%
%% Copyright (C) 2006 Mario S�nchez Prada (mario@mariospr.org)
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
%% Author: Mario S�nchez Prada (mario@mariospr.org)
%%----------------------------------------------------------------------

%% @copyright 2006 Mario S�nchez Prada (mario@mariospr.org)
%% @author Mario S�nchez Prada (mario@mariospr.org)
%% @version 0.1 beta, {@date} {@time}.
%% @end

-module(efortunes_action_login).

-include("efortunes_definitions.hrl").
-include("efortunes_records.hrl").

-export([execute/1]).

%%--------------------------------------------------------------------
%% Function: execute(Params) -> 
%%               {ok, LoginResult}
%%             | {model_error, user_not_found}
%%             | {model_error, incorrect_password}
%%             | {internal_error, Reason}
%%
%% Parameters:
%%   Params: A tuple containing all needed parameters for the action 
%%
%% Description: executes bussiness logic for the action
%%--------------------------------------------------------------------
execute({Login, ClearPassword}) ->
        
    error_logger:info_msg("Logging in user ~s~n", [Login]), % DEBUG INFO

    case ?USER_PROFILE_DAO:find(Login) of
	{ok, UserProfile} -> 
	    % Check password
	    EncryptedPassword = crypto:sha(ClearPassword),
	    if 
		UserProfile#user_profile.encryptedPassword == EncryptedPassword  ->

		    % Return a login_result record
		    LoginResult = #login_result{
		      login = Login, 
		      name = UserProfile#user_profile.name
		     },

		    error_logger:info_msg("User logged in: ~p~n", [LoginResult]), % DEBUG INFO

		    {ok, LoginResult};

		true ->
		    error_logger:info_msg("User NOT logged in: Incorrect password~n"), % DEBUG INFO
		    {model_error, incorrect_password}
	    end;		    
	{error, not_found} ->
	    error_logger:info_msg("User NOT logged in: User not found~n"), % DEBUG INFO
	    {model_error, user_not_found};
	{error, Reason} -> % Unknown error
	    error_logger:info_msg("User NOT logged in: ~p~n", [Reason]), % DEBUG INFO
	    {internal_error, Reason}
    end.
	    
