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

-module(efortunes_httpSessionManager).

-define(LOGIN_SESSION_ATTRIBUTE, "login").
-define(NAME_SESSION_ATTRIBUTE, "name").
-define(STATIC_DATA_SESSION_ATTRIBUTE, "staticData").

-include("efortunes_definitions.hrl").
-include("efortunes_records.hrl").

% Stored info about the session
-record(session_opaque, {
          login,
          name,
	  staticData
}).

-export([login/4, logout/2]).

-export([getLogin/1, getName/1, setName/2, 
	 getStaticData/1, setStaticData/2]).


%-------------------------------------%
% Delegate functions handling session %
%-------------------------------------%

login(Args, Login, Password, URL) ->
    case ?EFORTUNES_FACADE:login(Login, Password) of
        
        {ok, LoginResult} ->

            % build an opaque
            SessionOpaque = #session_opaque{
              login = LoginResult#login_result.login,
              name  = LoginResult#login_result.name
             },

            Result = registerAndRedirect(SessionOpaque, URL),

	    error_logger:info_msg("Cookie set. Checking (~p): ~p", 
				  [?SESSION_COOKIE_NAME, 
				   ?YAWS_UTILS:checkCookie(Args, ?SESSION_COOKIE_NAME)]), % DEBUG INFO

	    Result;

        {Error, Reason} ->
            {Error, Reason}
    end.

logout(Args, Login) ->
    case ?EFORTUNES_FACADE:logout(Login) of
        
        ok ->

            case ?YAWS_UTILS:getCookieVal(
                   Args, ?SESSION_COOKIE_NAME) of

                [] ->
                    {internal_error, session_cookie_not_found};

                Cookie ->
                    
                    % Redirect to main page
                    RedirectURL = {abs_path, ?LOGIN_PAGE_URL},
                    yaws_api:delete_cookie_session(Cookie),
                    Result = {redirect_local, RedirectURL},

		    error_logger:info_msg("Session cookie successfully deleted~n"), % DEBUG INFO

                    {ok, Result}
            end;

        {Error, Reason} ->
            {Error, Reason}
    end.

%------------------------------------%
% Functions for managing stored info %
%------------------------------------%

getLogin(Args) ->
    Fun = fun (S) -> S#session_opaque.login end,
    getSessionInfo(Args, Fun).

getName(Args) ->
    Fun = fun (S) -> S#session_opaque.name end,
    getSessionInfo(Args, Fun).

setName(Args, Name) ->
    Fun = fun (S) -> S#session_opaque{name=Name} end,
    setSessionInfo(Args, Fun).

getStaticData(Args) ->
    Fun = fun (S) -> S#session_opaque.staticData end,
    getSessionInfo(Args, Fun).

setStaticData(Args, StaticData) ->
    Fun = fun (S) -> S#session_opaque{staticData=StaticData} end,
    setSessionInfo(Args, Fun).


%%-------------------%%
%% Private functions %%
%%-------------------%%

registerAndRedirect(SessionOpaque, URL) ->

    % Register session into the session server
    Cookie = yaws_api:new_cookie_session(SessionOpaque),
            
    % Redirect
    RedirectURL = 
        case URL of
            undefined -> {abs_path, ?MAIN_PAGE_URL};
            _ -> {abs_path, URL}
        end,    
    Result = [{redirect_local, RedirectURL}, 
              yaws_api:setcookie(?SESSION_COOKIE_NAME, Cookie)],

    error_logger:info_msg("Cookie set. Redirect created: ~p~n", [Result]), % DEBUG INFO

    {ok, Result}.

getSessionInfo(Args, Which_fun) ->
    case ?YAWS_UTILS:checkCookie(Args, 
				 ?SESSION_COOKIE_NAME) of 
        {error, cookie_not_found} ->
            session_cookie_not_found;
        {error, OtherError} ->
            {internal_error, OtherError};
        {ok, SessionOpaque} ->
            Result = Which_fun(SessionOpaque),
	    
	    error_logger:info_msg("Retrieving info from cookies: ~p~n", [Result]), % DEBUG INFO

	    Result
    end.

setSessionInfo(Args, Which_fun) ->
    case ?YAWS_UTILS:checkCookie(Args, 
				 ?SESSION_COOKIE_NAME) of
        {error, cookie_not_found} ->
            {internal_error, session_cookie_not_found};
        {error, OtherError} ->
            {internal_error, OtherError};
        {ok, OldOpaque} ->
            NewOpaque = Which_fun(OldOpaque),

	    error_logger:info_msg("Replacing info from cookies: ~p~n", [NewOpaque]), % DEBUG INFO

            ?YAWS_UTILS:replaceCookie(Args, 
				      ?SESSION_COOKIE_NAME, 
				      NewOpaque)
    end.

