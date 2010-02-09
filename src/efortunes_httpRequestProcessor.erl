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

-module(efortunes_httpRequestProcessor).

-include("yaws_api.hrl").
-include("efortunes_definitions.hrl").

-export([arg_rewrite/1]).

arg_rewrite(Arg) ->
    case ?YAWS_UTILS:checkCookie(Arg, ?SESSION_COOKIE_NAME) of 
        {error, _} ->
            do_rewrite(Arg);
        {ok, _Session} ->

	    % Check if logins page is requested => forward to main page
	    Req = Arg#arg.req,
	    {abs_path, Path} = Req#http_request.path,	    
	    case lists:member(Path, ["/", "/index.yaws", ?LOGIN_PAGE_URL]) of
		true ->
		    Arg#arg{
		      req = Req#http_request{path = {abs_path, ?MAIN_PAGE_URL}},
		      state = {abs_path, Path}};
		false ->
		    Arg % Remain args untouched
	    end
    end.


%%====================================================================
%% Internal functions
%%====================================================================

%% Accesible pages without being logged in (from app configuration)
login_pages() -> ?LOGIN_PAGES.

do_rewrite(Arg) ->
    Req = Arg#arg.req,
    {abs_path, Path} = Req#http_request.path,

    %% Delete GET PARAMS if exist
    [BasePath|_] = string:tokens(Path, "?"),

    case lists:member(BasePath, login_pages()) of
        true ->
            Arg;
        false ->
            Arg#arg{
              req = Req#http_request{path = {abs_path, ?LOGIN_PAGE_URL}},
              state = {abs_path, Path}}
    end.
