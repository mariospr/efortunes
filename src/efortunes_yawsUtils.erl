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

-module(efortunes_yawsUtils).

-include("efortunes_definitions.hrl").
-include("yaws_api.hrl").

-export([getCookieVal/2, checkCookie/2, replaceCookie/3, getFieldValue/2, 
	 buildValidationErrorRedirect/3, buildValidationErrorRedirect/4,
	 buildErrorRedirect/3, buildErrorRedirect/4, buildSuccessRedirect/0]).

%%COOKIES

%% Get a cookie from args by name
getCookieVal(Arg, CookieName) ->
    H = Arg#arg.headers,
    yaws_api:find_cookie_val(CookieName, H#headers.cookie).


%% Get an opaque record associated to a Cookie
checkCookie(Arg, CookieName) ->
    case getCookieVal(Arg, CookieName) of
        [] ->
            {error, cookie_not_found};
        Cookie ->
            yaws_api:cookieval_to_opaque(Cookie)
    end.

%% Replace the opaque of a cookie with a newer one
replaceCookie(Arg, CookieName, NewOpaque) ->
    case getCookieVal(Arg, CookieName) of
        [] ->
            {error, cookie_not_found};
        Cookie ->
            yaws_api:replace_cookie_session(Cookie, NewOpaque)
    end.


%% MISC

% Used for getting Params and Fields from GET/POST http requests
getFieldValue(FieldsList, FieldName) ->
    case (lists:keysearch(FieldName, 1, FieldsList)) of
        false ->
            undefined;
        {value, {FieldName, Value}} ->
            if 
                is_atom(Value) ->
                    atom_to_list(Value);
                true ->
                    Value
            end            
    end.

% used for building validation errors
buildValidationErrorRedirect(FormPageURL, ErrorCode, InputName) ->
    buildValidationErrorRedirect(FormPageURL, ErrorCode, InputName, []).

buildValidationErrorRedirect(FormPageURL, ErrorCode, Input, FormValues) ->

    FormValuesParams = case FormValues of
	[] -> "";
	[{_InputName, _InputValue}|_] -> 
	    Fun = fun({InputName, InputValue}) ->
		    ["&", InputName, "=", yaws_api:url_encode(InputValue)]
	    end,
	    lists:map(Fun, FormValues)
    end,

    RedirectURL = {abs_path, lists:flatten(
			       [FormPageURL, "?", 
				"error=", atom_to_list(ErrorCode),
				"&input=", Input, FormValuesParams])},
    {redirect_local, RedirectURL}.

% Used to build error redirections from URL, ErrorType and Reason, logging error to disk
buildErrorRedirect(ErrorPageURL, ErrorType, Reason) ->
    buildErrorRedirect(ErrorPageURL, ErrorType, Reason, []).
    

% Same than the previous function, but accepting extra params for the URL
buildErrorRedirect(ErrorPageURL, ErrorType, Reason, ExtraParams) ->

    {Prefix, MsgCode} = case ErrorType of
	model_error -> 
            {"[Model Error]", atom_to_list(Reason)};
        internal_error -> 
            {"[Internal Error]", atom_to_list(internal_error)}
    end,

    % Log error first
    error_logger:error_msg("~s :: ~s~n", [Prefix, Reason]),


    % Build redirect
    RedirectURL = {abs_path, lists:flatten([ErrorPageURL, "?error=", MsgCode, "&", ExtraParams])},
    {redirect_local, RedirectURL}.


% build a redirect to the success page
buildSuccessRedirect() ->
    RedirectURL = {abs_path, ?SUCCESS_PAGE_URL},
    {redirect_local, RedirectURL}.
