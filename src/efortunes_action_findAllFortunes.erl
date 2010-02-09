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

-module(efortunes_action_findAllFortunes).

-export([execute/1]).

-include("efortunes_definitions.hrl").

execute(_Params) ->

    error_logger:info_msg("Finding all fortunes"),

    % Store Information
    case ?FORTUNE_DAO:find_all() of
	{ok, Fortunes} ->
	    error_logger:info_msg("Found all fortunes: ~p", [Fortunes]),
	    {ok, Fortunes};
	{error, not_found} ->
	    error_logger:info_msg("No fortunes were found"),
	    {model_error, fortune_not_found};
	{error, Reason} ->
	    error_logger:info_msg("An error has ocurred"),
	    {internal_error, Reason}
    end.
