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

-module(efortunes_action_addFortune).

-export([execute/1]).

-include("efortunes_records.hrl").
-include("efortunes_definitions.hrl").

execute(_Params={Author, Content, Date, Time}) ->

    error_logger:info_msg("Adding a new fortune: ~p"),

    % Store Information
    case ?FORTUNE_DAO:create(
	   #fortune{author=Author, 
		    content=Content, 
		    date=Date, 
		    time=Time}) of
	{ok, Fortune} ->
	    {ok, Fortune};
	{error, duplicated_tuple} ->
	    {model_error, fortune_already_exists};
	{error, Reason} ->
	    {internal_error, Reason}
    end.
