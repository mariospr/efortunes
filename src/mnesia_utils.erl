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

-module(mnesia_utils).

%% API
-export([check_not_null/1, check_boolean/1, check_not_exists/2, 
	 check_exists/2, next_secuence_number/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: check_not_null(Param) ->
%%               ok
%%             | {aborted, Reason}
%%
%% Parameters:
%%   Param: The parameter to be checked
%%
%% Description: Check if a parameter can be considered as a database NULL
%%--------------------------------------------------------------------
check_not_null([])               -> mnesia:abort(empty_parameter);
check_not_null(Param) when is_list(Param)   -> ok;
check_not_null(Param) when is_number(Param) -> ok;
check_not_null(Param) when is_binary(Param) -> ok;
check_not_null(_)                -> mnesia:abort(invalid_list_parameter).

%%--------------------------------------------------------------------
%% Function: check_boolean(Param) ->
%%               ok
%%             | {aborted, Reason}
%%
%% Parameters:
%%   Param: The parameter to be checked
%%
%% Description: Check if a parameter can be considered a boolean
%%--------------------------------------------------------------------
check_boolean(true)  -> ok;
check_boolean(false) -> ok;
check_boolean(_)     -> mnesia:abort(invalid_boolean_parameter).

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
check_not_exists(Table, Key) when atom(Table) ->
    case mnesia:read({Table, Key}) of
        []                  -> ok;
        'transaction abort' -> mnesia:abort(access_error);
        _                   -> mnesia:abort(duplicated_tuple)
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
check_exists(Table, Key) when is_atom(Table) ->
    case mnesia:read({Table, Key}) of
        []                  -> mnesia:abort(not_found);
        'transaction abort' -> mnesia:abort(access_error);
        _                   -> ok
    end.

%%--------------------------------------------------------------------
%% Function: 
%% Description:
%%--------------------------------------------------------------------
next_secuence_number(Table) ->
    mnesia:dirty_update_counter({counter, Table}, 1).


%%====================================================================
%% Internal functions
%%====================================================================
