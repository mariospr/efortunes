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

-module(efortunes_HTMLFormatting).

-include("efortunes_definitions.hrl").

-include("yaws_api.hrl").

-export([formatNoData/0, formatString/1, formatInteger/1, formatNumber/1,
	 formatDate/1, formatTime/1, formatDateAndTime/1, formatLink/1, 
         formatTable/4, formatTable/5, formatAnyData/1, formatNumericalSelector/4,
	 formatMonthSelector/4, formatMonthSelector/5]).

%%%%%%%%%%%%%%%%%%
%% Exported API %%
%%%%%%%%%%%%%%%%%%

% Formats no data (empty field)
formatNoData() -> 
    formatString("").

% Formats as string
formatString(_Param=String) -> 
    String.

% Formats an integer
formatInteger(_Param=Integer) -> 
    integer_to_list(Integer).

% Formats a number
formatNumber(_Param=Number) 
  when is_integer(Number) ->
    integer_to_list(Number);
formatNumber(_Param=Number) 
  when is_float(Number) ->
    [S] = io_lib:format("~.2f", [Number]),
    S;
formatNumber(_Param) ->
    [].

% Formats a date as dd/mm/yyyy
formatDate(_Param=DateAndTime) -> 
    ?VIEW_UTILS:dateToString(DateAndTime).

% Formats time as hh:mm
formatTime(_Param=DateAndTime) -> 
    ?VIEW_UTILS:timeToString(DateAndTime).

% Formats date and time as dd/mm/yyyy - hh:mm
formatDateAndTime(_Param=DateAndTime) -> 
    ?VIEW_UTILS:dateAndTimeToString(DateAndTime).

% Formats an html link
formatLink(_Param={LinkString, ""}) ->
    formatString(LinkString);

formatLink(_Param={LinkString, LinkRef}) ->
    {a, [{href, LinkRef}, {target, "_parent"}], LinkString};

formatLink(_Param={LinkString, BaseLinkRef, Params}) ->
    {a, 
     [{href, [BaseLinkRef, "?", getParams(Params)]}, {target, "_parent"}], 
     LinkString
    }.    

% Formats a table
formatTable(Data, Headers, Extraction_funs, Formatting_funs) ->
    formatTable(Data, Headers, Extraction_funs, Formatting_funs,
		{{table, []}, {tr, []}, {th, []}, {td, []}}).

% Formats a table - specifying style params
formatTable(Data, Headers, Extraction_funs, Formatting_funs, TableStyles) ->
    
    {{table, TableStyle}, {tr, TrStyle},{th, ThStyle}, {td, TdStyle}} = TableStyles,

    {table, 
     TableStyle, 
     [
      {tr, TrStyle, getHTMLHeaders(Headers, ThStyle)}
      |getHTMLRows(Data, Extraction_funs, Formatting_funs, TrStyle, TdStyle)
     ]
    }.

% Format any data passed through the param
formatAnyData(_Param=AnyData) -> 
    ?VIEW_UTILS:toString(AnyData).

%returns an HTML selector with the numbers between From and To
formatNumericalSelector(Name, From, To, Selected) 
  when is_list(Name), is_integer(From), is_integer(To), is_integer(Selected) ->
    {ehtml, 
     {select, [{name, Name}],
      case (From =< To) of
	  true ->
	      SelectedElement = {option, [{value, integer_to_list(Selected)}, {selected, "selected"}], ?VIEW_UTILS:getTwoDigitsNumberString(Selected)},
	      if 
		  (From < Selected), (Selected < To) ->
		      NoSelectedList1 = [{option, [{value, integer_to_list(X)}], ?VIEW_UTILS:getTwoDigitsNumberString(X)} || X <- lists:seq(From, (Selected-1))],
		      NoSelectedList2 = [{option, [{value, integer_to_list(X)}], ?VIEW_UTILS:getTwoDigitsNumberString(X)} || X <- lists:seq((Selected+1), To)],
		      NoSelectedList1 ++ [SelectedElement|NoSelectedList2];

		  (From == Selected) ->
		      NoSelectedList = [{option, [{value, integer_to_list(X)}], ?VIEW_UTILS:getTwoDigitsNumberString(X)} || X <- lists:seq((Selected+1), To)],
		      [SelectedElement|NoSelectedList];
	      
		  (To == Selected) ->
		      NoSelectedList = [{option, [{value, integer_to_list(X)}], ?VIEW_UTILS:getTwoDigitsNumberString(X)} || X <- lists:seq(From, (Selected-1))],
		      NoSelectedList ++ [SelectedElement];
		  
		  true -> % Selected is not between From and To => return list with first item selected (From)		      
		      formatNumericalSelector(Name, From, To, From)		      
	      end;

	  false ->
	      []
      end
     }}.

% returns an HTML selector with the month names between From and To,
% where From and To are month indexes (January=1..December=12)
formatMonthSelector(Name, From, To, Selected) ->
    formatMonthSelector(Name, From, To, Selected, ?DEFAULT_MESSAGES_LANGUAGE).

formatMonthSelector(Name, From, To, Selected, LanguageCode) 
  when is_atom(LanguageCode), is_list(Name), is_integer(From), is_integer(To), is_integer(Selected), 
       (From>0), (From<13), (To>0), (To<13) ->

    {ehtml, 
     {select, [{name, Name}],
      case (From =< To) of
	  true->
	      SelectedElement = {option, [{value, integer_to_list(Selected)}, {selected, "selected"}], ?VIEW_UTILS:getMonthString(Selected, LanguageCode)},
	      if 
		  (From < Selected), (Selected < To) ->
		      NoSelectedList1 = [{option, [{value, integer_to_list(X)}], ?VIEW_UTILS:getMonthString(X, LanguageCode)} || X <- lists:seq(From, Selected-1)],
		      NoSelectedList2 = [{option, [{value, integer_to_list(X)}], ?VIEW_UTILS:getMonthString(X, LanguageCode)} || X <- lists:seq(Selected+1, To)],
		      NoSelectedList1 ++ [SelectedElement|NoSelectedList2];

		  (From == Selected) ->
		      NoSelectedList = [{option, [{value, integer_to_list(X)}], ?VIEW_UTILS:getMonthString(X, LanguageCode)} || X <- lists:seq(Selected+1, To)],
		      [SelectedElement|NoSelectedList];
	      
		  (To == Selected) ->
		      NoSelectedList = [{option, [{value, integer_to_list(X)}], ?VIEW_UTILS:getMonthString(X, LanguageCode)} || X <- lists:seq(From, Selected-1)],
		      NoSelectedList ++ [SelectedElement];
		  
		  true -> % Selected is not between From and To => return list with first item selected (From)		      
		      formatMonthSelector(Name, From, To, From, LanguageCode)
	      end;

	  false ->
	      []
      end
     }}.


%%%%%%%%%%%%%%%%%%%%%%
%% Private functions %
%%%%%%%%%%%%%%%%%%%%%%

% Get params from a GET HTTP Request
getParams(Params=[{_ParamName, _ParamValue}|_]) ->
    [[ParamName, "=", ParamValue, "&"] || {ParamName, ParamValue} <- Params].

% Get Header columns for an HTML table
getHTMLHeaders(Headers, ThStyle) ->
    [{th, ThStyle, Header} || Header <- Headers].

% Get rows for an HTML table
getHTMLRows(Data, Extraction_funs, Formatting_funs, TrStyle, TdStyle) ->
    [{tr, TrStyle, getHTMLColumns(DataElement, Extraction_funs, Formatting_funs, TdStyle, [])}
     || DataElement <- Data].
            
% Get columns for the rows of an HTML table
getHTMLColumns(_Data, [], [], _TdStyle, Acc) ->
    lists:reverse(Acc);
getHTMLColumns(Data, [Extraction_fun|ExtractionTail], 
               [Formatting_fun|FormattingTail], TdStyle, Acc) ->

    getHTMLColumns(Data, ExtractionTail, FormattingTail, TdStyle, 
		   [{td, TdStyle, Formatting_fun(Extraction_fun(Data))} | Acc]).
