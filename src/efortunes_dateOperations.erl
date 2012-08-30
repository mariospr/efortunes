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

-module(efortunes_dateOperations).

-export([getCurrentDate/0, splitDateAndTime/1, joinDateAndTime/2, compareDates/2]).

-include("efortunes_records.hrl").

getCurrentDate() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    #full_date{year=Year,month=Month,day=Day,
	  hour=Hour, minute=Minute, second=Second}.

splitDateAndTime(DateAndTime) ->
    Date = {DateAndTime#full_date.year, 
	    DateAndTime#full_date.month, 
	    DateAndTime#full_date.day},

    Time = {DateAndTime#full_date.hour, 
	    DateAndTime#full_date.minute, 
	    DateAndTime#full_date.second},

    {Date, Time}.

joinDateAndTime(Date, Time) ->
    {Year, Month, Day} = Date,
    {Hour, Minute, Second} = Time,
    #full_date{year=Year,month=Month,day=Day,
	       hour=Hour, minute=Minute, second=Second}.

compareDates(DateA, DateB) ->
    TimeA = splitDateAndTime(DateA),
    TimeB = splitDateAndTime(DateB),
    SecondsA =  calendar:datetime_to_gregorian_seconds(TimeA),
    SecondsB =  calendar:datetime_to_gregorian_seconds(TimeB),
    SecondsA < SecondsB. % return true if TimeA is less than TimeB. false otherwise
    
    
