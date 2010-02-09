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

-module(efortunes_viewUtils).

-export([toString/1, dateAndTimeToString/1, dateToString/1, timeToString/1, 
	 getVisualDate/0, getVisualDate/1, getTwoDigitsNumberString/1, 
	 getWeekDayString/2, getMonthString/2]).

-include("efortunes_definitions.hrl").

-include("efortunes_records.hrl").

toString(Something) ->
    [String] = yaws_api:f("~p", [Something]),
    String.

dateToString(Date)  ->
    {Year, Month, Day} = Date,

    DayString = getTwoDigitsNumberString(Day),
    MonthString = getTwoDigitsNumberString(Month),
    YearString = integer_to_list(Year),

    % return formatted string
    [DayString, "/", MonthString, "/", YearString].

timeToString(Time)  ->
    {Hour, Minute, _Second} = Time,

    HourString = getTwoDigitsNumberString(Hour),    
    MinuteString = getTwoDigitsNumberString(Minute),

    % return formatted string
    [HourString, ":", MinuteString].

dateAndTimeToString(Date) when is_record(Date, full_date) ->
    {Day, Month, Year, Hour, Minute} = {Date#full_date.day, Date#full_date.month, Date#full_date.year,
					Date#full_date.hour, Date#full_date.minute},

    DayString = getTwoDigitsNumberString(Day),
    MonthString = getTwoDigitsNumberString(Month),
    YearString = integer_to_list(Year),
    HourString = getTwoDigitsNumberString(Hour),    
    MinuteString = getTwoDigitsNumberString(Minute),

    % return formatted string
    [DayString, "/", MonthString, "/", YearString,
     " - ", HourString, ":", MinuteString].

getVisualDate(LanguageCode) when is_atom(LanguageCode) ->
    {Date = {Year, Month, Day}, {Hour, Minute, _Seconds}} = calendar:local_time(),
    WeekDayString = getWeekDayString(Date, LanguageCode),
    MonthDayString = getTwoDigitsNumberString(Day),
    
    MonthString = getMonthString(Month, LanguageCode),
    YearString = integer_to_list(Year),
    HourString = getTwoDigitsNumberString(Hour),    
    MinuteString = getTwoDigitsNumberString(Minute),
    
    % return formatted string
    [WeekDayString, ", ", MonthDayString, " ", MonthString, " ", YearString,
     " - ", HourString, ":", MinuteString].

getVisualDate() ->
    getVisualDate(?DEFAULT_MESSAGES_LANGUAGE).


getTwoDigitsNumberString(Number) when is_integer(Number) ->
    if 
	(Number < 10) ->
	    "0" ++ integer_to_list(Number);
	true ->
	    integer_to_list(Number)
    end.

getWeekDayString(Date, LanguageCode) ->
    case calendar:day_of_the_week(Date) of
	1 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_monday);
	2 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_tuesday);
	3 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_wednesday);
	4 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_thursday);
	5 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_friday);
	6 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_saturday);
	7 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_sunday)
    end.

getMonthString(MonthNumber, LanguageCode) 
  when is_atom(LanguageCode), is_integer(MonthNumber) ->
    case MonthNumber of
	1 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_january);
	2 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_february);
	3 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_march);
	4 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_april);
	5 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_may);
	6 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_june);
	7 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_july);
	8 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_august);
	9 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_september);
	10 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_october);
	11 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_november);
	12 -> ?MESSAGES_TRADER:getMessage(LanguageCode, datetimeStrings_december)
    end.
