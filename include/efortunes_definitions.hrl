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

-define(DEFAULT_LOGS_DIR, "../logs").
-define(DEFAULT_LOG_PREFIX, "logfile").
-define(DEFAULT_LOG_TTY, false).
-define(DEFAULT_MESSAGES_DIR, "../messages").
-define(DEFAULT_MESSAGES_LANGUAGE, 'ES').
-define(DEFAULT_SERVER_NODE, localhost).


-define(EFORTUNES_FACADE, efortunes_facade).
-define(EFORTUNES_DATABASE_MANAGER, efortunes_dbManager).
-define(FORTUNE_DAO, efortunes_dao_fortune).
-define(USER_PROFILE_DAO, efortunes_dao_userProfile).

-define(MNESIA_UTILS, mnesia_utils).
-define(MNESIA_ACTION_PROCESSOR, mnesia_actionProcessor).
-define(STORAGE_NODES, [storage1@camilo, storage2@camilo]).

-define(MESSAGES_TRADER, efortunes_messagesTrader).
-define(SESSION_MANAGER, efortunes_httpSessionManager).
-define(SESSION_COOKIE_NAME, "sessionCookie").
-define(YAWS_UTILS, efortunes_yawsUtils).
-define(VIEW_UTILS, efortunes_viewUtils).
-define(HTML_FORMATTING, efortunes_HTMLFormatting).
-define(DATE_OPERATIONS, efortunes_dateOperations).

-define(LISTING_TABLE_STYLE, 
	{{table, [{class, "listing"}]},
	 {tr, []},{th, []},{td, []}}).


-define(LOGIN_PAGE_URL, "/login.yaws").
-define(SUCCESS_PAGE_URL, "/success.yaws").
-define(ERROR_PAGE_URL, "/error.yaws").
-define(MAIN_PAGE_URL, "/home.yaws").

-define(LOGIN_PAGES, 
	[
	 % Webapp locations
	 "/",
	 "/index.yaws",
	 "/login.yaws",
	 "/error.yaws",
	 "/doLogin.yaws", 
	 "/doLogout.yaws",

	 % Stylesheets
	 "/styles.css",

	 % Javascript
	 "/javascript/validation.js"

	 % Other resources

	]).
