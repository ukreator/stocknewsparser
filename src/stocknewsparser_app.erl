-module(stocknewsparser_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("snp_logging.hrl").


%% List of RSS feeds we monitor
-define(RSS_FEEDS, ["http://finance.yahoo.com/news/category-economy-govt-and-policy/rss",
					"http://finance.yahoo.com/news/category-earnings/rss"]).

% how often RSS should be retrieved from a remote site, in seconds
-define(RSS_FETCH_INTERVAL, 60).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	?INFO("Starting stocknewsparser application", []),
    Res = snp_sup:start_link(),
	case Res of
		{ok, _Pid} -> start_children();
		_ -> ok
	end,
	Res.

stop(_State) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


start_children() ->
	% generate index for additional seeding factor (increase seconds in current time)
	lists:foldl(
			fun(Url, Index) -> start_child(Url, Index),
		 	Index + 1
		 	end, 
		0, ?RSS_FEEDS),
	ok.

start_child(Url, Index) ->
	case snp_rss_downloader_server:create(Url, ?RSS_FETCH_INTERVAL, Index) of
		{error, Reason} -> ?ERROR("Failed to start child worker process ~p", [Reason]), ok;
		_Other -> ok
	end.
