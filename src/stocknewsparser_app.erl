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
	init(),
    Res = snp_sup:start_link(),
	case Res of
		{ok, _Pid} -> start_children();
		_ -> ok
	end,
	Res.

stop(_State) ->
    ok.


% helper functions

init() ->
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	ok.

start_children() ->
	lists:foreach(fun(Url) -> snp_sup:start_child(Url, ?RSS_FETCH_INTERVAL) end, ?RSS_FEEDS),
	ok.