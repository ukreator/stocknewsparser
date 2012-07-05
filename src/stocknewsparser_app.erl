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

-define(RIAK_ADDR, "127.0.0.1").
-define(RIAK_PORT, 8098).


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
	ConnectionResult = riakc_pb_socket:start_link(?RIAK_ADDR, ?RIAK_PORT),
	case ConnectionResult of
		{ok, RiakcPid} -> 
			?INFO("Connection to Riak established", []),
			% generate index for additional seeding factor (increase seconds in current time)
			lists:foldl(
				fun(Url, Index) -> start_child(Url, Index, RiakcPid),
			 	Index + 1
			 	end, 
			0, ?RSS_FEEDS);
		
		{error, Reason} ->
			Descr = io_lib:format("Failed to connect to RiakClient on ~s:~p. Reason is ~p", 
								  [?RIAK_ADDR, ?RIAK_PORT, Reason]),
			?ERROR(Descr, []),
			exit({riak_connection_failed, Descr})
	end,
	ok.

start_child(Url, Index, RiakcPid) ->
	case snp_rss_downloader_server:create(Url, ?RSS_FETCH_INTERVAL, Index, RiakcPid) of
		{error, Reason} -> ?ERROR("Failed to start child worker process ~p", [Reason]), ok;
		_Other -> ok
	end.
