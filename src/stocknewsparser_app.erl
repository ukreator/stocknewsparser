%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description :
%%%
%%% -------------------------------------------------------------------

-module(stocknewsparser_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("snp_logging.hrl").


% how often RSS should be retrieved from a remote site, in seconds
-define(RSS_FETCH_INTERVAL, 60).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	?INFO("Starting stocknewsparser application", []),
	FeedAddresses = snp_conf:get_val(rss_feed_addresses),
	RssFetchInterval = snp_conf:get_val(rss_fetch_interval),
	
    Res = snp_sup:start_link(),
	case Res of
		{ok, _Pid} -> start_children(FeedAddresses, RssFetchInterval);
		_ -> ok
	end,
	Res.

stop(_State) ->
    ok.



%% ====================================================================
%% Internal functions
%% ====================================================================


start_children(FeedAddresses, RssFetchInterval) ->
	% generate index for additional seeding factor (increase seconds in current time)
	lists:foldl(
			fun(Url, Index) -> start_child(Url, Index, RssFetchInterval),
		 	Index + 1
		 	end, 
		0, FeedAddresses),
	ok.

start_child(Url, Index, RssFetchInterval) ->
	case snp_rss_downloader_server:create(Url, RssFetchInterval, Index) of
		{error, Reason} -> ?ERROR("Failed to start child worker process ~p", [Reason]), ok;
		_Other -> ok
	end.

