
-module(snp_sup).

-behaviour(supervisor).

-include("snp_logging.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% TODO: load from the config file
-define(RSS_FEEDS, ["http://finance.yahoo.com/news/category-economy-govt-and-policy/rss"]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	?INFO("Starting stock news parser supervisor", []),
    RssDownloaderServer = ?CHILD(snp_rss_downloader_server, worker),
    {ok, { {one_for_one, 5, 10}, [RssDownloaderServer]} }.

