
-module(snp_sup).

-behaviour(supervisor).

-include("snp_logging.hrl").

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% TODO: load all these settings from the config file



%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Url, RepeatTime) ->
	supervisor:start_child(?MODULE, [Url, RepeatTime]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	?INFO("Starting stock news parser supervisor", []),
    %RssDownloaderServer = ?CHILD(snp_rss_downloader_server, worker, []),
	Children = ?CHILD(snp_rss_downloader_server, worker, []), 
	RestartStrategy = {simple_one_for_one, 5, 10},
    {ok, {RestartStrategy , [Children]}}.




%% How it should work:
%% - generate worker processes (one feed - one process), 
%%   they should not stop until signal sent or main supervisor dies
%% - 

%% feed downloader/parser worker process:
%% - download feed using cinet
%% - parse feed (whould not be too CPU-intensive)
%% - async request to the supervisor to launch download/parse document worker process


%% URL downloader/parser worker process:
% - download URL
% - parse for tickers
% - tickers present? if so - post a request to DB


% possible DB scheme:
% ticker
% news link
% datetime