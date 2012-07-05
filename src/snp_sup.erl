
-module(snp_sup).

-behaviour(supervisor).

-include("snp_logging.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


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
	% start bunch of children that should live while application is alive  
	Children = [?CHILD(snp_rss_parse_sup, supervisor), 
				?CHILD(snp_article_parser_sup, supervisor),
				?CHILD(snp_db_saver, worker)
				],
	RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, Children}}.

