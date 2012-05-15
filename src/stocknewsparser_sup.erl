
-module(stocknewsparser_sup).

-behaviour(supervisor).

-include("logging.hrl").

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
    StockNewsServer = ?CHILD(stocknewsparser_server, worker),
    {ok, { {one_for_one, 5, 10}, [StockNewsServer]} }.

