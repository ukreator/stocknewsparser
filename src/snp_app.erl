-module(snp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("snp_logging.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	?ERROR("Starting stocknewsparser application", []),
	init(),
    snp_sup:start_link().

stop(_State) ->
    ok.

% helper functions

init() ->
	%dksl = fvd,
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
	ok.