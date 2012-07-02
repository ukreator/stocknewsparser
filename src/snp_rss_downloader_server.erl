
-module(snp_rss_downloader_server).

-behaviour(gen_server).

-export([start_link/3, create/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include("snp_logging.hrl").


-record(state, {url, repeat_time}).

start_link(Url, RepeatTime, Index) ->
    %gen_server:start_link({local, ?MODULE}, ?MODULE, [Url, RepeatTime], []).
	gen_server:start_link(?MODULE, [Url, RepeatTime, Index], []).

create(Url, RepeatTime, Index) ->
	snp_sup:start_child(Url, RepeatTime, Index).

%% RepeatTime is passed in seconds
init([Url, RepeatTime, Index]) ->
	CurTime = erlang:now(),
	AdjustedSeed = setelement(2, CurTime, element(2, CurTime) + Index),
	random:seed(AdjustedSeed),
	RandStartTime = random:uniform(RepeatTime),
	?INFO("Starting RSS downloader worker for URL ~p in time ~p", [Url, RandStartTime]),
	erlang:send_after(RandStartTime * 1000, self(), rss_update),
    {ok, 
	#state{url = Url, repeat_time = RepeatTime * 1000}}.


%% callbacks


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(rss_update, State) ->
	process_rss(State#state.url),
	erlang:send_after(State#state.repeat_time, self(), rss_update),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% helper functions

process_rss(Url) ->
	?INFO("Fetching RSS update for URL ~p", [Url]).


