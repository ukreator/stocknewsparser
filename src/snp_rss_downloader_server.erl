%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description :
%%%
%%% -------------------------------------------------------------------

-module(snp_rss_downloader_server).

-behaviour(gen_server).

-export([start_link/3, create/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include("snp_logging.hrl").
-include("snp_rss_item.hrl").

-define(START_DELAY_MAX, 4000).

-record(state, {url, repeat_time}).


start_link(Url, RepeatTime, Index) ->
	gen_server:start_link(?MODULE, [Url, RepeatTime, Index], []).

create(Url, RepeatTime, Index) ->
	snp_rss_parse_sup:start_child(Url, RepeatTime, Index).

%% RepeatTime is passed in seconds
init([Url, RepeatTime, Index]) ->
	?INFO("Starting RSS download server", []),
	CurTime = erlang:now(),
	AdjustedSeed = setelement(2, CurTime, element(2, CurTime) + Index),
	random:seed(AdjustedSeed),
	RandStartTime = random:uniform(RepeatTime),
	?INFO("Starting RSS downloader worker for URL ~p in time ~p", [Url, RandStartTime]),
	erlang:send_after(RandStartTime * 1000, self(), {rss_update}),
    {ok, 
	#state{url = Url, repeat_time = RepeatTime * 1000}}.


%% callbacks


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({rss_update}, State) ->
	process_rss(State),
	erlang:send_after(State#state.repeat_time, self(), {rss_update}),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% private functions

process_rss(State) ->
	Url = State#state.url,
	?INFO("Fetching RSS update for URL ~p", [Url]),
	
	% TODO: add support for expiration headers (If-None-Match/ETag and If-Modified-Since/Last-Modified)
	% synchronous download of RSS feed
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = 
		httpc:request(get, {Url, []}, [], []),
	?INFO("Got RSS body of length ~p for Url ~p", [length(Body), Url]),
	{ok, Items} = snp_rss_parse:get_items(Body),
	
	lists:map(fun(Item) -> 
					  StartDelay = random:uniform(?START_DELAY_MAX),					  
					  snp_article_parser_server:create(Item#rss_item.link, StartDelay) 
			  end, Items),
	ok.
