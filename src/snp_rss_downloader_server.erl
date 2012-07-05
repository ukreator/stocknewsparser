
-module(snp_rss_downloader_server).

-behaviour(gen_server).

-export([start_link/4, create/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include("snp_logging.hrl").


-record(state, {url, repeat_time, riakc_pid}).

start_link(Url, RepeatTime, Index, RiakcPid) ->
    %gen_server:start_link({local, ?MODULE}, ?MODULE, [Url, RepeatTime], []).
	gen_server:start_link(?MODULE, [Url, RepeatTime, Index, RiakcPid], []).

create(Url, RepeatTime, Index, RiakcPid) ->
	snp_rss_parse_sup:start_child(Url, RepeatTime, Index, RiakcPid).

%% RepeatTime is passed in seconds
init([Url, RepeatTime, Index, RiakcPid]) ->
	?INFO("Starting RSS download server", []),
	CurTime = erlang:now(),
	AdjustedSeed = setelement(2, CurTime, element(2, CurTime) + Index),
	random:seed(AdjustedSeed),
	RandStartTime = random:uniform(RepeatTime),
	?INFO("Starting RSS downloader worker for URL ~p in time ~p", [Url, RandStartTime]),
	erlang:send_after(RandStartTime * 1000, self(), rss_update),
    {ok, 
	#state{url = Url, repeat_time = RepeatTime * 1000, riakc_pid = RiakcPid}}.


%% callbacks


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(rss_update, State) ->
	process_rss(State),
	erlang:send_after(State#state.repeat_time, self(), rss_update),
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
	% make synchronous request
	% TODO: add support for expiration headers (If-None-Match/ETag and If-Modified-Since/Last-Modified)
	{ok, {{Version, 200, ReasonPhrase}, Headers, Body}} = 
		httpc:request(get, {Url, []}, [], []),
	?INFO("Got body of length ~p for Url ~p", [length(Body), Url]),
	{ok, Items} = parse_rss(Body),
	
	ok.

parse_rss(Body) ->
	% TODO: 
	% - extract all <item> elements
	% - for each item:
	%  - get <link>, description, pubDate
	%  - download document from <link>
	%  - search for tickers from the list in the body
	%  - if ticker found, send article info to Riak DB
	{ok, []}.

download_article(Link) ->
	% - download articles asynchronously and parse them (start one seperate process?)
	ok.
