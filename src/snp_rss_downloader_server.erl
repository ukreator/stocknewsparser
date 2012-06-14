
-module(snp_rss_downloader_server).

-behaviour(gen_server).

-export([start_link/2, create/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

-include("snp_logging.hrl").


-record(state, {url, repeat_time}).

start_link(Url, RepeatTime) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Url, RepeatTime], []).

create(Url, RepeatTime) ->
	snp_sup:start_child(Url, RepeatTime).

init([Url, RepeatTime]) ->
	?INFO("Starting RSS downloader worker", []),
	RandStartTime = random:uniform(RepeatTime),
	erlang:send_after(RandStartTime, ?MODULE, rss_update),
    {ok, 
	#state{url = Url, repeat_time = RepeatTime}}.


%% callbacks


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(rss_update, State) ->
	process_rss(State#state.url),
	erlang:send_after(State#state.repeat_time, ?MODULE, rss_update),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% helper functions

process_rss(Url) ->
	%aas = frg,
	?INFO("Fetching RSS update for URL ~p", [Url]).


