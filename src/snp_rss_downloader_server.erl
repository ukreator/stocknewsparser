%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description :
%%%
%%% -------------------------------------------------------------------

-module(snp_rss_downloader_server).

-behaviour(gen_server).


%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("snp_logging.hrl").
-include("snp_rss_item.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% --------------------------------------------------------------------
%% External exports

-export([start_link/3, create/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).


-define(START_DELAY_MAX, 4000).

-record(state, {url, repeat_time, etag, last_modified}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link(Url, RepeatTime, Index) ->
	gen_server:start_link(?MODULE, [Url, RepeatTime, Index], []).

create(Url, RepeatTime, Index) ->
	snp_rss_parse_sup:start_child(Url, RepeatTime, Index).


%% ====================================================================
%% Server functions
%% ====================================================================

%% RepeatTime is passed in seconds
init([Url, RepeatTime, Index]) ->
	CurTime = erlang:now(),
	AdjustedSeed = setelement(2, CurTime, element(2, CurTime) + Index),
	random:seed(AdjustedSeed),
	RandStartTime = random:uniform(RepeatTime),
	?INFO("Starting RSS downloader worker for URL ~p in time ~p", [Url, RandStartTime]),
	erlang:send_after(RandStartTime * 1000, self(), {rss_update}),
    {ok, 
	#state{url=Url, repeat_time=RepeatTime * 1000, etag=none, last_modified=none}}.


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({rss_update}, State) ->
	NewState = process_rss(State),
	erlang:send_after(NewState#state.repeat_time, self(), {rss_update}),
	{noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

process_rss(State) ->
	Url = State#state.url,
	?INFO("Fetching RSS update for URL ~p", [Url]),
	
	EtagHeaders = get_etag_header(State),
	LastModifiedHeaders = get_last_modified_header(State),
	RequestHeaders = EtagHeaders ++ LastModifiedHeaders,
	?INFO("Request headers: ~p", [RequestHeaders]),
	{ok, {{_Version, ResponseCode, _ReasonPhrase}, Headers, Body}} = 
		httpc:request(get, {Url, RequestHeaders}, [], []),
	
	case ResponseCode of
		200 ->
			?INFO("Got RSS body of length ~p for Url ~p", [length(Body), Url]),
			parse_body(Body);
		304 ->
			?INFO("Feed is not modified since last call", []),
			ok
	end,
	State#state{
				etag=extract_etag(Headers), 
				last_modified=extract_last_modified(Headers)
			   }.
	

parse_body(Body) ->
	{ok, Items} = snp_rss_parse:get_items(Body),
	
	lists:map(fun(Item) -> 
					  StartDelay = random:uniform(?START_DELAY_MAX),					  
					  snp_article_parser_server:create(Item, StartDelay) 
			  end, Items),
	ok.


extract_etag(Headers) ->
	case lists:keyfind("etag", 1, Headers) of
		{_, ETag} -> ETag;
		false -> none
	end.

extract_last_modified(Headers) ->
	case lists:keyfind("last-modified", 1, Headers) of
		{_, LastModified} -> LastModified;
		false -> none
	end.


get_etag_header(#state{etag=none}) -> []; 
get_etag_header(#state{etag=ETag}) when is_list(ETag) ->
	[{"If-None-Match", ETag}].
	
get_last_modified_header(#state{last_modified=none}) -> [];
get_last_modified_header(#state{last_modified=LastModified}) 
  when is_list(LastModified) -> [{"If-Modified-Since", LastModified}].

	
%% --------------------------------------------------------------------
%%% Unit tests
%% --------------------------------------------------------------------


-ifdef(TEST).

get_etag_headers_none_test() ->
	State = #state{etag=none},
	Headers = get_etag_header(State),
	?assertEqual([], Headers).

get_etag_headers_test() ->
	State = #state{etag=45},
	?assertError(function_clause, get_etag_header(State)).

extract_etag_test() ->
	Headers = [{"Header1", "val1"}, {"etag", "etag-val"}, {"Header2", "val3"}],
	ETag = extract_etag(Headers),
	?assertEqual("etag-val", ETag).

extract_etag_not_found_test() ->
	Headers = [{"Header1", "val1"}, {"Header2", "val3"}],
	ETag = extract_etag(Headers),
	?assertEqual(none, ETag).	
	

-endif.