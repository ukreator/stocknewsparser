%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description :
%%%
%%% Created : 05.07.2012
%%% -------------------------------------------------------------------
-module(snp_article_parser_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("snp_logging.hrl").
-include("snp_ticker_record.hrl").
-include("snp_rss_item.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% server state
-record(state, {rss_item}).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------




%% ====================================================================
%% External functions
%% ====================================================================

start_link(RssItem, StartDelay) ->
	gen_server:start_link(?MODULE, [RssItem, StartDelay], []).


create(RssItem, StartDelay) ->
	snp_article_parser_sup:start_child(RssItem, StartDelay).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([RssItem, StartDelay]) ->
	?INFO("Start downloading and analyzing article text from URL ~p in ~p milliseconds", [RssItem#rss_item.link, StartDelay]),
	erlang:send_after(StartDelay, self(), {process_url}),
    {ok, #state{rss_item=RssItem}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({process_url}, State) ->
	process_url(State),
	% stop worker process after processing is done
	{stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

process_url(State) ->
	RssItem = State#state.rss_item,
	Url = RssItem#rss_item.link,

	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = 
		httpc:request(get, {Url, []}, [], []),
	?INFO("Got article body of length ~p for Url ~p", [length(Body), Url]),
	
	Tickers = extract_yahoo_tickers(Body),
	KnownTickers = snp_ticker_matcher_server:find_known_tickers(Tickers),
	FinalObjects = matches_to_db_objects(KnownTickers, RssItem),
	lists:foreach(fun(Obj) -> snp_db_saver:add_news(Obj) end, FinalObjects),
	ok.


%% This function is Yahoo-finance specific and is looking for RELATED QUOTES section
%% returns [binary()]
extract_yahoo_tickers(Body) ->
	{ok, MP} = re:compile("<a href=\"/q.s=\\w+\">(\\w+)<"),
	Result = re:run(Body, MP, [global, {capture, [1], binary}]),
	case Result of
		nomatch -> [];
		{match, Captured} -> lists:map(fun([Elem]) -> Elem end, Captured)
	end.


matches_to_db_objects(Matches, RssItem) ->
	lists:map(fun({Ticker, CompanyName}) -> 
					 #ticker_item{ticker_symbol=Ticker, company_name=CompanyName,
								  link=RssItem#rss_item.link, title=RssItem#rss_item.title, 
								  publish_date=RssItem#rss_item.publish_date, 
								  guid=RssItem#rss_item.guid} 
			 end, Matches).

%% --------------------------------------------------------------------
%%% Unit tests
%% --------------------------------------------------------------------

				   
-ifdef(TEST).

extract_yahoo_tickers_test() ->
	Body = "blabla
<a href=\"/q?s=es\">ES</a></span></td><td><span class=\"streaming-datum\" 
id=\"yfs_l84_es\">1.65</span></td><td><span class=\"streaming-datum yfi-price-change-red\">-0.01</span></td></tr>
<tr class=\"yui3-\"><td colspan=\"3\"><div><a href=\"/q?s=es\">
<img src=\"http://ichart.finance.yahoo.com/t?s=es&lang=en-US&region=US\"  ></a><<a href=\"/q?s=zaza\">ZAZA</a></span></td><td>",
	Tickers = extract_yahoo_tickers(Body),
	?assertEqual(2, length(Tickers)),
	?assert(lists:member(<<"ES">>, Tickers)),
	?assert(lists:member(<<"ZAZA">>, Tickers)).


-endif.
