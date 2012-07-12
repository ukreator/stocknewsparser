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
	
	{ok, WordsDict} = process_article_body(Body),
	?INFO("Word dict: ", [WordsDict]),
	Matches = snp_ticker_matcher_server:find_match(WordsDict),
	% form ready-to-store objects
	FinalObjects = lists:map(fun({Ticker, CompanyName}) -> 
									 #ticker_item{ticker_symbol=Ticker, company_name=CompanyName,
												   link=Url, title=RssItem#rss_item.title, 
												  publish_date=RssItem#rss_item.publish_date, 
												  guid=RssItem#rss_item.guid} 
							 end, Matches),
	
	lists:foreach(fun(Obj) -> snp_db_saver:add_news(Obj) end, FinalObjects),
	ok.

process_article_body(Body) ->
	% - preload ticker symbols and company names in a list
	% - process article to words and save to a hash set
	% - loop over all tickers and check if has set has i-th ticker
	%
	%

	Tokens = mochiweb_html:tokens(Body),
	Text = tokens_to_text(Tokens),
	Words = text_to_words(Text),
	% transform to dict of words with keys as 64 bit hash
	WordsDict = lists:foldl(fun(Word, Dict) -> 
									Hash = erlang:phash2(Word, 4294967296),
									AlreadyExists = case dict:find(Hash, Dict) of
														{ok, WordList} -> lists:member(Word, WordList);
														error -> false
													end,
									case AlreadyExists of 
										false -> dict:append(Hash, Word, Dict);
										true -> Dict
									end
							end, dict:new(), Words),
	{ok, WordsDict}.


tokens_to_text(Tokens) ->
	% text order is inversed, but it doesn't matter for us
	ConcatFun = fun(X, Text) -> [element(2, X) | Text] end,
	% TODO: skip JS blocks
	FilterFun = fun({data, _, false}) -> true;
				   (_OtherTokens)                -> false 
				end,
	lists:foldl(ConcatFun, [], lists:filter(FilterFun, Tokens)).

text_to_words(Text) ->
	Pattern = lists:map(fun(El) -> <<El>> end, ",.<>:;\"\\/$%#*&()=+?!'\r\n "), 
	SplittingFun = fun(Elem) -> binary:split(Elem, Pattern, [global, trim]) end,	
	lists:foldl(fun(Elem, Accum) -> 
						Words = SplittingFun(Elem),
						Accum ++ Words
				end, 
				[], Text).
				   


				   
