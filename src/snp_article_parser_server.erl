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

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, create/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

% server state
-record(state, {url}).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------




%% ====================================================================
%% External functions
%% ====================================================================

start_link(Url, StartDelay) ->
	gen_server:start_link(?MODULE, [Url, StartDelay], []).


create(Url, StartDelay) ->
	snp_article_parser_sup:start_child(Url, StartDelay).

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
init([Url, StartDelay]) ->
	?INFO("Start downloading and analyzing article text from URL ~p in ~p milliseconds", [Url, StartDelay]),
	erlang:send_after(StartDelay, self(), {process_url}),
    {ok, #state{url=Url}}.

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
	process_url(State#state.url),
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

process_url(Url) ->
	%  - search for tickers from the list in the body
	?INFO("Downloading from link ~p", [Url]),
	{ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = 
		httpc:request(get, {Url, []}, [], []),
	?INFO("Got article body of length ~p for Url ~p", [length(Body), Url]),
	{ok, NewsObj} = process_article_body(Body),
	snp_db_saver:add_news(NewsObj),
	ok.

process_article_body(Body) ->
	% - preload ticker symbols and company names in a list
	% - process article to words and save to a hash set
	% - loop over all tickers and check if has set has i-th ticker
	%
	%

	Tokens = mochiweb_html:tokens(Body),
	%Words = string:tokens(Body, ",.<>:;\"\\/$%#*&()=+?! "),
	Text = tokens_to_text(Tokens),
	Words = text_to_words(Text),
	?INFO("~p", Words),
	{ok, {1,2,3}}.


tokens_to_text(Tokens) ->
	% text order is inversed, but it doesn't matter for us
	ConcatFun = fun(X, Text) -> [element(2, X) | Text] end,
	FilterFun = fun({data, _, false}) -> true;
				   (_)                -> false 
				end,
	lists:foldl(ConcatFun, [], lists:filter(FilterFun, Tokens)).

text_to_words(Text) ->
	Pattern = lists:map(fun(El) -> <<El>> end, ",.<>:;\"\\/$%#*&()=+?! "), 
	SplittingFun = fun(Elem) -> binary:split(Elem, Pattern, [global, trim]) end,
	
	lists:foldl(fun(Elem, Accum) -> 
						Words = SplittingFun(Elem),
						Accum ++ Words
				end, 
				[], Text).
				   


				   
