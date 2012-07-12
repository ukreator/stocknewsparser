%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description : Record representing an RSS item.
%%%
%%% Created : 09.07.2012
%%% -------------------------------------------------------------------
-module(snp_ticker_matcher_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("snp_logging.hrl").
-include("snp_ticker_record.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, find_match/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {tickers}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

find_match(ArticleData) ->
	% TODO: use also company name, match at least one word of company name as additional check
	% words to exclude from check (case-insensitive): Inc, Ltd, Corp, Corporation, Group, Trust, Fund, Co, LLC, Company
	% iterate over tickers (from state) and check for presence in article word table
	gen_server:call(?MODULE, {find_match, ArticleData}).


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
init([]) ->
	Tickers = get_tickers(),
    {ok, #state{tickers=Tickers}}.

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
handle_call({find_match, WordsDict}, _From, State) ->
	TickerCheckFun = fun({Ticker, CompanyName, Hash}, MatchingList) ->
					% check for ticker presense in article text
					case dict:find(Hash, WordsDict) of
						{ok, WordsList} -> 
							% additional check if hash collision happened
							lists:foldl(fun(Word = Ticker, Accum) ->
									% accumulate {ticker, comapny_name} tuple on match
									[{Word, CompanyName} | Accum] 
								end, MatchingList, WordsList);
						error -> MatchingList
					end
				end,
	
	% iterate over all ticker symbols and try to find them in the passed word list
	Reply = lists:foldl(TickerCheckFun, [], State#state.tickers),
	{reply, Reply, State};
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

get_tickers() ->
	{ok, TickerFiles} = application:get_env(stocknewsparser, ticker_symbol_files),
	CompaniesInfo = lists:flatmap(fun(File) ->
			?INFO("Loading tickers from file ~p", [File]),
			[_Head | Records] = erfc_4180:parse_file(File, []),
			Records 
		end, TickerFiles),
	% extract ticker symbol and company name
	% FIXME: check that all strings are binaries!
	lists:map(fun(Elem) -> 
				Ticker = element(1, Elem),
				CompanyName = element(2, Elem), 
				{Ticker, CompanyName, erlang:phash2(Ticker, 4294967296)} 
			end, CompaniesInfo).
