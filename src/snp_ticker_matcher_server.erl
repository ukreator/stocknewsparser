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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
	{ok, TickerFiles} = application:get_env(stocknewsparser, ticker_symbol_files),
	Tickers = get_tickers(TickerFiles),
	?INFO("Tickers: ~p", [Tickers]),
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
	Reply = find_matches(WordsDict, State#state.tickers),
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

get_tickers(TickerFiles) ->
	CompaniesInfo = lists:flatmap(fun(File) ->
			?INFO("Loading tickers from file ~p", [File]),
			[_Head | Records] = erfc_4180:parse_file(File, []),
			Records 
		end, TickerFiles),
	% extract ticker symbol and company name + add hash of ticker symbol
	lists:map(fun(Elem) -> 
				Ticker = element(1, Elem),
				CompanyName = element(2, Elem), 
				{list_to_binary(Ticker), list_to_binary(CompanyName), 
				 snp_helpers:hash_32bit(list_to_binary(Ticker))} 
			end, CompaniesInfo).


find_matches(WordsDict, Tickers) ->
  	TickerCheckFun = fun({Ticker, CompanyName, Hash}, MatchingList) ->
					% check for ticker presense in article text
					case dict:find(Hash, WordsDict) of
						{ok, WordsList} -> 
							% additional check if hash collision happened
							lists:foldl(fun
										    % accumulate {ticker, company_name} tuple on match
											(Word, Accum) when Word == Ticker -> [{Word, CompanyName} | Accum];
											(_Word, Accum) -> Accum
										end,
										MatchingList, WordsList);
						error -> MatchingList
					end
				end,
	
	% iterate over all ticker symbols and try to find them in the passed word list
	lists:foldl(TickerCheckFun, [], Tickers).


%% --------------------------------------------------------------------
%%% Unit tests
%% --------------------------------------------------------------------

				   
-ifdef(TEST).

get_tickers_test() ->
	Tickers = get_tickers(["../priv/test/test1.csv", "../priv/test/test2.csv"]),
    ?assertEqual([{<<"DDD">>, <<"3D Systems Corporation">>, 2334519064}, {<<"MMM">>, <<"3M Company">>, 2907585361}, 
				  {<<"SVN">>, <<"7 Days Group Holdings Limited">>, 3128474594}, 
				  {<<"XXX">>, <<"Xprerts Fund">>, 4182447552}, {<<"FFF">>, <<"Freq From Far Inc.">>, 2057392867}, 
				  {<<"LWN">>, <<"Low Wan New Co.">>, 1860772198}],
				 Tickers).

find_matches_test() ->
	WordsDict = dict:from_list([{823394921, [<<"pat2">>]}, {4182447552, [<<"XXX">>]}, {123, [<<"aaa">>]}]),
	Tickers = [{<<"DDD">>, <<"c1">>, 2334519064}, 
				  {<<"SVN">>, <<"c2">>, 3128474594}, 
				  {<<"XXX">>, <<"c3">>, 4182447552}, 
				  {<<"LWN">>, <<"c4">>, 1860772198}],
	?assertEqual([{<<"XXX">>, <<"c3">>}], find_matches(WordsDict, Tickers)).

find_few_matches_test() ->
	WordsDict = dict:from_list([{823394921, [<<"pat2">>]}, {4182447552, [<<"XXX">>]}, {123, [<<"aaa">>]}, {1860772198, [<<"LWN">>]}]),
	Tickers = [{<<"DDD">>, <<"c1">>, 2334519064}, 
				  {<<"SVN">>, <<"c2">>, 3128474594}, 
				  {<<"XXX">>, <<"c3">>, 4182447552}, 
				  {<<"LWN">>, <<"c4">>, 1860772198}],
	Matches = find_matches(WordsDict, Tickers),
	?assertEqual(2, length(Matches)),
	?assert(lists:member({<<"XXX">>, <<"c3">>}, Matches)),
	?assert(lists:member({<<"LWN">>, <<"c4">>}, Matches)).

find_matches_hash_collision_test() ->
	WordsDict = dict:from_list([{823394921, [<<"pat2">>]}, {4182447552, [<<"pat3">>, <<"XXX">>]}, {123, [<<"aaa">>]}]),
	Tickers = [{<<"DDD">>, <<"c1">>, 2334519064}, 
				  {<<"SVN">>, <<"c2">>, 3128474594}, 
				  {<<"XXX">>, <<"c3">>, 4182447552}, 
				  {<<"LWN">>, <<"c4">>, 1860772198}],
	?assertEqual([{<<"XXX">>, <<"c3">>}], find_matches(WordsDict, Tickers)).

-endif.
