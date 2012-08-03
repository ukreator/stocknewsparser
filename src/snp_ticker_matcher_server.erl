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
-export([start_link/0, find_known_tickers/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {ticker_dict}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

find_known_tickers(ExtractedTickers) ->
	gen_server:call(?MODULE, {find_known_tickers, ExtractedTickers}).


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
	TickerDict = get_tickers(TickerFiles),
    {ok, #state{ticker_dict=TickerDict}}.

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

handle_call({find_known_tickers, ExtractedTickers}, _From, State) ->
	Reply = find_known_tickers(ExtractedTickers, State#state.ticker_dict),
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
	lists:foldl(fun(Elem, Accum) ->
				Ticker = element(1, Elem),
				CompanyName = element(2, Elem),
				dict:store(list_to_binary(Ticker), list_to_binary(CompanyName), Accum)
			end, dict:new(), CompaniesInfo).


find_known_tickers(TickersFromText, KnownTickersDict) ->
	TickerCheckFun = fun(Ticker, MatchingTickers) ->
						case dict:find(Ticker, KnownTickersDict) of
							{ok, CompanyName} -> [{Ticker, CompanyName} | MatchingTickers];
							error -> MatchingTickers
						end
					 end,
	lists:foldl(TickerCheckFun, [], TickersFromText).



%% --------------------------------------------------------------------
%%% Unit tests
%% --------------------------------------------------------------------


-ifdef(TEST).

get_tickers_test() ->
	Tickers = get_tickers(["../priv/test/test1.csv", "../priv/test/test2.csv"]),
	?assertEqual({ok, <<"3D Systems Corporation">>}, dict:find(<<"DDD">>, Tickers)),
	?assertEqual({ok, <<"Low Wan New Co.">>}, dict:find(<<"LWN">>, Tickers)),
	?assertEqual({ok, <<"Freq From Far Inc.">>}, dict:find(<<"FFF">>, Tickers)).

find_known_tickers_test() ->
	TickersDict = dict:from_list([{<<"SVN">>, <<"company 1">>}, {<<"XXX">>, <<"company 2">>}, {<<"YYY">>, <<"company 3">>}]),
	TickersFromText = [<<"DDD">>, <<"XXX">>],
	KnownTickers = find_known_tickers(TickersFromText, TickersDict),
	?assertEqual(1, length(KnownTickers)),
	?assert(lists:member({<<"XXX">>, <<"company 2">>}, KnownTickers)).

-endif.
