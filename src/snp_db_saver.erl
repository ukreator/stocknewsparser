%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description :
%%%
%%% Created : 05.07.2012
%%% -------------------------------------------------------------------
-module(snp_db_saver).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include("snp_logging.hrl").
-include("snp_ticker_record.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, add_news/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------


% server state
-record(state, {db, bucket_name}).


%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_news(NewsObj) ->
	gen_server:call(?MODULE, {create_or_update, NewsObj}).

%% ====================================================================
%% Server functions
%% ====================================================================

% TODO: check for record presence before adding it to the DB 

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	RiakHost = snp_conf:get_val(riak_host),
	RiakPort = snp_conf:get_val(riak_port),
	RiakBucketName = list_to_binary(snp_conf:get_val(riak_bucket_name)),
	?INFO("Starting DB backend helper server. Connecting to Riak at host ~p and port ~p", [RiakHost, RiakPort]),
	{ok, RiakcPid} = riakc_pb_socket:start_link(RiakHost, RiakPort),
	?INFO("Successfully connected to Riak. Pid is ~p", [RiakcPid]),
    {ok, #state{db=RiakcPid, bucket_name=RiakBucketName}}.

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
handle_call({create_or_update, Data}, _From, State) ->
	% let Riak assign key to our data:
	Obj = riakc_obj:new(State#state.bucket_name, undefined, Data),
	Reply = case riakc_pb_socket:put(State#state.db, Obj) of
		{ok, Key} -> ?INFO("Added new object to Riak DB bucket ~p with key ~p: ~p", 
						   [State#state.bucket_name, Key, Data]),
					 ok;
		Other -> ?INFO("Failed to write data to Riak: ~p", [Other]),
				 error
	end,
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
	?INFO("snp_db_saver terminating", []),
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

