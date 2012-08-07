%% Author: Dmitry Sobinov 
%% Email: sobinov@crystalnix.com
%% Created: 07.08.2012
%% Description: Helper module for handling 
%% configuration values from different sources.
%% It's not optimized and supposed to be used only on application startup 
%% or in not perfomance critical places.

-module(snp_conf).

%%
%% Include files
%%

-include("snp_logging.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% Exported Functions
%%
-export([init/0, get_val/1]).

%%
%% API Functions
%%

init() ->
	ok.

%% @spec get_val(atom()) -> string().
get_val(OptionName) ->
	case application:get_env(stocknewsparser, config_file) of
		undefined -> get_fallback_val(OptionName);
		{ok, ConfigFilePath} -> 
				try_config_file(OptionName, ConfigFilePath)
	end.



%%
%% Local Functions
%%

try_config_file(OptionName, ConfigFilePath) ->
	case get_config_file_option(OptionName, ConfigFilePath) of
		{ok, OptionVal} -> OptionVal;
		none ->
			get_fallback_val(OptionName)
	end.

get_fallback_val(OptionName) ->
	case application:get_env(stocknewsparser, OptionName) of
		{ok, EnvOptVal} -> EnvOptVal;
		undefined -> 
			?ERROR("Required option ~p is not set. Exiting...", [OptionName]),
			exit(config_option_is_not_set)
	end.

get_option_from_proplist(OptionName, Proplist) ->
	case proplists:lookup(OptionName, Proplist) of
		{_Key, Val} -> {ok, Val};
		none -> none
	end.

get_config_file_option(OptionName, ConfigFilePath) ->
	case read_config_file(ConfigFilePath) of
		{ok, Content} ->
			get_option_from_proplist(OptionName, Content);
		none -> none
	end.


read_config_file(FilePath) ->
	case file:consult(FilePath) of
	   {ok, Content} -> {ok, Content};
	   {error, Reason} ->
		    ?ERROR("Failed to read config file ~p (~p)", [FilePath, Reason]),
			none
	end.


%% --------------------------------------------------------------------
%%% Unit tests
%% --------------------------------------------------------------------


-ifdef(TEST).

not_found_option_test() ->
	?assertExit(config_option_is_not_set, get_fallback_val(unrecognized_optionnnn)).

read_none_config_file_test() ->
	Result = read_config_file("some_unknown_file.data"),
	?assertEqual(none, Result).

read_config_file_test() ->
	{ok, Content} = read_config_file("../priv/test/config.test"),
	?assert(is_list(Content)).

get_config_file_option_test() ->
	Result = get_config_file_option(option2, "../priv/test/config.test"),
	OptionVal = ["val1", "val2"],
	?assertEqual({ok, OptionVal}, Result).

get_non_existant_config_file_option_test() ->
	Result = get_config_file_option(not_existent_option, "../priv/test/config.test"),
	?assertEqual(none, Result).


-endif.