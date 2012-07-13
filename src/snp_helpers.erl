%% Author: Dmitry Sobinov 
%% Email: sobinov@crystalnix.com
%% Created: 13.07.2012
%% Description: Helper methods.
-module(snp_helpers).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([hash_32bit/1]).

%%
%% API Functions
%%

hash_32bit(Val) ->
	TwoPow32 = 4294967296,
	erlang:phash2(Val, TwoPow32).
	

%%
%% Local Functions
%%

