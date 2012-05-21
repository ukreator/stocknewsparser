%% Author: dmitry
%% Created: May 11, 2012
%% Description: TODO: Add description to rss_parse
-module(snp_rss_parse).

%%
%% Include files
%%
-include_lib("xmerl/include/xmerl.hrl").

%%
%% Exported Functions
%%
-export([compare_feed_items/2, get_item_time/1, get_feed_items/1, is_rss2_feed/1]).

%%
%% API Functions
%%

compare_feed_items(OldItem, NewItem) ->
	ItemComparators = lists:map(fun (Elem) -> 
					   fun (X, Y) -> 
								compare_xml_value(X, Y, Elem)
					   end 
			  end,
			  ["guid", "title", "link"]),
	Comparators = [fun is_the_same/2] ++ ItemComparators,
	Result = compare_feed_items_helper(Comparators, OldItem, NewItem),
	Result.
  

get_item_time(_ItemElement) ->
 % use httpd_util:convert_request_date/1 to convert a raw string into 
 % the Erlang form of date/time values: {{Year,Month,Date},{Hour,Min,Sec}}. 
 % Then, you can use the calendar:datetime_to_gregorian_seconds/1 function to convert 
 % this date/time value into a single integer. 
 % Your function should return the atom bad_date if the date doesn't get parsed correctly.
	0.


get_feed_items(RootElement) ->
	xmerl_xpath:string("//item[link]", RootElement).


%%
is_rss2_feed({Document, _Rest}) ->
	is_rss2_feed(Document);
is_rss2_feed(#xmlElement{name = rss, attributes = Attributes}) ->
	lists:any(fun (#xmlAttribute{name = version, value = "2.0"}) -> true; (_) -> false end, Attributes);
is_rss2_feed(_) ->
	false.



%%
%% Local Functions
%%

compare_feed_items_helper([], _, _) -> different;
compare_feed_items_helper([Comparator | Rest], OldItem, NewItem) ->
	Result = Comparator(OldItem, NewItem),
	case Result of
		same -> same;
		different -> compare_feed_items_helper(Rest, OldItem, NewItem)
	end.

is_the_same(Val1, Val2) ->
	if Val1 =:= Val2 -> same;
	   true -> different
	end.

compare_xml_value(OldItem, NewItem, ElemName) ->
	XpathExpression = ElemName ++ "[1]/text()",
	OldTextNode = xmerl_xpath:string(XpathExpression, OldItem),
	NewTextNode = xmerl_xpath:string(XpathExpression, NewItem),
	case {OldTextNode, NewTextNode} of 
		{[#xmlText{value=OldVal}], [#xmlText{value=NewVal}]} -> is_the_same(OldVal, NewVal);
		{_, _} -> different
	end.


