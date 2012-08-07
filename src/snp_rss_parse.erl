%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description : RSS XML document parsing functions.
%%%
%%% Created : 11.05.2012
%%% -------------------------------------------------------------------
-module(snp_rss_parse).


% TODO: cleanup this file

%%
%% Include files
%%

-include("snp_rss_item.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("snp_logging.hrl").

%%
%% Exported Functions
%%
-export([compare_feed_items/2, get_item_time/1, get_feed_items/1, is_rss2_feed/1, get_items/1]).

%%
%% API Functions
%%

get_items(XmlBody) ->
	{Document, _} = xmerl_scan:string(XmlBody),
	RssCheckResult = is_rss2_feed(Document),
	case RssCheckResult of
		false -> {error, not_rss2_document};
		true  -> {ok, parse_doc(Document)}
	end.

parse_doc(Document) ->
	XmlItems = get_feed_items(Document),
	lists:map(fun(XmlItem) ->				  
					  {ok, Link} = get_field(XmlItem, "link"),
					  {ok, PublishDate} = get_field(XmlItem, "pubDate"),
					  Title = get_field(XmlItem, "title"),
					  Guid = get_field(XmlItem, "guid"),
					  
					  Required = #rss_item{
								link=Link,
								publish_date=PublishDate},
					  WithTitle = case Title of
							{ok, TitleVal} -> Required#rss_item{title=TitleVal};
							_ -> Required
					  end,
					  case Guid of
							{ok, GuidVal} -> WithTitle#rss_item{guid=GuidVal};
							_ -> WithTitle
					  end
				end, XmlItems).
			  



get_field(XmlItem, FieldName) ->
	FieldVal = xmerl_xpath:string(FieldName ++ "[1]/text()", XmlItem),
	case FieldVal of
		[Val] -> {ok, Val#xmlText.value};
		[]    -> {error}
	end.


			  
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


