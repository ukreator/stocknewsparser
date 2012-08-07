%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description : RSS XML document parsing functions.
%%%
%%% Created : 11.05.2012
%%% -------------------------------------------------------------------
-module(snp_rss_parse).



%%
%% Include files
%%

-include("snp_rss_item.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include("snp_logging.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%
%% Exported Functions
%%
-export([get_items/1]).

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


%%
%% Local Functions
%%

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
			  

is_rss2_feed({Document, _Rest}) ->
	is_rss2_feed(Document);
is_rss2_feed(#xmlElement{name = rss, attributes = Attributes}) ->
	lists:any(fun (#xmlAttribute{name = version, value = "2.0"}) -> true; (_) -> false end, Attributes);
is_rss2_feed(_) ->
	false.

get_field(XmlItem, FieldName) ->
	FieldVal = xmerl_xpath:string(FieldName ++ "[1]/text()", XmlItem),
	case FieldVal of
		[Val] -> {ok, Val#xmlText.value};
		[]    -> {error}
	end.


get_feed_items(RootElement) ->
	xmerl_xpath:string("//item[link]", RootElement).


%% --------------------------------------------------------------------
%%% Unit tests
%% --------------------------------------------------------------------

				   
-ifdef(TEST).

-define(TestDirPath, "../priv/test").


is_rss2_feed_test_() ->
	{
	 setup,
	 fun is_rss2_feed_start/0,
	 fun ({Rss2Content, Rss1Content, NonRssXml}) -> [
		?_assertNot(is_rss2_feed({test_not_a_doc, []})),
	 	?_assert(is_rss2_feed(Rss2Content)),
		?_assertNot(is_rss2_feed(Rss1Content)),
		?_assertNot(is_rss2_feed(NonRssXml))
	 ] end}.


is_rss2_feed_start() ->
	Rss2Content = xmerl_scan:file(testfile("test_rss2_feed.xml")),
	Rss1Content = xmerl_scan:file(testfile("test_rss1_feed.xml")),
	NonRssXml = xmerl_scan:file(testfile("non_rss.xml")),
	{Rss2Content, Rss1Content, NonRssXml}.

% helper function
testfile(Path) ->
	filename:join([?TestDirPath, Path]).

-endif.