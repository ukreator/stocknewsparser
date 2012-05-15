%% Author: Dmitry Sobinov
%% Created: May 11, 2012
%% Description: TODO: Add description to rss_parse_tests
-module(rss_parse_tests).

% rebar-specific path to get test files when issuing 'rebar eunit'
-define(TestDirPath, "../priv/test").

%%
%% Include files
%%
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%
%% Exported Functions
%%
-export([]).

%%
%% API Functions
%%



%%
%% Local Functions
%%


is_rss2_feed_test_() ->
	{
	 setup,
	 fun is_rss2_feed_start/0,
	 fun ({Rss2Content, Rss1Content, NonRssXml}) -> [
		?_assertNot(rss_parse:is_rss2_feed({test_not_a_doc, []})),
	 	?_assert(rss_parse:is_rss2_feed(Rss2Content)),
		?_assertNot(rss_parse:is_rss2_feed(Rss1Content)),
		?_assertNot(rss_parse:is_rss2_feed(NonRssXml))
	 ] end}.



is_rss2_feed_start () ->
	Rss2Content = xmerl_scan:file(testfile("test_rss2_feed.xml")),
	Rss1Content = xmerl_scan:file(testfile("test_rss1_feed.xml")),
	NonRssXml = xmerl_scan:file(testfile("non_rss.xml")),
	{Rss2Content, Rss1Content, NonRssXml}.



compare_feed_items_test_() ->
	{setup,
	 fun compare_feed_items_start/0,
	 fun ({Item1, Item1GuidSame, Item1TitleSame, Item2}) -> [
		?_assertEqual(rss_parse:compare_feed_items(Item1, Item1GuidSame), same),
		?_assertEqual(rss_parse:compare_feed_items(Item1, Item1TitleSame), same),
		?_assertEqual(rss_parse:compare_feed_items(Item1, Item2), different)
	 ] end}.

compare_feed_items_start() ->
	{Item1, _} = xmerl_scan:string("<item>" ++
                      "<guid>12345</guid>" ++
                      "<link>http://some-link.com/some/element.html</link>" ++
                      "<title>New Title</title>" ++
                    "</item>"),
	{Item1GuidSame, _} = xmerl_scan:string("<item>" ++
                      "<guid>12345</guid>" ++
                    "</item>"),
	{Item1TitleSame, _} = xmerl_scan:string("<item>" ++
					"<title>New Title</title>" ++
                    "</item>"),
	{Item2, _} = xmerl_scan:string("<item>" ++
                      "<guid>8888</guid>" ++
                      "<link>http://some-ANOTHER-link.com/some/element.html</link>" ++
                      "<title>New-New Title</title>" ++
                    "</item>"),
	{Item1, Item1GuidSame, Item1TitleSame, Item2}.


% helper function
testfile(Path) ->
	filename:join([?TestDirPath, Path]).
