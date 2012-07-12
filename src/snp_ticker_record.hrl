%%% -------------------------------------------------------------------
%%% Author  : Dmitry Sobinov
%%% Email: sobinov@crystalnix.com
%%% Description : Record representing a matched ticker item ready to be recorded to DB.
%%%
%%% Created : 12.07.2012
%%% -------------------------------------------------------------------

-record(ticker_item, {ticker_symbol, company_name, link, title, publish_date, guid}).
