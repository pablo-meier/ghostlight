-module(ghostlight_resource_behavior).

-type property() :: {atom(), string() | binary() | null}.
-type proplist() :: list(property()).
 
-callback
get_html(Id :: binary()) -> proplist().
 
-callback
get_listings_html() -> proplist().
 
-callback
edit_html(Id :: binary()) -> proplist().


-callback
get_listings_json() -> proplist().

-callback
get_json(Record :: tuple()) -> proplist().

-callback
post_json(Input :: tuple()) -> binary().

-callback
edit_json(Input :: proplist()) ->  binary().

-callback
json_to_record(Json :: proplist()) -> tuple().

-callback
get_id(Record :: tuple()) -> binary().
