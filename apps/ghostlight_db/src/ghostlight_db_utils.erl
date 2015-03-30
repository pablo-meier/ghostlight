-module(ghostlight_db_utils).
-export([connect_to_postgres/0,
         fresh_uuid/0,
         null_if_unspecified/1]).


connect_to_postgres() ->
    {ok, C} = epgsql:connect("localhost", "pablo", "", [{database, "ghostlight-dev"}]),
    C.

fresh_uuid() ->
    uuid:to_string(uuid:uuid4()).

null_if_unspecified({}) -> null;
null_if_unspecified(<<"">>) -> null;
null_if_unspecified(Else) -> Else.



