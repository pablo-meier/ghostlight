-module(ghostlight_show).
-export([init/2]).
-export([content_types_provided/2]).
-export([show_to_html/2]).
-export([show_to_json/2]).
-export([show_to_text/2]).


init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, show_to_html},
      {<<"application/json">>, show_to_json},
      {<<"text/plain">>, show_to_text}
     ], Req, State}.


show_to_html(Req, State) ->
    ShowInfo = [{title, <<"An Octoroon">>}],
    {ok, Body} = show_template:render(ShowInfo),
    {Body, Req, State}.

show_to_json(Req, State) ->
    Body = <<"{\"data\": \"It's a show!\"}">>,
    {Body, Req, State}.

show_to_text(Req, State) ->
    {<<"It's a show!">>, Req, State}.
