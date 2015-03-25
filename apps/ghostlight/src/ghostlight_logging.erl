-module(ghostlight_logging).
-behavior(cowboy_middleware).
-export([execute/2]).

%%% Module responsible for logging every request.
execute(Req, Env) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    Now = iso8601:format(now()),
    lager:info("~s  ~s - ~s", [Now, Method, Path]),
    {ok, Req, Env}.
