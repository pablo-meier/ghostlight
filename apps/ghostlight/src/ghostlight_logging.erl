-module(ghostlight_logging).
-behavior(cowboy_middleware).
-export([execute/2]).

%%% @doc Middleware module responsible for logging every request.
execute(Req, Env) ->
    Path = cowboy_req:path(Req),
    case re:run(Path, "^/static/.*$") of
        nomatch ->
            Method = cowboy_req:method(Req),
            Now = iso8601:format(os:timestamp()),
            lager:info("~s  ~s - ~s", [Now, Method, Path]);
        _Else ->
            ok
    end,
    {ok, Req, Env}.
