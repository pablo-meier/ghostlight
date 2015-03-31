-module(ghostlight_markdown).
-export([init/0,
         parse_markdown/1]).
-on_load(init/0).
-define(APPNAME, ghostlight_markdown).


init() ->
    case code:priv_dir(?APPNAME) of
        {error, _} ->
            lager:error("~w priv dir not found~n", [?APPNAME]),
            exit(error);
        PrivDir ->
            erlang:load_nif(filename:join([PrivDir, "ghostlight_markdown_nifs"]), 0)
    end.

parse_markdown(_Input) ->
    erlang:nif_error(nif_not_loaded).

