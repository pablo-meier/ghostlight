-module(ghostlight_test_utils).
-include("apps/ghostlight/include/ghostlight_data.hrl").
-export([read_fixture/1]).

read_fixture(Paths) ->
    PrivDir = code:priv_dir(ghostlight),
    Path = filename:join([PrivDir, "test", "fixtures"] ++ Paths),
    {ok, Bin} = file:read_file(Path),
    Bin.
