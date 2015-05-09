-module(ghostlight_devtools_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 60}, 
           [{ghostlight_devtools_gen_sever,
             {ghostlight_devtools, start_link, []},
             permanent,
             5000,
             worker,
             [ghostlight_devtools]}]} }.


