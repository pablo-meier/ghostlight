%%%-------------------------------------------------------------------
%% @doc ghostlight_db top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ghostlight_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 5, 60}, 
           [{postgres_toplevel,
             {ghostlight_db, start_link, []},
             permanent,
             5000,
             worker,
             [ghostlight_db]},
          {show_server,
             {ghostlight_db_show, start_link, []},
             permanent,
             5000,
             worker,
             [ghostlight_db_show]},
          {person_server,
             {ghostlight_db_person, start_link, []},
             permanent,
             5000,
             worker,
             [ghostlight_db_person]},
          {works_server,
             {ghostlight_db_work, start_link, []},
             permanent,
             5000,
             worker,
             [ghostlight_db_work]},
          {resource_server,
             {ghostlight_db_resource, start_link, []},
             permanent,
             5000,
             worker,
             [ghostlight_db_resource]} 
           ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
