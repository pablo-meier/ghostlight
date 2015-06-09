%%%-------------------------------------------------------------------
%% @doc ghostlight top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ghostlight_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 5, 60},
          [{resource_listener,
            {ghostlight_resource, start_link, []},
            permanent,
            5000,
            worker,
            [ghostlight_resource]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
