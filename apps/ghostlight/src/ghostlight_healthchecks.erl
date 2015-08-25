%%%-------------------------------------------------------------------
%%% @author pablo
%%% @copyright (C) 2015, pablo
%%% @doc
%%% Runs healthchecks for the various non-module services we use (Markdown,
%%% Jinterface nodes, PSQL), Dropwizard-style.
%%% @end
%%% Created : 2015-08-25 14:19:51.654805
%%%-------------------------------------------------------------------
-module(ghostlight_healthchecks).

-behaviour(gen_server).

%% API
-export([start_link/0,
         register/3,
         run/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(healthcheck, {
          module   = throw(empty_module_for_healthcheck)   :: atom(),
          function = throw(empty_function_for_healthcheck) :: atom(),
          message = throw(empty_msg_for_healthcheck)       :: binary()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Adds a healthcheck to the system. The healthcheck should have a
%% module, exported function to call, and a message. It should return
%% `ok` or `{error, Description :: binary()}`.
%% @end
%%--------------------------------------------------------------------
register(Module, Function, Msg) ->
    gen_server:cast(?MODULE, {register, #healthcheck {
        module = Module,
        function = Function,
        message = Msg
    }}).

%%--------------------------------------------------------------------
%% @doc
%% Runs the healthchecks.
%% @end
%%--------------------------------------------------------------------
run() ->
    Healthchecks = gen_server:call(?MODULE, get_healthchecks),
    Results = lists:map(fun run_healthcheck/1, Healthchecks),
    case lists:all(fun (X) -> X end, Results) of
        true -> lager:info("ALL GOOD IN THE HOOD");
        false -> lager:info("WE HAD ERRORS X_X")
    end.

run_healthcheck(#healthcheck {
    module = M,
    function = F,
    message = Msg
}) ->                   
    lager:info("---------- Running Healthcheck: ~s~n", [Msg]),
    case M:F() of
        ok -> true;
        {error, Err} ->
            lager:error("Error! ~p~n", [Err]),
            false
    end.


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, []}.

handle_call(get_healthchecks, _From, State) ->
    {reply, State, State};
handle_call(Op, _From, _State) ->
    throw({unknown_operation, Op}).

handle_cast({register, Healthcheck}, State) ->
    {noreply, [Healthcheck|State]};
handle_cast(Op, _State) ->
    throw({unknown_operation, Op}).

handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




