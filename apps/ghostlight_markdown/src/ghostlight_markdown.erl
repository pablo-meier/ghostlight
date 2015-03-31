-module(ghostlight_markdown).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([parse_markdown/1]).

-define(SERVER, ?MODULE).
-define(APPNAME, ghostlight_markdown).

-record(state, {port}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Port = create_port(),
    {ok, #state{port=Port}}.

handle_call({parse_markdown, Body}, _From, State=#state{port=Port}) ->
    Length = size(Body),
    WithLength = <<Length:4/native-unsigned-unit:8, Body/binary>>,
    Port ! {self(), {command, WithLength}},
    receive
        {Port, {data, Data}} ->
            {reply, list_to_binary(Data), State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, Status}}, #state{port=Port}) ->
    lager:error("Markdown port exited with status ~p; restarting", [Status]),
    NewPort = create_port(),
    {noreply, #state{port=NewPort}};

handle_info(Info, State) ->
    lager:info("Other info received! ~p", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


parse_markdown(Body) when is_binary(Body) ->
    gen_server:call(?MODULE, {parse_markdown, Body}).

create_port() ->
    case code:priv_dir(?APPNAME) of
        {error, _} ->
            error_logger:format("~w priv dir not found~n", [?APPNAME]),
            exit(error);
        PrivDir ->
            open_port({spawn, filename:join([PrivDir, "cmark_wrapper"])},
                             [stream, exit_status])
    end.

