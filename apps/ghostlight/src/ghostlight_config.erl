%%% Gets config data from either the `sys.config` file that got loaded in
%%% the release, or the environment if applicable. Caches it's values,
%%% crashes the app if it can't find something.
-module(ghostlight_config).
% -behaviour(gen_server).
% 
% -export([start_link/0]).
% -export([init/1,
%          handle_call/3,
%          handle_cast/2,
%          handle_info/2,
%          terminate/2,
%          code_change/3]).
% 
-export([get/1]).

% -record(state, {cache=#{}}).
% -define(SERVER, ?MODULE).

% start_link() ->
%     gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
% 
% init([]) ->
%     State = #state{},
%     {ok, State}.
% 
% handle_call({get, Key}, _From, State=#state{cache=Cache}) ->
%     case maps:is_key(Key, Cache) of
%         true ->
%             Value = maps:get(Key, Cache),
%             {reply, Value, State};
%         false ->
%             Value = fetch_fresh(Key),
%             NewCache = maps:update(Key, Value, Cache),
%             {reply, Value, NewCache}
%     end.

fetch_fresh(Key) when is_atom(Key) ->
    case application:get_env(Key) of
        {ok, Value} ->
            Value;
        undefined ->
            KeyAsStr = atom_to_list(Key),
            case os:getenv(KeyAsStr) of
                false ->
                    throw({missing_config_var, Key});
                EnvVal -> EnvVal
            end
    end.

% handle_cast(_Msg, State) ->
%     {noreply, State}.
% handle_info(_Info, State) ->
%     {noreply, State}.
% terminate(_Reason, _State) ->
%     lager:info("Closed the config module (?)").
% code_change(_OldVsn, State, _Extra) ->
%     {ok, State}.


get(Key) ->
    fetch_fresh(Key).
%    gen_server:call(?MODULE, {get, Key}).
