%%% @doc Wrapper around the Cowboy REST endpoint to handle common callback values,
%%% since every resource will take the same content-types, allow the same methods,
%%% etc.
-module(ghostlight_resource).
-behaviour(gen_server).

-export([start_link/0]).
%% gen_server exports
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([
         register/1
        ]).

%% Cowboy REST exports
-export([init/2,
         content_types_provided/2,
         content_types_accepted/2,
         charsets_provided/2,
         allowed_methods/2,

         post_resource/2,
         resource_to_html/2,
         resource_to_json/2]).


-define(SERVER, ?MODULE).

-record(render_pack, {
            module,
            get_template,
            get_listing_template,
            update_template
         }).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%% @private
init([]) ->
    {ok, maps:new()}.

%%% @private
handle_call({get_module, Name}, _From, State) ->
    Reply = maps:get(Name, State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%% @private
handle_cast({register, Name, Pack}, State) ->
    NewState = maps:put(Name, Pack, State),
    {noreply, NewState}.

%%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%%% @private
terminate(_Reason, _State) ->
    ok.

%%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Cowboy handlers

%%% @private
init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.


%%% @private
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"PUT">>],
     Req, State}.


%%% @private
charsets_provided(Req, State) ->
    {[<<"utf-8">>], Req, State}.


%%% @private
content_types_provided(Req, State) ->
    {[
      {<<"text/html">>, resource_to_html},
      {<<"application/json">>, resource_to_json}
     ], Req, State}.


%%% @private
content_types_accepted(Req, State) ->
    {[
      {<<"application/json; charset=utf-8">>, post_resource},
      {<<"application/json">>, post_resource}
     ], Req, State}.


resource_to_json(Req, State) ->
    ResourceName = cowboy_req:binding(resource, Req),
    Id = cowboy_req:binding(resource_id, Req),
    #render_pack{module = Module} = gen_server:call(?SERVER, {get_module, ResourceName}),
    Req2 = cowboy_req:set_meta(response_type, json, Req),

    case Id of
        undefined ->
            ToEncode = Module:get_listings_json(),
            Body = jiffy:encode({ToEncode}),
            {Body, Req2, State};
        _ ->
            ToEncode = Module:get_json(Id),
            AsJson = jiffy:encode(ToEncode),
            {AsJson, Req2, State}
    end.


resource_to_html(Req, State) ->
    ResourceName = cowboy_req:binding(resource, Req),
    Id = cowboy_req:binding(resource_id, Req),
    Command = cowboy_req:binding(command, Req),

    #render_pack {
       module = Module,
       get_template = GetTemplate,
       get_listing_template = ListingTemplate,
       update_template = UpdateTemplate
    } = gen_server:call(?SERVER, {get_module, ResourceName}),
    Req2 = cowboy_req:set_meta(response_type, html, Req),

    case {Id, Command} of
        {undefined, undefined} ->
            Proplist = Module:get_listings_html(),
            {ok, Body} = ListingTemplate:render(Proplist),
            {Body, Req2, State};
        {<<"new">>, _} ->
            {ok, Body} = UpdateTemplate:render([]),
            {Body, Req2, State};
        {_, undefined} ->
            Proplist = Module:get_html(Id),
            {ok, Body} = GetTemplate:render(Proplist),
            {Body, Req2, State};
        {_, <<"edit">>} ->
            Proplist = Module:edit_html(Id),
            {ok, Body} = UpdateTemplate:render(Proplist),
            {Body, Req2, State}
    end.

post_resource(Req, State) ->
    ResourceName = cowboy_req:binding(resource, Req),
    #render_pack{module = Module} = gen_server:call(?SERVER, {get_module, ResourceName}),
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    Req3 = cowboy_req:set_meta(response_type, html, Req2),
    AsJson = jiffy:decode(RequestBody),
    Record = Module:json_to_record(AsJson),
    Method = cowboy_req:method(Req3),
    Id = Module:get_id(Record),
    case {Id, Method} of
        {null, <<"POST">>} ->
            NewId = Module:post_json(Record),
            Response = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"id">>, list_to_binary(NewId)}]}),
            {true, cowboy_req:set_resp_body(Response, Req3), State};
        {_Else, <<"POST">>} ->
            Body = jiffy:encode({[{<<"error">>, <<"You may not insert with the field 'id'.">>}]}),
            Req3 = cowboy_req:set_resp_body(Body, Req3),
            {false, Req3, State};
        {null, <<"PUT">>} ->
            Body = jiffy:encode({[{<<"error">>, <<"You must PUT on an existing resource.">>}]}),
            Req4 = cowboy_req:set_resp_body(Body, Req3),
            {false, Req4, State};
        {_PersonId, <<"PUT">>} ->
            Success = Module:update_json(Record),
            case Success of
                true ->
                    Response = jiffy:encode({[{<<"status">>, ok}]}),
                    {true, cowboy_req:set_resp_body(Response, Req3), State};
                false ->
                    Body = jiffy:encode({[{<<"error">>, <<"An error occurred.">>}]}),
                    Req4 = cowboy_req:set_resp_body(Body, Req3),
                    {false, Req4, State}
            end
    end.


%% External API

-type ghostlight_rest_resource() :: {resource_name, Name::binary()}
                                  | {module, Module::module()}
                                  | {get_template, Module::module()}
                                  | {get_listing_template, Module::module()}
                                  | {update_template , Module::module()}.
-spec register(Options :: list(ghostlight_rest_resource())) -> none().
%% @doc Allows a module to register itself as a resource.
register(Options) ->
    Name = ensure_binary(proplists:get_value(resource_name, Options)),
    Module = proplists:get_value(module, Options),
    [
     {get_template, GetHtml},
     {get_listing_template, ListingsHtml},
     {update_template, UpdateHtml}
    ] = make_template_names(proplists:get_value(template_base, Options)),

    Pack = #render_pack {
        module = Module,
        get_template = GetHtml,
        get_listing_template = ListingsHtml,
        update_template = UpdateHtml
    },
    gen_server:cast(?MODULE, {register, Name, Pack}).


-spec ensure_binary(Bin :: string() | binary()) -> binary().
ensure_binary(Bin) when is_list(Bin) -> list_to_binary(Bin);
ensure_binary(Bin) when is_binary(Bin) -> Bin.

make_template_names(Basename) ->
    AsString = atom_to_list(Basename),
    [
     {get_template, list_to_atom(AsString ++ "_template")},
     {get_listing_template, list_to_atom(AsString ++ "_listing_template")},
     {update_template, list_to_atom("insert_" ++ AsString ++ "_template")}
    ].
