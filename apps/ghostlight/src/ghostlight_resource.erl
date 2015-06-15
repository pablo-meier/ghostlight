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
    Resources = 
        [
         [{resource_name, "people"},
          {module, ghostlight_people},
          {template_base, person}],

         [{resource_name, "shows"},
          {module, ghostlight_show},
          {template_base, show}],

         [{resource_name, "organizations"},
          {module, ghostlight_org},
          {template_base, org}],

         [{resource_name, "works"},
          {module, ghostlight_work},
          {template_base, work}]
        ],
    State = register(Resources),

    {ok, State}.

%%% @private
handle_call({get_module, Name}, _From, State) ->
    case maps:find(Name, State) of
        error -> {reply, not_found, State};
        {ok, Reply} ->{reply, Reply, State} 
    end;
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

%%% @private
resource_to_json(Req, State) ->
    Req2 = cowboy_req:set_meta(response_type, json, Req),
    ResourceName = cowboy_req:binding(resource, Req2),
    Id = cowboy_req:binding(resource_id, Req2),
    try
        #render_pack{module = Module} = get_renderpack(ResourceName),
        Body = make_appropriate_json(Id, Module),
        {Body, Req2, State}
    catch
        throw:not_found -> pass_on_error(404, Req2, State);
        error:Err -> 
            lager:error("ERROR! ~n~p", [Err]),
            pass_on_error(500, Req2, State)
    end.

make_appropriate_json(undefined, Module) ->
    ToEncode = Module:get_listings_json(),
    jiffy:encode({ToEncode});
make_appropriate_json(Id, Module) ->
    ToEncode = Module:get_json(Id),
    jiffy:encode(ToEncode).


%%% @private
resource_to_html(Req, State) ->
    Req2 = cowboy_req:set_meta(response_type, html, Req),
    ResourceName = cowboy_req:binding(resource, Req2),
    Id = cowboy_req:binding(resource_id, Req2),
    Command = cowboy_req:binding(command, Req2),

    try
        RenderPack = get_renderpack(ResourceName),
        Body = make_appropriate_html(Id, Command, RenderPack),
        {Body, Req2, State}
    catch
        throw:not_found -> pass_on_error(404, Req2, State);
        error:Err ->
            lager:error("ERROR! ~n~p", [Err]),
            pass_on_error(500, Req2, State)
    end.


make_appropriate_html(undefined, undefined, #render_pack{module=Module,
                                                         get_listing_template=ListingTemplate}) ->
    Proplist = Module:get_listings_html(),
    {ok, Body} = ListingTemplate:render(Proplist),
    Body;
make_appropriate_html(<<"new">>, _, #render_pack{update_template=UpdateTemplate}) ->
    {ok, Body} = UpdateTemplate:render([]),
    Body;
make_appropriate_html(Id, undefined,#render_pack{module=Module,
                                                 get_template=GetTemplate}) ->
    Proplist = Module:get_html(Id),
    {ok, Body} = GetTemplate:render(Proplist),
    Body;
make_appropriate_html(Id, <<"edit">>, #render_pack{module=Module,
                                                   update_template=UpdateTemplate} ) ->
    Proplist = Module:edit_html(Id),
    {ok, Body} = UpdateTemplate:render(Proplist),
    Body.




%%% @private
post_resource(Req, State) ->
    ResourceName = cowboy_req:binding(resource, Req),
    #render_pack{module = Module} = get_renderpack(ResourceName),
    {ok, RequestBody, Req2} = cowboy_req:body(Req),
    Req3 = cowboy_req:set_meta(response_type, json, Req2),
    AsJson = jiffy:decode(RequestBody),
    Record = Module:json_to_record(AsJson),
    Method = cowboy_req:method(Req3),
    Id = Module:get_id(Record),

    try make_appropriate_json(Id, Method, Module, Record) of
        {Status, Response} -> {Status, cowboy_req:set_resp_body(Response, Req3), State}
    catch
        throw:not_found -> pass_on_error(404, Req3, State);
        error:Err ->
            lager:error("ERROR: ~p~n", [Err]),
            pass_on_error(500, Req2, State)
    end.


make_appropriate_json(null, <<"POST">>, Module, Record) ->
    NewId = Module:post_json(Record),
    Response = jiffy:encode({[{<<"status">>, <<"ok">>}, {<<"id">>, list_to_binary(NewId)}]}),
    {true, Response};
make_appropriate_json(_Else, <<"POST">>, _Module, _Record) ->
    Body = jiffy:encode({[{<<"error">>, <<"You may not insert with the field 'id'.">>}]}),
    {false, Body};
make_appropriate_json(null, <<"PUT">>, _Module, _Record) ->
    Body = jiffy:encode({[{<<"error">>, <<"You must PUT on an existing resource.">>}]}),
    {false, Body};
make_appropriate_json(_Else, <<"PUT">>, Module, Record) ->
    Success = Module:update_json(Record),
    case Success of
        true ->
            Response = jiffy:encode({[{<<"status">>, ok}]}),
            {true, Response};
        false ->
            Body = jiffy:encode({[{<<"error">>, <<"An error occurred.">>}]}),
            {false, Body}
    end.


pass_on_error(StatusCode, Req, State) ->
    Req2 = cowboy_req:reply(StatusCode, Req),
    {halt, Req2, State}.


get_renderpack(ResourceName) ->
    case gen_server:call(?SERVER, {get_module, ResourceName}) of
        not_found -> throw(not_found);
        Else -> Else
    end.

%% External API
-type ghostlight_rest_resource() :: {resource_name, Name::binary()}
                                  | {module, Module::module()}
                                  | {get_template, Module::module()}
                                  | {get_listing_template, Module::module()}
                                  | {update_template , Module::module()}.
-spec register(Options :: list(list(ghostlight_rest_resource()))) -> none().
%% @doc Allows a module to register itself as a resource.
register(ResourceSpecs) ->
    lists:foldl(fun (Option, Accum) ->
                        {Name, RenderPack} = register_resource(Option),
                        maps:put(Name, RenderPack, Accum)
                end, maps:new(), ResourceSpecs).

register_resource(Options) ->
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
    {Name, Pack}.


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
