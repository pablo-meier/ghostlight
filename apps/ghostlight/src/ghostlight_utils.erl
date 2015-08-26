-module(ghostlight_utils).

-export([erl_date_to_iso8601/1,

         external_links_json_to_record/1,
         external_links_record_to_proplist/1,
         external_links_record_to_json/1,
         validate_external_links/1,

         vanity_name_json_to_binary/1,
         validate_vanity_name/1,

         json_with_valid_values/1,
         proplist_with_valid_values/1,

         handle_errors/4,
         person_or_org_json_to_record/1,
         person_or_org_record_to_json/1,
         person_or_org_record_to_proplist/1,
         validate_person_or_org/1,

         default_list/1,
         ensure_minimum_length/3,
         remove_null/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").


external_links_json_to_record(Json) when is_list(Json) ->
    case proplists:get_value(<<"social">>, Json) of
        undefined -> null;
        SocialBlock ->
            #external_links{
                website = proplists:get_value(<<"website">>, SocialBlock, null),
                email_address = proplists:get_value(<<"email">>, SocialBlock, null),
                blog = proplists:get_value(<<"blog">>, SocialBlock, null),
                mailing_list = proplists:get_value(<<"newsletter">>, SocialBlock, null),
                facebook = proplists:get_value(<<"facebook">>, SocialBlock, null),
                twitter = proplists:get_value(<<"twitter">>, SocialBlock, null),
                instagram = proplists:get_value(<<"instagram">>, SocialBlock, null),
                vimeo = proplists:get_value(<<"vimeo">>, SocialBlock, null),
                youtube = proplists:get_value(<<"youtube">>, SocialBlock, null),
                pinterest = proplists:get_value(<<"pinterest">>, SocialBlock, null),
                tumblr = proplists:get_value(<<"tumblr">>, SocialBlock, null),
                gplus = proplists:get_value(<<"gplus">>, SocialBlock, null),
                patreon = proplists:get_value(<<"patreon">>, SocialBlock, null),
                newplayx = proplists:get_value(<<"newplayx">>, SocialBlock, null)
            }
    end.


vanity_name_json_to_binary(Json) when is_list(Json) ->
    validate_vanity_name(proplists:get_value(<<"vanity">>, Json, null)).

validate_vanity_name(null) -> null;
validate_vanity_name(Vanity) when is_binary(Vanity), byte_size(Vanity) < 25 ->
    matches_vanity_pattern(Vanity, re:run(Vanity, "^[a-zA-Z0-9_.]+$"));
validate_vanity_name(_) -> throw(invalid_vanity_name_format).

matches_vanity_pattern(Value, {match, _}) -> Value;
matches_vanity_pattern(_, nomatch) -> throw(invalid_vanity_name_format).

external_links_record_to_proplist(null) -> [];
external_links_record_to_proplist(
    #external_links{
        website=Website,
        email_address=Email,
        blog=Blog,
        mailing_list=Newsletter,
        facebook=Facebook,
        twitter=Twitter,
        instagram=Instagram,
        vimeo=Vimeo,
        youtube=YouTube,
        pinterest=Pinterest,
        tumblr=Tumblr,
        gplus=GPlus,
        patreon=Patreon,
        newplayx=NewPlayX
    }) ->
    Candidate = [{website, Website},
                 {email, Email},
                 {blog, Blog},
                 {newsletter, Newsletter},
                 {facebook, Facebook},
                 {twitter, Twitter},
                 {instagram, Instagram},
                 {vimeo, Vimeo},
                 {youtube, YouTube},
                 {pinterest, Pinterest},
                 {tumblr, Tumblr},
                 {gplus, GPlus},
                 {patreon, Patreon},
                 {newplayx, NewPlayX}],
    lists:filter(fun({_, V}) -> V =/= null end, Candidate).

 
external_links_record_to_json(null) -> [];
external_links_record_to_json(
    #external_links{
        website=Website,
        email_address=Email,
        blog=Blog,
        mailing_list=Newsletter,
        facebook=Facebook,
        twitter=Twitter,
        instagram=Instagram,
        vimeo=Vimeo,
        youtube=YouTube,
        pinterest=Pinterest,
        tumblr=Tumblr,
        gplus=GPlus,
        patreon=Patreon,
        newplayx=NewPlayX
    }) ->
    Candidate = [{<<"website">>, Website},
                 {<<"email">>, Email},
                 {<<"blog">>, Blog},
                 {<<"newsletter">>, Newsletter},
                 {<<"facebook">>, Facebook},
                 {<<"twitter">>, Twitter},
                 {<<"instagram">>, Instagram},
                 {<<"vimeo">>, Vimeo},
                 {<<"youtube">>, YouTube},
                 {<<"pinterest">>, Pinterest},
                 {<<"tumblr">>, Tumblr},
                 {<<"gplus">>, GPlus},
                 {<<"patreon">>, Patreon},
                 {<<"newplayx">>, NewPlayX}],
    json_with_valid_values(Candidate).


%% Ideally we could re-use regexes rather than compile them each time,
%% but let's just ship this kitten.
validate_external_links(null) -> ok;
validate_external_links(#external_links{
        website=_Website,
        email_address=_Email,
        blog=_Blog,
        mailing_list=_Newsletter,
        facebook=Facebook,
        twitter=Twitter,
        instagram=Instagram,
        vimeo=_Vimeo,
        youtube=YouTube,
        pinterest=Pinterest,
        tumblr=Tumblr,
        gplus=_GPlus,
        patreon=_Patreon,
        newplayx=_NewPlayX}) ->
    ToTry = [
        {Facebook, "^(https?://)?(www\\.)?facebook\\.com", facebook},
        {Twitter, "^(https?://)?(www\\.)?twitter\\.com", twitter},
        {Instagram, "^(https?://)?(www\\.)?instagram\\.com", instagram},
        {Pinterest, "^(https?://)?(www\\.)?pinterest\\.com", pinterest},
        {YouTube, "^(https?://)?(www\\.)?youtube\\.com", youtube},
        {Tumblr, "^(https?://)?(www\\.)?[a-z0-9-]+\\.tumblr\\.com", tumblr}
    ],
    Tried = [ try_external(Candidate, ToMatch, Symb) || {Candidate, ToMatch, Symb} <- ToTry ],
    case lists:filter(fun ({X, _}) -> X =/= ok end, Tried) of
        [] -> ok;
        Else ->
            Errored = [ Symb || {_, Symb} <- Else ],
            throw({external_error, Errored})
    end.


try_external(null, _, _) -> {ok, not_specified};
try_external(Candidate, ToMatch, Symb) ->
    case re:run(Candidate, ToMatch, [caseless]) of
        nomatch -> {fail, Symb};
        error -> {error, Symb};
        _ -> {ok, Symb}
    end.


person_or_org_json_to_record(Object) ->
    case proplists:get_value(<<"person">>, Object, null) of
        null ->
            Org = proplists:get_value(<<"org">>, Object, null),
            ghostlight_org:json_to_record(Org);
        Person ->
            ghostlight_people:json_to_record(Person)
    end.

person_or_org_record_to_json(Person=#person{}) ->
  PersonJson = ghostlight_people:record_to_json(Person),
  [{<<"person">>, PersonJson}];
person_or_org_record_to_json(Org=#organization{}) ->
  OrgJson = ghostlight_org:record_to_json(Org),
  [{<<"org">>, OrgJson}].


person_or_org_record_to_proplist(#organization{id=Id, name=Name}) ->
    [{type, <<"org">>},
     {id, Id},
     {name, Name}];
person_or_org_record_to_proplist(#person{id=Id, name=Name}) ->
    [{type, <<"person">>},
     {id, Id},
     {name, Name}].


validate_person_or_org(P=#person{}) -> ghostlight_people:validate_person(P);
validate_person_or_org(O=#organization{}) -> ghostlight_org:validate_org(O).


%% iso8601 is pretty great, and epgsql are pretty great, but they don't play well together.
%% Namely, epgsql returns dates where the seconds value is a float, which iso8601 doesn't 
%% pattern match for. We truncate it out to let them play together.
erl_date_to_iso8601({}) -> undefined;
erl_date_to_iso8601(null) -> undefined;
erl_date_to_iso8601(Date) ->
    iso8601:format(remove_float_from_date(Date)).

remove_float_from_date({{Year, Month, Day}, {Hour ,Min, Second}}) ->
    {{Year, Month, Day}, {Hour ,Min, trunc(Second)}}.


json_with_valid_values(Candidates) ->
    lists:filter(fun({_K, V}) ->
                     suitable_to_show(V)
                 end, Candidates).

proplist_with_valid_values(Candidates) ->
    lists:filter(fun({_K, V}) ->
        suitable_to_show(V)
    end, Candidates).


%% Helps us filter out unshowable values.
suitable_to_show(<<"">>) -> false;
suitable_to_show([]) -> false;
suitable_to_show(null) -> false;
suitable_to_show(undefined) -> false;
suitable_to_show(_) -> true.


remove_null(null) -> <<"">>;
remove_null(E) -> E.


default_list(X) when is_list(X) -> X;
default_list(_) -> [].


handle_errors(400, _Headers, _Body, Req) ->
    Req;
handle_errors(401, _Headers, _Body, Req) ->  %% Unauthorized
    Req;
handle_errors(403, _Headers, _Body, Req) ->  %% Forbidden
    Req;
handle_errors(404, Headers, _Body, Req) ->
    handle_error_with(Req, Headers, 404, ghostlight_404_template);
handle_errors(500, Headers, _Body, Req) ->  %% Internal Server Error
    handle_error_with(Req, Headers, 500, ghostlight_500_template);
handle_errors(_, _, _, Req) ->
    Req.


handle_error_with(Req, Headers, StatusCode, Template) ->
    Type = cowboy_req:meta(response_type, Req, html),
    NewBody = case Type of
                  html ->
                      {ok, Body} = Template:render(),
                      Body;
                  json ->
                      list_to_binary("{\"error\": \"" ++ integer_to_list(StatusCode) ++ "\"}")
              end,
    Headers2 = augment_request_header(NewBody, Headers),
    cowboy_req:reply(StatusCode, Headers2, NewBody, Req).


augment_request_header(Body, Headers) ->
    NewValue = {<<"content-length">>, integer_to_list(iolist_size(Body))},
    lists:keyreplace(<<"content-length">>, 1, Headers, NewValue).


ensure_minimum_length(Lst, Len, _) when length(Lst) >= Len ->
    ok;
ensure_minimum_length(_, _, Msg) ->
    throw({list_not_correct_size, Msg}).
    

