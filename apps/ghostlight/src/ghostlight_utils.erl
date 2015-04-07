-module(ghostlight_utils).

-export([erl_date_to_iso8601/1,
         json_with_valid_values/1,
         proplist_with_valid_values/1,
         person_or_org_json_to_record/1,
         remove_null/1,
         external_links_json_to_record/1,
         external_links_record_to_proplist/1]).

-include("apps/ghostlight/include/ghostlight_data.hrl").

external_links_json_to_record(Json) when is_list(Json) ->
    #external_links{
        website = proplists:get_value(<<"website_link">>, Json, null),
        email_address = proplists:get_value(<<"email">>, Json, null),
        blog = proplists:get_value(<<"blog">>, Json, null),
        mailing_list = proplists:get_value(<<"newsletter">>, Json, null),
        facebook = proplists:get_value(<<"facebook">>, Json, null),
        twitter = proplists:get_value(<<"twitter">>, Json, null),
        instagram = proplists:get_value(<<"instagram">>, Json, null),
        vimeo = proplists:get_value(<<"vimeo">>, Json, null),
        youtube = proplists:get_value(<<"youtube">>, Json, null),
        pinterest = proplists:get_value(<<"pinterest">>, Json, null),
        tumblr = proplists:get_value(<<"tumblr">>, Json, null),
        gplus = proplists:get_value(<<"gplus">>, Json, null),
        patreon = proplists:get_value(<<"patreon">>, Json, null),
        newplayx = proplists:get_value(<<"newplayx">>, Json, null)
    }.

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
 

person_or_org_json_to_record({Object}) ->
    case proplists:get_value(<<"person">>, Object, null) of
        null ->
            Org = proplists:get_value(<<"org">>, Object, null),
            ghostlight_org:json_to_record(Org);
        Person ->
            ghostlight_people:json_to_record(Person)
    end.


%% iso8601 is pretty great, and epgsql are pretty great, but they don't play well together.
%% Namely, epgsql returns dates where the seconds value is a float, which iso8601 doesn't 
%% pattern match for. We truncate it out to let them play together.
erl_date_to_iso8601({}) -> undefined;
erl_date_to_iso8601(Date) ->
    iso8601:format(remove_float_from_date(Date)).

remove_float_from_date({{Year, Month, Day}, {Hour ,Min, Second}}) ->
    {{Year, Month, Day}, {Hour ,Min, trunc(Second)}}.


json_with_valid_values(Candidates) ->
    Inclusions = lists:filter(fun({_K, V}) ->
                                  suitable_to_show(V)
                              end, Candidates),
    {Inclusions}.

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


