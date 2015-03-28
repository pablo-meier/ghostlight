-module(ghostlight_utils).

-export([erl_date_to_iso8601/1,
         json_with_valid_values/1,
         proplist_with_valid_values/1,
         remove_null/1]).

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


