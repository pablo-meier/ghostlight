-module(ghostlight_people_tests).
-include_lib("eunit/include/eunit.hrl").

-include("apps/ghostlight/include/ghostlight_data.hrl").


%%% JSON-TO-RECORD
kitchen_sink_test() -> 
    Input = <<"
{
  \"id\": \"481e9c01-8284-4b9f-8263-f8e3a8f0aa32\",
  \"name\": \"Daria Miyeko Marinelli\",
  \"description\": \"Ms. Marinelli was born and raised in New York and lives in Astoria.\",

  \"social\": {
    \"website\": \"http://www.dariamiyekomarinelli.com\",
    \"twitter\": \"https://twitter.com/dariamiyeko\",
    \"instagram\": \"http://instagram.com/daria.miyeko.marinelli\",
    \"email\": \"daria.miyeko.marinelli@gmail.com\",
    \"tumblr\": \"http://whatindarnation.tumblr.com/\"
  }
}">>,
    Expected = #person{
        id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">>,
        name = <<"Daria Miyeko Marinelli">>,
        external_links = #external_links{
            website = <<"http://www.dariamiyekomarinelli.com">>,
            email_address = <<"daria.miyeko.marinelli@gmail.com">>,
            twitter = <<"https://twitter.com/dariamiyeko">>,
            instagram = <<"http://instagram.com/daria.miyeko.marinelli">>,
            tumblr = <<"http://whatindarnation.tumblr.com/">>
        },
        description = <<"Ms. Marinelli was born and raised in New York and lives in Astoria.">>
    },
    test_json_to_record(Input, Expected).


test_json_to_record(Input, Expected) ->
    Result = ghostlight_people:json_to_record(jsx:decode(Input)),
    ?assertEqual(Expected, Result).


%%% RECORD VALIDATION

% Minimum Viability
person_validate_name_test() ->
   ghostlight_people:validate_person(#person{name = <<"Urbit NockBlippo">>}).
person_validate_id_test() ->
   ghostlight_people:validate_person(#person{id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>}).

person_validate_throws_no_identification_test() ->
  ?assertException(throw, person_missing_identifying_information, ghostlight_people:validate_person(#person{})).
person_validate_id_bad_test() ->
   ?assertException(throw, not_valid_uuid, ghostlight_people:validate_person(#person{id = <<"STTTEEEEEVEEEEE">>})).

person_bad_external_links_test() ->
    PersonToTest = #person{
        name = <<"Bilbo Swaggins">>,
        external_links = #external_links{
            instagram = <<"http://critters.com">>
        }
    },
    ?assertException(throw, {external_error, [instagram]}, ghostlight_people:validate_person(PersonToTest)).


org_bad_vanity_name_test() ->
    PersonToTest = #person{
        name = <<"Blade GunnBlade">>,
        vanity_name = <<"💀👻/unicode">>
    },
    ?assertException(throw, invalid_vanity_name_format, ghostlight_people:validate_person(PersonToTest)).
