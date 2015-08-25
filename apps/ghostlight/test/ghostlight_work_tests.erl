-module(ghostlight_work_tests).
-include_lib("eunit/include/eunit.hrl").

-include("apps/ghostlight/include/ghostlight_data.hrl").



%%% JSON to record

kitchen_sink_test() ->
    Input = <<"
{
  \"id\": \"d96042f6-8529-4b20-915d-3e93abafc036\",
  \"authors\": [
    { \"person\": { \"name\": \"Charly Evon Simpson\", \"id\": \"bd74206d-7bac-4d24-af99-0910e762c81a\" }}
  ],
  \"title\": \"or what she will\",
  \"vanity\": \"owsw\",
  \"description\": \"Twins Willa and Faulkner share everything, including their parents' love of literature. At age 13, they are old souls yet still secretly believe in fantastical worlds… a storm rolls in… waves crash… FLASH… an empty playground… a man walks by… FLASH… Sometimes a moment changes everything.\",
  \"minutes_long\": 90
}">>,
    Expected = #work{
        id = <<"d96042f6-8529-4b20-915d-3e93abafc036">>,
        title= <<"or what she will">>,
        vanity_name = <<"owsw">>,
        authors = [#person{ id = <<"bd74206d-7bac-4d24-af99-0910e762c81a">>, name = <<"Charly Evon Simpson">> }],
        description = <<"Twins Willa and Faulkner share everything, including their parents' love of literature. At age 13, they are old souls yet still secretly believe in fantastical worlds… a storm rolls in… waves crash… FLASH… an empty playground… a man walks by… FLASH… Sometimes a moment changes everything.">>,
        minutes_long = 90
    },
    test_json_to_record(Input, Expected).


test_json_to_record(Input, Expected) ->
    Result = ghostlight_work:json_to_record(jsx:decode(Input)),
    ?assertEqual(Expected, Result).



%%% RECORD VALIDATION

%% Minimum viability
work_validate_id_test() ->
    ghostlight_work:validate_work(#work{id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>}).
work_validate_name_test() ->
    ghostlight_work:validate_work(#work{title = <<"Winners lol">>}).


work_bad_author_person_test() ->
    WorkToTest = #work {
        id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>,
        authors = [
            #person {
                id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">>
            },
            #person {
                id = <<"beat-it-ese">>
            }
        ]
    },
    ?assertException(throw, not_valid_uuid, ghostlight_work:validate_work(WorkToTest)).


work_bad_author_org_test() ->
    WorkToTest = #work {
        id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>,
        authors = [
            #person {
                id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">>
            },
            #organization {}
        ]
    },
    ?assertException(throw, org_missing_identifying_information, ghostlight_work:validate_work(WorkToTest)).


work_bad_vanity_name_test() ->
    WorkToTest = #work{
        title = <<"It's a Shakespeare knock-off!!!!">>,
        vanity_name = <<"jkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjkjk">>
    },
    ?assertException(throw, invalid_vanity_name_format, ghostlight_work:validate_work(WorkToTest)).
