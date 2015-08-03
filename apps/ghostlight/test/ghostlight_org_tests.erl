-module(ghostlight_org_tests).
-include_lib("eunit/include/eunit.hrl").

-include("apps/ghostlight/include/ghostlight_data.hrl").

%% Test the JSON to record for various org configs

%% Full, with everything
%% * shows produced
%% * members
%% * employees

kitchen_sink_test() -> 
    Input = <<"
{
  \"id\": \"481e9c01-8284-4b9f-8263-f8e3a8f0aa32\",
  \"name\": \"PIEHOLE\",
  \"tagline\": \"Your face is made for **PIE.**\",
  \"description\": \"The length of time I've loved Chipotle has outlasted all my romantic relationships combined.\",

  \"members\": [
    {
      \"person\": {
        \"id\": \"2d7029c7-d911-460d-914d-4eb2189b38fa\"
      },
      \"description\": \"Pop lock and drop it.\"
    },
    {
      \"person\": {
        \"name\": \"Poopface McGee\"
      }
    }
  ],

  \"employees\": [
    {
      \"person\": {
        \"id\": \"87ac576a-5645-400a-90e3-60e8f717a889\"
      },
      \"title\": \"Crunch Muncher\",
      \"description\": \"_Never gonna give you up_, never gonna let you down.\"
    },
    {
      \"person\": {
        \"name\": \"Arvo McLarbo\"
      },
      \"title\": \"Back Cracker\"
    }
  ],

  \"social\": {
    \"website\": \"http://www.pieholed.com/\",
    \"email\": \"pieholed@gmail.com\",
    \"facebook\": \"https://www.facebook.com/pages/Piehole/163999200321547\",
    \"instagram\": \"http://instagram.com/pieholed\",
    \"newsletter\": \"http://eepurl.com/Lxxbb\",
    \"blog\": \"http://www.pieholed.com/notebook\"
  }
}
">>,
    Expected = #organization {
      id = <<"481e9c01-8284-4b9f-8263-f8e3a8f0aa32">>,
      name = <<"PIEHOLE">>,
      tagline = <<"Your face is made for **PIE.**">>,
      description = <<"The length of time I've loved Chipotle has outlasted all my romantic relationships combined.">>,
      members = [#org_member{
                    person = #person {
                                id = <<"2d7029c7-d911-460d-914d-4eb2189b38fa">>
                             },
                    description = <<"Pop lock and drop it.">>
                   },
                 #org_member{
                    person = #person {
                                name = <<"Poopface McGee">>
                             }
                   }],
      employees = [#org_employee{
                    person = #person {
                                id = <<"87ac576a-5645-400a-90e3-60e8f717a889">>
                             },
                    title = <<"Crunch Muncher">>,
                    description = <<"_Never gonna give you up_, never gonna let you down.">>
                   },
                   #org_employee{
                    person = #person {
                                name = <<"Arvo McLarbo">>
                             },
                    title = <<"Back Cracker">>
                   }],
      external_links = #external_links {
        website = <<"http://www.pieholed.com/">>,
        email_address = <<"pieholed@gmail.com">>,
        facebook = <<"https://www.facebook.com/pages/Piehole/163999200321547">>,
        instagram = <<"http://instagram.com/pieholed">>,
        mailing_list = <<"http://eepurl.com/Lxxbb">>,
        blog = <<"http://www.pieholed.com/notebook">>
      }
    },
    test_json_to_record(Input, Expected).

test_json_to_record(Input, Expected) ->
    Result = ghostlight_org:json_to_record(jsx:decode(Input)),
    ?assertEqual(Expected, Result).
