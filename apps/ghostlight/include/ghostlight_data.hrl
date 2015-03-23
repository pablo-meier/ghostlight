%% DATATYPES!
%%
%% We use records where possible, and provide defaults. It's a bit like Go where
%% we try to define a '0 type' for each record so we don't get surprising
%% 'undefined's. 

-type person() :: {name, binary()} | {id, binary()}.
-type datetime() :: {{integer(), integer(), integer()}, {integer(), integer(), float()}}.
-type organization_parent() :: {id, binary()}.


-record(organization, {
    id = <<"">>                :: binary(),
    name = <<"">>              :: binary(),
    tagline = <<"">>           :: binary(),
    parent = {id, <<"">>}      :: organization_parent(),
    description = <<"">>       :: binary(),
    vanity_name = <<"">>       :: binary(),
    date_founded = {}          :: datetime(),
    visibility = <<"public">>  :: binary()
}).

-record(work, {
    id = <<"">>    :: binary(),
    title = <<"">> :: binary(),
    authors = []   :: list(person())
}).

-record(onstage, {
    role = <<"">>           :: binary(),
    person = {name, <<"">>} :: person()
}).

-record(offstage, {
    job = <<"">>            :: binary(),
    person = {name, <<"">>} :: person()
}).

-record(performance, {
    work = #work{} :: #work{},
    onstage = []   :: list(#onstage{}),
    offstage = []  :: list(#offstage{}),
    directors = [] :: list(person())
}).

-record(show, {
    id = <<"">>             :: binary(),
    title = <<"">>          :: binary(),
    org = #organization{}   :: #organization{},
    performances = []       :: list(#performance{}),
    special_thanks = <<"">> :: binary(),
    dates = []              :: list(datetime())
}).


-record(org_work, {
  org_id = <<"">>   :: binary(),
  org_name = <<"">> :: binary(),
  title = <<"">>    :: binary()
}).

%% Tentative about this -- should we be defining records per-call? I dislike the granularity
%% of the above records for returning, say all the contributions for a person, since they'll
%% touch so many things.

-record(person_return, {
    name = <<"">>  :: binary(),
    authored = []  :: list(#work{}),
    directed = []  :: list(#performance{}),
    onstage = []   :: list(#show{}),
    offstage = []  :: list(#show{}),
    orgs = []      :: list(#organization{})
}).


