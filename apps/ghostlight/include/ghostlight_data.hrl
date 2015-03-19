%% DATATYPES!
%%
%% We use records where possible, and provide defaults. It's a bit like Go where
%% we try to define a '0 type' for each record so we don't get surprising
%% 'undefined's. 

-type person() :: {name, binary()} | {id, binary()}.
-type datetime() :: {{integer(), integer(), integer()}, {integer(), integer(), float()}}.
-type organization_parent() :: {id, binary()}.

-record(organization, {
    name = <<"">>                         :: binary(),
    tagline = <<"">>                      :: binary(),
    parent = {id, <<"">>}                 :: organization_parent(),
    description = <<"">>                  :: binary(),
    vanity_name = <<"">>                  :: binary(),
    date_founded = {}                     :: datetime(),
    visibility = <<"public">>             :: binary()
}).

-record(work, {
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
    title = <<"">>          :: binary(),
    org = #organization{}   :: #organization{},
    performances = []       :: list(#performance{}),
    special_thanks = <<"">> :: binary(),
    dates = []              :: list(datetime())
}).

