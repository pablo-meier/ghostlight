%% DATATYPES!
%%
%% We use records where possible, and provide defaults. It's a bit like Go where
%% we try to define a '0 type' for each record so we don't get surprising
%% 'undefined's. 

-type datetime() :: tuple(Date::calendar:date(), Time::calendar:time()) | null.

-record(external_links, {
    website = null             :: null | binary(),
    email_address = null       :: null | binary(),
    blog = null                :: null | binary(),
    mailing_list = null        :: null | binary(),
    facebook = null            :: null | binary(),
    twitter = null             :: null | binary(),
    instagram = null           :: null | binary(),
    vimeo = null               :: null | binary(),
    youtube = null             :: null | binary(),
    pinterest = null           :: null | binary(),
    tumblr = null              :: null | binary(),
    gplus = null               :: null | binary(),
    patreon = null             :: null | binary(),
    newplayx = null            :: null | binary()
}).

-record(press_link, {
    link = null         :: null | binary(),
    label = null        :: null | binary(),
    pull_quote = null   :: null | binary()
}).

-record(person, {
    id = null             :: null | binary(),
    name = null           :: null | binary(),
    vanity_name = null    :: null | binary(),
    external_links = null :: null | #external_links{},
    description = null    :: null | binary()
}).

-record(org_member, {
    person = throw(empty_person_in_org_member) :: #person{},
    description = null                         :: null | binary()
}).

-record(org_employee, {
    person = throw(empty_person_in_org_employee) :: #person{},
    title = null                                 :: null | binary(),
    description = null                           :: null | binary()
}).

-record(organization, {
    id = null                          :: null | binary(),
    name = null                        :: null | binary(),
    vanity_name = null                 :: null | binary(),
    tagline = null                     :: null | binary(),
    description = null                 :: null | binary(),
    date_founded = null                :: null | datetime(),
    external_links = #external_links{} :: #external_links{},
    members = []                       :: list(#org_member{}),
    employees = []                     :: list(#org_employee{})
}).

-record(work, {
    id = null                  :: null | binary(),
    title = null               :: null | binary(),
    vanity_name = null         :: null | binary(),
    authors = []               :: list(#person{}) | list(#organization{}),
    description = null         :: null | binary(),
    collaborating_orgs = []    :: [#organization{}],
    minutes_long = null        :: null | integer()
}).

-record(onstage, {
    role = <<"">>      :: binary(),
    person = #person{} :: #person{}
}).

-record(offstage, {
    job = <<"">>            :: binary(),
    contributor = #person{} :: #organization{} | #person{}
}).

-record(performance, {
    id = null               :: null | binary(),
    work = #work{}          :: #work{},
    onstage = []            :: list(#onstage{}),
    offstage = []           :: list(#offstage{}),
    directors = []          :: list(#person{}),
    directors_note = null   :: null | binary(),
    description = null      :: null | binary()
}).

-record(show, {
    id = null               :: binary(),
    title = <<"">>          :: binary(),
    vanity_name = null      :: null | binary(),
    producers = []          :: list(#person{} | #organization{}),
    performances = []       :: list(#performance{}),
    special_thanks = null   :: null | binary(),
    description = null      :: null | binary(),
    hosts = []              :: list(#person{}),
    press_links = []        :: list(#press_link{}),
    external_links = null   :: null | #external_links{},
    dates = []              :: list(datetime())
}).


-record(org_work, {
  org = throw(org_work_without_org) :: #organization{},
  title = <<"">>                    :: binary()
}).

%% Tentative about this -- should we be defining records per-call? I dislike the granularity
%% of the above records for returning, say all the contributions for a person, since they'll
%% touch so many things.

-record(person_return, {
    person=#person{}      :: #person{},
    authored = []         :: list(#work{}),
    directed = []         :: list(#show{}),
    onstage = []          :: list(#show{}),
    offstage = []         :: list(#show{}),
    orgs_employee = []    :: list(#org_work{}),
    orgs_member = []      :: list(#organization{}),
    shows_produced = []   :: list(#show{})
}).

-record(org_return, {
    org = #organization{} :: #organization{},
    shows_produced = []   :: list(#show{})
}).

-record(work_return, {
    work = #work{}   :: #work{},
    shows = []       :: list(#show{})
}).
