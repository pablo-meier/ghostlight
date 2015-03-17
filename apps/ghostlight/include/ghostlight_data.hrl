-type person() :: {name, binary()} | {id, binary()}.
-type organization_parent() :: {name, binary()} | {id, binary()}.
-type visibility() :: public | private.

%% This is bad but Erlangs native libraries don't have timezones.
%% Also considering:
%%   https://github.com/choptastic/qdate
%%   https://github.com/seansawyer/erlang_iso8601
%%
%% But, like with pgsql statements, unclear how to import any data definitions
%% for it atm.
-record(ghostlight_datetime, {
    time8601 :: binary(),
    timezone :: binary()
}).

-record(organization, {
    name :: binary(),
    tagline :: binary(),
    parent :: organization_parent(),
    description :: binary(),
    vanity_name :: binary(),
    date_founded :: #ghostlight_datetime{},
    visibility=public :: visibility()
}).

-record(work, {
    title :: binary(),
    authors :: list(person())
}).

-record(onstage, {
    role :: binary(),
    person :: person()
}).

-record(offstage, {
    job :: binary(),
    person :: person()
}).

-record(performance, {
    work :: #work{},
    onstage :: list(#onstage{}),
    offstage :: list(#offstage{}),
    directors :: list(person())
}).

-record(show, {
    title :: binary(),
    org :: #organization{},
    performances :: list(#performance{}),
    special_thanks :: binary(),
    dates :: list(#ghostlight_datetime{})
}).

