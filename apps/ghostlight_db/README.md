ghostlight_db
=====

So Ghostlight hit monolight status... uh, very early. By that I mean, it was
clear that Postgres and all its baggage either had to be a) spread out
everywhere, or b) all in one file, both of which were unacceptable.

Given the way the architecture of the app kind of fell into place (where the
cowboy-side doesn't really speak SQL and the SQL side doesn't speak JSON), it
made sense to break this off into its own app, so we have more clear places for
each resources to tweak its needs.

Build
-----

    $ rebar3 compile
