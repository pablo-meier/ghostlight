# ghostlight_db

So Ghostlight hit monolight status... uh, very early. By that I mean, it was
clear that Postgres and all its baggage either had to be a) spread out
everywhere, or b) all in one file, both of which were unacceptable.

Given the way the architecture of the app kind of fell into place (where the
cowboy-side doesn't really speak SQL and the SQL side doesn't speak JSON), it
made sense to break this off into its own app, so we have more clear places for
each resources to tweak its needs.

# "Design Doc"
Design docs aren't thrilling to write but they have their place. Here's a little
manifesto that used to live in the old monolithic DB file that more or less
applies to this whole application.

---
## The DATABASE

The primary value-prop of Ghostlight comes from its database. Virtually
_everything_ that gives it value over a pile of playbills comes from the
fact that a) we have a well-maintained, well-run database containing all
the information, and b) that data is easily searchable, well-presented,
and c) that the data is Good.

While c) is mostly a policy issue (and some data science I don't know),
and b) is the interface to the app itself, most of a) gets handled here!

Here is the high-level on the database:
* It exposes a number of API functions to the app that are mostly wrappers
  to `gen\_server:call/cast`. Technically speaking, we can change everything
  about this module EXCEPT this -- we could change DB engines, ditch
  gen\_server, whatever. Highly unlikely, but that's the hard module
  boundary for the rest of the app.
* We make it a gen\_server for the well-known obvious reasons: we can restart
  it, supervise it, etc., and it's a solid way to maintain the state that
  we'll need (parsed queries, PSQL connections).
* We use Postgres because it's actually Free Free, unlike Neo4j, relies on
  SQL which we and the world know, and is rock-rock-steady.

Some ground rules for the application:

* SQL is invisible. No evidence of the statements/queries or the rows and
  row structure should escape this module. We trade entirely in the records
  specified in `ghostlight_data`. Sometimes they're the same as input records,
  sometimes they they're `<datatype>_return` for when we couple lots of data
  together during a query (e.g. when you `/GET` a person you almost always want
  to know about their shows, etc.

### Inserting New Data

Since the data is so spread out with a myriad of hairy foreign key
relationships, generating the exact queries for the use case requires something
of a calculus. Virtually all data types have a `get_<type>_inserts` which takes a
record from `ghostlight_data` and returns two-tuple -> a list of inserts, and
the newly-generated ID of the entity being created. So:

```
get_show_inserts(#show{}) -> {[Inserts], ShowId}
  Insert :: {PreparedStmt, [Arg]}
  ShowId :: UUID
```

Here's where it gets interesting: given than a show contains performances, _it_
will call `get_performance_inserts`, and bundle THOSE inserts into its own. So it
really becomes something like Russian Nesting Dolls.



# Build

    $ rebar3 compile
