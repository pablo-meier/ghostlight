# Ghostlight

👻

Ghostlight is a web application written in Erlang meant to be the 'IMDb for live
performance': it would be a searchable, linked catalogue of all the information
in a show's program. My primary use case was for theater and musicals, but
there's no reason it couldn't be used for concerts, magic shows, opera, modern
dance…

It's incomplete, the engineering unpolished, and butt-ugly, but it was a
learning experience as much as anything else, and done entirely by me in spare
time when I _wasn't_ going to the 60-80ish shows I went to that year.

I might resurrect it, but in case I don't, it's here for the world to see/pick
apart.

See the [blog](http://blog.ghostlight.io), or visit the [running
instance](https://ghostlight.io), bugs and all.

## Build

I tried using [rebar3](https://www.rebar3.org/) for everything (rather than the
rebar-but-also-a-Makefile of times previous), so you should _ostensibly_ be able
to just invoke

    $ rebar3 compile

But `merl` you to jump into the `_build` director and run `make` in it, because
the Erlang ecosystem is great.

Note also that `erlydtl` can also trip you up by requires you to have about a
gig of memory to run its compile step. I learned this when I hit an
out-of-memory error building this on a tiny Joyent instance.

To run, you can `rebar3 shell`, or just do what I did and build a full release
with

    $ rebar3 release

then running `./_build/default/rel/ghostlight/bin/ghostlight console`. It's
nasty.

Config is done in `config/sys.config`. It's not checked in, but copy
`sys.config.sample` and fill in the blanks.

## Dependencies, making a mess

If it isn't clear, I didn't invest a whole lot in the build system or ops of
this. Here some irregularities/WTFs:

### Backends

Ghostlight uses Postgres for the Source Of Truth and most real data, because
it's just the best database. Fight me. Migrations that describe the data model
are in `scripts/sql`. One of the more fun, and I'd argue, successful, parts of
this project was modelling shows and their components relationally.

I didn't put any real work into carefully adding indices or anything, so there
are easy wins there.

This uses ElasticSearch as something of searchable cache: this lets us do
full-text search on user queries without bending Postgres, as well as a fairly 
straightforward way to do dynamic queries. There are no config parameters for ES
because I was running it locally; it obviously wouldn't be too hard to remedy
this.

### Jinterface, C ports

Part of the fun of this was learning about the [Jinterface][1] and [Ports][2].
Part of the hell in developing and maintaining this was using the Jinterface
and Ports. Here's the short summary:

* Both `ghostlight_markdown` and `ghostlight_devtools` use the Jinterface for
  specific functionality, and `ghostlight_markdown` has a C port.

* This means for the app to function properly, you need to ensure that the C and
  Java sources are compiled. C uses the [CommonMark][3] C implementation shared
  library, Java uses a few JARs that should be checked in.

* After the app starts up, you need to ensure the connection of the Jinterface
  nodes. This is a bear and a half: refer to [Learn You Some Erlang][4] for
  instructions on how to set up a cluster of the several nodes.

* If you want to make sure everything that needs to be connected is, run
  `ghostlight_healthchecks:run()` in a connected erlang shell.

* I can't guarantee some of the names of nodes aren't defined in sources. Grep
  around.

If I'm a little jaded here, it's because I am! But this was a genuinely
rewarding part of the project.

##### Sidebar: rationale for this shit, because who chooses this?

My favorite web apps that include any level of sophisticated text use
[Markdown][5], but [Markdown implementations are notoriously inconsistent][6],
and Erlang doesn't have many implementations. The main one appears to be this
[erlware][7] one, which, guess what!? Is [home-rolled!][8] So you get the
efficiencies of Erlang with the unpredictability of Markdown!

So I went with [CommonMark][9], our Best Last Hope at some standardization of
Markdown, and their C implementation, since if I was going to call a port, why
not the fastest one? That's the reason for the C port.

As for Jinterface: once we allow users to submit content that we will display
back, we become vulnerable to [XSS][10], and as in Markdown, I don't want to
write the sanitization code myself (mostly a function of laziness, but also
because I'm bound to get it wrong and expose a security vulnerability). So I
relied on the [OWASP Sanitizer][11] to scrub user input, which relies on a way
to "speak Java."

`ghostlight_devtools` is usually never connected because it's an app that I
never really finished, but the libraries it's talking to are the [Google Closure
Compiler][12] and the [YUI Compressor][13] for JS and CSS
concatenation/minification, respectively.

### There will/should be a whole lot more…

…as there were a million terrible design decisions, compromises, lessons
learned, and Delightful Hacks, but those probably belong in blog posts or on the
wiki, which I'll Totally Write™. Still, this was a few months of passionate
work, so let this be a first step in Sharing.

   [1]: http://www.erlang.org/doc/apps/jinterface/jinterface_users_guide.html
   [2]: http://erlang.org/doc/reference_manual/ports.html
   [3]: https://github.com/jgm/CommonMark
   [4]: http://learnyousomeerlang.com/distribunomicon#setting-up-an-erlang-cluster
   [5]: https://help.github.com/articles/markdown-basics/
   [6]: http://johnmacfarlane.net/babelmark2/?normalize=1&text=-+top%0A+-+indented+one%0A++-+indented+two%0A+++-+indented+three%0A++++-+indented+four%0A+++++-+indented+five%0A
   [7]: https://github.com/erlware/erlmarkdown
   [8]: https://github.com/erlware/erlmarkdown/blob/master/src/markdown.erl
   [9]: http://commonmark.org/
   [10]: https://www.owasp.org/index.php/Cross-site_Scripting_(XSS)
   [11]: https://www.owasp.org/index.php/OWASP_Java_HTML_Sanitizer_Project
   [12]: https://developers.google.com/closure/compiler/?hl=en
   [13]: http://yui.github.io/yuicompressor/
