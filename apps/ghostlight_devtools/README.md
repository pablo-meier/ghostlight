ghostlight\_devtools
=====

There are currently two pretty Bad things about developing ghostlight:

* Given that we serve static content from the `priv/` directory, we have to
  create a new release every time I want to test a change.
* There really isn't a notion of an 'asset pipeline': I could theoretically
  write a Grunt script or something to minify + uglify + concat the CSS/JS, but
  how do I tell the watcher 'go into the priv directory, go into the template,
  re-create any modules' etc.?

Solution is to use a tiny app that will watch, re-create templates, run the
asset pipeline. Based loosely on [Sync][1], but a lot less general.

---

A notion of file _Types_ that determine what operation to perform when mutated.

* Erlang source gets recompiled, then the module reloaded.
* CSS or Javascript gets its source files concatted, minified, uglified, and
  placed into the priv directory.
* HTML templates get minified, the template module re-compiled, and reloaded
  into the running system.

How do we get sources and dests? Config file? Config module?

Build
-----

    $ rebar3 compile

   [1]: https://github.com/rustyio/sync
