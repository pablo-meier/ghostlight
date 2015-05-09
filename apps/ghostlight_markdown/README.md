# ghostlight\_markdown

## Original Motivation

Most of this adapted from the exploratory Trello card.

---
So Erlang is great and doesn't have libraries for anything cool, and adding
Markdown, while cool and convenient, brings security implications. There's no
way I can just plug in an Erlang lib here and call it a day.

There are two steps to this: a) how do I generate the HTML from Markdown source,
and b) how do I sanitize that resulting HTML to prevent things like XSS?

For a) I'm trying to write this to be at least a little bit efficient, so using
the GitHub recommendation ([marked](https://github.com/chjj/marked) in Node, or
[Markup](https://github.com/github/markup) in Ruby) won't do, since it puts
critical infrastructure a weaker, slower language's process. The other Good
Markdown, CommonMark, has [a good C implementation](https://github.com/jgm/cmark).
The plan is to have a Learning Experience interfacing to a C lib from Erlang.

Sanitation story is worse: it looks like most frameworks have some kind of
ad-hoc library (YAWS has [an encoding function](http://stackoverflow.com/a/8184612),
which is not what I want, [Mochiweb has an escape func](https://github.com/mochi/mochiweb/blob/master/src/mochiweb_html.erl)
with no guarantees). Cowboy has nothing!

Best I can find for Security and Performance (_again_ all these goddamn libraries
are written in PHP or Node or something!) is the owasp-java-html-sanitizer
[hosted on the ill-fated Google Code](https://code.google.com/p/owasp-java-html-sanitizer/)

But how to get Erlang to talk to Java? Fuck man, idk. Probably go full Elasticsearch
and write a little wrapper function uses an HTTP interface for exactly one
endpoint. I'd have to do a bit of research to ensure that it won't block things,
and that the Java program won't be accessible to the Wider Web. I want this feature
bad, so I might just do all this crap, but damn it's kind of expensive.

## So now, this!

This is the Erlang application that contains a NIF packaged with `libcmark.a` to
actually parse the Markdown. It will also get the Java process started to use
the owasp sanitizer via Jinterface?

On inserts, we'll store both the Markdown source of any "mardownable" text, as
well as the output after parsing -- this means we only do Markdown parses on
inserts and updates, not on reads.

If we wanted to be really fancy, we could also strip ALL HTML after a Markdown
parse for JSON endpoints, so `**bold text**` comes out as `bold text`. We'll
see.


