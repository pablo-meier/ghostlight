funcs
=====

A simple combinator library. Scheme lets you write code like

`(define sum-of-list (partial foldl + '()))`

or somesuch, so I thought it was sad we didn't have simple function composition
tools.

Build
-----

    $ rebar3 compile
