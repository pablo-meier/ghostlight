-module(funcs).

-export([partial/2,
         partial_right/2,
         compose/2,
         complement/1]).

compose(FnA, FnB) ->
    fun(Args) ->
            FnB(apply(FnA, Args))
    end.


partial(Fn, PartiallyApplied) ->
    fun(Args) ->
            apply(Fn, PartiallyApplied ++ Args)
    end.

partial_right(Fn, PartiallyApplied) ->
    fun(Args) ->
            apply(Fn, Args ++ PartiallyApplied)
    end.

complement(Fn) ->
    fun (Args) ->
            case apply(Fn, Args) of
                true -> false;
                _ -> true
            end
    end.
