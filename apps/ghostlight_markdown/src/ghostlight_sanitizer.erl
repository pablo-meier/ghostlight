-module(ghostlight_sanitizer).
-export([sanitize/1]).

-define(OWASP_JAVA_NODE, 'jawbone_tower@Sancho.local').

%% Takes the dirty HTML that may be Bad for the site and returns sanitized HTML.
sanitize(Body) when is_binary(Body) ->
    sanitize(binary_to_list(Body));
sanitize(Body) when is_list(Body) ->
    Ref = make_ref(),
    {owasp_java_server, ?OWASP_JAVA_NODE} ! {self(), Ref, Body},
    receive
        {_Node, Ref, Sanitized} ->
            list_to_binary(Sanitized)
    after 5000 ->
          {error, timeout}
    end.
