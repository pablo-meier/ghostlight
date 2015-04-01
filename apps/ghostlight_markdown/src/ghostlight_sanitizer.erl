-module(ghostlight_sanitizer).
-export([sanitize/1]).

-define(OWASP_JAVA_NODE, "jawbone_tower@localhost").

%% Takes the dirty HTML that may be Bad for the site and returns sanitized HTML.
sanitize(Body) when is_binary(Body) ->
    Ref = make_ref(),
    {owasp_java_server, ?OWASP_JAVA_NODE} ! {self(), Ref, Body},
    receive
        {_Node, Ref, Sanitized} ->
            Sanitized
    after 5000 ->
          {error, timeout}
    end.
