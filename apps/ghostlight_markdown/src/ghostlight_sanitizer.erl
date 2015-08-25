-module(ghostlight_sanitizer).
-export([sanitize/1,
         healthcheck/0]).

-define(OWASP_JAVA_NODE, 'jawbone_tower@Sancho.hsd1.pa.comcast.net').

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

healthcheck() ->
    Resp = sanitize(<<"<script>alert('pwned');</script><p>hi!</p>">>),
    case Resp of
        <<"<p>hi!</p>">> -> ok;
        Else -> {error, {unexpected_sanitization, Else}}
    end.
