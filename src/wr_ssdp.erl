%% dispatch:
%% {["_ssdp"],      	wr_ssdp, []}.
%% {["_ssdp", env],		wr_ssdp, []}.

-module(wr_ssdp).
-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(Request, Context) ->
    {[{"application/json", to_json}], Request, Context}.

resource_exists(Request, Context) ->
    case wrq:path_info(env, Request) of
        undefined ->
            {true, Request, {struct, ssdp:discovered()}}; 
        Key ->
            case ssdp:discovered(Key) of
                undefined -> {false, Request, Context};
                false -> {false, Request, Context};
                Result -> {true, Request, {struct, Result}}
            end
    end.

to_json(Request, Context) ->
    {mochijson2:encode(Context), Request, Context}.
	
