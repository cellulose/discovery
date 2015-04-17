%% Author: ua
%% Created: Jan 27, 2010
%% Description: TODO: Add description to msg_factory
-module(ssdp_msg_factory).

%%
%% Include files
%%

-include("../include/upnp.hrl").
%%
%% Exported Functions
%%
-export([build_is_alive/2, build_bye_bye/1, build_msearch_response/3]).

-define(UPNP_HOST_PORT, "239.255.255.250:1900").
-define(CRLF,[13, 10]). %% "\r\n"
-define(ISALIVE,"ssdp:alive").
-define(BYE, "ssdp:byebye").
%%
%% API Functions
%%
build_is_alive(NT, Uri) ->
    List = ["NOTIFY * HTTP/1.1", ?CRLF,
            "HOST: ", ?UPNP_HOST_PORT, ?CRLF,
            "NT: ", NT, ?CRLF,
            "NTS: ssdp:alive", ?CRLF,
            "LOCATION: http://", ssdp_root_device:get_ip_port(), Uri, ?CRLF,
            "USN: uuid:", ssdp_root_device:get_uuid(), ?CRLF,
            "CACHE-CONTROL: max-age=1800", ?CRLF,
            "SERVER: CELL", ?CRLF, ?CRLF
            ],
    % error_logger:info_msg("~p~n", [lists:append(List)]),
    lists:append(List).


build_bye_bye(NT) ->
    List = ["NOTIFY * HTTP/1.1", ?CRLF,
            "HOST: ", ?UPNP_HOST_PORT, ?CRLF,
            "NT: ", NT, ?CRLF,
            "NTS: ssdp:byebye", ?CRLF,
            "USN: uuid", ssdp_root_device:get_uuid(), ?CRLF
            ],
    % error_logger:info_msg("~p~n", [lists:append(List)]),
    lists:append(List).

build_msearch_response(ST, Uri, _Service_Type) ->
    List = ["HTTP/1.1 200 OK", ?CRLF,
            "CACHE-CONTROL: max-age = 1200", ?CRLF,
%           "DATE: ", get_date(), ?CRLF,
            "EXT:", ?CRLF,
            "LOCATION: http://", ssdp_root_device:get_ip_port(), Uri,?CRLF,
            "ST: " , ST, ?CRLF,
            "USN: uuid:", ssdp_root_device:get_uuid(), ?CRLF,
            "CACHE-CONTROL: max-age=1800", ?CRLF,
            "SERVER: CELL", ?CRLF, ?CRLF
            ],
    %error_logger:info_msg("MSEARCH RESPONSE: ~p~n", [lists:append(List)]),
    lists:append(List).

% get_date() ->
%     httpd_util:rfc1123_date(erlang:localtime()).
