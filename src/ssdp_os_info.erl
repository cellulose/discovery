%% Author: ua
%% Created: Jan 17, 2010
%% Description: TODO: Add description to os_info
-module(ssdp_os_info).

%%
%% Include files
%%
-include("../include/upnp.hrl").
-include_lib("eunit/include/eunit.hrl").
%%
%% Exported Functions
%%

-export([get_type/0, get_version/0, get_os_description/0,  get_ip/0, get_iflist/0]).
-export([get_ip_as_string/0, get_ip_as_string/1]).
-export([get_active_ip/0, get_active_ip/1, get_loopback/1, get_ip/1, get_all/1, get_os/0, get_os/1]).
%%
%% API Functions
%%

get_type() ->
	get_type(get_os()).

get_version() ->
	get_version(get_os()).

get_os() ->
	get_os(os:type()).

get_os({Osfamily, Osname}) ->
	case Osname of
	    darwin -> case check_if_sw_vers_avaible() of
				      true -> mac;
				      false -> Osfamily
			      end;
	    _ -> Osfamily
	end.

check_if_sw_vers_avaible() ->
	filelib:is_file('/usr/bin/sw_vers').

get_all(mac) ->
	os:cmd('sw_vers');

get_all(unix) ->
	os:cmd('uname -a').

get_type(mac) -> 
	Type = os:cmd('sw_vers -productName'),
	string:strip(Type, right, 10).
	
get_version(mac) ->
	Version = os:cmd('sw_vers -productVersion'),
	string:strip(Version, right, 10);

get_version(unix) ->
	os:cmd('uname -r').

get_os_description() ->
	get_os_description(get_os()).

get_os_description(mac) ->
	List = [get_type(mac),
			" ",
			get_version(mac),
			" ",
			get_upnp()		
			],
	lists:append(List);

get_os_description(unix) ->
	List = [os:cmd('uname -v'),
  			" ",
			get_upnp()
			],
	lists:append(List).

get_upnp() ->
	?UPNP.

get_ip() ->
	get_active_ip().

get_active_ip() ->
	get_active_ip(get_iflist()).

get_active_ip(If_list) ->
	get_ip([A || A <- If_list, inet:ifget(A,[addr]) /= {ok,[{addr,{127,0,0,1}}]}, filter_networkcard(list_to_binary(A))]).
	
filter_networkcard(<<"vnic", _R/binary>>) ->
	false;
filter_networkcard(<<"vmnet", _R/binary>>) ->
	false;
filter_networkcard(<<"eth0", _R/binary>>) ->
	true;
filter_networkcard(_) ->
	false.

get_ip([]) ->
	get_loopback();

get_ip([If]) ->
	case inet:ifget(If, [addr]) of
		{ok, []} -> get_loopback();
		{_, [{_, Ip}]} -> Ip
	end.

get_ip_as_string() ->
	get_ip_as_string(get_active_ip()).

get_ip_as_string(Ip) ->
	inet_parse:ntoa(Ip).

get_loopback() ->
	get_loopback(get_iflist()).

get_loopback(If_list) ->
	get_ip([A || A <- If_list, inet:ifget(A,[addr]) == {ok,[{addr,{127,0,0,1}}]}]).

get_iflist() ->
	{ok, IfList} = inet:getiflist(),
	IfList.


%%
%% Local Functions
%%
%% -------------------------------------------------------------------------
%% Test Functions
%% -------------------------------------------------------------------------
get_ip_as_string_test() ->
	?assertEqual("192.168.2.32", get_ip_as_string({192,168,2,32})).

get_active_ip_test() ->
	?assertEqual({127,0,0,1}, get_active_ip(["lo", "unknown"])),
	?assertEqual({127,0,0,1}, get_active_ip([])).

get_os_test() ->
	?assertEqual(unix, get_os({unix, linux})).
