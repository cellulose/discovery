%%% -------------------------------------------------------------------
%%% Author  : ua
%%% Description :
%%%
%%% Created : Jan 23, 2010
%%% -------------------------------------------------------------------

-module(ssdp).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
     code_change/3, start_link/0, start/0]).

-include_lib("eunit/include/eunit.hrl").

-include("../include/upnp.hrl").
-export([start_all/0]).

-export([tokenize/1, test_notify_msg/0]).

%% gen_server callbacks

-record(state, {socket, discovered, announce_count}).

-define(M_SEARCH, "M-SEARCH").
-define(NOTIFY, "NOTIFY").

start_all() ->
    ssdp_root_device:start(),
    start().

start() ->  start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Socket = open_multicast_socket(),
    [send_is_alive(Socket, get_message_is_alive(X)) || X <- ssdp_root_device:get_services()],
    start_timer(timer_interval(0)),
    {ok, #state{socket=Socket, announce_count=1}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info({udp, _Socket, IPtuple, InPortNo, Packet}, State) ->
    %error_logger:info_msg("~n~nFrom IP: ~p~nPort: ~p~nData: ~p~n", [IPtuple, InPortNo, Packet]),
    {ok, NewState} = case is_msearch(Packet) of
        true -> handle_msearch(IPtuple, InPortNo, Packet, State);
        false -> case is_notify(Packet) of
            true -> handle_notify(IPtuple, Packet, State);
            false -> % not msearch or notify, so forward to Elixir IP
                'Elixir.Echo.Hardware.Ethernet':ssdp_not_search_or_notify(list_to_binary(Packet), IPtuple, InPortNo),
                {ok, State}
             %% error_logger:info_msg("I don't understand the Message : ~p~n", [Packet]);
        end
    end,
    {noreply, NewState};

handle_info(timeout, State) ->
    [send_is_alive(State#state.socket, get_message_is_alive(X)) || X <- ssdp_root_device:get_services()],
    AC=State#state.announce_count,
    start_timer(timer_interval(AC)),
    {noreply, State#state{announce_count=(AC+1)}}.

terminate(Reason, State) ->
    error_logger:info_msg("stopping ssdp with Reason : ", [Reason]),
    [send_byebye(State#state.socket, get_message_byebye(X)) || X <- ssdp_root_device:get_services()],
    close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

close(Socket) ->
    gen_udp:close(Socket).

open_multicast_socket() ->
    {ok,Socket} = gen_udp:open(?MULTICAST_PORT, ?SOCKET_OPTIONS),
    inet:setopts(Socket, [{add_membership,{?MULTICAST_GROUP, ssdp_root_device:get_ip()}}]),
    Socket.

is_msearch(Message) -> ssdp_util:startsWith(Message, ?M_SEARCH).

is_notify(Message) ->  ssdp_util:startsWith(Message, ?NOTIFY).

handle_notify(_IP, _Message, State) -> % do nothing for now
    %error_logger:info_msg("~n~nNOTIFY From IP: ~p~n Message: ~p~n", [Ip, Message]),
    %NotifyTree = notify_msg_to_update_tree(IP, Message),
    %hub:update([ssdp,alive], NotifyTree, []),
    {ok, State}.

handle_msearch(Ip, InPort, InMessage, State) ->
    %error_logger:info_msg("~n~nM-SEARCH From IP: ~p~nPort: ~p~nMessage: ~p~n", [Ip, InPort, InMessage]),
    ST = get_st(InMessage),
    case ssdp_root_device:get_service(ST) of
        {ok, Service} -> send_msearch_response(Ip, InPort, ST, Service, State);
        {error, _Reason} -> pass
        %error_logger:info_msg("This type is not available : ~p~n", [ST])
    end,
    {ok, State}.

send_msearch_response(Ip, InPort, ST, Service, State) ->
    %error_logger:info_msg("send msearch response ~n"),
    Message = ssdp_msg_factory:build_msearch_response(ST, Service:get_uri(), Service:get_service_type()),
    gen_udp:send(State#state.socket, Ip, InPort, erlang:list_to_binary(Message)),
    ok.

get_st(Message) ->
    [ST] = [string:strip(string:sub_string(X, 4)) || X <- string:tokens(Message, "\r\n"), ssdp_util:startsWith(X, "ST")],
    %error_logger:info_msg("found ST in message : ~p~n", [ST]),
    ST.

% get_time() ->
%   {ok, Timer} = application:get_env(?ERLMEDIASERVER_APP_FILE, timer), Timer.

tokenize(Message) ->
    string:tokens(Message, "\r\n").

get_message_is_alive(Service) ->
    ssdp_msg_factory:build_is_alive(Service:get_nt(), Service:get_uri()).

get_message_byebye(Service) ->
    ssdp_msg_factory:build_bye_bye(Service:get_nt()).

send_is_alive(Socket, Message) ->
    % error_logger:info_msg("SENDING ALIVE: ~p~n", [Message]),
    ok = gen_udp:send(Socket, ?MULTICAST_GROUP, ?MULTICAST_PORT, list_to_binary(Message)).

send_byebye(Socket, Message) ->
    ok = gen_udp:send(Socket, ?MULTICAST_GROUP, ?MULTICAST_PORT, list_to_binary(Message)).

%% reset timer for every 30 seconds.
%% used to do: erlang:send_after(get_time() * 60 * 1000, self(), timeout).
start_timer(Interval) ->
    erlang:send_after(Interval, self(), timeout).

timer_interval(N) when N > 10 -> 30000;
timer_interval(N) when N > 3 -> 5000;
timer_interval(_N) -> 1000.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_st_with_space_test() ->
    Message = "ST: urn:schemas-upnp-org:device:MediaServer:1\r\n",
    ?assertEqual("urn:schemas-upnp-org:device:MediaServer:1", get_st(Message)).

get_st_without_space_test() ->
    Message = "ST:urn:schemas-upnp-org:device:MediaServer:1\r\n",
    ?assertEqual("urn:schemas-upnp-org:device:MediaServer:1", get_st(Message)).

test_notify_msg() ->
    "NOTIFY * HTTP/1.1\r\nHOST: 239.255.255.250:1900\r\nNT: upnp:rootdevice\r\nNTS: ssdp:alive \r\nLOCATION: http://127.0.0.1:5001/description/fetch\r\nUSN : advertisement:uuid:718ec2f0-f023-4b91-bc88-415c60186c82\r\nCACHE-CONTROL: max-age=1800\r\nServer : Mac OS X 10.8.2 UPnP/1.0 NEMO/0.1\r\n".


%% %% Takes a key:value form line, and turns it into a tuple
%% %% Used for cracking various config and http headers
%% line_to_prop(Line) ->
%%     L = re:split(Line, "\\W*:\\W*", [{parts, 2}, {return, list}]),
%%     case length(L) of
%%         2 -> { to_lowercase_atom(hd(L)), erlang:list_to_binary(tl(L))    };
%%         _ -> nil
%%     end.

%% to_lowercase_atom(Str) ->
%%     erlang:list_to_atom(inet_db:tolower(Str)).

%% %% break text into a list of lines
%% text_to_lines(Text) ->
%%     string:tokens(Text, "\r\n").

%% %% turns foo:whatever style lines into flat proplists,
%% %% discarding anything that doesn't fit that format.
%% lines_to_props(Lines) ->
%%     RawProplist = [ line_to_prop(Line) || Line <- Lines ],
%%     proplists:delete(nil, RawProplist).

%% %% returns a tuple/structure for a notify message, suitable for passing
%% %% to hub:update
%% notify_msg_to_update_tree(IP, Message) ->
%%     Lines = text_to_lines(Message),
%%     Pl1 = lines_to_props(tl(Lines)),
%%     Pl2 = lists:append(Pl1, [{ip, erlang:list_to_binary(inet_parse:ntoa(IP))}]),
%%     Usn = proplists:get_value(usn, Pl2),
%%     [{Usn, Pl2}].
