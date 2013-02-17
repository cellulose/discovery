%%% -------------------------------------------------------------------
%%% Author  : ua
%%% Description :
%%%
%%% Created : Jan 23, 2010
%%% -------------------------------------------------------------------
-module(ssdp).
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("../include/upnp.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_all/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0]).
-record(state, {socket}).
-define(M_SEARCH, "M-SEARCH").
-define(NOTIFY, "NOTIFY").
%% ====================================================================
%% External functions
%% ====================================================================

start_all() ->
    nemo_config:start(),
    em_root_device:start(),
    start().

%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start() ->
	start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------

init([]) ->
    Socket = open_multicast_socket(),
    start_timer(),
    {ok, #state{socket=Socket}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}. 
%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({udp, _Socket, IPtuple, InPortNo, Packet}, State) ->
	% error_logger:info_msg("~n~nFrom IP: ~p~nPort: ~p~nData: ~p~n", [IPtuple, InPortNo, Packet]),
	case is_msearch(Packet) of
		false -> pass; %% error_logger:info_msg("I don't understand the Message : ~p~n", [Packet]);
		true -> handle_msearch(IPtuple, InPortNo, Packet, State)
	end,		
	{noreply, State};

handle_info(timeout, State) ->
    [send_is_alive(State#state.socket, get_message_is_alive(X)) || X <- em_root_device:get_services()],
    start_timer(),
    {noreply, State}.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	error_logger:info_msg("stopping ssdp with Reason : ", [Reason]),
	[send_byebye(State#state.socket, get_message_byebye(X)) || X <- em_root_device:get_services()],
	close(State#state.socket),
    ok.
%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
close(Socket) -> 
	gen_udp:close(Socket).

open_multicast_socket() ->
	{ok,Socket} = gen_udp:open(?MULTICAST_PORT, ?SOCKET_OPTIONS),
	inet:setopts(Socket, [{add_membership,{?MULTICAST_GROUP, ?CONTROL_PORT_IP}}]),
	Socket.
 
is_msearch(Message) ->
	%%error_logger:info_msg("Parse : ~p~n", [Message]),
	ssdp_util:startsWith(Message, ?M_SEARCH).

handle_msearch(Ip, InPort, InMessage, State) ->
	error_logger:info_msg("~n~nM-SEARCH From IP: ~p~nPort: ~p~nMessage: ~p~n", [Ip, InPort, InMessage]),
	ST = get_st(InMessage),
	case ssdp_root_device:get_service(ST) of
		{ok, Service} -> send_msearch_response(Ip, InPort, ST, Service, State);
		{error, _Reason} -> error_logger:info_msg("This type is not available : ~p~n", [ST])
	end,
	ok.

send_msearch_response(Ip, InPort, ST, Service, State) ->
	error_logger:info_msg("send msearch response ~n"),
	Message = ssdp_msg_factory:build_msearch_response(ST, Service:get_uri(), Service:get_service_type()),	
	gen_udp:send(State#state.socket, Ip, InPort, erlang:list_to_binary(Message)),
	ok.

get_st(Message) ->
	[ST] = [string:strip(string:sub_string(X, 4)) || X <- string:tokens(Message, "\r\n"), ssdp_util:startsWith(X, "ST")],
	error_logger:info_msg("found ST in message : ~p~n", [ST]),
	ST.

% get_time() ->
%	{ok, Timer} = application:get_env(?ERLMEDIASERVER_APP_FILE, timer),
%	Timer.	

get_message_is_alive(Service) ->
	ssdp_msg_factory:build_is_alive(Service:get_nt(), Service:get_uri()).

get_message_byebye(Service) ->
	ssdp_msg_factory:build_bye_bye(Service:get_nt()).

send_is_alive(Socket, Message) ->
	ok = gen_udp:send(Socket, ?MULTICAST_GROUP, ?MULTICAST_PORT, list_to_binary(Message)).

send_byebye(Socket, Message) ->	
	ok = gen_udp:send(Socket, ?MULTICAST_GROUP, ?MULTICAST_PORT, list_to_binary(Message)).

%% reset timer for every 30 seconds.
%% used to do: erlang:send_after(get_time() * 60 * 1000, self(), timeout).
start_timer() ->
	erlang:send_after(30*1000, self(), timeout).

%% Test Functions
get_st_with_space_test() ->
	Message = "ST: urn:schemas-upnp-org:device:MediaServer:1\r\n",
	?assertEqual("urn:schemas-upnp-org:device:MediaServer:1", get_st(Message)). 	

get_st_without_space_test() ->
	Message = "ST:urn:schemas-upnp-org:device:MediaServer:1\r\n",
	?assertEqual("urn:schemas-upnp-org:device:MediaServer:1", get_st(Message)).