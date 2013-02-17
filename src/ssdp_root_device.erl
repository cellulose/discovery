%% Author: ua
%% Created: Apr 20, 2010
%% Description: TODO: Add description to em_root_device

-module(ssdp_root_device).

-behaviour(gen_server).
%%
%% Include files
%%
-include("../include/upnp.hrl").
%%
%% Exported Functions
%%
-export([get_ip/0, get_port/0, get_services/0, get_description_uri/0, get_uuid/0, get_os/0, createRootdevice/0]).
-export([get_ip_as_string/0, get_ip_port/0, get_os_info/0, get_root_device/0, get_service/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start/0]).
-export([get_nt/0, get_st/0, get_uri/0, get_service_type/0]).

-record(state, {rootdevice}).

-define(UPNP_ROOTDEVICE, "upnp:rootdevice").
%% ====================================================================
%% Server functions
%% ====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok, #state{rootdevice=createRootdevice()}}.
%%
%% API Functions
%%
get_root_device() ->
	gen_server:call(?MODULE, {get_root_device}). 

get_ip() ->
	gen_server:call(?MODULE, {get_ip}). 
	
get_ip_as_string() ->
	gen_server:call(?MODULE, {get_ip_as_string}).

get_port() ->
	gen_server:call(?MODULE, {get_port}).

get_services() ->
	gen_server:call(?MODULE, {get_services}).

get_service(Search_Type) ->
	gen_server:call(?MODULE, {get_service, Search_Type}).
	
get_description_uri() ->
	gen_server:call(?MODULE, {get_description_uri}).

get_uuid() ->
	gen_server:call(?MODULE, {get_uuid}).

get_os() ->
	gen_server:call(?MODULE, {get_os}).

get_os_info() ->
	gen_server:call(?MODULE, {get_os_info}).

get_ip_port() ->
	gen_server:call(?MODULE, {get_ip_port}).
get_nt() ->
	?UPNP_ROOTDEVICE.
get_st() ->
	get_nt().
get_service_type() ->
	"upnp:rootdevice".
get_uri() ->
	"/description/fetch".

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
handle_call({get_root_device}, _From, State) ->
	{reply, State#state.rootdevice, State};
handle_call({get_ip}, _From, State) ->
	{reply, get_ip(State#state.rootdevice), State};
handle_call({get_ip_as_string}, _From, State) ->
	{reply, get_ip_as_string(State#state.rootdevice), State};
handle_call({get_port}, _From, State) ->
	{reply, get_port(State#state.rootdevice), State};
handle_call({get_services}, _From, State) ->
	{reply, get_services(State#state.rootdevice), State};
handle_call({get_description_uri}, _From, State) ->
	{reply, get_description_uri(State#state.rootdevice), State};
handle_call({get_os}, _From, State) ->
	{reply, get_os(State#state.rootdevice), State};
handle_call({get_os_info}, _From, State) ->
	{reply, get_os_info(State#state.rootdevice), State};
handle_call({get_uuid}, _From, State) ->
	{reply, get_uuid(State#state.rootdevice), State};
handle_call({get_ip_port}, _From, State) ->
	{reply, get_ip_port(State#state.rootdevice), State};
handle_call({get_service, Search_Type}, _From, State) ->
	{reply, get_service(get_services(State#state.rootdevice), Search_Type), State}.

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
handle_info(Info, State) ->
	error_logger:info_report("Info : ~n~p : ", [Info]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Local Functions
%%	
get_ip(#rootdevice{ip = Ip})->
	Ip.

get_ip_as_string(#rootdevice{ip = Ip}) ->
	em_os_info:get_ip_as_string(Ip).

get_port(#rootdevice{port = Port}) ->
	Port.

get_services(#rootdevice{services = Services})  ->
	Services.

get_description_uri(#rootdevice{descriptionuri = Descriptionuri}) ->
	Descriptionuri.

get_uuid(#rootdevice{uuid = Uuid}) ->
	Uuid.

get_os(#rootdevice{os = Os}) ->
	Os.

get_os_info(#rootdevice{os = Os}) ->
	Os.

get_ip_port(#rootdevice{ip = Ip, port = Port}) ->
	em_os_info:get_ip_as_string(Ip) ++ ":" ++ integer_to_list(Port).

get_service(Services, ST) ->
	error_logger:info_msg("get_service ~p : in : ~p~n", [ST, Services]),
	case [X || X <- Services, string:equal(ST, X:get_st())] of
		[] -> {error, "no service found"};
		[Service] -> {ok, Service}
	end.

createRootdevice() ->
	#rootdevice{uuid=em_util:v4_as_string(), os=em_os_info:get_os_description(),
				ip=em_os_info:get_active_ip(), port=em_config:get_value(port),
				services=em_config:get_value(services)}.	
				