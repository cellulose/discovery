-module(ssdp_root_device).

-behaviour(gen_server).
%%
%% Include files
%%
-include("../include/upnp.hrl").
%%
%% Exported Functions
%%
-export([get_ip/0, get_port/0, get_services/0, get_description_uri/0, get_uuid/0, get_os/0, createRootdevice/1]).
-export([get_ip_as_string/0, get_ip_port/0, get_os_info/0, get_root_device/0, get_service/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1, start/0, start/1]).
-export([get_nt/0, get_st/0, get_st/1, get_uri/0, get_service_type/0]).

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
  start_link([]).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start() ->
  start([]).

start(Args) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Args, []).

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init(Args) ->
    {ok, #state{rootdevice=createRootdevice(Args)}}.
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
  gen_server:call(?MODULE, {get_nt}).

get_st() ->
  get_nt().

get_st(State) ->
	get_nt(State#state.rootdevice).

get_service_type() ->
  gen_server:call(?MODULE, {get_service_type}).

get_uri() ->
  gen_server:call(?MODULE, {get_uri}).

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
handle_call({get_service_type}, _From, State) ->
  {reply, get_service_type(State#state.rootdevice), State};
handle_call({get_nt}, _From, State) ->
  {reply, get_nt(State#state.rootdevice), State};
handle_call({get_uri}, _From, State) ->
  {reply, get_uri(State#state.rootdevice), State};
handle_call({get_service, Search_Type}, _From, State) ->
	{reply, get_service(get_services(State#state.rootdevice), Search_Type, State), State}.


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
handle_info(_Info, State) ->
	% error_logger:info_report("Info : ~n~p : ", [Info]),
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
	ssdp_os_info:get_ip_as_string(Ip).

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
	ssdp_os_info:get_ip_as_string(Ip) ++ ":" ++ integer_to_list(Port).

get_service(Services, ST, State) ->
	case [X || X <- Services, string:equal(ST, X:get_st(State))] of
		[] -> {error, "no service found"};
		[Service] -> {ok, Service}
	end.

get_service_type(#rootdevice{uuid = Uuid}) ->
  Uuid.

get_nt(#rootdevice{uuid = Uuid}) ->
  Uuid.

get_uri(#rootdevice{uri = Uri}) ->
  Uri.

createRootdevice(Args) ->
    USN = proplists:get_value(usn, Args),
    Port = proplists:get_value(port, Args),
    Uri = proplists:get_value(uri, Args),
    #rootdevice{uuid=binary_to_list(USN),
		os=ssdp_os_info:get_os_description(),
		ip=ssdp_os_info:get_active_ip(), port=Port,
		services=[ssdp_root_device],
    uri=binary_to_list(Uri)}.
