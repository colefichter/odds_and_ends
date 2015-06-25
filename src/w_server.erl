-module(w_server).
-behaviour(wx_object).

-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

-include_lib("wx/include/wx.hrl").

-define(WXSERVER, wx_server).

% Client API
%------------------------------------------------------------------
start() -> wx_object:start_link({local, ?MODULE}, ?MODULE, [], []).

% Server Implementation (wx_object behaviour callbacks)
%------------------------------------------------------------------
init([]) ->
    io:format("w_server:init ~p~n", [self()]),
    create_identity(),
    WxServer = wx:new(),
    store_wx_server(WxServer),
    UselessWindow = wxWindow:new(),
    {UselessWindow, []}.

% Terminate the process loop when the window closes.
handle_event(#wx{event=#wxClose{}}, State) -> {stop, normal, State};

handle_event(Msg = #wx{id=?wxID_EXIT}, State) -> 
    io:format("Close. ~p~n", [Msg]),
    wxWindow:destroy(State),
    {noreply, State};

% handle_event(Msg = #wx{id=100001}, State) -> 
%     io:format("Nested menu click. ~p~n", [Msg]),
%     {noreply, State};
% handle_event(Msg = #wx{id=200002}, State) -> 
%     io:format("Simple menu click. ~p~n", [Msg]),
%     {noreply, State};

handle_event(Msg = #wx{event=#wxCommand{ type=command_menu_selected }}, State) -> 
    io:format("wxCommand menu click. ~p ~p~n", [self(), Msg]),
    {noreply, State};

handle_event(Msg, State) -> 
    io:format("Generic handler click. ~p~n", [Msg]),
    {noreply, State}.


handle_info(_Msg, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.


handle_call(fake_click, _From, CallbackModule) ->
    %CallbackModule:button_clicked("some state goes here"),
    CallbackModule ! {fake_click, "some state goes here"},
    {reply, ok, CallbackModule};
handle_call({window, Title}, From, CallbackModule) ->
    io:format("w_server:handle_call({window, Title}) ~p~n", [self()]),
    Frame = wxFrame:new(load_wx_server(), next_id(), Title, [{size, {300, 250}}]),

    %TODO: store client Pids somewhere!
    {ClientPid, _} = From,
    ClientPid ! {window, created},

    wxWindow:show(Frame),
    {reply, Frame, ClientPid};
% handle_call(build_frame, _From, CallbackModule) ->
%     io:format("BUILDING FRAME (server)~n"),
%     CallbackModule:build_frame("build_frame invoked!"),
%     {reply, nothing, CallbackModule};
handle_call(_Msg, _From, State) -> {reply, ok, State}.

code_change(_, _, State) -> {stop, ignore, State}.
terminate(_Reason, _State) -> ok.

% Internal Functions
%------------------------------------------------------------------


store_wx_server(Server) -> put(?WXSERVER, Server).
load_wx_server() -> get(?WXSERVER).

-define(IDENTITY, identity).
create_identity() -> put(?IDENTITY, 1).
next_id() ->    
    Id = get(?IDENTITY),
    put(?IDENTITY, Id+1).