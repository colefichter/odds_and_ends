-module(w_server).
-behaviour(wx_object).

% Client API
-export([start/0, start/1, stop/0]).

% wx_object callbacks
-export([init/1, terminate/2,  code_change/3, handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

-include_lib("wx/include/wx.hrl").

% Client API
%------------------------------------------------------------------
start() -> wx_object:start_link({local, ?MODULE}, ?MODULE, [wrepo], []).
start(RepoModule) -> wx_object:start_link({local, ?MODULE}, ?MODULE, [RepoModule], []).

stop() -> wx_object:cast(?MODULE, stop).

% Server Implementation (wx_object behaviour callbacks)
%------------------------------------------------------------------
init([RepoModule]) ->
    io:format("w_server:init ~p~n", [self()]),
    put(repo, RepoModule),
    create_identity(),
    WxServer = wx:new(),
    set_wx_server(WxServer),
    UselessWindow = wxWindow:new(),
    {UselessWindow, []}.

% Terminate the process loop when the window closes.
handle_event(#wx{event=#wxClose{}}, State) -> {stop, normal, State};

handle_event(Msg = #wx{id=?wxID_EXIT}, State) -> 
    io:format("Close. ~p~n", [Msg]),
    wxWindow:destroy(State),
    {noreply, State};

handle_event(Msg = #wx{event=#wxCommand{ type=command_menu_selected }}, State) -> 
    io:format("wxCommand menu click. ~p ~p~n", [self(), Msg]),
    {noreply, State};

handle_event(Msg, State) -> 
    io:format("Generic handler click. ~p~n", [Msg]),
    {noreply, State}.


handle_call({new_frame, Title, Options}, From, State) ->
    io:format("w_server:handle_call({window, Title}) ~p~n", [self()]),
    F = {Id, _WxFrame} = new_window(Title, Options), %TODO: options
    store_frame(From, F),
    {reply, {frame, Id}, State};

handle_call({new_panel, FrameId, Options}, From, State) ->
    io:format("FRAMEID ~p~n", [FrameId]),
    {FrameId, WxFrame} = get_frame(From, FrameId), %TODO: not found?
    P = {Id, _wxPanel} = add_panel(WxFrame, Options),
    store_panel(From, P),
    {reply, {panel, Id}, State};

handle_call({show, {frame, FrameId}}, From, State) ->
    {FrameId, WxFrame} = get_frame(From, FrameId), %TODO: not found?
    wxWindow:show(WxFrame),
    {reply, ok, State};

handle_call(Msg, _From, State) -> {reply, {unknown_message, Msg}, State}.


handle_info(_Msg, State) -> {noreply, State}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.
code_change(_, _, State) -> {stop, ignore, State}.
terminate(_Reason, _State) -> ok.

% Internal Functions
%------------------------------------------------------------------

new_window(Title, Options) ->
    Id = next_id(),
    WxFrame = wxFrame:new(get_wx_server(), Id, Title, Options),
    % Terminate the process loop when the window closes:
    wxFrame:connect(WxFrame, close_window),
    wxFrame:connect(WxFrame, command_menu_selected),
    {Id, WxFrame}.

add_panel(WxFrame, []) -> {next_id(), wxPanel:new(WxFrame)};
add_panel(WxFrame, Options) -> {next_id(), wxPanel:new(WxFrame, Options)}.

% Wrapper around the repo:
%------------------------------------------------------------------
repo() -> get(repo).

create_identity() -> (repo()):create_identity().
next_id() -> (repo()):next_id().

get_wx_server() -> (repo()):get_wx_server().
set_wx_server(Server) -> (repo()):set_wx_server(Server).

% get_env({ClientPid, _}) -> (repo()):get_env(ClientPid).
% set_env({ClientPid, _}, Env) -> (repo()):set_env(ClientPid, Env).


get_frame({ClientPid, _}, FrameId) -> (repo()):get_frame(ClientPid, FrameId).
store_frame({ClientPid, _}, Frame) -> (repo()):store_frame(ClientPid, Frame).

get_panel({ClientPid, _}, PanelId) -> (repo()):get_panel(ClientPid, PanelId).
store_panel({ClientPid, _}, Panel) -> (repo()):store_panel(ClientPid, Panel).