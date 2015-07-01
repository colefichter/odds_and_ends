-module(w_server).
-behaviour(wx_object).

% Client API
-export([start/0, start/1, stop/0]).

% wx_object callbacks
-export([init/1, terminate/2,  code_change/3, handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

% Exports for convenience
-export([add_panel/2, build_toolbar/5]).

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

handle_event(Msg = #wx{id=ControlId, event=#wxCommand{ type=command_menu_selected }}, State) -> 
    io:format("wxCommand menu click. ~p ~p~n", [self(), Msg]),
    ClientPid = get_owner_pid(ControlId),
    {_WxControl, Text} = get_control(ControlId),
    io:format("   sending callback to client ~p~n", [ClientPid]),
    ClientPid ! {click, {button, ControlId, Text}},


    {noreply, State};

handle_event(Msg, State) -> 
    io:format("Generic handler click. ~p~n", [Msg]),
    {noreply, State}.


handle_call({new_frame, Title, Options}, From, State) ->
    {Id, WxFrame} = new_window(Title, Options), %TODO: options
    set_control(From, Id, WxFrame),
    {reply, {frame, Id}, State};

handle_call({add_statusbar, FrameId}, _From, State) ->
    load_frame_and_run(FrameId, wxFrame, createStatusBar),
    {reply, ok, State};

handle_call({set_status, FrameId, Text}, _From, State) ->
    load_frame_and_run(FrameId, wxFrame, setStatusText, [Text]),
    {reply, ok, State};

handle_call({add_panel, FrameId, Options}, From, State) ->
    {Id, WxPanel} = load_frame_and_run(FrameId, ?MODULE, add_panel, [Options]),
    set_control(From, Id, WxPanel),
    {reply, {panel, Id}, State};

handle_call({add_toolbar, FrameId, Def, W, H}, From, State) ->
    Buttons = load_frame_and_run(FrameId, ?MODULE, build_toolbar, [From, Def, W, H]),
    Buttons2 = [{button, Id, Title} || {Id, _WxButton, Title} <- Buttons],
    {reply, Buttons2, State};

handle_call({show, {frame, FrameId}}, _From, State) ->
    load_frame_and_run(FrameId, wxWindow, show),
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

load_frame_and_run(FrameId, Mod, Fun) -> load_frame_and_run(FrameId, Mod, Fun, []).
load_frame_and_run(FrameId, Mod, Fun, ExtraArgs) ->
    %WxFrame = get_control(From, FrameId), % TODO: what to do if control is not found?
    WxFrame = get_control(FrameId),
    erlang:apply(Mod, Fun, [WxFrame|ExtraArgs]).

build_toolbar(WxFrame, From, Def, W, H) ->
    % TODO: pass in styles
    WxToolbar = wxFrame:createToolBar(WxFrame, [{style, ?wxNO_BORDER bor ?wxTB_HORIZONTAL}]),
    Buttons = [new_toolbar_button(WxToolbar, From, X, W, H) || X <- Def],
    wxToolBar:realize(WxToolbar),
    wxFrame:setToolBar(WxFrame,WxToolbar),
    Buttons.

new_toolbar_button(Toolbar, From, {Title, IconName}, W, H) ->
    Icon = get_bitmap(IconName, W, H),    
    Id = next_id(),    
    WxButton = wxToolBar:addTool(Toolbar, Id, Title, Icon, [{shortHelp, Title}]),
    set_control(From, Id, {WxButton, Title}),
    {Id, WxButton, Title};
new_toolbar_button(Toolbar, From, {Title, IconName, LongHelp}, W, H) ->
    {Id, WxButton, Title} = new_toolbar_button(Toolbar, From, {Title, IconName}, W, H),
    wxToolBar:setToolLongHelp(Toolbar, Id, LongHelp),
    {Id, WxButton, Title}.

get_bitmap(Name, W, H) -> wxArtProvider:getBitmap(Name, [{size, {W, H}}]).  % TODO: allow different sizes!


% Wrapper around the repo:
%------------------------------------------------------------------
repo() -> get(repo).

create_identity() -> (repo()):create_identity().
next_id() -> (repo()):next_id().

get_wx_server() -> (repo()):get_wx_server().
set_wx_server(Server) -> (repo()):set_wx_server(Server).

get_owner_pid(ControlId) ->
    {ok, {ControlId, OwnerPid, _WxControl}} = (repo()):get_control(ControlId),
    OwnerPid.
get_control(ControlId) ->
    {ok, {ControlId, _OwnerPid, WxControl}} = (repo()):get_control(ControlId),
    WxControl.
set_control({ClientPid, _}, ControlId, Control) ->
    (repo()):set_control(ControlId, ClientPid, Control).

%------------------------------------------------------------------
% Unit tests: Move these into a separate module
%------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

simple_window_test() ->
    w_server:start(), %Do this in a supervision tree instead!
    Frame = {frame, _FrameId} = w:new_frame("TESTING!", [{size, {200, 200}}]),
    ok = w:add_statusbar(Frame, "Statusbar text set quickly!"),
    {panel, _PanelId} = w:add_panel(Frame),

    ToolbarButtonDef = [
        {"New", "wxART_NEW", "This is long help for 'New'"},
        {"Press Me", "wxART_ERROR"},
        {"Copy", "wxART_COPY", "Copy something to the clipboard"} %Long Help ends up in status bar!
    ],
    [
        {button, _B1Id, "New"},
        {button, _B2Id, "Press Me"},
        {button, _B3Id, "Copy"}
    ] = w:add_toolbar(Frame, ToolbarButtonDef),
    ok = w:show(Frame),
    ok = w_server:stop().