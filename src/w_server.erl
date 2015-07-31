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
    put(repo, RepoModule),
    create_identity(),
    WxServer = wx:new(),
    set_wx_server(WxServer),
    UselessWindow = wxWindow:new(),
    {UselessWindow, []}.

% Terminate the process loop when the window closes.
handle_event(#wx{event=#wxClose{}}, State) -> 
    % TODO: TEST! This is probably wrong, since this server manages a bunch of controls.
    % Cleanup the state related to the closing window.
    {stop, normal, State};
handle_event(_Msg = #wx{id=?wxID_EXIT}, State) -> 
    % TODO: this is wrong. Cleanup state (which is not in State).
    wxWindow:destroy(State),
    {noreply, State};
% Handle menu & toolbar click events:
handle_event(Msg = #wx{id=ControlId, event=#wxCommand{ type=command_menu_selected }}, State) -> 
    io:format("wxCommand menu click. ~p ~p~n", [self(), Msg]),
    ClientPid = get_owner_pid(ControlId),
    {_WxControl, Text} = get_control(ControlId),
    io:format("   sending callback to client ~p~n", [ClientPid]),
    ClientPid ! {click, {button, ControlId, Text}},
    {noreply, State};
% Handle regular button click events:
handle_event(Msg = #wx{id=ButtonId, event=#wxCommand{type = command_button_clicked}}, State) ->
    io:format("wxCommand button click. ~p ~p~n", [self(), Msg]),
    ClientPid = get_owner_pid(ButtonId),
    {_WxButton, Text} = get_control(ButtonId),
    io:format("   sending callback to client ~p~n", [ClientPid]),
    ClientPid ! {click, {button, ButtonId, Text}},  %TODO: differentiate between toolbar & regular buttons?
    {noreply, State};
handle_event(_Msg, State) -> {noreply, State}.


% Frame
%------------------------------------------------------------------
handle_call({new_frame, Title, Options}, From, State) ->
    {Id, WxFrame} = new_window(Title, Options), %TODO: options
    set_control(From, Id, WxFrame),
    {reply, {frame, Id}, State};
handle_call({show, {frame, FrameId}}, _From, State) ->
    load_control_and_run(FrameId, wxWindow, show),
    {reply, ok, State};

% Panel
%------------------------------------------------------------------
handle_call({add_panel, FrameId, Options}, From, State) ->
    {Id, WxPanel} = load_control_and_run(FrameId, ?MODULE, add_panel, [Options]),
    set_control(From, Id, WxPanel),
    {reply, {panel, Id}, State};

handle_call({set_sizer, PanelId, SizerId}, _From, State) ->
    WxPanel = get_control(PanelId),
    WxSizer = get_control(SizerId),
    wxPanel:setSizer(WxPanel, WxSizer),
    {reply, ok, State};

% Label
%------------------------------------------------------------------
handle_call({new_label, PanelId, Text}, From, State) ->
    WxPanel = get_control(PanelId),
    Id = next_id(),
    WxControl = wxStaticText:new(WxPanel, Id, Text),
    set_control(From, Id, WxControl),
    {reply, {label, Id}, State};

% Statusbar
%------------------------------------------------------------------
handle_call({add_statusbar, FrameId}, _From, State) ->
    load_control_and_run(FrameId, wxFrame, createStatusBar),
    {reply, ok, State};
handle_call({set_status, FrameId, Text}, _From, State) ->
    load_control_and_run(FrameId, wxFrame, setStatusText, [Text]),
    {reply, ok, State};

% Toolbar
%------------------------------------------------------------------
handle_call({add_toolbar, FrameId, Def, W, H}, From, State) ->
    Buttons = load_control_and_run(FrameId, ?MODULE, build_toolbar, [From, Def, W, H]),
    Buttons2 = [{button, Id, Title} || {Id, _WxButton, Title} <- Buttons],
    {reply, Buttons2, State};

% Boxsizer
%------------------------------------------------------------------
handle_call({new_box_sizer, Orientation}, From, State) ->
    Sizer = wxBoxSizer:new(Orientation),
    Id = next_id(),
    set_control(From, Id, Sizer),
    {reply, {box_sizer, Id}, State};

handle_call({set_min_size, SizerId, Width, Height}, _From, State) ->
    load_control_and_run(SizerId, wxSizer, setMinSize, [Width, Height]),
    {reply, ok, State};

handle_call({append_child, ParentId, ChildId, Flags}, _From, State) ->
    WxParent = get_control(ParentId),
    WxChild = get_control(ChildId),
    wxSizer:add(WxParent, WxChild, Flags),
    {reply, ok, State};

handle_call({append_spacer, SizerId, Amount}, _From, State) ->
    load_control_and_run(SizerId, wxSizer, addSpacer, [Amount]),
    {reply, ok, State};

% Gridsizer
%------------------------------------------------------------------
handle_call({new_grid_sizer, Rows, Columns, VerticlePadding, HorizontalPadding}, From, State) ->
    Sizer = wxGridSizer:new(Rows, Columns, VerticlePadding, HorizontalPadding),
    Id = next_id(),
    set_control(From, Id, Sizer),
    {reply, {grid_sizer, Id}, State};

handle_call({fill_grid_sizer, GsId, Controls}, _From, State) ->
    Sizer = get_control(GsId),
    fill_grid_sizer(Sizer, Controls),
    {reply, ok, State};

% FlexGrid sizer
%------------------------------------------------------------------
handle_call({new_flexgrid_sizer, Rows, Columns, VerticlePadding, HorizontalPadding}, From, State) ->
    Sizer = wxFlexGridSizer:new(Rows, Columns, VerticlePadding, HorizontalPadding),
    Id = next_id(),
    set_control(From, Id, Sizer),
    {reply, {flexgrid_sizer, Id}, State};

handle_call({fill_flexgrid_sizer, GsId, Controls}, _From, State) ->
    Sizer = get_control(GsId),
    fill_grid_sizer(Sizer, Controls),
    {reply, ok, State};

handle_call({expand_row, Id, Index, Proportion}, _From, State) ->
    load_control_and_run(Id, wxFlexGridSizer, addGrowableRow, [Index, [{proportion, Proportion}]]),
    {reply, ok, State};
handle_call({expand_col, Id, Index, Proportion}, _From, State) ->
    load_control_and_run(Id, wxFlexGridSizer, addGrowableCol, [Index, [{proportion, Proportion}]]),
    {reply, ok, State};

% Buttons
%------------------------------------------------------------------
handle_call({new_button, PanelId, Text}, From, State) ->
    Panel = get_control(PanelId),
    Button = new_button(Panel, From, Text),
    {reply, Button, State};


% Textbox constructors
%------------------------------------------------------------------
handle_call({new_textbox, PanelId, Options}, From, State) ->
    Id = next_id(),
    Textbox = load_control_and_run(PanelId, wxTextCtrl, new, [Id, Options]),
    set_control(From, Id, Textbox),
    {reply, {textbox, Id}, State};

% Textbox manipulation functions
%------------------------------------------------------------------
handle_call({append_text, TextboxId, Text}, _From, State) ->
    load_control_and_run(TextboxId, wxTextCtrl, appendText, [Text]),
    {reply, ok, State};
handle_call({get_text, TextboxId}, _From, State) ->
    Text = load_control_and_run(TextboxId, wxTextCtrl, getValue),
    {reply, Text, State};
handle_call({set_text, TextboxId, Text}, _From, State) ->
    load_control_and_run(TextboxId, wxTextCtrl, setValue, [Text]),
    {reply, ok, State};
handle_call({clear, TextboxId}, _From, State) ->
    load_control_and_run(TextboxId, wxTextCtrl, clear),
    {reply, ok, State};

% Listbox constructors
%------------------------------------------------------------------
handle_call({new_listbox, PanelId}, From, State) ->
    Id = next_id(),
    Listbox = load_control_and_run(PanelId, wxListBox, new, [Id, [{size, {-1,50}}]]), % TODO: size needs to be an option!
    set_control(From, Id, Listbox),
    {reply, {listbox, Id}, State};

% Listbox manipulation functions
%------------------------------------------------------------------
handle_call({fill_listbox, ListboxId, Items}, _From, State) ->
    load_control_and_run(ListboxId, wxListBox, set, [Items]),
    {reply, ok, State};

% Unused wx_object callbacks
%------------------------------------------------------------------
handle_call(Msg, _From, State) -> {reply, {unknown_message, Msg}, State}.

handle_info(_Msg, State) -> {noreply, State}.
handle_cast(stop, State) -> {stop, normal, State};
handle_cast(_Msg, State) -> {noreply, State}.
code_change(_, _, State) -> {stop, ignore, State}.
terminate(_Reason, _State) -> ok.

%%%%%
%% TODO: break internal functions into a stand-alone module!
%%%%%
% Internal Functions
%------------------------------------------------------------------
new_window(Title, Options) ->
    Id = next_id(),
    WxFrame = wxFrame:new(get_wx_server(), Id, Title, Options),
    % Terminate the process loop when the window closes:
    wxFrame:connect(WxFrame, close_window),
    wxFrame:connect(WxFrame, command_menu_selected), %Toolbar & menu commands
    wxFrame:connect(WxFrame, command_button_clicked), %Regular button commands
    {Id, WxFrame}.

add_panel(WxFrame, []) -> {next_id(), wxPanel:new(WxFrame)};
add_panel(WxFrame, Options) -> {next_id(), wxPanel:new(WxFrame, Options)}.

% Load a wxControl by id, then invoke the given Fun with the wxControl as the first or only argument.
% EG:   load_control_and_run(ControlId, wxFrame, setStatusText, [Text]) invokes
%       wxFrame:setStatusText(TheWxFrameObject, Text)
load_control_and_run(ControlId, Mod, Fun) -> load_control_and_run(ControlId, Mod, Fun, []).
load_control_and_run(ControlId, Mod, Fun, ExtraArgs) ->
    WxControl = get_control(ControlId), % TODO: what to do if control is not found?
    erlang:apply(Mod, Fun, [WxControl|ExtraArgs]).

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

get_bitmap(Name, W, H) -> wxArtProvider:getBitmap(Name, [{size, {W, H}}]).

fill_grid_sizer(Sizer, Def) ->
    Controls = [add_to_grid_sizer(Sizer, X) || X <- Def],
    Controls.


% TODO: pull options out. Refactor and conbine similar clauses.
add_to_grid_sizer(Sizer, blank) ->
    wxSizer:addSpacer(Sizer, 0),
    blank;
add_to_grid_sizer(Sizer, {label, Id}) ->
    WxControl = get_control(Id),
    wxSizer:add(Sizer, WxControl, [{flag, ?wxEXPAND}]); % TODO: need to handle proportion too.
add_to_grid_sizer(Sizer, {textbox, Id}) ->
    WxControl = get_control(Id),
    wxSizer:add(Sizer, WxControl, [{flag, ?wxEXPAND}]); % TODO: does it make sense to have this as a default?

add_to_grid_sizer(Sizer, {listbox, Id}) ->
    WxControl = get_control(Id),
    wxSizer:add(Sizer, WxControl, [{flag, ?wxEXPAND}]); % TODO: options??

add_to_grid_sizer(Sizer, {button, Id, _Text}) ->
    {WxButton, _Text} = get_control(Id),
    wxSizer:add(Sizer, WxButton, [{proportion, 0}, {flag, ?wxEXPAND}]). % TODO: options!


new_button(WxPanel, From, Text) ->
    Id = next_id(),
    WxButton = wxButton:new(WxPanel, Id, [{label, Text}]),
    set_control(From, Id, {WxButton, Text}),
    {button, Id, Text}.



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

% TODO: WRITE LOTS AND LOTS OF UNIT TESTS!