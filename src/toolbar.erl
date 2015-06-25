-module(toolbar).
-behaviour(wx_object).

-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

-include_lib("wx/include/wx.hrl").

% Client API
%------------------------------------------------------------------
start() -> wx_object:start_link(?MODULE, [], []).


% Server Implementation (behaviour callbacks)
%------------------------------------------------------------------
init([]) ->
    Frame = make_window(),
    wxFrame:show(Frame),
    {Frame, Frame}.

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
handle_call(_Msg, _From, State) -> {reply, ok, State}.
code_change(_, _, State) -> {stop, ignore, State}.
terminate(_Reason, _State) -> ok.

% Internal Stuff
%------------------------------------------------------------------
make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, 999, "Toolbar Test", [{size, {300, 250}}]),

    % TODO: what about the ID?
    % TODO: shorthelp? Just make it the same as the text?
    Def = [
        {"New", "wxART_NEW", "This is long help for 'New'"},
        {"Press Me", "wxART_ERROR"},
        {"Copy", "wxART_COPY", "Copy something to the clipboard"} %Long Help ends up in status bar!
    ],

    _Toolbar = build_toolbar(Frame, Def),   

    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "The tool bar is working!"),

    % Terminate the process loop when the window closes:
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_menu_selected),
    Frame.

%------------------------------------------------------------------
%------------------------------------------------------------------
% LIBRARY STUFF
%------------------------------------------------------------------
%------------------------------------------------------------------
-type toolbar_def() :: [toolbar_button_def()].
-type toolbar_button_def() :: {string(), string()}
                            | {string(), string(), string()}.

-type toolbar_button() :: {integer(), any()}. %TODO: does wx.hrl define types?

-spec build_toolbar(any(), toolbar_def()) -> any().
build_toolbar(Frame, Def) ->
    Toolbar = wxFrame:createToolBar(Frame, [{style, ?wxNO_BORDER bor ?wxTB_HORIZONTAL}]),
    _Buttons = [new_toolbar_button(Toolbar, X) || X <- Def],
    wxToolBar:realize(Toolbar),
    wxFrame:setToolBar(Frame,Toolbar),
    Toolbar.  % TODO: wrap the wx type

-spec new_toolbar_button(any(), toolbar_button_def()) -> toolbar_button().
new_toolbar_button(Toolbar, {Title, IconName}) ->
    Icon = get_bitmap(IconName),    
    Id = random:uniform(10000), % TODO: IDs
    % TODO: shortHelp fix
    Button = wxToolBar:addTool(Toolbar, Id, Title, Icon, [{shortHelp, Title}]),
    {Id, Button};
new_toolbar_button(Toolbar, {Title, IconName, LongHelp}) ->
    {Id, Button} = new_toolbar_button(Toolbar, {Title, IconName}),
    wxToolBar:setToolLongHelp(Toolbar, Id, LongHelp),
    {Id, Button}.



-spec get_bitmap(string()) -> any(). %TODO: does wx.hrl define types?
get_bitmap(Name) -> wxArtProvider:getBitmap(Name, [{size, {16,16}}]).