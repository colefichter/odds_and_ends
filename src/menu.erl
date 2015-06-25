-module(menu).
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

handle_event(Msg = #wx{id=100001}, State) -> 
    io:format("Nested menu click. ~p~n", [Msg]),
    {noreply, State};
handle_event(Msg = #wx{id=200002}, State) -> 
    io:format("Simple menu click. ~p~n", [Msg]),
    {noreply, State};

handle_event(Msg = #wx{event=#wxCommand{ type=command_menu_selected }}, State) -> 
    io:format("wxCommand menu click. ~p~n", [Msg]),
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
    Frame = wxFrame:new(Server, -1, "Menu Test", [{size, {300, 250}}]),

    MenuDef = [ {"&File", ["&Open", "&Hello", "&Quit"]}, 
                {"&Edit", ["&Copy", "&Paste", separator,
                            %This is a submenu!
                            {"&Import", ["Import &mail", "Import &bookmarks", separator,
                                %This is a sub-submenu!
                                {"Import &widgets", ["&Flex widget", "&Static widget", separator, "&Imaginary widget"]}
                            ]}
                         ]
                },
                "&Help"
              ],

    _MenuBar = build_menubar(Frame, MenuDef),

    % Terminate the process loop when the window closes:
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_menu_selected),
    Frame.

%------------------------------------------------------------------
%------------------------------------------------------------------
% LIBRARY STUFF
%------------------------------------------------------------------
%------------------------------------------------------------------
-type menu_def() :: [menu_item_def()].
-type menu_item_def() :: separator
                        | string()
                        | {string(), [menu_item_def()]}.

-spec build_menubar(any(), menu_def()) -> any().
build_menubar(Frame, Def) ->
    MenuBar = wxMenuBar:new(),
    Menus = [new_menu(X) || X <- Def],
    [wxMenuBar:append(MenuBar, Menu, Title) || {Title, Menu} <- Menus],
    wxFrame:setMenuBar(Frame, MenuBar),
    MenuBar.

-spec new_menu(menu_item_def()) -> {string(), any()}.
new_menu({Title, Items}) when is_list(Items) ->
    Menu = wxMenu:new(),
    [new_menu_item(Menu, X) || X <- Items],
    {Title, Menu};
new_menu(Title) -> {Title, wxMenu:new()}.

-spec new_menu_item(any(), menu_item_def()) -> any().
new_menu_item(Menu, separator) -> wxMenu:appendSeparator(Menu);
new_menu_item(Menu, {Title, Items}) -> 
    % Build a nested submenu (which itself may also contain submenus)
    {Title, Submenu} = new_menu({Title, Items}),
    Id = 1111, % TODO IDs        
    wxMenu:append(Menu, Id, Title, Submenu); % TODO: wrap the wx type
new_menu_item(Menu, Title) -> 
    % Build a simple menu item.
    Id = 2222, % TODO IDs. If we set an ID to ?wxID_EXIT the window can be closed from the menu.
    %wxMenu:append(Menu, Id, Title).
    NewMenuItem = wxMenuItem:new([{id, Id}, {text, Title}]),
    Icon = get_bitmap("wxART_QUIT"),
    wxMenuItem:setBitmap(NewMenuItem, Icon),
    wxMenu:append(Menu, NewMenuItem). % TODO: wrap the wx type



-spec get_bitmap(string()) -> any(). %TODO: does wx.hrl export types?
get_bitmap(Name) -> wxArtProvider:getBitmap(Name, [{size, {16,16}}]).