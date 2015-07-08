-module(calculator_gui).
-behaviour(wx_object).

-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).
 
-include_lib("wx/include/wx.hrl").
 
-define(BUTTON_OPTIONS, [{proportion, 0}, {flag, ?wxEXPAND}]).
-define(SIZE, {300, 200}).

% Client API
%------------------------------------------------------------------
start() -> wx_object:start_link(?MODULE, [], []).

% Server Implementation (behaviour callbacks)
%------------------------------------------------------------------
init([]) ->
    Frame = make_window(),
    {Frame, Frame}.

% Terminate the process loop when the window closes.
handle_event(#wx{event=#wxClose{}}, State) -> {stop, normal, State};

handle_event(#wx{id=?wxID_EXIT}, State) -> 
    wxWindow:destroy(State),
    {noreply, State};
handle_event(#wx{id=ButtonId, event=#wxCommand{type = command_button_clicked}}, State) ->
    ButtonText = get_button(ButtonId),
    io:format("CLICK ~p~n", [State]),
    handle_button_click(ButtonText, State),
    {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_call(_Msg, _From, State) -> {reply, ok, State}.
code_change(_, _, State) -> {stop, ignore, State}.
terminate(_Reason, _State) -> ok.

% Internal Stuff
%------------------------------------------------------------------
handle_button_click("Clear", _State) ->
    clear_display();
handle_button_click("Del", _State) ->
    Display = get("wx_textctrl_90001"),
    Text = wxTextCtrl:getValue(Display),
    case length(Text) of
        0 -> nothing;
        Len -> wxTextCtrl:setValue(Display, string:left(Text, Len - 1))
    end;
handle_button_click("-", _State) ->
    Display = get("wx_textctrl_90001"),
    Text = wxTextCtrl:getValue(Display),
    X = calculator:list_to_numeric(Text),
    calculator:subtract(X),
    clear_display();
handle_button_click("+", _State) -> 
    Display = get("wx_textctrl_90001"),
    Text = wxTextCtrl:getValue(Display),
    X = calculator:list_to_numeric(Text),
    calculator:add(X),
    clear_display();
handle_button_click("=", _State) ->
    Display = get("wx_textctrl_90001"),
    Text = wxTextCtrl:getValue(Display),
    X = calculator:list_to_numeric(Text),
    Result = calculator:enter(X),
    io:format("RESULT ~p~n", [Result]),
    wxTextCtrl:setValue(Display, calculator:numeric_to_list(Result));
handle_button_click(Character, _State) -> % "0" to "9" and "."
    Display = get("wx_textctrl_90001"),
    wxTextCtrl:appendText(Display, Character).

clear_display() ->
    Display = get("wx_textctrl_90001"),
    wxTextCtrl:clear(Display).

make_window() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Test", [{size, ?SIZE}]),
    Panel = wxPanel:new(Frame),

    Sizer = wxBoxSizer:new(?wxVERTICAL), % for the whole window
    GS = wxGridSizer:new(5, 4, 3, 3), % for the buttons

    % The digit readout
    Display = wxTextCtrl:new(Panel, 90001, [{value, ""}, {style, ?wxTE_RIGHT}]),
    put("wx_textctrl_90001", Display), 
    wxSizer:add(Sizer, Display, [{flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM}]),
    wxSizer:addSpacer(Sizer, 10), %some padding below the readout

    % Top button row
    wxSizer:add(GS, wxButton:new(Panel, ?wxID_EXIT, [{label, "Quit"}]), ?BUTTON_OPTIONS),
    add_blank(GS, Panel),
    add_blank(GS, Panel),
    add_button(GS, Panel, "Clear"),

    % Second button row
    add_buttons(GS, Panel, ["7", "8", "9", "Del"]),

    % Third button row
    add_buttons(GS, Panel, ["4", "5", "6", "-"]),

    % Fourth button row
    add_buttons(GS, Panel, ["1", "2", "3", "+"]),

    % Bottom button row
    add_blank(GS, Panel),   
    add_buttons(GS, Panel, ["0", ".", "="]),

    % Put the assembled controls onto the panel
    wxSizer:add(Sizer, GS, [{flag, ?wxEXPAND}])
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:setMinSize(Sizer, ?SIZE),
    wxFrame:show(Frame),

    % Terminate the process loop when the window closes:
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Panel, command_button_clicked),
    Frame.

%------------------------------------------------------------------
%------------------------------------------------------------------
% LIBRARY STUFF
%------------------------------------------------------------------
%------------------------------------------------------------------

add_blank(Sizer, Panel) -> wxSizer:add(Sizer, wxStaticText:new(Panel, -1, ""), ?BUTTON_OPTIONS).
add_buttons(Sizer, Panel, Labels) -> [add_button(Sizer, Panel, Label) || Label <- Labels].
add_button(Sizer, Panel, Text) -> 
    Id = next_button_id(),
    Button = wxButton:new(Panel, Id, [{label, Text}]),
    save_button(Id, Text),
    wxSizer:add(Sizer, Button, ?BUTTON_OPTIONS).

save_button(Id, Text) -> 
    Key = button_key(Id),
    %io:format("K: ~p T: ~p~n", [Key, Text]),
    put(Key, Text).

get_button(Id) -> get(button_key(Id)).
button_key(Id) -> "wx_button_" ++ integer_to_list(Id).

next_button_id() -> next_button_id(get("button_identity")).
next_button_id(undefined) -> 
    put("button_identity", 1),
    1;
next_button_id(Id) -> 
    put("button_identity", Id+1),
    Id+1.