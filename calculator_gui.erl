 -module(calculator_gui).

-export([start/0]).
 
-include_lib("wx/include/wx.hrl").
 
-define(BUTTON_OPTIONS, [{proportion, 0}, {flag, ?wxEXPAND}]).
-define(SIZE, {200, 200}).

start() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Test", [{size, ?SIZE}]),
    %Frame = wxFrame:new(Server, -1, "Test", []),
    Panel = wxPanel:new(Frame),

    Sizer = wxBoxSizer:new(?wxVERTICAL), % for the whole window
    GS = wxGridSizer:new(5, 4, 3, 3), % for the buttons

    % The digit readout
    Display = wxTextCtrl:new(Panel, -1, [{value, "Type here..."}, {style, ?wxTE_RIGHT}]), 
    wxSizer:add(Sizer, Display, [{flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM}]),
    wxSizer:addSpacer(Sizer, 10), %some padding below the readout

    % Top button row
    add_buttons(GS, Panel, ["Cls", "Bck"]),
    wxSizer:add(GS, wxStaticText:new(Panel, -1, ""), ?BUTTON_OPTIONS),
    add_button(GS, Panel, "Close"),

    % Second button row
    add_buttons(GS, Panel, ["7", "8", "9"]),
    wxSizer:add(GS, wxStaticText:new(Panel, -1, ""), ?BUTTON_OPTIONS),

    % Third button row
    add_buttons(GS, Panel, ["4", "5", "6", "-"]),

    % Fourth button row
    [add_button(GS, Panel, Label) || Label <- ["1", "2", "3", "+"]],

    % Bottom button row
    wxSizer:add(GS, wxStaticText:new(Panel, -1, ""), ?BUTTON_OPTIONS),    
    add_buttons(GS, Panel, ["0", ".", "="]),

    % Put the assembled controls onto the panel
    wxSizer:add(Sizer, GS, [{flag, ?wxEXPAND}]),
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:setMinSize(Sizer, ?SIZE),
    wxFrame:show(Frame).

add_buttons(Sizer, Panel, Labels) -> [add_button(Sizer, Panel, Label) || Label <- Labels].
add_button(Sizer, Panel, Text) -> wxSizer:add(Sizer, wxButton:new(Panel, -1, [{label, Text}]), ?BUTTON_OPTIONS).