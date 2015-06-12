 -module(dlg1).

-export([start/0]).
 
-include_lib("wx/include/wx.hrl").
 
start() ->
    Server = wx:new(),
    Frame = wxFrame:new(Server, -1, "Test", [{size, {300, 200}}]),
    Panel = wxPanel:new(Frame),

    Vbox = wxBoxSizer:new(?wxVERTICAL),
    HBox1 = wxBoxSizer:new(?wxHORIZONTAL),
    HBox2 = wxBoxSizer:new(?wxHORIZONTAL),

    Ok = wxButton:new(Panel, -1, [{label, "OK"}]),
    Cancel = wxButton:new(Panel, -1, [{label, "Cancel"}]),

    wxSizer:add(HBox1, wxPanel:new(Panel)),
    wxSizer:add(Vbox, HBox1, [{proportion, 1}, {flag, ?wxEXPAND}]),

    wxSizer:add(HBox2, Ok),
    wxSizer:add(HBox2, Cancel),

    wxSizer:add(Vbox, HBox2, [{flag, ?wxALIGN_RIGHT bor ?wxRIGHT bor ?wxBOTTOM}]),
    wxSizer:addSpacer(HBox2, 10), % Pad to the right of the Cancel button
    wxSizer:addSpacer(Vbox, 10), % Pad to the bottom of the buttons

    wxPanel:setSizer(Panel, Vbox),

    wxFrame:show(Frame).