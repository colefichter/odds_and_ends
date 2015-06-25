-module(form).
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
    Frame = wxFrame:new(Server, -1, "Form Test", [{size, {300, 250}}]),
    Panel = wxPanel:new(Frame),

    HBox = wxBoxSizer:new(?wxHORIZONTAL), %HBox adds some padding around the FGS table.
    FlexGridSizer = wxFlexGridSizer:new(3, 2, 9, 25),

    Title = wxStaticText:new(Panel, 1000, "Title"),
    Author = wxStaticText:new(Panel, 1001, "Author"),
    Review = wxStaticText:new(Panel, 1002, "Review"),

    TbTitle = wxTextCtrl:new(Panel, 2000),
    TbAuthor = wxTextCtrl:new(Panel, 2001),
    TbReview = wxTextCtrl:new(Panel, 2002, [{style, ?wxTE_MULTILINE}]),

    wxSizer:add(FlexGridSizer, Title),
    wxSizer:add(FlexGridSizer, TbTitle, [{flag, ?wxEXPAND}]), %The tutorial has 1 as param 3... why?

    wxSizer:add(FlexGridSizer, Author),
    wxSizer:add(FlexGridSizer, TbAuthor, [{flag, ?wxEXPAND}]), %The tutorial has 1 as param 3... why?

    wxSizer:add(FlexGridSizer, Review),
    wxSizer:add(FlexGridSizer, TbReview, [{flag, ?wxEXPAND}]), %The tutorial has 1 as param 3... why?


    % Make the multiline Review TB (in the 3rd row) grow vertically when the window is resized.    
    wxFlexGridSizer:addGrowableRow(FlexGridSizer, 2, [{proportion, 1}]),
    % Make the right col grow horizontally when the window is resized.
    wxFlexGridSizer:addGrowableCol(FlexGridSizer, 1, [{proportion, 1}]),
    %NOTE: for the lines above, the indices are zero-based!


    % Border ~= margin in css? Seems like it.
    wxSizer:add(HBox, FlexGridSizer, [{border, 15}, {proportion, 1}, {flag, ?wxALL bor ?wxEXPAND}]),

    wxPanel:setSizer(Panel, HBox),

    %Playing with databinding:
    % wxTextCtrl:setValue(TbTitle, "War and Peace"),
    % wxTextCtrl:setValue(TbAuthor, "Tolstoy"),
    % wxTextCtrl:setValue(TbReview, "Seems like it will never end..."),
    Controls = [TbTitle, TbAuthor, TbReview],
    Values = ["War and Peace", "Tolstoy", "Seems like it will never end..."],
    bind_all(Controls, Values),



    % Terminate the process loop when the window closes:
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_menu_selected),
    Frame.

%------------------------------------------------------------------
%------------------------------------------------------------------
% LIBRARY STUFF
%------------------------------------------------------------------
%------------------------------------------------------------------

bind_all([], []) -> ok;
bind_all(L1 = [Tb|OtherTbs], L2 = [Val|OtherVals]) when length(L1) == length(L2) ->
    bind(Tb, Val),
    bind_all(OtherTbs, OtherVals).

bind(Textbox, Value) -> wxTextCtrl:setValue(Textbox, Value).