-module(statusbar).
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
    Frame = wxFrame:new(Server, -1, "StatusBar Test", [{size, {300, 250}}]),

    wxFrame:createStatusBar(Frame),
    wxFrame:setStatusText(Frame, "The status bar is working!"),


    % Terminate the process loop when the window closes:
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, command_menu_selected),
    Frame.

%------------------------------------------------------------------
%------------------------------------------------------------------
% LIBRARY STUFF
%------------------------------------------------------------------
%------------------------------------------------------------------
