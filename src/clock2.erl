-module(clock2).
-behaviour(wx_object).

-export([start/0, init/1, terminate/2,  code_change/3,
handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

-include_lib("wx/include/wx.hrl").

% There must be some strange padding that is affecting the placement...
-define(ORIGIN, {150, 125}). 

-define(HOUR_WIDTH, 5).
-define(MINUTE_WIDTH, 2).
-define(SECOND_WIDTH, 1).

-define(HOUR_LENGTH, 45).
-define(MINUTE_LENGTH, 80).
-define(SECOND_LENGTH, 95).
-define(RADIUS, 100).

-define(TIMEOUT, 1000).

-record(state, {frame, panel}).

% Client API
%--------------------------------------------------------------------------------
start() -> wx_object:start_link(?MODULE, [], []).

% Server API
%--------------------------------------------------------------------------------
init([]) -> 
    {Frame, Panel} = make_window(),
    {Frame, #state{ frame=Frame, panel=Panel}, ?TIMEOUT}.

% Terminate the process loop when the window closes:
handle_event(#wx{event=#wxClose{}}, State) -> {stop, normal, State}.

%% Callbacks handled as normal gen_server callbacks
handle_info(_Msg, State = #state{panel=Panel}) ->
    wxPanel:refresh(Panel),
    {noreply, State, ?TIMEOUT}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p\n",[Msg]),
    {reply, {error, nyi}, State}.
 
code_change(_, _, State) ->
    {stop, ignore, State}.
 
terminate(_Reason, _State) ->
    ok.

% Internal functions
%--------------------------------------------------------------------------------
make_window() ->
    Wx = wx:new(), % Start the server
    Frame = wxFrame:new(Wx, -1, "Clock", [{size, {300, 300}}]),
    Panel = wxPanel:new(Frame), 
    OnPaint = fun(_Event, _Object) ->
        Paint = wxPaintDC:new(Panel),
        wxDC:drawCircle(Paint, ?ORIGIN, ?RADIUS),
        {_, {H, M, S}} = erlang:localtime(),
        draw_hour_arm(Paint, H),
        draw_minute_arm(Paint, M),
        draw_second_arm(Paint, S),
        wxPaintDC:destroy(Paint)
    end,
    % Terminate the process loop when the window closes:
    wxFrame:connect(Frame, close_window), 
    connect(Panel, OnPaint),
    wxFrame:show(Frame),
    {Frame, Panel}.

connect(Panel, Callback) ->
    wxFrame:connect(Panel, paint, [{callback, Callback}]).

hour_to_radian(H) -> (2 * math:pi() * (H/12.0)).
minute_to_radian(M) -> (2 * math:pi() * (M/60.0)).

draw_hour_arm(Paint, H) when H > 12 -> 
    % erlang:localtime() gives the hour in 24-hour format.
    draw_hour_arm(Paint, H-12); 
draw_hour_arm(Paint, H) ->
    Radians = hour_to_radian(H),
    draw_arm(Paint, ?HOUR_LENGTH, Radians, ?wxBLACK, ?HOUR_WIDTH).

draw_minute_arm(Paint, M) ->
    Radians = minute_to_radian(M),
    draw_arm(Paint, ?MINUTE_LENGTH, Radians, ?wxBLACK, ?MINUTE_WIDTH).

draw_second_arm(Paint, S) ->
    Radians = minute_to_radian(S),
    draw_arm(Paint, ?SECOND_LENGTH, Radians, ?wxRED, ?SECOND_WIDTH).

draw_arm(Paint, Length, Radians, Color, Width) ->    
    {X, Y} = ?ORIGIN,
    % Compute the end point. It seems like X + cos(rad) and Y + sin(rad) would work, but this coordinate
    % system is not the same as a Cartesian plane. Doing that would create a clock that started at three
    % and rotated anti-clockwise. There are a few ways to fix this, but this transformation is concise:
    %  * subtracting in the Y component reverses the direction of rotation (from anti-CW to CW)
    %  * swapping sin and cos mirrors the whole thing about the origin, which essentially moves the
    %    starting location from 3 o'clock to 12 o'clock where we need it.
    EndPoint = {X + math:sin(Radians) * Length, Y - math:cos(Radians) * Length},
    draw_line(Paint, ?ORIGIN, EndPoint, Color, Width).

draw_line(Paint, From, To, Color, Width) ->
    Pen = wxPen:new(),
    wxPen:setColour(Pen, Color),
    wxPen:setWidth(Pen, Width),
    wxDC:setPen(Paint, Pen),    
    wxDC:drawLine(Paint, truncate(From), truncate(To)).

truncate({X, Y}) -> {trunc(X), trunc(Y)}.