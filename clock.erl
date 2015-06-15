-module(clock).

-export([start/0]).

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

start() ->
    State = make_window(),
    loop(State).

loop(State = {Frame, Panel}) ->
    receive
         #wx{event=#wxClose{}} -> % Terminate the process loop when the window closes.
            wxWindow:destroy(Frame)
    after 1000 -> % Animate the clock
        wxPanel:refresh(Panel),
        loop(State)
    end,
    ok.

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
    wxFrame:connect(Frame, close_window), % Terminate the process loop when the window closes.
    connect(Panel, OnPaint),
    wxFrame:show(Frame),
    {Frame, Panel}.

connect(Panel, Callback) ->
    wxFrame:connect(Panel, paint, [{callback, Callback}]).

hour_to_radian(H) -> (2 * math:pi() * (H/12.0)) - math:pi() / 2.
minute_to_radian(M) -> (2 * math:pi() * (M/60.0)) - math:pi() / 2.

draw_hour_arm(Paint, H) when H > 12 -> draw_hour_arm(Paint, H-12); % erlang:localtime() gives the hour in 24-hour format.
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
    % Compute the end point
    EndPoint = {X + math:cos(Radians) * Length, Y + math:sin(Radians) * Length},
    draw_line(Paint, ?ORIGIN, EndPoint, Color, Width).

draw_line(Paint, From, To, Color, Width) ->
    Pen = wxPen:new(),
    wxPen:setColour(Pen, Color),
    wxPen:setWidth(Pen, Width),
    wxDC:setPen(Paint, Pen),    
    wxDC:drawLine(Paint, truncate(From), truncate(To)).

truncate({X, Y}) -> {trunc(X), trunc(Y)}.