-module(draw).
-export([start/0]).
 
-include_lib("wx/include/wx.hrl").
 
start() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Points", [{size, {250, 150}}]),
    Panel = wxPanel:new(Frame),
    Pen = wxPen:new(),
    wxPen:setColour(Pen, ?wxRED),
 
    OnPaint = fun(_Evt, _Obj) ->
            Paint = wxPaintDC:new(Panel),
            wxDC:setPen(Paint, Pen), 
            %{A1,A2,A3} = now(),
            %random:seed(A1, A2, A3),
            drawpoints(Paint, 0),
            wxPaintDC:destroy(Paint)
   end,
 
    wxFrame:connect(Panel, paint, [{callback, OnPaint}]),
    wxFrame:connect(Panel, close_window),
 
    wxFrame:center(Frame),
    wxFrame:show(Frame).
 
drawpoints(_,10000) -> ok;
drawpoints(Paint,Count) ->
    X  = rand:uniform(250) - 1 , 
    Y  = rand:uniform(150) - 1,
    wxDC:drawPoint(Paint,{X,Y}),
    drawpoints(Paint, Count + 1).