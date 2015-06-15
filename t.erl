-module(t).
-include_lib("wx/include/wx.hrl").

-export([start/0]).
-compile(export_all).

-behavoiur(wx_object).

-record(state, {win}).

start() ->
    wx_object:start_link(?MODULE, [], []).

%% Init is called in the new process.
init([]) ->
    wx:new(),
    Frame = wxFrame:new(wx:null(), 
            -1, % window id
            "Hello World", % window title
            [{size, {600,400}}]),
    
    

    %% if we don't handle this ourselves, wxwidgets will close the window
    %% when the user clicks the frame's close button, but the event loop still runs
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, paint, [callback]),

    % Panel = wxPanel:new(Frame, []),
    % wxPanel:connect(Panel, paint, [callback]),
    
    wxWindow:show(Frame),
    {Frame, #state{win=Frame}}. %, panel=Panel}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Sync events i.e. from callbacks must return ok, it can not return a new state.
%% Do the redrawing here.
handle_sync_event(#wx{event = #wxPaint{}}, _, #state{win=Panel}) ->
    %#state{win=Panel}) ->
    io:format("SYNC"),
    invoke_draw(Panel),    
    ok.

invoke_draw(Panel) ->
    Pen = ?wxBLACK_PEN,
    Brush = wxBrush:new({30, 175, 23, 127}),
    Font = ?wxITALIC_FONT,
    %% PaintDC must be created in a callback to work on windows.
    DC = wxPaintDC:new(Panel),
    %% Nothing is drawn until wxPaintDC is destroyed.
    draw(DC, Pen, Brush, Font),
    wxPaintDC:destroy(DC).

draw(Win, Pen, Brush, Font) ->
    try
    Canvas = wxGraphicsContext:create(Win),
    wxGraphicsContext:setPen(Canvas, Pen),
    wxGraphicsContext:setBrush(Canvas, Brush),
    wxGraphicsContext:setFont(Canvas, Font, {0, 0, 50}),
    
    wxGraphicsContext:drawRoundedRectangle(Canvas, 35.0,35.0, 100.0, 50.0, 10.0),
    wxGraphicsContext:drawText(Canvas, "This text should be antialised", 60.0, 55.0),
    Path = wxGraphicsContext:createPath(Canvas),
    wxGraphicsPath:addCircle(Path, 0.0, 0.0, 40.0),
    wxGraphicsPath:closeSubpath(Path),
    wxGraphicsContext:translate(Canvas, 100.0, 250.0),
    
    F = fun(N) ->
            wxGraphicsContext:scale(Canvas, 1.1, 1.1),
            wxGraphicsContext:translate(Canvas, 15.0,-1.0*N),
            wxGraphicsContext:drawPath(Canvas, Path)
        end,
    wx:foreach(F, lists:seq(1,10)),
    ok
    catch _:{not_supported, _} ->
        Err = "wxGraphicsContext not available in this build of wxwidgets",
        io:format(Err,[])
    end.


%% Handled as in normal gen_server callbacks
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.

%% Async Events are handled in handle_event as in handle_info
handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    wxWindow:destroy(Frame),
    {stop, normal, State}.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    ok.