-module(wtest2).

%-export([start/0, build_frame/1]).
-compile([export_all]).

start() -> spawn(?MODULE, init, []).

init() -> 
    w_server:start(), %Do this in a supervision tree instead!
    Frame = w:new_frame("TESTING!", [{size, {200, 200}}]),
    w:add_statusbar(Frame, "Statusbar text set quickly!"),
    Panel = w:new_panel(Frame),
    w:show(Frame),
    loop().

loop() ->
    receive
        stop -> ok;
        Message ->
            io:format("CALLBACK INVOKED ~p: ~p~n", [self(), Message]),
            loop()
    end.

% gen_gui callbacks
%------------------------------------------------------------------
% command_invoked(WhatArg) -> io:format("command_invoked, ~p~n", [WhatArg]).
% button_clicked(WhatArg) -> io:format("button_clicked, ~p~n", [WhatArg]).