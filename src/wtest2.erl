-module(wtest2).

%-export([start/0, build_frame/1]).
-compile([export_all]).

start() -> spawn(?MODULE, init, []).

init() -> 
    io:format("wtest:start ~p~n", [self()]),
    w_server:start(),
    Frame = w:new_frame("TESTING!"),

    io:format("init frame ~p~n", [Frame]),

    Panel = w:new_panel(Frame),
    w:show(Frame),
    %wx_object:call(w_server, fake_click), % Simulate a fake click in the UI...
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
command_invoked(WhatArg) -> io:format("command_invoked, ~p~n", [WhatArg]).
button_clicked(WhatArg) -> io:format("button_clicked, ~p~n", [WhatArg]).