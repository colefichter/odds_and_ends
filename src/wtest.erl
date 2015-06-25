-module(wtest).

-behaviour(gen_gui).

%-export([start/0, build_frame/1]).
-compile([export_all]).

start() -> 
    io:format("wtest:start ~p~n", [self()]),
    %_S = w:start_gui(?MODULE).
    w:start(?MODULE),
    w:new_frame("TESTING!").

% gen_gui callbacks
%------------------------------------------------------------------
command_invoked(WhatArg) -> io:format("command_invoked, ~p~n", [WhatArg]).
button_clicked(WhatArg) -> io:format("button_clicked, ~p~n", [WhatArg]).
