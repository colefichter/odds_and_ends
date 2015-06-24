-module(wtest).

-behaviour(gen_gui).

-export([start/0, build_frame/1]).

start() -> _S = w:start_gui(?MODULE).

build_frame(Arg) -> io:format("HANDLER ~p ~n", [Arg]).