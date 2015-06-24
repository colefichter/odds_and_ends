-module(w).

-compile([export_all]).

-define(SERVER, w_server).


% API for building wx GUIs.
%------------------------------------------------------------------
start_gui(CallbackModule) -> w_server:start(CallbackModule).

build_frame() -> wx_object:call(?SERVER, build_frame).


% Internal Helpers
%------------------------------------------------------------------