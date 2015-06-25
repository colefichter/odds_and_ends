-module(w).

-compile([export_all]).

-define(SERVER, w_server).

% API for building wx GUIs.
%------------------------------------------------------------------
start() -> w_server:start().

%TODO: add size and options.
new_frame(Title) -> wx_object:call(?SERVER, {window, Title}).


click() -> wx_object:call(?SERVER, fake_click).

% Internal Helpers
%------------------------------------------------------------------