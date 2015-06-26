-module(w).

-compile([export_all]).

-define(SERVER, w_server).

% API for building wx GUIs.
%------------------------------------------------------------------

%TODO: add size and options.
new_frame(Title) -> new_frame(Title, []).
new_frame(Title, Options) -> wx_object:call(?SERVER, {new_frame, Title, Options}).

add_statusbar({frame, FrameId}) -> wx_object:call(?SERVER, {add_statusbar, FrameId}).
add_statusbar({frame, FrameId}, Text) -> 
    ok = wx_object:call(?SERVER, {add_statusbar, FrameId}),
    set_status({frame, FrameId}, Text).

set_status({frame, FrameId}, Text) -> wx_object:call(?SERVER, {set_status, FrameId, Text}).

% TODO: name this "add_panel"
new_panel({frame, FrameId}) -> new_panel({frame, FrameId}, []).
new_panel({frame, FrameId}, Options) -> wx_object:call(?SERVER, {new_panel, FrameId, Options}).

show({frame, FrameId}) -> wx_object:call(?SERVER, {show, {frame, FrameId}}).

% Internal Helpers
%------------------------------------------------------------------