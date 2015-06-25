-module(w).

-compile([export_all]).

-define(SERVER, w_server).

% API for building wx GUIs.
%------------------------------------------------------------------

%TODO: add size and options.
new_frame(Title) -> new_frame(Title, []).
new_frame(Title, Options) -> wx_object:call(?SERVER, {new_frame, Title, Options}).

new_panel({frame, FrameId}) -> new_panel({frame, FrameId}, []).
new_panel({frame, FrameId}, Options) -> wx_object:call(?SERVER, {new_panel, FrameId, Options}).

show({frame, FrameId}) -> wx_object:call(?SERVER, {show, {frame, FrameId}}).

% Internal Helpers
%------------------------------------------------------------------