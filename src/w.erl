-module(w).

-compile([export_all]).

-define(SERVER, w_server).

% API for building wx GUIs.
%------------------------------------------------------------------

% Return types for this API:
-type frame_handle() :: {frame, integer()}.
-type panel_handle() :: {panel, integer()}.
-type toolbar_button_handle() :: {button, integer(), string()}.

% Reusable types for common options:
-type opt_pos() :: {pos, {integer(), integer()}}.
-type opt_size() :: {size, {integer(), integer()}}.
-type opt_style() :: {style, integer()}.

% Types for the various Options list arguments.
-type frame_options() :: [frame_option()].
-type frame_option() :: opt_pos()
                     |  opt_size()
                     |  opt_style().

-type panel_options() :: [panel_option()].
-type panel_option() :: {winid, integer()} % TODO: do we need this option?
                     |  opt_pos()
                     |  opt_size()
                     |  opt_style().

-type toolbar_def() :: [toolbar_button_def()].
-type toolbar_button_def() :: {string(), string()}
                            | {string(), string(), string()}.


-spec new_frame(string()) -> frame_handle().
-spec new_frame(string(), frame_options()) -> frame_handle().
new_frame(Title) -> new_frame(Title, []).
new_frame(Title, Options) -> wx_object:call(?SERVER, {new_frame, Title, Options}).

-spec add_statusbar(frame_handle()) -> ok.
-spec add_statusbar(frame_handle(), string()) -> ok.
add_statusbar({frame, FrameId}) -> wx_object:call(?SERVER, {add_statusbar, FrameId}).
add_statusbar({frame, FrameId}, Text) -> 
    ok = wx_object:call(?SERVER, {add_statusbar, FrameId}),
    set_status({frame, FrameId}, Text).

-spec set_status(frame_handle(), string()) -> ok.
set_status({frame, FrameId}, Text) -> wx_object:call(?SERVER, {set_status, FrameId, Text}).

-spec add_panel(panel_handle()) -> panel_handle().
-spec add_panel(panel_handle(), panel_options()) -> panel_handle(). 
add_panel({frame, FrameId}) -> add_panel({frame, FrameId}, []).
add_panel({frame, FrameId}, Options) -> wx_object:call(?SERVER, {add_panel, FrameId, Options}).

-spec add_toolbar(frame_handle(), toolbar_def()) -> [toolbar_button_handle()].
-spec add_toolbar(frame_handle(), toolbar_def(), integer(), integer ()) -> [toolbar_button_handle()].
% TODO: pass in styles
add_toolbar({frame, FrameId}, Definition) -> wx_object:call(?SERVER, {add_toolbar, FrameId, Definition, 16, 16}).
add_toolbar({frame, FrameId}, Definition, W, H) -> wx_object:call(?SERVER, {add_toolbar, FrameId, Definition, W, H}).

-spec show(frame_handle()) -> ok.
show({frame, FrameId}) -> wx_object:call(?SERVER, {show, {frame, FrameId}}).