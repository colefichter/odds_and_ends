-module(w).

-compile([export_all]).

-define(SERVER, w_server).

-include_lib("wx/include/wx.hrl").

% API for building wx GUIs.
%------------------------------------------------------------------

% Return types for this API:
-type frame_handle() :: {frame, integer()}.
-type panel_handle() :: {panel, integer()}.
-type toolbar_button_handle() :: {button, integer(), string()}.
-type sizer_handle() :: {box_sizer, integer()}.
-type grid_sizer_handle() :: {grid_sizer, integer()}.
-type textbox_handle() :: {textbox, integer()}.

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

-type textbox_options() :: [textbox_option()].
-type textbox_option() :: opt_pos()
                        | opt_size().
                            %TODO: style and validators! http://www.erlang.org/doc/man/wxTextCtrl.html#new-3

-type toolbar_def() :: [toolbar_button_def()].
-type toolbar_button_def() :: {string(), string()}
                            | {string(), string(), string()}.

% Frame
%------------------------------------------------------------------
-spec new_frame(string()) -> frame_handle().
-spec new_frame(string(), frame_options()) -> frame_handle().
new_frame(Title) -> new_frame(Title, []).
new_frame(Title, Options) -> wx_object:call(?SERVER, {new_frame, Title, Options}).

-spec show(frame_handle()) -> ok.
show({frame, FrameId}) -> wx_object:call(?SERVER, {show, {frame, FrameId}}).

% Panel
%------------------------------------------------------------------
-spec add_panel(panel_handle()) -> panel_handle().
-spec add_panel(panel_handle(), panel_options()) -> panel_handle(). 
add_panel({frame, FrameId}) -> add_panel({frame, FrameId}, []).
add_panel({frame, FrameId}, Options) -> wx_object:call(?SERVER, {add_panel, FrameId, Options}).

% Statusbar
%------------------------------------------------------------------
-spec add_statusbar(frame_handle()) -> ok.
-spec add_statusbar(frame_handle(), string()) -> ok.
add_statusbar({frame, FrameId}) -> wx_object:call(?SERVER, {add_statusbar, FrameId}).
add_statusbar({frame, FrameId}, Text) -> 
    ok = wx_object:call(?SERVER, {add_statusbar, FrameId}),
    set_status({frame, FrameId}, Text).

-spec set_status(frame_handle(), string()) -> ok.
set_status({frame, FrameId}, Text) -> wx_object:call(?SERVER, {set_status, FrameId, Text}).

% Toolbar
%------------------------------------------------------------------
-spec add_toolbar(frame_handle(), toolbar_def()) -> [toolbar_button_handle()].
-spec add_toolbar(frame_handle(), toolbar_def(), integer(), integer ()) -> [toolbar_button_handle()].
% TODO: pass in styles
add_toolbar({frame, FrameId}, Definition) -> wx_object:call(?SERVER, {add_toolbar, FrameId, Definition, 16, 16}).
add_toolbar({frame, FrameId}, Definition, W, H) -> wx_object:call(?SERVER, {add_toolbar, FrameId, Definition, W, H}).

% Boxsizer
%------------------------------------------------------------------
% Create a new verticle box sizer.
-spec new_column_sizer() -> ok.
new_column_sizer() -> wx_object:call(?SERVER, {new_box_sizer, ?wxVERTICAL}).

% Create a new horizontal box sizer.
-spec new_row_sizer() -> sizer_handle().
new_row_sizer() -> wx_object:call(?SERVER, {new_box_sizer, ?wxHORIZONTAL}).

% Gridsizer
%------------------------------------------------------------------
%TODO: add an overload that uses a default padding value?
-spec new_grid_sizer(integer(), integer(), integer(), integer()) -> grid_sizer_handle().
new_grid_sizer(Rows, Columns, VerticlePadding, HorizontalPadding) ->
    wx_object:call(?SERVER, {new_grid_sizer, Rows, Columns, VerticlePadding, HorizontalPadding}).

% Textbox constructors
%------------------------------------------------------------------
-spec new_textbox(panel_handle()) -> textbox_handle().
new_textbox(PanelHandle) -> new_textbox(PanelHandle, "", []).

-spec new_textbox(panel_handle(), string()) -> textbox_handle().
new_textbox(PanelHandle, Text) -> new_textbox(PanelHandle, Text, []).

%TODO: style and validators! http://www.erlang.org/doc/man/wxTextCtrl.html#new-3
-spec new_textbox(panel_handle(), string(), textbox_options()) -> textbox_handle().
new_textbox({panel, PanelId}, Text, Options) ->
    Options2 = [{value, Text}|Options],
    wx_object:call(?SERVER, {new_textbox, PanelId, Options2}).

% Textbox manipulation functions
%------------------------------------------------------------------
-spec append_text(textbox_handle(), string()) -> ok.
append_text({textbox, TextboxId}, Text) -> wx_object:call(?SERVER, {append_text, TextboxId, Text}).

-spec get_text(textbox_handle()) -> string().
get_text({textbox, TextboxId}) -> wx_object:call(?SERVER, {get_text, TextboxId}).

-spec set_text(textbox_handle(), string()) -> ok.
set_text({textbox, TextboxId}, Text) -> wx_object:call(?SERVER, {set_text, TextboxId, Text}).

-spec clear(textbox_handle()) -> ok.
clear({textbox, TextboxId}) -> wx_object:call(?SERVER, {clear, TextboxId}).

% Helpful utils to make WX easier to work with
%------------------------------------------------------------------

% Given a proplist, convert known styles into a single aggregated WX style tuple
% and return a new list containing that WX style tuple and all other non-style
% tuples from the original list. See unit tests below.
to_wx_style(Items) when is_list(Items) -> 
    Items2 = lists:map(fun to_wx_style/1, Items),
    {WxStyle, {others, UnmodifiedTuples}} = extract_style_codes(Items2),
    [WxStyle|UnmodifiedTuples];

to_wx_style({align, left})      -> ?wxTE_LEFT;
to_wx_style({align, center})    -> ?wxTE_CENTER;
to_wx_style({align, right})     -> ?wxTE_RIGHT;
to_wx_style(Unknown)            -> Unknown.

extract_style_codes(Items) -> extract_style_codes(Items, 0, []).
extract_style_codes([], StyleCode, Tuples) ->
    {{style, StyleCode}, {others, Tuples}};
extract_style_codes([H|T], StyleCode, Tuples) when is_integer(H) ->
    extract_style_codes(T, StyleCode bor H, Tuples);
extract_style_codes([H|T], StyleCode, Tuples) ->
    extract_style_codes(T, StyleCode, [H|Tuples]).

%------------------------------------------------------------------
% Unit tests: Move these into a separate module
%------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

to_wx_style_test() ->
    Input = [{value, "Cole"}, {align, right}, {align, center}, {size, {1,2}}, {pos, {3,4}}],
    Output = to_wx_style(Input),
    ?assertEqual(4, length(Output)),
    ?assertEqual({value, "Cole"}, proplists:lookup(value, Output)),
    ?assertEqual({size, {1,2}}, proplists:lookup(size, Output)),
    ?assertEqual({pos, {3,4}}, proplists:lookup(pos, Output)),
    ?assertEqual({style, ?wxTE_RIGHT bor ?wxTE_CENTER}, proplists:lookup(style, Output)).