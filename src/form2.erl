-module(form2).

-export([start/0, init/0]).

start() -> spawn(?MODULE, init, []).

init() ->
    w_server:start(), %Do this in a supervision tree instead!

    Frame = w:new_frame("Whatchamacallit Form", [{size, {300, 250}}]),
    Panel = w:add_panel(Frame),
    HBox = w:new_row_sizer(),
    FlexGridSizer = w:new_flexgrid_sizer(3, 2, 9, 25),

    ControlDef = [
        {label, "Title"}, {textbox, ""}, % Just for fun, we'll use data binding (below).
        {label, "Author"}, {textbox, ""},
        {label, "Review"}, {textbox, "", [multiline]}
    ],
    Controls = w:build_controls(Panel, ControlDef),

    % TODO: how to send EXPAND flag?
    w:fill_flexgrid_sizer(FlexGridSizer, Controls),

    % Make the multiline Review TB (in the 3rd row) grow vertically when the window is resized.    
    w:expand_row(FlexGridSizer, 2), %NOTE: the index is zero-based!

    % Make the right col grow horizontally when the window is resized.
    w:expand_col(FlexGridSizer, 1), %NOTE: the index is zero-based!

    % Border ~= margin in css? Seems like it.    
    w:append_child(HBox, FlexGridSizer, [{border, 15}, {proportion, 1}, all, expand]),
    % TODO: change "border" to "margin", since that's what it is?

    w:set_sizer(Panel, HBox),

    % Experimental data binding.
    Values = ["War and Peace", "Tolstoy", "Seems like it will never end..."],
    w:bind_values_to_controls(Controls, Values), % Note: the library is ignoring the label controls!

    w:show(Frame),
    loop().


loop() ->
    receive Msg ->
        io:format("CALLBACK ~p~n", [Msg]),
        loop()
    end.