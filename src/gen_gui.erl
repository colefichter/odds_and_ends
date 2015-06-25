-module (gen_gui).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> 
    [
        {command_invoked, 1}, %Menu & Toolbar clicks
        {button_clicked, 1}
        % TODO: replace build_frame with proxied CBs for menu, button, toolbar, etc.
    ].