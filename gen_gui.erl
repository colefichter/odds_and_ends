-module (gen_gui).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> 
    [
        {start, 0},
        {build_frame, 1}
        % TODO: replace build_frame with proxied CBs for menu, button, toolbar, etc.
    ].