-module(wrepo).

% A simple repository that stores data in the process dictionary.

-compile([export_all]).

-define(WXSERVER, wx_server).
-define(IDENTITY, identity).

-record(env, {
        pid,
        frames = [],
        panels = []
}).

set_wx_server(Server) -> put(?WXSERVER, Server).
get_wx_server() -> get(?WXSERVER).

create_identity() -> put(?IDENTITY, 1).
next_id() -> 
    Id = get(?IDENTITY),
    put(?IDENTITY, Id+1).

get_env(Pid) -> % Must always return an ENV... If it doesn't exist, return a new env.
    case get(Pid) of
        undefined -> new_env(Pid);
        Env -> Env
    end.
set_env(Pid, Env) -> put(Pid, Env).
new_env(Pid) -> #env{pid = Pid}.


get_frame(Pid, FrameId) ->
    _Env = #env{frames=Frames} = get_env(Pid),
    lists:keyfind(FrameId, 1, Frames). %TODO: not found?
store_frame(Pid, Frame) ->
    Env = #env{frames=Frames} = get_env(Pid),
    Frames2 = lists:delete(Frame, Frames),
    Env2 = Env#env{frames=[Frame|Frames2]},
    io:format("store_frame ~p -> ~p~n", [Env, Env2]),
    set_env(Pid, Env2).

get_panel(Pid, PanelId) ->
    _Env = #env{panels=Panels} = get_env(Pid),
    lists:keyfind(PanelId, 1, Panels). %TODO: not found?
store_panel(Pid, Panel) ->
    Env = #env{panels=Panels} = get_env(Pid),
    Panels2 = lists:delete(Panel, Panels),
    Env2 = Env#env{panels=[Panel|Panels2]},
    io:format("store_panel ~p -> ~p~n", [Env, Env2]),
    set_env(Pid, Env2).
