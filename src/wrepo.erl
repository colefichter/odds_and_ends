-module(wrepo).

% A simple repository that stores data in the process dictionary.

-export([set_wx_server/1, get_wx_server/0, create_identity/0, next_id/0, get_control/2, set_control/3]).

-define(WXSERVER, wx_server).
-define(IDENTITY, identity).

-record(env, {
        pid,
        controls = dict:new() % Should this be an orddict (or something else)?
}).

% Repository Interface:
%------------------------------------------------------------------
set_wx_server(Server) -> put(?WXSERVER, Server).
get_wx_server() -> get(?WXSERVER).

create_identity() -> put(?IDENTITY, 1).
next_id() -> 
    Id = get(?IDENTITY),
    put(?IDENTITY, Id+1).

get_control(Pid, ControlId) ->
    _Env = #env{controls=Controls} = get_env(Pid),
    dict:find(ControlId, Controls).

set_control(Pid, ControlId, Control) ->
    Env = #env{controls=Controls} = get_env(Pid),
    NewControls = dict:store(ControlId, Control, Controls),
    Env2 = Env#env{controls=NewControls},
    set_env(Pid, Env2).

% Internal helpers
%------------------------------------------------------------------
get_env(Pid) -> % Must always return an ENV... If it doesn't exist, return a new env.
    case get(Pid) of
        undefined -> create_env(Pid);
        Env -> Env
    end.
set_env(Pid, Env) -> put(Pid, Env).
create_env(Pid) -> #env{pid = Pid}.