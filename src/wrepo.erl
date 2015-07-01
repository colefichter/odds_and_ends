-module(wrepo).

% A simple repository that stores data in the process dictionary.

-export([set_wx_server/1, get_wx_server/0, create_identity/0, next_id/0, get_control/2, set_control/3]).

-define(WXSERVER, wx_server).
-define(IDENTITY, identity).

% -record(env, {
%         pid,
%         % frames = [],
%         % panels = []
%         controls = dict:new() % Should this be an orddict (or something else)?
% }).

set_wx_server(Server) -> 
    put(?WXSERVER, Server),
    ok.
get_wx_server() -> get(?WXSERVER).

create_identity() -> 
    put(?IDENTITY, 1),
    ok.
next_id() -> 
    Id = get(?IDENTITY),
    put(?IDENTITY, Id+1).

-define(ENV, environment).
get_env() -> % Must always return an ENV... If it doesn't exist, return a new env.
    case get(?ENV) of
        undefined -> new_env();
        Env -> Env
    end.
set_env(Env) -> put(?ENV, Env).
new_env() -> dict:new().

get_control(ControlId) ->
    Env = get_env(),
    % Should return something like: {ok, {ControlId, Pid, Control}}
    dict:find(ControlId, Env).

set_control(ControlId, OwnerPid, Control) ->
    Env = get_env(),
    NewEnv = dict:store(ControlId, {ControlId, OwnerPid, Control}, Env),
    set_env(NewEnv),
    ok.

%------------------------------------------------------------------
% Unit Tests
%------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

set_get_control_test() ->
    ?assertEqual(error, get_control(5)),
    Control = {my, control, is, here},
    ?assertEqual(ok, set_control(5, self(), Control)),
    ?assertEqual({ok, {5, self(), Control}}, get_control(5)).

set_get_wx_server_test() ->
    ?assertEqual(ok, set_wx_server(blah)),
    ?assertEqual(blah, get_wx_server()).

identity_test() ->
    ?assertError(badarith, next_id()), % ID not created yet.
    ?assertEqual(ok, create_identity()),
    ?assertEqual(1, next_id()),
    ?assertEqual(2, next_id()),
    ?assertEqual(3, next_id()).