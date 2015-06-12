-module(calculator).
-behaviour(gen_fsm).

-export([start_link/0, stop/0]).
-export([enter/1, add/1, subtract/1, waiting/2]).
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

% Client API
%-------------------------------------------------------------------------


-export([run/0]).
run() ->
    start_link(),
    add(5),
    subtract(7),
    enter(3),
    stop().




start_link() -> gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

enter(Number) -> gen_fsm:send_event(?MODULE, {enter, Number}).
add(Number) -> gen_fsm:send_event(?MODULE, {add, Number}).
subtract(Number) -> gen_fsm:send_event(?MODULE, {subtract, Number}).

stop() -> gen_fsm:send_all_state_event(?MODULE, stop).


% FSM Implementation
%-------------------------------------------------------------------------
init([]) -> {ok, waiting, []}.

waiting({add, Number}, LoopData) -> {next_state, waiting, ['+'|[Number|LoopData]]};
waiting({subtract, Number}, LoopData) -> {next_state, waiting, ['-'|[Number|LoopData]]};
waiting({enter, Number}, LoopData) -> 
    Expression = [Number|LoopData],
    Result = evaluate(Expression),
    io:format("~p = ~p~n", [lists:reverse(Expression), Result]),
    {next_state, waiting, []}.

% Internal helpers
evaluate(Operations) -> evaluate_int(lists:reverse(Operations), 0).

evaluate_int([], Result) -> Result;
evaluate_int([H|T], _Result) when is_integer(H) -> evaluate_int(T, H);
evaluate_int(['+'|T], Result) -> 
    [Next|T2] = T,
    io:format("~p + ~p~n", [Result, Next]),
    evaluate_int(T2, Result+Next);
evaluate_int(['-'|T], Result) -> 
    [Next|T2] = T,
    io:format("~p - ~p~n", [Result, Next]),
    evaluate_int(T2, Result-Next).

%---------------------------------------------------------------------------------------------
% Unused callbacks required by gen_fsm
%---------------------------------------------------------------------------------------------
%handle_sync_event(stop, _From, StateName, StateData) -> {stop, normal, StateData};
handle_sync_event(_Any, _From, StateName, StateData) -> {ok, StateName, StateData}.
handle_info(_Any, StateName, StateData) -> {ok, StateName, StateData}.
handle_event(stop, _StateName, LoopData) -> {stop, normal, LoopData};
handle_event(_Any, StateName, StateData) -> {ok, StateName, StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.
terminate(_Any, StateName, StateData) -> {ok, StateName, StateData}.

% Unit tests
%-------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

evaluate_test() ->
    calculator:start_link(),
    ?assertEqual(5, evaluate([2, '+', 3])),
    ?assertEqual(9, evaluate([4, '+', 2, '+', 3])),
    ?assertEqual(1, evaluate([4, '-', 2, '+', 3])),
    ?assertEqual(5, evaluate([4, '+', 2, '-', 3])),
    ?assertEqual(-15, evaluate([20, '-', 4, '+', 2, '-', 3])),
    calculator:stop().