-module(epipe).

%% API exports
-export([run/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec run(FuncList, InitialState) -> Result when
      FuncList     :: [{Label, PipeFun}],
      Label        :: term(),
      InitialState :: term(),
      PipeFun      :: fun((SomeState :: term()) -> PipeFunRes),
      PipeFunRes   :: {ok, NextState :: term()}
                    | {stop, State :: term()}
                    | {error, Reason :: term()}
                    | {error, Reason :: term(), NewState :: term()},
      Result       :: {ok, State :: term()}
                    | {ok, Label, State :: term()}
                    | {stop, Label, State::term()}
                    | {error, Label, Reason :: term(), NewState :: term()}.

run([], State) ->
    {ok, State};
run(FuncList, State) when erlang:is_tuple(State) ->
    run(FuncList, erlang:tuple_to_list(State));
run([{Label, Func} | Rest], State) ->
    case erlang:apply(Func, State) of
        ok                        -> {ok, Label, State};
        {ok, NewState}            -> run(Rest, NewState);
        {stop, NewState}          -> {stop, Label, NewState};
        {error, Reason}           -> {error, Label, Reason, State};
        {error, Reason, NewState} -> {error, Label, Reason, NewState}
    end.