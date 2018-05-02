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
                    | {error, Reason :: term()}
                    | {error, Reason :: term(), NewState :: term()},
      Result       :: {ok, State :: term()}
                    | {error, Label, Reason :: term(), NewState :: term()}.
run([], State) ->
    {ok, State};
run([{Label, Func} | Rest], State) ->
    case Func(State) of
        {ok, NewState}            -> run(Rest, NewState);
        {error, Reason}           -> {error, Label, Reason, State};
        {error, Reason, NewState} -> {error, Label, Reason, NewState}
    end.