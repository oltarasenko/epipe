-module(epipe_tests).

-include_lib("eunit/include/eunit.hrl").


pipe_test_() ->
    [
        {"empty pipe", fun() -> 
            ?assertEqual({ok, initial_state}, epipe:run([], initial_state)) 
        end},
        {"one function which produces ok result", fun() ->
            Fun = fun(state) -> {ok, new_state} end,
            ?assertEqual(
                {ok, new_state},
                epipe:run([{function_label, Fun}], state)
            )
        end},
        {"one function which produces error, does not change state", fun() ->
            Fun = fun(state) -> {error, error_reason} end,
            ?assertEqual(
                {error, function_label, error_reason, state},
                epipe:run([{function_label, Fun}], state))
        end},
        {"one function which produces error, changes state", fun() ->
            Fun = fun(state) -> {error, error_reason, new_state} end,
            ?assertEqual(
                {error, function_label, error_reason, new_state}, 
                epipe:run([{function_label, Fun}], state)
            )
        end},
        {"Error interrupts pipeline evaluation", fun() ->
            FunOk = fun(Val) -> {ok, Val + 1} end,
            FunOk1 = fun(Val) -> {ok, Val + 1} end,
            FunError = fun(Val) -> {error, reason} end,

            FuncList = [{fun_ok, FunOk}, {fun_error, FunError}, {fun_ok2, FunOk1}],
            ?assertEqual(
                {error, fun_error, reason, 1},
                epipe:run(FuncList, 0)
            )
        end},
        {"all functions in pipe are evaluated", fun() ->
            Fun1 = fun(Val) -> {ok, Val + 1} end,
            Fun2 = fun(Val) -> {ok, Val + 1} end,
            ?assertEqual(
                {ok, 2},
                epipe:run([{f1, Fun1}, {f2, Fun2}], 0)
            )
        end},
        {"Example 1", fun() ->
            Fun1 = fun(Val) -> {ok, Val + 1} end,
            Fun2 = fun(Val) -> {ok, Val + 2} end,
            ?assertEqual(
                {ok, 3},
                epipe:run([{add_one, Fun1}, {add_two, Fun2}], 0)
            )
        end},
        {"Example 2", fun() ->
            Fun1 = fun(Val) -> {ok, Val + 1} end,
            Fun2 = fun(_Val) -> {error, "Can't process data"} end,

            ?assertEqual(
                {error, step2, "Can't process data", 1},
                epipe:run([{step1, Fun1}, {step2, Fun2}, {step3, Fun1}], 0)
            )
        end},
        {"Function raises an error", fun() ->
            Fun1 = fun(Val) -> {ok, Val + 1} end,
            Fun2 = fun(_Val) -> erlang:error("Bad CRC")  end,

            ?assertEqual(
                {error, step2, "Can't process data", 1},
                epipe:run([{step1, Fun1}, {step2, Fun2}, {step3, Fun1}], 0)
            )
        end},
        {"Function throws an error", fun() ->
            Fun1 = fun(Val) -> {ok, Val + 1} end,
            Fun2 = fun(_Val) -> throw("Terrible CRC")  end,

            ?assertEqual(
                {error, step2, "Can't process data", 1},
                epipe:run([{step1, Fun1}, {step2, Fun2}, {step3, Fun1}], 0)
            )
        end}
    ].
