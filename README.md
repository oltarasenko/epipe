epipe 
=====

[![Build Status](https://travis-ci.org/oltarasenko/epipe.svg?branch=master)](https://travis-ci.org/oltarasenko/epipe)

Erlang pipes

Influenced by Elixir pipe (|>) and with operators. Brings similar functionality to erlang


Intro
-----

Probably you've seen code like this:

``` erlang
case gen_tcp:connect(Host, Port, Otps) of
    {ok, Socket} -> 
        case do_handshake(Socket) of
            ok -> send_message(Socket);
            {error, Reason} -> exit(normal)
        end;
    {error, Reason} -> exit(normal)
end

```

Sometimes the amount of the nested cases goes even deeper.... Which makes the code quite
complex for understanding and debugging.

Epipe allows to rewrite it in a very flat way:

``` erlang
connect({Host, Port, Opts}) -> 
    case gen_tcp:connect(Host, Port, Otps) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> {error, Reason}
    end.

handshake(Socket) -> 
    case do_handshake(Socket) of
        ok -> {ok, Socket};
        {error, Reason} -> {error, Reason}
    end.

send_message(Socket) ->
    case do_send_message(Socket) of
        response -> {ok, response};
        Error -> {error, Error}
    end.

FunctionsList = [
    {connect_fun, fun connect/1}, 
    {handshake_fun, fun handshake/1}, 
    {send_message_fun, fun send_message/1}
],
epipe:run(FunctionsList, {Host, Port, Opts}).

```

Which allows to 'pipe' the initial data through FunctionsList, stopping on any step
which would produce {error, ...} value.

Examples
--------------
```
sample1() -> 
    Fun1 = fun(Val) -> {ok, Val + 1} end,
    Fun2 = fun(Val) -> {ok, Val + 2} end,
    epipe:run([{add_one, Fun1}, {add_two, Fun2}], 0).
```

Would produce {ok, 3}

```
sample2() -> 
    Fun1 = fun(Val) -> {ok, Val + 1} end,
    Fun2 = fun(Val) -> {error, "Can't process data"} end,
    epipe:run([{step1, Fun1}, {step2, Fun2}, {step3, Fun1}], 0).
```


Would produce {error, step2, "Can't process data", 1}, giving not only error reason,
but also would give a hint, about the failing step. 


Build
-----

    $ rebar3 compile
