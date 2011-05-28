eep0018
=======

Now available as a NIF.

Compiling
---------

    $ make

Assuming rebar works for you that should build everything. Yay.

Testing
-------

    $ make check

Hopefully the tests pass.

Usage
-----

Put this app in your Erlang path.

    $ erl -pa ebin/
    Erlang R13B04 (erts-5.7.5) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

    Eshell V5.7.5  (abort with ^G)
    1> json:decode(<<"{\"foo\": true}">>).
    {ok,{[{<<"foo">>,true}]}}
    2> json:encode([true, 1.2, null]).
    {ok,<<"[true,1.2,null]">>}

Yeah. It's that easy.
