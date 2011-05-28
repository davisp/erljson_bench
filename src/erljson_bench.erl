-module(erljson_bench).

-export([main/1]).

workers() -> 10.
iters() -> 1000.

%% Just make sure we have everythign on the
%% code path.
smoke() ->
    lists:map(fun(Module) ->
        {ok, true} = Module:decode(<<"true">>),
        {ok, <<"true">>} = Module:encode(true)
    end, [jiffy, ejson_test, mochijson2]).


load_doc(DocName) ->
    {ok, [Doc]} = file:consult("data/" ++ DocName),
    Doc.

load_json(DocName) ->
    {ok, Json} = mochijson2:encode(load_doc(DocName)),
    iolist_to_binary(Json).


test_encode(Workers, Iters, Module, Doc) ->
    Self = self(),
    Fun = fun() -> run_encode(Self, Iters, Module, Doc, 0) end,
    [spawn(Fun) || _ <- lists:seq(1, Workers)],
    Total = collect_times(Workers, 0),
    io:format("encode: ~15s: ~8b~n", [Module, Total]).

run_encode(Dst, 0, _, _, Total) ->
    Dst ! {time, Total};
run_encode(Dst, Iters, Module, Doc, Total) ->
    {Time, {ok, _}} = timer:tc(Module, encode, [Doc]),
    run_encode(Dst, Iters-1, Module, Doc, Total+Time).


test_decode(Workers, Iters, Module, Json) ->
    Self = self(),
    Fun = fun() -> run_decode(Self, Iters, Module, Json, 0) end,
    [spawn(Fun) || _ <- lists:seq(1, Workers)],
    Total = collect_times(Workers, 0),
    io:format("decode: ~15s: ~8b~n", [Module, Total]).

run_decode(Dst, 0, _, _, Total) ->
    Dst ! {time, Total};
run_decode(Dst, Iters, Module, Json, Total) ->
    {Time, {ok, _}} = timer:tc(Module, decode, [Json]),
    run_decode(Dst, Iters-1, Module, Json, Total+Time).


collect_times(0, Total) ->
    Total;
collect_times(N, Total) ->
    receive
        {time, Time} -> Time
    end,
    collect_times(N-1, Total+Time).

main([]) ->
    main(["base_doc.erl"]);
main([DocName]) ->
    smoke(),
    Doc = load_doc(DocName),
    test_encode(workers(), iters(), jiffy, Doc),
    timer:sleep(5000),
    test_encode(workers(), iters(), json, Doc),
    test_encode(workers(), iters(), ejson_test, Doc),
    test_encode(workers(), iters(), mochijson2, Doc),

    Json = load_json(DocName),
   
    test_decode(workers(), iters(), jiffy, Json),
    test_decode(workers(), iters(), ejson_test, Json),
    test_decode(workers(), iters(), mochijson2, Json),

    ok.
    

