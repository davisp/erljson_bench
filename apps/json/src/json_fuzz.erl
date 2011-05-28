% This file is part of eep0018 released under the MIT license. 
% See the LICENSE file for more information.
-module(json_fuzz).

-export([fuzz/0, fuzz/1, choose/4]).

fuzz() ->
    fuzz(fun choose/4).

fuzz(Fun) ->
    put(jf_chooser, Fun),
    put(jf_state, [0]),
    value().

choose(_Depth, _Width, _State, Possible) ->
    choice(Possible).


choice(Possible) when is_list(Possible) ->
    Idx = trunc(random:uniform() * length(Possible)),
    lists:nth(Idx+1, Possible).


value() ->
    process(value).

character() ->
    % TODO: UTF-8 generation
    trunc(random:uniform() * 128).

string() ->
    process(string).

ws() ->
    process(open_ws).


process(InitState) ->
    States = lists:reverse(transition(InitState, [InitState])),
    lists:flatten(lists:map(fun render/1, States)).

transition(State, Acc) ->
    Chooser = get(jf_chooser),
    {Depth, Width} = setctx(State),
    case states(State) of
        [] ->
            Acc;
        Possible ->
            Next = Chooser(Depth, Width, State, Possible),
            true = lists:member(Next, Possible),
            transition(Next, [Next | Acc])
    end.


states(value) ->        [null, true, false, number, string, array, map];
states(value_end) ->    [];

states(null) ->         [value_end];
states(true) ->         [value_end];
states(false) ->        [value_end];

states(number) ->       [hyphen, single_zero, one_to_nine];
states(hyphen) ->       [single_zero, one_to_nine];
states(single_zero) ->  [decimal, exp, value_end];
states(one_to_nine) ->  [mantissa, end_mantissa];
states(mantissa) ->     [mantissa, end_mantissa];
states(end_mantissa) -> [decimal, exp, value_end];
states(decimal) ->      [fraction];
states(fraction) ->     [fraction, end_fraction];
states(end_fraction) -> [exp, value_end];
states(exp) ->          [exp_sign, exp_digit];
states(exp_sign) ->     [exp_digit];
states(exp_digit) ->    [exp_digit, exp_end];
states(exp_end) ->      [value_end];

states(string) ->       [open_quotes];
states(open_quotes) ->  [string_elem];
states(string_elem) ->  [character, escape_seq, close_quotes];
states(character) ->    [string_elem];
states(escape_seq) ->   [quotes, $\\, $/, b, f, n, r, t, u];
states(quotes) ->       [string_elem];
states($\\) ->          [string_elem];
states($/) ->           [string_elem];
states(b) ->            [string_elem];
states(f) ->            [string_elem];
states(n) ->            [string_elem];
states(r) ->            [string_elem];
states(t) ->            [string_elem];
states(u) ->            [hex_char_1];
states(hex_char_1) ->   [hex_char_2];
states(hex_char_2) ->   [hex_char_3];
states(hex_char_3) ->   [hex_char_4];
states(hex_char_4) ->   [string_elem];
states(close_quotes) -> [value_end];

states(array) ->        [open_array];
states(open_array) ->   [first_elem, close_array];
states(first_elem) ->   [second_elem, close_array];
states(second_elem) ->  [second_elem, close_array];
states(close_array) ->  [value_end];

states(map) ->          [open_map];
states(open_map) ->     [first_pair, close_map];
states(first_pair) ->   [second_pair, close_map];
states(second_pair) ->  [second_pair, close_map];
states(close_map) ->    [value_end];

states(open_ws) ->      [ws, close_ws];
states(ws) ->           [ws, close_ws];
states(close_ws) ->     [].


render(value) ->        ws();
render(value_end) ->    ws();

render(null) ->         "null";
render(true) ->         "true";
render(false) ->        "false";

render(number) ->       "";
render(hyphen) ->       "-";
render(single_zero) ->  "0";
render(one_to_nine) ->  choice("123456789");
render(mantissa) ->     choice("0123456789");
render(end_mantissa) -> "";
render(decimal) ->      ".";
render(fraction) ->     choice("0123456789");
render(end_fraction) -> "";
render(exp) ->          choice("eE");
render(exp_sign) ->     choice("+-");
render(exp_digit) ->    choice("0123456789");
render(exp_end) ->      "";

render(string) ->       "";
render(string_elem) ->  "";
render(open_quotes) ->  "\"";
render(character) ->    character();
render(escape_seq) ->   "\\";
render(quotes) ->       "\"";
render($\\) ->          "\\";
render($/) ->           "/";
render(b) ->            "b";
render(f) ->            "f";
render(n) ->            "n";
render(r) ->            "r";
render(t) ->            "t";
render(u) ->            "u";
render(hex_char_1) ->   choice("0123456789ABCDEF");
render(hex_char_2) ->   choice("0123456789ABCDEF");
render(hex_char_3) ->   choice("0123456789ABCDEF");
render(hex_char_4) ->   choice("0123456789ABCDEF");
render(close_quotes) -> "\"";

render(array) ->        "";
render(open_array) ->   "[";
render(first_elem) ->   value();
render(second_elem) ->  "," ++ value();
render(close_array) ->  "]";

render(map) ->          "";
render(open_map) ->     "{";
render(pair) ->         ws() ++ string() ++ ws() ++ ":" ++ value();
render(first_pair) ->   render(pair);
render(second_pair) ->  "," ++ render(pair);
render(close_map) ->    "}";

render(open_ws) ->      "";
render(ws) ->           choice(" \t\r\n");
render(close_ws) ->     "".


setctx(one_to_nine) ->  ctxpush();
setctx(mantissa) ->     ctxwiden();
setctx(end_mantissa) -> ctxpop();
setctx(decimal) ->      ctxpush();
setctx(fraction) ->     ctxwiden();
setctx(end_fraction) -> ctxpop();
setctx(exp) ->          ctxpush();
setctx(exp_digit) ->    ctxwiden();
setctx(exp_end) ->      ctxpop();

setctx(open_quotes) ->  ctxpush();
setctx(string_elem) ->  ctxwiden();
setctx(close_quotes) -> ctxpop();

setctx(open_array) ->   ctxpush();
setctx(first_elem) ->   ctxwiden();
setctx(second_elem) ->  ctxwiden();
setctx(close_array) ->  ctxpop();

setctx(open_map) ->     ctxpush();
setctx(first_pair) ->   ctxwiden();
setctx(second_pair) ->  ctxwiden();
setctx(close_map) ->    ctxpop();

setctx(open_ws) ->      ctxpush();
setctx(ws) ->           ctxwiden();
setctx(close_ws) ->     ctxpop();

setctx(_) ->            getctx().


ctxpush() ->
    put(jf_state, [0 | get(jf_state)]),
    getctx().

ctxpop() ->
    put(jf_state, lists:nthtail(1, get(jf_state))),
    getctx().

ctxwiden() ->
    [Curr | State] = get(jf_state),
    put(jf_state, [Curr+1 | State]),
    getctx().

getctx() ->
    State = get(jf_state),
    {length(State), hd(State)}.
