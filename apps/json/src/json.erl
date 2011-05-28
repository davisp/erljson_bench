% This file is part of eep0018 released under the MIT license. 
% See the LICENSE file for more information.
-module(json).
-export([encode/1, decode/1, fuzz/0, fuzz/1]).
-on_load(init/0).

init() ->
    SoName = case code:priv_dir(json) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, json]);
                _ ->
                    filename:join([priv, json])
            end;
        Dir ->
            filename:join(Dir, json)
    end,
    erlang:load_nif(SoName, 0).

decode(V) ->
    json_decode(V).

json_decode(_) ->
    not_loaded(?LINE).

encode(V) ->
    json_encode(V).

json_encode(_) ->
    not_loaded(?LINE).

fuzz() ->
    json_fuzz:fuzz(fun json_fuzz:choose/4).
fuzz(Chooser) ->
    json_fuzz:fuzz(Chooser).

not_loaded(Line) ->
    exit({json_not_loaded, module, ?MODULE, line, Line}).
