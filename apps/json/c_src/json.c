// This file is part of eep0018 released under the MIT license. 
// See the LICENSE file for more information.

#include "erl_nif.h"

ERL_NIF_TERM json_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM json_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    return 0;
}

int
on_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    return 0;
}

int
on_upgrade(ErlNifEnv* env, void** priv_data, void** old_data, ERL_NIF_TERM info)
{
    return 0;
}

static ErlNifFunc nif_funcs[] =
{
    {"json_encode", 1, json_encode},
    {"json_decode", 1, json_decode}
};

ERL_NIF_INIT(json, nif_funcs, &on_load, &on_reload, &on_upgrade, NULL);
