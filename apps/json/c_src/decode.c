// This file is part of eep0018 released under the MIT license. 
// See the LICENSE file for more information.

#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "erl_nif.h"
#include "erl_nif_compat.h"
#include "yajl/yajl_parse.h"
#include "yajl/yajl_parser.h"
#include "yajl/yajl_lex.h"

#define MAX_DEPTH       2048
#define OK              1
#define ERROR           0

#define WHERE \
    (fprintf(stderr, "(%s)%d:%s\r\n", __FILE__, __LINE__, __FUNCTION__))

// LIFO stack instead of lists:reverse/1
#define OBJ_SLAB_SIZE   512
typedef struct _obj
{
    ERL_NIF_TERM    key;
    ERL_NIF_TERM    slab[OBJ_SLAB_SIZE];
    short           used;
    struct _obj*    next;
} Object; // Map or Array

// Depth stack to handle nested objects
typedef struct
{
    ErlNifEnv*      env;
    ERL_NIF_TERM    error;
    ERL_NIF_TERM    val;
    Object*         stack[MAX_DEPTH];
    int             depth;
} Decoder;

void
init_decoder(Decoder* dec, ErlNifEnv* env)
{
    dec->env = env;
    dec->val = 0;
    dec->depth = -1;
    memset(dec->stack, '\0', sizeof(ERL_NIF_TERM) * MAX_DEPTH);
}

void
destroy_decoder(Decoder* dec, ErlNifEnv* env)
{
    Object* obj = NULL;
    while(dec->depth >= 0)
    {
        while(dec->stack[dec->depth] != NULL)
        {
            obj = dec->stack[dec->depth];
            dec->stack[dec->depth] = obj->next;
            enif_free_compat(dec->env, obj);
        }
        dec->depth--;
    }
}

const char* LEX_ERRORS[] =
{
    "ok",
    "invalid_utf8",
    "invalid_escaped_char",
    "invalid_json_char",
    "invalid_hex_char",
    "invalid_char",
    "invalid_string",
    "missing_integer_after_decimal",
    "missing_integer_after_exponent",
    "missing_integer_after_minus",
    "unallowed_comment"
};

const char* PARSE_ERRORS[] =
{
    "ok",
    "client_cancelled",
    "integer_overflow",
    "numeric_overflow",
    "invalid_token",
    "internal_invalid_token",
    "key_must_be_string",
    "pair_missing_colon",
    "bad_token_after_map_value",
    "bad_token_after_array_value"
};

ERL_NIF_TERM
make_error(yajl_handle handle, ErlNifEnv* env)
{
    ERL_NIF_TERM atom;
    
    yajl_parser_error pe = handle->parserError;
    yajl_lex_error le = yajl_lex_get_error(handle->lexer);

    if(le != yajl_lex_e_ok)
    {
        atom = enif_make_atom(env, LEX_ERRORS[le]);
    }
    else if(pe != yajl_parser_e_ok)
    {
        atom = enif_make_atom(env, PARSE_ERRORS[pe]);
    }
    else
    {
        atom = enif_make_atom(env, "unknown");
    }

    return enif_make_tuple(env, 2,
        enif_make_atom(env, "error"),
        enif_make_tuple(env, 2,
            enif_make_uint(env, handle->bytesConsumed),
            atom
        )
    );
}

static inline int
push_value(Decoder* dec, ERL_NIF_TERM val)
{
    Object* obj = NULL;
    Object* new = NULL;

    // Single value parsed
    if(dec->depth < 0)
    {
        if(dec->val != 0) return ERROR;
        dec->val = val;
        return OK;
    }
    
    assert(dec->stack[dec->depth] != NULL);
    obj = dec->stack[dec->depth];

    if(obj->key != 0)
    {
        val = enif_make_tuple(dec->env, 2, obj->key, val);
        obj->key = 0;
    }

    // Room left in object slab
    if(obj->used < OBJ_SLAB_SIZE)
    {
        obj->slab[obj->used++] = val;
        return OK;
    }
    
    // New object slab required
    new = (Object*) enif_alloc_compat(dec->env, sizeof(Object));
    if(new == NULL)
    {
        dec->error = enif_make_atom(dec->env, "memory_error");
        return ERROR;
    }
    memset(new, '\0', sizeof(Object));
    new->key = 0;
    new->slab[0] = val;
    new->used = 1;
    new->next = obj;
    dec->stack[++dec->depth] = new;
    
    return OK;
}

static inline int
pop_object(Decoder* dec, ERL_NIF_TERM* val)
{
    Object* curr = NULL;
    ERL_NIF_TERM ret = enif_make_list(dec->env, 0);

    if(dec->depth < 0)
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_depth");
        return ERROR;
    }
    if(dec->stack[dec->depth]->used > OBJ_SLAB_SIZE)
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_slab_use");
        return ERROR;
    }
    
    while(dec->stack[dec->depth] != NULL)
    {
        curr = dec->stack[dec->depth];
        while(curr->used > 0)
        {
            ret = enif_make_list_cell(dec->env, curr->slab[--curr->used], ret);
        }
        dec->stack[dec->depth] = curr->next;
        enif_free_compat(dec->env, curr);
    }

    dec->depth--;
    *val = ret;
    return OK;
}

static int
decode_null(void* ctx)
{
    Decoder* dec = (Decoder*) ctx;
    return push_value(dec, enif_make_atom(dec->env, "null"));
}

static int
decode_boolean(void* ctx, int val)
{
    Decoder* dec = (Decoder*) ctx;
    if(val)
    {
        return push_value(dec, enif_make_atom(dec->env, "true"));
    }
    else
    {
        return push_value(dec, enif_make_atom(dec->env, "false"));
    }

    return OK;
}

static int
decode_integer(void* ctx, long val)
{
    Decoder* dec = (Decoder*) ctx;
    return push_value(dec, enif_make_long(dec->env, val));
}

static int
decode_double(void* ctx, double val)
{
    Decoder* dec = (Decoder*) ctx;
    return push_value(dec, enif_make_double(dec->env, val));
}

static int
decode_string(void* ctx, const unsigned char* data, unsigned int size)
{
    ErlNifBinary bin;
    Decoder* dec = (Decoder*) ctx;
    if(!enif_alloc_binary_compat(dec->env, size, &bin))
    {
        dec->error = enif_make_atom(dec->env, "memory_error");
        return ERROR;
    }
    memcpy(bin.data, data, size);
    return push_value(dec, enif_make_binary(dec->env, &bin));
}

static int
decode_start_obj(void* ctx)
{
    Object* obj = NULL;
    Decoder* dec = (Decoder*) ctx;
    
    if(dec->depth+1 < 0)
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_depth");
        return ERROR;           
    }
    if(dec->depth+1 >= MAX_DEPTH)
    {
        dec->error = enif_make_atom(dec->env, "max_depth_exceeded");
        return ERROR;   
    }
    dec->depth++;
    
    obj = (Object*) enif_alloc_compat(dec->env, sizeof(Object));
    if(obj == NULL)
    {
        dec->error = enif_make_atom(dec->env, "memory_error");
        return ERROR;
    }
    memset(obj, '\0', sizeof(Object));
    obj->key = 0;
    obj->used = 0;
    obj->next = NULL;
    dec->stack[dec->depth] = obj;
    
    return OK;
}

static int
decode_map_key(void* ctx, const unsigned char* data, unsigned int size)
{
    ErlNifBinary bin;
    Decoder* dec = (Decoder*) ctx;
    if(dec->stack[dec->depth] < 0)
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_map_key_depth");
        return ERROR;
    }
    if(dec->stack[dec->depth]->key != 0)
    {
        dec->error = enif_make_atom(dec->env, "invalid_internal_no_key_set");
        return ERROR;
    }
    if(!enif_alloc_binary_compat(dec->env, size, &bin))
    {
        dec->error = enif_make_atom(dec->env, "memory_error");
        return ERROR;
    }
    memcpy(bin.data, data, size);
    dec->stack[dec->depth]->key = enif_make_binary(dec->env, &bin);
    return OK;
}

static int
decode_end_map(void* ctx)
{
    ERL_NIF_TERM val;
    Decoder* dec = (Decoder*) ctx;
    if(!pop_object(dec, &val)) return ERROR;
    val = enif_make_tuple(dec->env, 1, val);
    return push_value(dec, val);
}

static int
decode_end_array(void* ctx)
{
    ERL_NIF_TERM val;
    Decoder* dec = (Decoder*) ctx;
    if(!pop_object(dec, &val)) return ERROR;
    return push_value(dec, val);
}

static yajl_callbacks
decoder_callbacks = {
    decode_null,
    decode_boolean,
    decode_integer,
    decode_double,
    NULL,
    decode_string,
    decode_start_obj,
    decode_map_key,
    decode_end_map,
    decode_start_obj,
    decode_end_array
};

static int
check_rest(unsigned char* data, unsigned int size, unsigned int used)
{
    unsigned int i = 0;
    for(i = used; i < size; i++)
    {
        switch(data[i])
        {
            case ' ':
            case '\t':
            case '\r':
            case '\n':
                continue;
            default:
                return ERROR;
        }
    }
    
    return OK;
}

ERL_NIF_TERM
json_decode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Decoder dec;
    yajl_parser_config conf = {0, 1}; // No comments, check utf8
    yajl_handle handle = yajl_alloc(&decoder_callbacks, &conf, NULL, &dec);
    yajl_status status;
    unsigned int used;
    ErlNifBinary bin;
    ERL_NIF_TERM ret;
    
    if(handle == NULL)
    {
        ret = enif_make_tuple(env, 2,
            enif_make_atom(env, "error"),
            enif_make_atom(env, "memory_error")
        );
        goto done;
    }

    if(argc != 1)
    {
        ret = enif_make_badarg(env);
        goto done;
    }

    if(!enif_inspect_iolist_as_binary(env, argv[0], &bin))
    {
        ret = enif_make_badarg(env);
        goto done;
    }
    
    init_decoder(&dec, env);
    status = yajl_parse(handle, bin.data, bin.size);
    used = handle->bytesConsumed;
    destroy_decoder(&dec, env);

    // Parsing something like "2.0" (without quotes) will
    // cause a spurious semi-error. We add the extra size
    // check so that "2008-20-10" doesn't pass.
    if(status == yajl_status_insufficient_data && used == bin.size)
    {
        status = yajl_parse_complete(handle);
    }

    if(status == yajl_status_ok && used != bin.size)
    {
        if(check_rest(bin.data, bin.size, used) != OK)
        {
            ret = enif_make_tuple(env, 2,
                enif_make_atom(env, "error"),
                enif_make_atom(env, "garbage_after_value")
            );
            goto done;
        }
    }

    switch(status)
    {
        case yajl_status_ok:
            ret = enif_make_tuple(env, 2, enif_make_atom(env, "ok"), dec.val);
            goto done;

        case yajl_status_error:
            ret = make_error(handle, env);
            goto done;

        case yajl_status_insufficient_data:
            ret = enif_make_tuple(env, 2,
                enif_make_atom(env, "error"),
                enif_make_atom(env, "insufficient_data")
            );
            goto done;

        case yajl_status_client_canceled:
            ret = enif_make_tuple(env, 2,
                enif_make_atom(env, "error"),
                enif_make_tuple(env, 2,
                    enif_make_uint(env, handle->bytesConsumed),
                    dec.error
                )
            );
            goto done;

        default:
            ret = enif_make_tuple(env, 2,
                enif_make_atom(env, "error"),
                enif_make_atom(env, "unknown")
            );
            goto done;
    }

done:
    if(handle != NULL) yajl_free(handle);
    return ret;
}
