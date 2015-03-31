#include <erl_nif.h>
#include "cmark.h"
#include <string.h>


static ERL_NIF_TERM parse_markdown_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[0], &input_bin)) {
        return enif_make_badarg(env);
    }

    char* processed = cmark_markdown_to_html(input_bin.data, input_bin.size, NULL);
    
    ErlNifBinary cmarked;
    cmarked.data = processed;

    // BOO strlen! BOO!!!!
    // But most of the 'safe' alternatives require a maxlen, and I don't want to cut us
    // off. Plus, we're doing this for CMark output, which I trust.
    cmarked.size = strlen(processed);

    enif_make_binary(env, &cmarked);
}

static ErlNifFunc nif_funcs[] = {
    {"parse_markdown", 1, parse_markdown_nif}
};

ERL_NIF_INIT(ghostlight_markdown, nif_funcs, NULL, NULL, NULL, NULL)
