// Copyright 2018 VMware, all rights reserved.
//
// NIF wrapper for crypto functions in helen_eth_web3.

#include <erl_nif.h>
#include <keccak.h>

// Compute the Keccak-256 digest of argv[0]
static ERL_NIF_TERM
keccak_digest(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
   ErlNifBinary binary;
   unsigned char *digest;
   ERL_NIF_TERM result;

   CryptoPP::Keccak_256 keccak;
   int digestSize = keccak.DigestSize();

   if (argc != 1) {
      return enif_make_badarg(env);
   }

   if (!enif_inspect_binary(env, argv[0], &binary)) {
      return enif_make_badarg(env);
   }

   digest = enif_make_new_binary(env, digestSize, &result);
   if (digest == NULL) {
      return enif_raise_exception(env, enif_make_atom(env, "no_memory"));
   }

   keccak.CalculateDigest(digest, binary.data, binary.size);

   return result;
}

static ErlNifFunc nif_funcs[] =
{
   {"keccak_digest", 1, keccak_digest}
};

ERL_NIF_INIT(helen_eth_web3, nif_funcs,
             NULL, //load
             NULL, //NULL (unsupported reload)
             NULL, //upgrade
             NULL) //unload
