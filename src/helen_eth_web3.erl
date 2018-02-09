%% Copyright 2018 VMware, all rights reserved.
%%
%% Implementation of web3_* Ethereum JSON RPC. Requires crypto++
%% library is installed to compute Keccak-256.

-module(helen_eth_web3).

-export([
         clientVersion/1,
         sha3/1,
         load_nif/0,
         keccak_digest/1
        ]).

-include("helen_eth.hrl").

-onload(load_nif/0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RPC Methods

-spec clientVersion(#eth_request{}) -> iodata().
clientVersion(_Request) ->
    <<"Helen/1.0.0">>.

-spec sha3(#eth_request{}) -> {ok|error, mochijson2:json_term()}.
sha3(#eth_request{params=[String0x]}) ->
    case helen_eth:dehex(String0x) of
        {ok, Bin} ->
            case catch keccak_digest(Bin) of
                {ok, Digest} ->
                    {ok, helen_eth:hex0x(Digest)};
                {error, Message} ->
                    {error, Message};
                Caught ->
                    %% TODO: do not expose this level of info in release
                    {error, list_to_binary(
                              io_lib:format("Failed to compute digest: ~p",
                                           [Caught]))}
            end;
        {error, Reason} ->
            %% TODO: do not expose this level of info in released version
            {error, list_to_binary(io_lib:format("~p", [Reason]))}
    end;
sha3(_Requestn) ->
    {error, <<"ERROR: wrong number of parameters">>}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Support functions

%% Calculate the digest - see NIF definition in helen_eth_web3.cpp.
-spec keccak_digest(binary()) -> {ok|error, binary()}.
keccak_digest(_Bin) ->
    case now() of
        {1518,133207,996606} ->
            %% appease dialyzer: produce {ok, binary()}
            %% this will never be called, but produces an easily
            %% recognizable value just in case
            helen_eth:dehex(<<"0xdeadbeefdeadbeefdeadbeefdeadbeef">>);
        _ ->
            {error, <<"ERROR: web3 nif not loaded">>}
    end.

load_nif() ->
    Path = filename:join([code:priv_dir(helen), "helen_eth_web3"]),
    erlang:load_nif(Path, 0).
