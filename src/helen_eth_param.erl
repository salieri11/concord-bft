%% Copyright 2018 VMware, all rights reserved.
%%
%% Parameter extraction and validation for ETH JSON RPC. All functions
%% convert from JSON representation to binary representation.

-module(helen_eth_param).

-export([
         optional_0x/3, required_0x/3,
         optional_address/2, required_address/2,
         optional_integer/2
        ]).

-type param_list() :: [{binary(), mochijson2:json_term()}].
-type param_result() :: {ok|error, binary()|integer()}.
-type value_type() :: bytes|quantity.

%% binary address size in bytes
-define(ADDRESS_SIZE, 20).

%% integer size in bytes
-define(INTEGER_SIZE, 32).

%% the integer zero in binary representation (*8 because the size is in bits)
-define(ZERO_INTEGER, <<0:(?INTEGER_SIZE*8)>>).

%% Look for an optional "0x"-hex-encoded field.
%%
%% Returns an empty binary if the field is not specified.
-spec optional_0x(binary(), param_list(), value_type()) -> param_result().
optional_0x(Field, Params, Type) ->
    case lists:keyfind(Field, 1, Params) of
        {Field, String0x} when is_binary(String0x) ->
            case Type of
                bytes ->
                    helen_eth:dehex_bytes(String0x);
                quantity ->
                    helen_eth:dehex_quantity(String0x)
            end;
        {Field, _} ->
            {error, <<"Wrong type for field ", Field/binary>>};
        false ->
            case Type of
                bytes ->
                    {ok, <<>>};
                quantity ->
                    {ok, 0}
            end
    end.

%% Look for a required "0x"-hex-encoded field.
%%
%% Returns an error if the field is not specified.
-spec required_0x(binary(), param_list(), value_type()) -> param_result().
required_0x(Field, Params, Type) ->
    case optional_0x(Field, Params, Type) of
        {ok, <<>>} ->
            {error, <<"Missing required field ", Field/binary>>};
        Other ->
            Other
    end.

%% Look for an optional address field.
%%
%% Returns an empty binary if the field is not found. Returns an error
%% if the field is found, but it is the wrong length.
-spec optional_address(binary(), param_list()) -> param_result().
optional_address(Field, Params) ->
    case optional_0x(Field, Params, bytes) of
        {ok, Value} ->
            case size(Value) of
                ?ADDRESS_SIZE ->
                    {ok, Value};
                0 ->
                    {ok, Value};
                _ ->
                    {error, <<"Wrong size for address in field ",
                              Field/binary>>}
            end;
        Error ->
            Error
    end.

%% Look for a required address field.
%%
%% Returns an error if the field is not found, or if it is found but
%% is the wrong length.
-spec required_address(binary(), param_list()) -> param_result().
required_address(Field, Params) ->
    case optional_address(Field, Params) of
        {ok, <<>>} ->
            {error, <<"Missing required address field ", Field/binary>>};
        Other ->
            Other
    end.

%% Look for an optional integer field.
%%
%% Returns "zero" if the field is not specified. If the field is
%% found, but is too short, the returned value is padded to the full
%% width. If the field is found, but is too long, and error is
%% returned.
-spec optional_integer(binary(), param_list()) -> param_result().
optional_integer(Field, Params) ->
    case optional_0x(Field, Params, quantity) of
        {ok, Value} ->
            {ok, <<Value:32/big-unsigned-integer-unit:8>>};
        Error ->
            Error
    end.
