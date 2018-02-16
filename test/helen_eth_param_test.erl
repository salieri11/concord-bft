%% Copyright 2018 VMware, all rights reserved.
%%
%% Test the parameter extraction functions

-module(helen_eth_param_test).

%% eunit includes -comple(export_all).
-include_lib("eunit/include/eunit.hrl").

-define(TEST_DATA,
        [
         {<<"valid_address">>,
          <<"0x1111111111111111111111111111111111111111">>},
         {<<"short0x">>,
          <<"0x2222">>},
         {<<"full_integer">>,
          <<"0x33333333333333333333333333333333",
              "33333333333333333333333333333333">>},
         {<<"long0x">>,
          <<"0x44444444444444444444444444444444"
              "4444444444444444444444444444444444">>},
         {<<"not0x">>, 1}
         %% and don't forget "undefined"!
        ]).

common_test_list(Fun, Expects) ->
    [?_assertEqual(lists:sort([ Name || {Name, _} <- Expects ]),
                   lists:sort([<<"undefined">>|
                               [ Name || {Name, _} <- ?TEST_DATA ]]))
     |[?_assertEqual(Expect, element(1, Fun(Field, ?TEST_DATA)))
       || {Field, Expect} <- Expects]].

%% Anything that is a valid 0x-encoded string
common_0x_expect() ->
    [{<<"valid_address">>, ok},
     {<<"short0x">>, ok},
     {<<"full_integer">>, ok},
     {<<"long0x">>, ok},
     {<<"not0x">>, error}].

optional_0x_test_() ->
    common_test_list(fun helen_eth_param:optional_0x/2,
                     [{<<"undefined">>, ok} | common_0x_expect()]).

required_0x_test_() ->
    common_test_list(fun helen_eth_param:required_0x/2,
                     [{<<"undefined">>, error} | common_0x_expect()]).

%% Only valid 0x-encoded addresses.
common_address_expect() ->
    [{<<"valid_address">>, ok},
     {<<"short0x">>, error},
     {<<"full_integer">>, error},
     {<<"long0x">>, error},
     {<<"not0x">>, error}].

optional_address_test_() ->
    common_test_list(fun helen_eth_param:optional_address/2,
                     [{<<"undefined">>, ok} | common_address_expect()]).

required_address_test_() ->
    common_test_list(fun helen_eth_param:required_address/2,
                     [{<<"undefined">>, error} | common_address_expect()]).

%% Only valid 0x-encoded integers
optional_integer_test_() ->
    common_test_list(fun helen_eth_param:optional_integer/2,
                     [{<<"valid_address">>, ok},
                      {<<"short0x">>, ok},
                      {<<"full_integer">>, ok},
                      {<<"long0x">>, error},
                      {<<"not0x">>, error},
                      {<<"undefined">>, ok}]).
