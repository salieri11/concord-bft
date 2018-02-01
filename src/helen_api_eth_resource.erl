%% Copyright 2018 VMware, all rights reserved.
%%
%% The Ethereum JSON RPC resource. GET returns a list of available
%% methods. POST makes calls.

-module(helen_api_eth_resource).

-export([
         init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2,
         content_types_accepted/2,
         process_post/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").

-define(ETH_JSON_RPC_VERSION, <<"2.0">>).

%% This record and the following list record how we handle each of the
%% RPC methods.
-record(eth_rpc, {
          %% The name of the method (value of the "method" field)
          name :: binary(),

          %% Details about the params (TODO: currently unspecified)
          params = [] :: [binary()],

          %% Type of the return
          returns :: binary(),

          %% How we handle the method. 'undefined' means pass through
          %% to an Athena client. Otherwise the attached function will
          %% be called.
          handler :: undefined |
                     fun((wrq:reqdata(), term(), mochijson2:json_term())
                         -> {boolean() | halt, wrq:reqdata(), term()})
         }).

-define(ETH_RPC_METHODS,
        [
         #eth_rpc{name = <<"web3_clientVersion">>,
                  returns = <<"string">>,
                  handler = fun web3_clientVersion/3},
         #eth_rpc{name = <<"web3_sha3">>,
                  returns = <<"string">>,
                  handler = fun web3_sha3/3},
         #eth_rpc{name = <<"eth_sendTransaction">>,
                  returns = <<"string">>}
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init(list()) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

-spec allowed_methods(wrq:reqdata(), term()) ->
          {[atom()], wrq:reqdata(), term()}.
allowed_methods(ReqData, State) ->
    {['GET','HEAD','POST'], ReqData, State}.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

-spec to_json(wrq:reqdata(), term()) ->
          {{halt, integer()}|iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    %% This is very static, and we may want to cache the pre-built
    %% JSON, but we could also alter this list depending on auth
    %% headers or other configs.
    MochiJson = [ [{<<"name">>, N}, {<<"params">>, P}, {<<"returns">>, R}]
                  || #eth_rpc{name=N, params=P, returns=R}
                         <- ?ETH_RPC_METHODS ],
    {mochijson2:encode(MochiJson), ReqData, State}.

-spec content_types_accepted(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_accepted(ReqData, State) ->
    %% This resource does not allow PUT, so there is no accept method
    %% defined. This result is just for content negotiation.
    {[{"application/json", undefined}], ReqData, State}.

-spec process_post(wrq:reqdata(), term()) ->
          {boolean() | halt, wrq:reqdata(), term()}.
process_post(ReqData, State) ->
    %% TODO: this will blow up if the body was not valid JSON. catch
    %% and provide a better error
    {struct, MochiJson} = mochijson2:decode(wrq:req_body(ReqData)),
    case lists:keyfind(<<"method">>, 1, MochiJson) of
        {<<"method">>, MethodName} ->
            case lists:keyfind(MethodName, #eth_rpc.name, ?ETH_RPC_METHODS) of
                #eth_rpc{handler=undefined} ->
                    pass_through_handler(ReqData, State, MochiJson);
                #eth_rpc{handler=H} ->
                    H(ReqData, State, MochiJson);
                false ->
                    error_message(ReqData, State, MochiJson,
                                  ["unknown method ", MethodName])
            end;
        false ->
            error_message(ReqData, State, MochiJson, "missing method name")
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Response JSON utitlities

-spec encode_result(wrq:reqdata(), mochijson2:json_term(), binary()) ->
          wrq:reqdata().
encode_result(ReqData, RequestMJ, Result) ->
    Id = case lists:keyfind(<<"id">>, 1, RequestMJ) of
             {<<"id">>, I} -> I;
             false -> 0
         end,
    Response = [{<<"id">>, Id},
                {<<"jsonrpc">>, ?ETH_JSON_RPC_VERSION},
                {<<"result">>, Result}],
    wrq:set_resp_body(mochijson2:encode(Response), ReqData).

error_message(ReqData, State, MochiJson, Message) ->
    Result = list_to_binary(["ERROR: ", Message]),
    {{halt, 400}, encode_result(ReqData, MochiJson, Result), State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Method handlers

pass_through_handler(ReqData, State, MochiJson) ->
    Result = <<"TODO: forward request">>,
    {true, encode_result(ReqData, MochiJson, Result), State}.

web3_clientVersion(ReqData, State, MochiJson) ->
    Result = <<"Helen/1.0.0">>,
    {true, encode_result(ReqData, MochiJson, Result), State}.

web3_sha3(ReqData, State, MochiJson) ->
    Result = <<"TODO: Keccak-256">>,
    {true, encode_result(ReqData, MochiJson, Result), State}.
