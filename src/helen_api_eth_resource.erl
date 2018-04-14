%% Copyright 2018 VMware, all rights reserved.
%%
%% The Ethereum JSON RPC resource. GET returns a list of available
%% methods. POST makes calls.

-module(helen_api_eth_resource).

-export([
         init/1,
         allowed_methods/2,
         malformed_request/2,
         content_types_provided/2,
         to_json/2,
         content_types_accepted/2,
         process_post/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("helen_eth.hrl").

-define(ETH_JSON_RPC_VERSION, <<"2.0">>).

-type eth_rpc_handler() ::
        fun((#eth_request{}) -> {ok|error, mochijson2:json_term()}).

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
          handler :: undefined | eth_rpc_handler()
         }).

-define(ETH_RPC_METHODS,
        [
         #eth_rpc{name = <<"rpc_modules">>,
                  returns = <<"object">>,
                  handler = fun rpc_modules/1},

         %%%%%%%%%%%%%%%%%%%%
         %% web3

         #eth_rpc{name = <<"web3_clientVersion">>,
                  returns = <<"string">>,
                  handler = fun helen_eth_web3:clientVersion/1},
         #eth_rpc{name = <<"web3_sha3">>,
                  returns = <<"string">>,
                  handler = fun helen_eth_web3:sha3/1},


         %%%%%%%%%%%%%%%%%%%%
         %% eth

         #eth_rpc{name = <<"eth_sendTransaction">>,
                  returns = <<"string">>,
                  handler = fun helen_eth_eth:sendTransaction/1},
         #eth_rpc{name = <<"eth_mining">>,
                  returns = <<"boolean">>,
                  handler = fun helen_eth_eth:mining/1},
         #eth_rpc{name = <<"eth_getTransactionReceipt">>,
                  returns = <<"object">>,
                  handler = fun helen_eth_eth:getTransactionReceipt/1},
         #eth_rpc{name = <<"eth_getStorageAt">>,
                  returns = <<"string">>,
                  handler = fun helen_eth_eth:getStorageAt/1},

         %%%%%%%%%%%%%%%%%%%%
         %% make dialyzer happy

         #eth_rpc{ } %% no name == can never match
        ]).

-record(state, {
          request :: #eth_request{}
}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init(list()) -> {ok|{trace,string()}, #state{}}.
init(_) ->
    {ok, #state{}}.

-spec allowed_methods(wrq:reqdata(), #state{}) ->
          {[atom()], wrq:reqdata(), #state{}}.
allowed_methods(ReqData, State) ->
    {['GET','HEAD','POST'], ReqData, State}.

-spec malformed_request(wrq:reqdata(), #state{}) ->
          {boolean(), wrq:reqdata(), #state{}}.
malformed_request(ReqData, State) ->
    case wrq:method(ReqData) of
        'POST' ->
            case decode_request(ReqData, State) of
                {ok, NewState} ->
                    %% false = NOT malformed
                    {false, ReqData, NewState};
                {error, Reason} ->
                    NewReqData = error_message(ReqData, State, Reason),
                    %% true = IS malformed
                    {true, NewReqData, State}
            end;
        _ ->
            %% false = NOT malformed
            {false, ReqData, State}
    end.

-spec content_types_provided(wrq:reqdata(), #state{}) ->
          {[{string(), atom()}], wrq:reqdata(), #state{}}.
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

-spec to_json(wrq:reqdata(), #state{}) ->
          {iodata(), wrq:reqdata(), #state{}}.
to_json(ReqData, State) ->
    %% This is very static, and we may want to cache the pre-built
    %% JSON, but we could also alter this list depending on auth
    %% headers or other configs.
    MochiJson = [ [{<<"name">>, N}, {<<"params">>, P}, {<<"returns">>, R}]
                  || #eth_rpc{name=N, params=P, returns=R}
                         <- ?ETH_RPC_METHODS ],
    {mochijson2:encode(MochiJson), ReqData, State}.

-spec content_types_accepted(wrq:reqdata(), #state{}) ->
          {[{string(), atom()}], wrq:reqdata(), #state{}}.
content_types_accepted(ReqData, State) ->
    %% This resource does not allow PUT, so there is no accept method
    %% defined. This result is just for content negotiation.
    {[{"application/json", undefined}], ReqData, State}.

-spec process_post(wrq:reqdata(), #state{}) ->
          {boolean(), wrq:reqdata(), #state{}}.
process_post(ReqData, State=#state{request=#eth_request{method=Method}})
  when is_binary(Method) ->
    case lists:keyfind(Method, #eth_rpc.name, ?ETH_RPC_METHODS) of
        RPC=#eth_rpc{} ->
            call_handler(ReqData, State, RPC);
        false ->
            error_logger:info_msg("Unknown method: ~p, params: ~p",
                                  [Method, (State#state.request)#eth_request.params]),
            error_message(ReqData, State,["unknown method ", Method])
    end;
process_post(ReqData, State) ->
    error_message(ReqData, State, <<"Missing method name">>).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Response JSON utitlities

-spec encode_result(wrq:reqdata(), #eth_request{}|undefined,
                   {ok|error, mochijson2:json_term()}) ->
          wrq:reqdata().
encode_result(ReqData, Request, Result) ->
    Id = case Request of
             #eth_request{id=I} when is_integer(I) ->
                 I;
             _ ->
                 0
         end,
    {Label, Value} = case Result of
                         {ok, V} ->
                             {<<"result">>, V};
                         {error, V} when is_binary(V) ->
                             {<<"error">>, {struct, [{<<"message">>, V}]}};
                         {error, {struct, _}=V} ->
                             {<<"error">>, V}
                     end,
    Response = [{<<"id">>, Id},
                {<<"jsonrpc">>, ?ETH_JSON_RPC_VERSION},
                {Label, Value}],
    wrq:set_resp_body(mochijson2:encode(Response), ReqData).

error_message(ReqData, State=#state{request=Request}, Message) ->
    Result = list_to_binary(["ERROR: ", Message]),
    %% "true": JSONRPC always returns 200 status, and encodes errors
    %% in the response body
    {true, encode_result(ReqData, Request, {error, Result}), State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utilities

-spec decode_request(wrq:reqdata(), #state{}) ->
          {ok, #state{}} | {error, iodata()}.
decode_request(ReqData, State=#state{request=undefined}) ->
    case mochijson2:decode(wrq:req_body(ReqData)) of
        {struct, Props} ->
            %% No validation beyond parsing done here - the rest is in
            %% process_response
            Request = #eth_request{
                         method = request_prop(<<"method">>, Props),
                         id = request_prop(<<"id">>, Props),
                         params = request_prop(<<"params">>, Props)},
            {ok, State#state{request=Request}};
        _ ->
            {error, <<"Unable to parse request">>}
    end;
decode_request(_ReqData, State) ->
    %% state was already parsed - return as if it succeeded again
    {ok, State}.

request_prop(Name, Props) ->
    case lists:keyfind(Name, 1, Props) of
        {Name, Val} ->
            Val;
        false ->
            undefined
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Method handlers

call_handler(ReqData,
             State=#state{request=Request},
             #eth_rpc{handler=Handler}) ->
    case Handler of
        undefined ->
            Result = pass_through_handler(Request);
        _ ->
            Result = Handler(Request)
    end,
    {true, encode_result(ReqData, Request, Result), State}.

-spec pass_through_handler(#eth_request{}) ->
          {ok|error, mochijson2:json_term()}.
pass_through_handler(_Request) ->
    {ok, <<"TODO: forward request">>}.

%% Names the RPC modules exposed by this endpoint. ETH uses this to
%% remove things like "admin" from public access. Format is an object,
%% where keys are module names and values are module versions.
-spec rpc_modules(#eth_request{}) -> {ok|error, mochijson2:json_term()}.
rpc_modules(_Request) ->
    {ok, {struct, [
                   {<<"eth">>, <<"1.0">>},
                   {<<"web3">>, <<"1.0">>}
                   %% TODO: "personal"?
                  ]}}.
