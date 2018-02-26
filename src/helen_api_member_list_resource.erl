%% Copyright 2018 VMware, all rights reserved.
%%
%% List members in this Athena cluster. Includes only Athena nodes,
%% not Helen nodes.

-module(helen_api_member_list_resource).

-export([
         init/1,
         content_types_provided/2,
         to_json/2
        ]).

-include_lib("webmachine/include/webmachine.hrl").
-include("athena_pb.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resource definitions

-spec init([]) -> {ok, term()}.
init([]) ->
    {ok, undefined}.

-spec content_types_provided(wrq:reqdata(), term()) ->
          {[{string(), atom()}], wrq:reqdata(), term()}.
content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

-spec to_json(wrq:reqdata(), term()) ->
          {{halt, integer()}|iodata(), wrq:reqdata(), term()}.
to_json(ReqData, State) ->
    Request = #athenarequest{
                 peer_request =
                     #peerrequest{return_peers = true}},
    case helen_athena_conn:send_request(Request) of
        #athenaresponse{peer_response=#peerresponse{peer=Peers}}
          when is_list(Peers) ->
            Response = [{struct, [
                                  {<<"host">>, format_host(A,P)},
                                  {<<"status">>, iolist_to_binary(S)}
                                 ]}
                        || #peer{address=A, port=P, status=S} <- Peers];
        _ ->
            Response = {struct, [{<<"error">>,
                                  <<"invalid response from server">>}]}
    end,
    {mochijson2:encode(Response), ReqData, State}.

format_host(Address, Port) ->
    iolist_to_binary(io_lib:format("~s:~b", [Address, Port])).
