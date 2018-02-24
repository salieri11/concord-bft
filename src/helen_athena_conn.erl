-module(helen_athena_conn).

%% Produced by the compilation of athena.proto
-include("athena_pb.hrl").

-export([
         send/1,
         send_echo_request/1
        ]).

send_echo_request(String) ->
    Msg = #athenarequest{
             testrequest =
                 #athenarequest_testrequest{
                    echo = String
                   }},
    case helen_athena_conn:send(athena_pb:encode(Msg)) of
        {ok, #athenaresponse{testresponse=#athenaresponse_testresponse{echo=S}}} when S /= undefined ->
            {ok, S};
        {ok, #athenaresponse{testresponse=#athenaresponse_testresponse{}}} ->
            {error, <<"no echo">>};
        {ok, #athenaresponse{}} ->
            {error, <<"no testresponse">>};
        {error, _}=E ->
            E
    end.
               

send(Message) ->
    send(Message, hd(athena_nodes())).

send(Message, {IP, Port}) ->
    case catch gen_tcp:connect(IP, Port, [binary,{active,false}]) of
        {ok, Socket} ->
            case gen_tcp:send(Socket, [<<(iolist_size(Message)):2/little-unsigned-integer-unit:8>>, Message]) of
                ok ->
                    case gen_tcp:recv(Socket, 0, 2000) of
                        {ok, <<_Length:2/little-unsigned-integer-unit:8, Reply/binary>>} ->
                            case athena_pb:decode_athenaresponse(Reply) of
                                #athenaresponse{}=R ->
                                    {ok, R};
                                Other ->
                                    {error, Other}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason};
        {'EXIT', Reason} ->
            {error, Reason}
    end.

athena_nodes() ->
    case application:get_env(helen, athena_nodes) of
        {ok, Nodes} ->
            Nodes;
        undefined ->
            []
    end.
