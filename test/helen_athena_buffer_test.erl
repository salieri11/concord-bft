%% Copyright 2018 VMware, all rights reserved.
%%
%% Test the receive buffer used to talk to Athena.

-module(helen_athena_buffer_test).

-include_lib("eunit/include/eunit.hrl").

-include("athena_pb.hrl").

onebyte_test() ->
    {Stream, ExpectedMessages} = one_message_example_data(),
    ByteList = [ B || <<B:1/binary>> <= Stream ],
    {ActualMessages, EndBuffer} =
        lists:foldl(fun(C, {M, B}) ->
                            case helen_athena_conn:find_response(C, B) of
                                {ok, Msg, NewB} ->
                                    {[Msg|M], NewB};
                                {wait, NewB} ->
                                    {M, NewB}
                            end
                    end, {[], empty_buffer()}, ByteList),
    ?assertEqual({ExpectedMessages, empty_buffer()},
                 {lists:reverse(ActualMessages), EndBuffer}).

twobyte_test() ->
    {Stream, ExpectedMessages} = two_message_example_data(),
    ByteList = [ B || <<B:2/binary>> <= Stream ],
    {ActualMessages, EndBuffer} =
        lists:foldl(fun(C, {M, B}) ->
                            case helen_athena_conn:find_response(C, B) of
                                {ok, Msg, NewB} ->
                                    {[Msg|M], NewB};
                                {wait, NewB} ->
                                    {M, NewB}
                            end
                    end, {[], empty_buffer()}, ByteList),
    ?assertEqual({ExpectedMessages, empty_buffer()},
                 {lists:reverse(ActualMessages), EndBuffer}).

nosplit_test() ->
    {Stream, ExpectedMessages} = two_message_example_data(),
    {ActualMessages, EndBuffer} = read_until_wait(Stream),
    ?assertEqual(empty_buffer(), EndBuffer),
    ?assertEqual(ExpectedMessages, lists:reverse(ActualMessages)).

read_until_wait(Binary) ->
    read_until_wait(helen_athena_conn:find_response(Binary, empty_buffer()),
                    []).

read_until_wait({ok, Msg, Buffer}, Msgs) ->
    read_until_wait(helen_athena_conn:find_response(<<>>, Buffer),
                    [Msg|Msgs]);
read_until_wait({wait, Buffer}, Msgs) ->
    {Msgs, Buffer}.

empty_buffer() ->
    %% TODO: move buffer record definition to include file
    {buffer, undefined, {[], []}}.

one_message_example_data() ->
    Msg1 = #athenaresponse{
              protocol_response=
                  #protocolresponse{
                     server_version=3}},
    EncMsg1 = iolist_to_binary(athena_pb:encode(Msg1)),
    LenMsg1 = <<(size(EncMsg1)):2/little-unsigned-integer-unit:8>>,
    Stream = iolist_to_binary([LenMsg1, EncMsg1]),
    {Stream, [Msg1]}.

two_message_example_data() ->
    Msg1 = #athenaresponse{
              protocol_response=
                  #protocolresponse{
                     server_version=3}},
    Msg2 = #athenaresponse{
              test_response=
                  #testresponse{
                     %% TODO: find a string=binary encoder
                     echo= "foobarbazquux"}},
    EncMsg1 = iolist_to_binary(athena_pb:encode(Msg1)),
    EncMsg2 = iolist_to_binary(athena_pb:encode(Msg2)),
    LenMsg1 = <<(size(EncMsg1)):2/little-unsigned-integer-unit:8>>,
    LenMsg2 = <<(size(EncMsg2)):2/little-unsigned-integer-unit:8>>,
    Stream = iolist_to_binary([LenMsg1, EncMsg1, LenMsg2, EncMsg2]),
    {Stream, [Msg1, Msg2]}.
