%%%-----------------------------------------------------------------------------
%%% Copyright (c) 2015 Feng Lee <feng@emqtt.io>, All Rights Reserved.
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%%-----------------------------------------------------------------------------
%%% @doc
%%% emodbus frame parser.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emodbus_frame).

-include("emodbus.hrl").

-export([parse/2, serialize/1]).

%% Modbus MBAP Header

parse(Bin, {none, Side}) when size(Bin) < ?MBAP_LENGTH ->
    {more, fun(BinMore) -> parse(<<Bin/binary, BinMore/binary>>, {none, Side}) end};

parse(<<Tid:16, 0:16, Len:16, UnitId:8, Rest/binary>>, {none, Side}) ->
    parse_pdu(Rest, #mbap_header{tid = Tid, length = Len, unit_id = UnitId}, Side);

parse(Bin, Cont) -> 
    io:format("~p~n", [Bin]), 
    Cont(Bin).

parse_pdu(Bin, Hdr = #mbap_header{length = Len}, Side) when size(Bin) < (Len -1) -> 
    {more, fun(BinMore) -> parse_pdu(<<Bin/binary, BinMore/binary>>, Hdr, Side) end};

parse_pdu(Bin, Hdr = #mbap_header{length = Length}, Side) ->
    Len = Length - 2,
    <<FunCode:8, Data:Len/binary, Rest/binary>> = Bin,
    {ok, Pdu} = 
    case {Side, FunCode} of
        {server, ?FUN_CODE_READ_COILS} ->
            <<Offset:16, Count:16>> = Data,
            {ok, #modbus_req{funcode = FunCode, offset = Offset, quantity = Count}};
        {server, ?FUN_CODE_WRITE_HREGS} ->
            <<Offset:16, Val:16>> = Data,
            {ok, #modbus_req{funcode = FunCode, offset = Offset, value = Val}};
        {client, ?FUN_CODE_READ_COILS} ->
            {ok, #modbus_resp{funcode = FunCode, bytes = Data}};
        {client, ?FUN_CODE_WRITE_HREGS} ->
            {ok, #modbus_resp{funcode = FunCode, bytes = Data}}
    end,
    {ok,  #modbus_frame{hdr = Hdr, pdu = Pdu}, Rest}.

serialize(#modbus_frame{header = #mbap_header{tid = Tid, unit_id = Uid},
                        funcode = FunCode, payload = Payload}) ->
    Len = 2 + size(Payload),
    <<Tid:16, 0:16, Len:16, Uid:8, FunCode:8, Payload/binary>>.

