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
%%% emodbus server.
%%%
%%% @end
%%%-----------------------------------------------------------------------------

-module(emodbus_server).

-include("emodbus.hrl").

-behaviour(gen_server).

%% esockd callback 
-export([start_link/2]).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {transport, socket, parser, handler}).

start_link(SockArgs, Handler) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [[SockArgs, Handler]])}.

init([SockArgs = {Transport, _Sock, _SockFun}, Handler]) ->
    {ok, NewSock} = esockd_connection:accept(SockArgs),
    State = #state{transport = Transport,
                   socket    = NewSock,
                   parser    = parser(),
                   handler   = Handler},
    gen_server:enter_loop(?MODULE, [], run_sock(State)).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, State=#state{transport = Transport, socket = Sock}) ->
	{ok, PeerName} = Transport:peername(Sock),
	io:format("~s - ~p~n", [esockd_net:format(peername, PeerName), Data]),
    received(Data, run_sock(State));

handle_info({tcp_error, Sock, Reason}, State=#state{socket = Sock}) ->
    {stop, {shutdown, {tcp_error, Reason}}, State};

handle_info({tcp_closed, Sock}, State=#state{socket = Sock}) ->
	{stop, {shutdown, tcp_closed}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

received(<<>>, State) ->
    {noreply, State, hibernate};

received(Data, State = #state{parser = Parser}) ->
    case Parser(Data) of
        {more, NewParser} ->
            {noreply, State#state{parser = NewParser}};
        {ok, Frame, Rest} ->
            case handle_frame(Frame, State) of
                {ok, NewState} ->
                    received(Rest, NewState#state{parser = parser()});
                {error, Error} ->
                    {stop, {shutdown, Error}, State}
            end;
        {error, Error} ->
            {stop, {shutdown, Error}, State}
    end.

handle_frame(Frame = #modbus_frame{header = Header,
                                   funcode = FunCode,
                                   payload = Payload},
             State = #state{handler = Handler}) ->
    error_logger:info_msg("RECV: ~p", [Frame]),
    case Handler:handle_request(FunCode, Payload) of
        {ok, {Code, Data}} ->
            RespFrame = #modbus_frame{header = Header,
                                      funcode = Code,
                                      payload = Data},
            send(RespFrame, State), {ok, State};
        {error, Error} ->
            {error, Error}
    end.

send(Frame, #state{transport = Transport, socket = Sock}) ->
    error_logger:info_msg("SENT: ~p", [Frame]),
    Transport:send(Sock, serialize(Frame)).

run_sock(State=#state{transport = Transport, socket = Sock}) ->
    Transport:setopts(Sock, [{active, once}]), State.

%% parser
parser() ->
    fun(Bin) -> parse(Bin, none) end.

parse(Bin, none) when size(Bin) < ?MBAP_LENGTH ->
    {more, fun(BinMore) -> parse(<<Bin/binary, BinMore/binary>>, none) end};

parse(<<Tid:16, 0:16, Len:16, Rest/binary>>, none) ->
    parse_pdu(Rest, #mbap_header{tid = Tid, length = Len}).

parse_pdu(Bin, Hdr = #mbap_header{length = Len}) when size(Bin) < (Len) ->
    {more, fun(BinMore) -> parse_pdu(<<Bin/binary, BinMore/binary>>, Hdr) end};

parse_pdu(Bin, Hdr = #mbap_header{length = Len}) ->
    <<Data:Len/binary, Rest/binary>> = Bin,
    <<UnitId:8, FunCode:8, Payload/binary>> = Data,
    Frame = #modbus_frame{header = Hdr#mbap_header{unit_id = UnitId},
                          funcode = FunCode,
                          payload = Payload},
    {ok, Frame, Rest}.

serialize(#modbus_frame{header = #mbap_header{tid = Tid, unit_id = Uid},
                        funcode = FunCode, payload = Payload}) ->
    Len = 2 + size(Payload),
    <<Tid:16, 0:16, Len:16, Uid:8, FunCode:8, Payload/binary>>.

