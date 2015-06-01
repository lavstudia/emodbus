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
%%% emodbus server module
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

-record(state, {transport, socket, parse_state}).

start_link(SockArgs, Opts) ->
	{ok, proc_lib:spawn_link(?MODULE, init, [[SockArgs, Opts]])}.

init([SockArgs = {Transport, _Sock, _SockFun}, _Opts]) ->
    {ok, NewSock} = esockd_connection:accept(SockArgs),
    State = #state{transport = Transport, socket = NewSock,
                   parse_state = emodbus_frame:init(server)},
    gen_server:enter_loop(?MODULE, [], run_sock(State)).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, Sock, Data}, State=#state{transport = Transport, socket = Sock}) ->
	{ok, PeerName} = Transport:peername(Sock),
	io:format("~s - ~p~n", [esockd_net:format(peername, PeerName), Data]),
    handle_received_data(Data, run_sock(State));

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

handle_received_data(<<>>, State) ->
    {noreply, State, hibernate};

handle_received_data(Bytes, State = #state{parse_state = ParseState}) ->
    case emodbus_frame:parse(Bytes, ParseState) of
        {more, ParseState1} ->
            {noreply, run_sock(State#state{parse_state = ParseState1}), hibernate};
        {ok, Frame, Rest} ->
            {ok, NewState} = handle_frame(Frame, State),
            handle_received_data(Rest,NewState#state{parse_state = emodbus_frame:init(server)});
        {error, Error} ->
            {stop, {shutdown, Error}, State}
    end.

handle_frame(Frame = #modbus_frame{hdr = Hdr, pdu = ReqPdu}, State) ->
    io:format("RECV Frame: ~p~n", [Frame]),
    RespPdu = handle_pdu(ReqPdu),
    send(#modbus_frame{hdr = Hdr, pdu = RespPdu}, State).

handle_pdu(#modbus_req{funcode = FunCode, offset = _Offset, quantity = Count}) ->
    #modbus_resp{funcode = FunCode, count  = Count, bytes = <<0,0,0>>}.

send(Frame, State = #state{transport = Transport, socket = Sock}) ->
    Transport:send(Sock, emodbus_frame:serialize(Frame)),
    {ok, State}.

run_sock(State=#state{transport = Transport, socket = Sock}) ->
    Transport:setopts(Sock, [{active, once}]), State.

