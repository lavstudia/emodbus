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
%%% emodbus TCP client.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emodbus).

-include("emodbus.hrl").

%% API Exports
-export([connect/0, connect/2, connect/3,
         disconnect/1]).

%% modbus API Exports
-export([read_coils/3,
         read_inputs/3,
         read_hregs/3,
         read_iregs/3,
         write_coil/3,
         write_hreg/3,
         write_coils/3,
         write_hregs/3]).

-behaviour(gen_server).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        socket      :: inet:socket(),
        tid = 1     :: 1..16#ff,
        unit_id = 1 :: byte()
 }).

-define(MBAP_LENGTH, 7).

-define(CALL_TIMEOUT, 60000).

-define(SOCK_TIMEOUT, 30000).

-define(RECV_TIMEOUT, 10000).

%% Modbus Socket Options
-define(SOCKOPTS, [
        binary,
        {active, false},
        {packet, 0},
        {reuseaddr, true},
        {nodelay,   true}
]).

connect() ->
    connect("localhost", ?MODBUS_PORT).

connect(Host, Port) ->
    connect(Host, Port, 1).

connect(Host, Port, UnitId) ->
    gen_server:start_link(?MODULE, [Host, Port, UnitId], []).

disconnect(Device) ->
    call(Device, stop).

read_coils(Device, Offset, Count) ->
    call(Device, {?FUN_CODE_READ_COILS, {Offset, Count}}).

read_inputs(Device, Offset, Count) ->
    call(Device, {?FUN_CODE_READ_INPUTS, {Offset, Count}}).

read_hregs(Device, Offset, Count) ->
    call(Device, {?FUN_CODE_READ_HREGS, {Offset, Count}}).

read_iregs(Device, Offset, Count) ->
    call(Device, {?FUN_CODE_READ_IREGS, {Offset, Count}}).

write_coil(Device, Offset, Coil) ->
    call(Device, {?FUN_CODE_WRITE_COIL, {Offset, Coil}}).

write_hreg(Device, Offset, Value) ->
    call(Device, {?FUN_CODE_WRITE_HREG, {Offset, Value}}).

write_coils(Device, Offset, Coils) ->
    call(Device, {?FUN_CODE_WRITE_COILS, {Offset, Coils}}).

write_hregs(Device, Offset, Value) ->
    call(Device, {?FUN_CODE_WRITE_HREGS, {Offset, Value}}).

call(Device, Request) ->
    gen_server:call(Device, Request, ?CALL_TIMEOUT).

init([Host, Port, UnitId]) ->
    case gen_tcp:connect(Host, Port, ?SOCKOPTS, ?SOCK_TIMEOUT) of
        {ok, Sock} -> 
            {ok, #state{socket = Sock, unit_id = UnitId}};
        {error, Error} -> 
            {stop, {error, Error}}
    end.

handle_call({FunCode, Params}, _From, State) ->
    case catch request(FunCode, Params, State) of
        {'EXIT', Error} ->
            {reply, {error, Error}, next_id(State)};
        Response ->
            {reply, Response, next_id(State)}
    end;

handle_call(stop, _From, State=#state{socket = Sock}) ->
	gen_tcp:close(Sock),
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, {error, badreq}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request(?FUN_CODE_READ_COILS, {Offset, Count}, State) ->
    send_and_recv(?FUN_CODE_READ_COILS, <<Offset:16, Count:16>>, State);

request(?FUN_CODE_READ_INPUTS, {Offset, Count}, State) ->
    send_and_recv(?FUN_CODE_READ_INPUTS, <<Offset:16, Count:16>>, State);

request(?FUN_CODE_READ_HREGS, {Offset, Count}, State) ->
    send_and_recv(?FUN_CODE_READ_HREGS, <<Offset:16, Count:16>>, State);

request(?FUN_CODE_READ_IREGS, {Offset, Count}, State) ->
    send_and_recv(?FUN_CODE_READ_IREGS, <<Offset:16, Count:16>>, State);

request(?FUN_CODE_WRITE_COIL, {Offset, Coil}, State) ->
    send_and_recv(?FUN_CODE_WRITE_COIL, <<Offset:16, Coil:16>>, State);

request(?FUN_CODE_WRITE_HREG, {Offset, Value}, State) ->
    send_and_recv(?FUN_CODE_WRITE_HREG, <<Offset:16, Value:16>>, State);

request(?FUN_CODE_WRITE_COILS, {Offset, Coils}, State) ->
    {error, unsupported};

request(?FUN_CODE_WRITE_HREGS, {Offset, Values}, State) ->
    {error, unsupported};

request(_FunCode, _Params, _State) ->
    {error, unsupported}.

send_and_recv(FunCode, Payload, State) ->
    case send(FunCode, Payload, State) of
        ok ->
            case recv(FunCode, State) of
                {ok, Frame} ->
                    response(FunCode, Frame);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

response(?FUN_CODE_READ_COILS, Frame) ->
    ok;

response(?FUN_CODE_READ_COILS, Frame) ->
    ok.

response(?FUN_CODE_READ_INPUTS, Frame) ->
    
response(?FUN_CODE_READ_HREGS, Frame) ->

response(?FUN_CODE_READ_IREGS, Frame) ->

response(?FUN_CODE_WRITE_COIL, Frame) ->

response(?FUN_CODE_WRITE_HREG, Frame) ->


send(FunCode, Payload, #state{tid = Tid, unit_id = UnitId, socket = Sock}) ->
    Hdr= #mbap_header{tid = Tid, unit_id = UnitId},
    Frame = #modbus_frame{header = Hdr, funcode = FunCode, payload = Payload},
    gen_tcp:send(Sock, serialize(Frame)).

serialize(#modbus_frame{header = #mbap_header{tid = Tid, unit_id = Uid},
                        funcode = FunCode, payload = Payload}) ->
    Len = 2 + size(Payload),
    <<Tid:16, 0:16, Len:16, Uid:8, FunCode:8, Payload/binary>>.
    
recv(FunCode, State = #state{tid = Tid, socket = Sock}) ->
    case recv(header, FunCode, State) of
        {ok, Header} ->
            recv(payload, Header, State);
        {error,Error} ->
            {error,Error}
    end.

recv(header, _FunCode, State = #state{tid = Tid, socket = Sock}) ->
    case gen_tcp:recv(Sock, ?MBAP_LENGTH, ?RECV_TIMEOUT) of
        {ok, <<Tid:16, 0:16, Len:16, Uid:8>>} ->
            recv(payload, #mbap_header{tid = Tid, length = Len, unit_id = Uid}, State);
        {ok, Header} ->
            error_logger:error_msg("Response cannot match request: request tid=~p, response header =~p", [Tid, Header]),
            {error, badresp};
        {error, Reason} ->
            {error, Reason}
    end;

recv(payload, Header = #mbap_header{length = Len}, #state{socket = Sock}) ->
    case gen_tcp:recv(Sock, Len - 1, ?RECV_TIMEOUT) of
        {ok, <<FunCodeOrErr:8, Payload/binary>>} ->
            {ok, #modbus_frame{header = Header, funcode = FunCodeOrErr, payload = Payload}};
        {error, Reason} ->
            {error, Reason}
    end.

next_id(State = #state{tid = Tid}) when Tid >= 16#EFFF ->
    State#state{tid = 1};

next_id(State = #state{tid = Tid}) ->
    State#state{tid = Tid+1}.

