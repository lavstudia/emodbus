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
-export([start/0, start/1,
         start_link/0, start_link/1,
         stop/1]).

%% modbus API Exports
-export([read_coils/3,
         read_inputs/3,
         read_hregs/3,
         read_iregs/3,
         write_coil/3,
         write_coils/3,
         write_hregs/3]).

-behaviour(gen_server).

%% gen_server Function Exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(MBAP_LENGTH, 7).

-define(CALL_TIMEOUT, 60000).

-define(SOCK_TIMEOUT, 30000).

-define(RECV_TIMEOUT, 10000).

%% Modbus Socket Options
-define(SOCKOPTS, [
        binary,
        {packet,    raw},
        {reuseaddr, true},
        {nodelay,   true}
]).

-type option() :: {host, inet:ip_address() | string()}
                | {port, inet:port_number()}
                | {logger, atom() | {atom(), atom()}
                | {unit_id, byte()}}.

-record(?MODULE, {name                :: atom(),
                  host = "localhost"  :: inet:ip_address() | string(),
                  port = ?MODBUS_PORT :: inet:port_number(),
                  socket              :: inet:socket(),
                  timeout             :: pos_integer(),
                  tid = 1             :: 1..16#ff,
                  unit_id = 1         :: byte(),
                  parse_state,
                  logger              :: gen_logger:logmod()}).

start() ->
    start([]).

-spec start(list(option())) -> {ok, pid()} | {error, any()}.
start(Opts) ->
    gen_server:start(?MODBUS_PORT, [Opts], []).

start_link() ->
    start_link([]).

-spec start_link(list(option())) -> {ok, pid()} | {error, any()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, [Opts], []).

read_coils(Device, Offset, Count) ->
    call(Device, {?FUN_CODE_READ_COILS, {Offset, Count}}).

read_inputs(Device, Offset, Count) ->
    call(Device, {?FUN_CODE_READ_INPUTS, {Offset, Count}}).

read_iregs(Device, Offset, Count) ->
    call(Device, {?FUN_CODE_READ_IREGS, {Offset, Count}}).

read_hregs(Device, Offset, Count) ->
    call(Device, {?FUN_CODE_READ_HREGS, {Offset, Count}}).

write_coil(Device, Offset, Coil) ->
    call(Device, {?FUN_CODE_WRITE_COIL, {Offset, Coil}}).

write_coils(Device, Offset, Coils) ->
    call(Device, {?FUN_CODE_WRITE_COILS, {Offset, Coils}}).

write_hregs(Device, Offset, Value) ->
    call(Device, {?FUN_CODE_WRITE_HREGS, {Offset, Value}}).

stop(Device) ->
    call(Device, stop).

call(Device, Request) ->
    gen_server:call(Device, Request, ?CALL_TIMEOUT).

init([Opts]) ->
    Logger = gen_logger:new(proplists:get_value(logger, Opts, {console, debug})),

    Opts1 = proplists:delete(logger, Opts),

    State = init(Opts1, #?MODULE{host    = "localhost",
                                 port    = ?MODBUS_PORT,
                                 logger  = Logger,
                                 unit_id = 1}),

    case connect(State) of
        {ok, Sock} -> 
            {ok, State#?MODULE{socket = Sock}};
        {error, Error} ->
            {stop, {error, Error}}
    end.

init([], State) ->
    State;
init([{host, Host} | Opts], State) ->
    init(Opts, State#?MODULE{host = Host});
init([{port, Port} | Opts], State) ->
    init(Opts, State#?MODULE{port = Port});
init([{logger, Cfg} | Opts], State) ->
    init(Opts, State#?MODULE{logger = gen_logger:new(Cfg)});
init([{unit_id, Id} | Opts], State) ->
    init(Opts, State#?MODULE{unit_id = Id});
init([_Opt | Opts], State) ->
    init(Opts, State).

connect(#?MODULE{host = Host, port = Port, timeout = Timeout}) ->
    case gen_tcp:connect(Host, Port, ?SOCKOPTS, Timeout) of
        {ok, Sock}     -> {ok, Sock}; 
        {error, Error} -> {error, Error}
    end.

handle_call(Req = {FunCode, Params}, From, State) ->
    Resp = catch request(FunCode, Params, State),
    {reply, Resp, next_id(State)};

handle_call(stop, _From, State=#state{socket = Sock}) ->
	gen_tcp:close(Sock),
	{stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request(?FUN_CODE_READ_COILS, {Offset, Count}, State) ->
    Payload = <<Offset:16, Count:16>>,
    send(?FUN_CODE_READ_COILS, Payload, State),
    case recv(?FUN_CODE_READ_COILS, State)) of
        {ok, Frame} ->
            response(?FUN_CODE_READ_COILS, Frame);
        {error, Erro} ->
            {error, Error}
    end.

request(FunCode, Params, State) ->
    {error, unsupported}.

send(FunCode, Payload, #?MODULE{tid = Tid, unit_id = UnitId, socket = Sock}) ->
    Hdr= #mbap_header{tid = Tid, unit_id = UnitId},
    Frame = #modbus_frame{header = Hdr, funcode = FunCode, payload = Payload},
    gen_tcp:send(Sock, emodbus_frame:serialize(Frame)).
    
recv(FunCode, State = #?MODULE{tid = Tid, socket = Sock}) ->
    case recv(header, FunCode, State) of
        {ok, Header} ->
            recv(payload, Header, State);
        {error,Error} ->
            {error,Error}
    end.

recv(header, _FunCode, State = #?MODULE{tid = Tid, socket = Sock}) ->
    case gen_tcp:recv(Sock, ?MBAP_LENGTH, ?RECV_TIMEOUT) of
        {ok, <<Tid:16, 0:16, Len:16, Uid:8>>} ->
            recv(payload, #mbap_header{tid = Tid, length = Len, unit_id = Uid}, State);
        {ok, Header} ->
            error_logger:error_msg("Response cannot match request: request tid=~p, response header =~p", [Tid, Header]),
            {error, badresp};
        {error, Reason} ->
            {error, Reason}
    end;

recv(payload, Header = #mbap_header{length = Len}, #?MODULE{socket = Sock}) ->
    case gen_tcp:recv(Sock, Len - 1, ?RECV_TIMEOUT) of
        {ok, <<FunCodeOrErr:8, Payload/binary>>} ->
            {ok, #modbus_frame{header = Header, funcode = FunCodeOrErr, payload = Payload}};
        {error, Reason} ->
            {error, Reason}
    end.


send(PDU, #state{tid = Tid, unit_id = Uid, socket = Sock}) ->

received(<<>>, State) ->
    {noreply, State, hibernate};

received(Bytes, State = #state{parse_state = ParseState}) ->
    case emodbus_frame:parse(Bytes, ParseState) of
        {more, ParseState1} ->
            {noreply, run_sock(State#state{parse_state = ParseState1}), hibernate};
        {ok, Frame, Rest} ->
            {ok, NewState} = handle_frame(Frame, State),
            received(Rest, NewState#state{parse_state = emodbus_frame:init(client)});
        {error, Error} ->
            {stop, {shutdown, Error}, State}
    end.

handle_frame(Frame = #modbus_frame{hdr = #mbap_header{tid = Tid},
                                   pdu = Resp}, State = #state{pending_reqs = Reqs}) ->
    io:format("RECV Frame: ~p~n", [Frame]),
    case gb_trees:lookup(Tid, Reqs) of
        {value, {From, _Req}} ->
            gen_server:reply(From, {ok, Resp#modbus_resp.bytes}),
            {ok, State#state{pending_reqs = gb_trees:delete(Tid, Reqs)}};
        none ->
            {ok, State}
    end.

run_sock(State=#state{socket = Sock}) ->
    inet:setopts(Sock, [{active, once}]), State.

next_id(State = #state{tid = Tid}) when Tid >= 16#EFFF ->
    State#state{tid = 1};

next_id(State = #state{tid = Tid}) ->
    State#state{tid = Tid+1}.




