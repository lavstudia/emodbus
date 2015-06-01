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
-module(emodbus_frame_test).

-ifdef(TEST).

-include("emodbus.hrl").

-include_lib("eunit/include/eunit.hrl").

-import(emodbus_frame, [init/0, parse/2]).

parse_test() ->

    parse(<<0,1,0,0,0,6,1,1,0,10,0,10>>, init(server)),

    ?assertMatch({more, _Cont}, parse(<<01, 16#5B, 00, 00, 00>>, init())),

    ?assertMatch({ok, #modbus_frame{hdr = #mbap_header{tid = 16#15B, length = 6},
                                    pdu = #modbus_pdu{opcode = 01}}, <<>>}, 
                 begin
                     {more, Cont} = parse(<<01, 16#5B, 00, 00, 00>>, init()),
                     parse(<<06, 01, 01, 00, 00, 00, 16#0A>>, Cont)
                 end),
    
    ?assertMatch({ok, #modbus_frame{hdr = #mbap_header{tid = 16#15B, length = 6},
                                    pdu = #modbus_pdu{opcode = 01}}, <<>>},
                parse(<<01, 16#5B, 00, 00, 00, 06, 01, 01, 00, 00, 00, 16#0A>>, init())),
    ?assertMatch({ok, #modbus_frame{hdr = #mbap_header{tid = 16#15B, length = 5},
                                    pdu = #modbus_pdu{opcode = 01}}, <<>>},
                parse(<<01, 16#5B, 00, 00, 00, 05, 01, 01, 02, 16#7F, 00>>, init())),
    ?assertMatch({ok, #modbus_frame{hdr = #mbap_header{tid = 16#34, length = 6},
                                    pdu = #modbus_pdu{opcode = 3}}, <<>>},
                parse(<<00, 16#34, 00, 00, 00, 06, 01, 03, 00, 00, 00, 16#0A>>, init())),

    ?assertMatch({ok, #modbus_frame{hdr = #mbap_header{tid = 16#34, length = 17},
                                    pdu = #modbus_pdu{opcode = 3}}, <<0,0,0,0,0,0>>},
                parse(<<00, 16#34, 00, 00, 00, 17, 01, 03, 14, 00, 01, 00, 02, 00, 03, 00, 04, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00>>, init())).

-endif.


    %?assertMatch({ok, #modbus_frame{hdr = #mbap_header{tid = 16#34, length = 17},
    %                                pdu = #modbus_pdu{opcode = 3}}, <<>>},
    %             begin
    %                 {more, Cont} = parse(<<00, 16#34, 00, 00, 00, 17, 01, 03, 14, 00, 01>>, init()),
    %                 parse(<<00, 02, 00, 03, 00, 04, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00, 00>>, Cont)
    %             end),
