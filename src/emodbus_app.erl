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
%%% emodbus app.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(emodbus_app).

-include("emodbus.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = emodbus_sup:start_link(),
    case application:get_env(server) of
        {ok, ServerOpts} ->
            start_server(ServerOpts);
        undefined ->
            ok
    end,
    {ok, Sup}.

start_server(ServerOpts) ->
    Handler = proplists:get_value(handler, ServerOpts),
    {Port, Opts} = proplists:get_value(listener, ServerOpts),
    esockd:open(modbus, Port, merge_sockopts(Opts),
                    {emodbus_server, start_link, [Handler]}).

merge_sockopts(Options) ->
    SockOpts = merge_opts(?MODBUS_SOCKOPTS,
                          proplists:get_value(sockopts, Options, [])),
    merge_opts(Options, [{sockopts, SockOpts}]).

%% Merge Options
merge_opts(Defaults, Options) ->
    lists:foldl(
        fun({Opt, Val}, Acc) ->
                case lists:keymember(Opt, 1, Acc) of
                    true ->
                        lists:keyreplace(Opt, 1, Acc, {Opt, Val});
                    false ->
                        [{Opt, Val}|Acc]
                end;
            (Opt, Acc) ->
                case lists:member(Opt, Acc) of
                    true -> Acc;
                    false -> [Opt | Acc]
                end
        end, Defaults, Options).

stop(_State) ->
    ok.

