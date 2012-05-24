%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : server.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Main file
%%%-------------------------------------------------------------------

-module(server).


%% -------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------

-include("./records.hrl").


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([start/1]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

start([Port]) ->
    io:format("Port is: ~p~n",[Port]),
    server(list_to_integer(atom_to_list(Port))).

server(Port) ->
    {MegaSec,Sec,MicroSec} = erlang:now(),
    random:seed(MegaSec,Sec,MicroSec),

    process_flag(trap_exit,true),
    lib_mnesia:init(),
    {ok,Socket} = gen_udp:open(Port,[binary]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp,Socket,Host,Port,Bin} ->
            Pid = spawn_link(fun() -> queryhandler:loop() end),
            Pid ! {dnsquery,Socket,Host,Port,Bin},
            loop(Socket);
        {'EXIT',_Pid,normal} ->
            ok,
            loop(Socket);
        {'EXIT',Pid,Reason} ->
            io:format("~p crashed with ~p.~n",[Pid,Reason]),
            loop(Socket)
    end.
