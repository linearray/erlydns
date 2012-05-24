%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : suffix_server.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Serves DNS Compression
%%%-------------------------------------------------------------------

-module(suffix_server).


%% -------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------

-import(lib_misc).


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([start/0]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

loop(Dict) ->
    receive
        {Peer,{get,Offset}} ->
            [{Offset,DN}] = get_DN(Dict,Offset),
%%            io:format("encountered compression.~n"),
            Peer ! {self(),{dn,DN}},
            loop(Dict);

        {Peer,{set,{Offset,DN}}} ->
             NewDict = set(Dict,{Offset,DN}),
             Peer ! {self(),ok},
             loop(NewDict);

        {Peer,{terminate}} ->
            Peer ! {self(),ok};

        _Other ->
            error,
            loop(Dict)
    end.


start() ->
    Dict = dict:new(),
    loop(Dict).


set(Dict,{Offset,DN}) ->
    dict:store(Offset,DN,Dict).


get_DN(Dict,Offset) ->
    dict:fetch(Offset,Dict).
