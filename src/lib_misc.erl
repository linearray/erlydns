%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : lib_misc.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Miscellaneous functions
%%%-------------------------------------------------------------------

-module(lib_misc).


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([rpc/2,
         for/3]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

%% sends a Request to a Pid, and waits for a reply.
rpc(Pid,Request) ->
    Pid ! {self(),Request},
    receive
        {Pid,Response} ->
            Response
    end.


for(Max,Max,F) -> [F(Max)];
for(I,Max,F) -> [F(I)|for(I+1,Max,F)].
