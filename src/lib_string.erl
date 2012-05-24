%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : lib_string.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : A library for string manipulation
%%%-------------------------------------------------------------------

-module(lib_string).


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([domainname_to_string/1,
         string_to_binary/1]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

%% Input: String denoting a domain name (with or without trailing period).
%% Output: Binary containing a DNS Domainname.
string_to_binary(String) ->
    domainname_to_binary(string_to_domainname(String)).


%% Input: String denoting a domain name (with or without trailing period).
%% Output: List containing tuples of the form {Length,String}.
string_to_domainname(String) ->
    Tokens = string:tokens(String,"."),
    Lengths = lists:map(fun length/1,Tokens),
    I = lists:zip(Lengths,Tokens),
    lists:append(I,[{0,""}]).


%% FIXME This function does not understand message compression (yet)!
%% Input: List containing tuples of the form {Length,String}.
%% Output: String denoting a domain name (with trailing period).
domainname_to_string(DN) ->
    lists:flatmap(fun({_Length,Label}) -> lists:append(Label,".") end,DN).


%% Input: List containing tuples of the form {Length,String}.
%% Output: Binary containing a DNS Domainname.
domainname_to_binary(L) ->
    domainname_to_binary(L,[]).

domainname_to_binary([],Acc) ->
    list_to_binary(lists:reverse(Acc));
domainname_to_binary([{Len,Label}|Rest],Acc) ->
    domainname_to_binary(Rest,[[<<Len:8>>,list_to_binary(Label)]|Acc]).
