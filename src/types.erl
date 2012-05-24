%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : types.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Defines all DNS Types used
%%%-------------------------------------------------------------------

-module(types).


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([type_numeral_to_atom/1,
	 type_atom_to_numeral/1]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

type_numeral_to_atom(1) ->
    a;
type_numeral_to_atom(2) ->
    ns;
type_numeral_to_atom(6) ->
    soa;
type_numeral_to_atom(15) ->
    mx;
type_numeral_to_atom(252) ->
    axfr;
type_numeral_to_atom(255) ->
    any.

type_atom_to_numeral(a) ->
    1;
type_atom_to_numeral(ns) ->
    2;
type_atom_to_numeral(soa) ->
    6;
type_atom_to_numeral(mx) ->
    15;
type_atom_to_numeral(axfr) ->
    252;
type_atom_to_numeral(any) ->
    255.
