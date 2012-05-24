%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : dns.hrl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Macros for some DNS constants.
%%%-------------------------------------------------------------------

%% Query kinds
-define(QUERY, 16#0).                 %% standard query
-define(IQUERY,16#1).                 %% inverse query
-define(STATUS,16#2).                 %% nameserver status query

%% Response codes
-define(NOERROR,  0).                 %% no error
-define(FORMERR,  1).                 %% format error
-define(SERVFAIL, 2).                 %% server failure
-define(NXDOMAIN, 3).                 %% non-existent domain
-define(NOTIMP,	  4).                 %% not implemented
-define(REFUSED,  5).                 %% query refused

%% Resource Records
-define(T_A,      1).                 %% host address
-define(T_NS,     2).                 %% authoritative name server
-define(T_MD,     3).                 %% mail destination
-define(T_MF,     4).                 %% mail forwarder
-define(T_CNAME,  5).                 %% canonical name (alias)
-define(T_SOA,    6).                 %% start of zone of authority
-define(T_MB,     7).                 %% mailbox domain name
-define(T_MG,     8).                 %% mail group member
-define(T_MR,     9).                 %% mail rename domain name
-define(T_NULL,  10).                 %% null resource record
-define(T_WKS,   11).                 %% well known service description
-define(T_PTR,   12).                 %% domain name pointer
-define(T_HINFO, 13).                 %% host information
-define(T_MINFO, 14).                 %% mailbox information
-define(T_MX,    15).                 %% mail routing information
-define(T_TXT,   16).                 %% text strings

-define(T_AXFR, 252).                 %% transfer zone of authority
-define(T_MAILB,253).                 %% transfer mailbox records
-define(T_MAILA,254).                 %% transfer mail agent records
-define(T_ANY,  255).                 %% wildcard match
