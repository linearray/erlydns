%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : records.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Record definitions
%%%-------------------------------------------------------------------

%% These records are primarily used to construct replies
-record(header,
        {id,
         isResponse,
         querykind,
         isAuthoritative,
         isTruncated,
         isRecursionDesired,
         isRecursionAvailable,
         responseCode,
         numberQuestionEntries,
         numberAnswerRRs,
         numberAuthorityRRs,
         numberAdditionalRRs}).

-record(question,
        {name,
         type,
         class}).

-record(aaa,
        {name,
         type,
         class=1,                        % maybe a more beautiful way?
         ttl,
         data}).

-record(message,
        {header,
         question,
         answer,
         authority,
         additional}).



%% These records are used to keep track of labels in queries
-record(suffix,
        {pos,                 % position in packet
         domainname}).

-record(labelpos,
        {pos,
         length,
         label}).


%% These records are used to store things in Mnesia
-record(domain,
        {name,                % domain name in non-inverted labels
         ttl}).               % Default TTL for all records in this domain.

-record(host,                 % this record is used for AXFR control
        {domainname,
         ip}).

-record(record,
        {hostname,
         domainname,
         ttl,
         type,
         rdata}).


-record(soa,
        {nsname,
         mail,
         serial,
         refresh,
         retry,
         expire,
         minimum}).

-record(a,
        {ip1,
         ip2,
         ip3,
         ip4}).

-record(ns,
        {nsname}).

-record(mx,
        {pref,
         exchange}).
