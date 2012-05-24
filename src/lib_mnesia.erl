%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : lib_mnesia.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Database access routines
%%%-------------------------------------------------------------------

-module(lib_mnesia).


%% -------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------

-include("./records.hrl").
-include("./dns.hrl").

-include_lib("stdlib/include/qlc.hrl").

-import(lib_string).


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([create/0,
         init/0,
         find_record/1,
         find_domain/1,
         insert_new_record/1,
         insert_new_domain/1,
         find_recurse_labels/1,
         find_domains_ending_in/1,
         find_record_by_domain/1,
         delete_record/1,
         delete_domain/1,
         print_record_info/0,
         populate/0]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

print_record_info() ->
    io:format("~p~n~p~n~p~n",[record_info(fields,domain),
                              record_info(fields,record),
                              record_info(fields,host)]).

create() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:delete_table(domain),
    mnesia:delete_table(record),
    mnesia:delete_table(host),
    mnesia:create_table(domain, [{attributes, record_info(fields,domain)},
                                 {disc_copies,[node()]}]),
    mnesia:create_table(record, [{attributes, record_info(fields,record)},
                                 {disc_copies,[node()]},
                                 {type,bag}]),

    mnesia:add_table_index(domain,name),
    mnesia:add_table_index(record,domainname).

populate() ->
    prepare_db().


init() ->
    mnesia:start().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INSERTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% DOMAINS

%% Input: A Domain record.
%% Output: none.
insert_new_domain(Domain) ->
    Name = Domain#domain.name,
    TTL = Domain#domain.ttl,
    F = fun() ->
                mnesia:write(#domain{name=Name,ttl=TTL})
        end,
    mnesia:transaction(F).


%%%% RECORDS

%% Input: A record record to be inserted in the datastore
%% Output: None
insert_new_record(#record{hostname = Hostname,
                          domainname = Domainname,
                          ttl=TTL,
                          type=Type,
                          rdata=Rdata}) ->
    F = fun() ->
                mnesia:write(#record{hostname=Hostname,
                                     domainname=Domainname,
                                     ttl=TTL,
                                     type=Type,
                                     rdata=Rdata})
        end,
    mnesia:transaction(F).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% QUERY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% DOMAINS

%% Input: Domain record with the Name to find the domain for.
%% Output: #domain record
find_domain(#domain{name=Name}) ->
    F = fun() ->
                mnesia:read(domain,Name)
        end,
    {atomic, Domains} = mnesia:transaction(F),

    case length(Domains) of
        0 ->
            {};
        _ ->
            lists:nth(1,Domains)
    end.


%% Input: Domain record where Name is the string the domain shall end in.
%% Output: List of matching domains.
find_domains_ending_in(#domain{name=Name}) ->
    F = fun() ->
                Q = qlc:q([E || E <- mnesia:table(domain),
                                lists:suffix(Name,E#domain.name)]),
                qlc:e(Q)
        end,
    {atomic, Domains} = mnesia:transaction(F),
    Domains.


%%%% RECORDS

%% Input: A #record record with at least hostname and type set.
%% Output: A #record record.
find_record(#record{
               hostname = Hostname,
               type = Type}) ->
    %% DEBUG
    %% io:format("The Hostname in find_record is: ~p~n",[Hostname]),
    F = fun() ->
                mnesia:read(record,Hostname)
        end,
    {atomic, Records} = mnesia:transaction(F),

    lists:filter(fun(X) ->
                         X#record.type =:= Type
                 end,Records).


%% This is a special case of find_record/1.
%% It will consecutively remove the first label of the record name, until
%% it finds a match or until no more labels are left.
%% This is useful to find NS entries for delegated domains.
find_recurse_labels(#record{
                      hostname = []}) ->
    [];
find_recurse_labels(Record = #record{
                      hostname = [_LastLabel|Labels],
                      type = Type}) ->
    case find_record(Record) of
        [] ->
            find_recurse_labels(#record{hostname=Labels, type=Type});
        FoundRecord ->
            FoundRecord
    end.


%% Input: A #record record with at least domainname set.
%% Output: A list of #record records.
find_record_by_domain(#record{domainname = Domainname}) ->
    %% DEBUG
    %io:format("The Domainname in find_record_by_domain is: ~p~n",[Domainname]),
    F = fun() ->
                mnesia:index_read(record,Domainname,#record.domainname)
        end,
    {atomic, Records} = mnesia:transaction(F),
    Records.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DELETION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%% DOMAINS

%% Input: Domain Record with at least Name set.
%% Behavior: Will delete the domain and all associated RRs.
delete_domain(#domain{name=Name}) ->
    G = fun() ->
                F = fun() ->
                            mnesia:index_read(record,Name,#record.domainname)
                    end,
                {atomic, Records} = mnesia:transaction(F),

                %% DEBUG
                %% io:format("the records we found are: ~p~n",[Records]),

                lists:foreach(fun mnesia:delete_object/1, Records),

                F1 = fun() ->
                             mnesia:read(domain,Name)
                     end,
                {atomic, Domain} = mnesia:transaction(F1),

                %% DEBUG
                %% io:format("the domain we found is: ~p~n",[Record]),

                lists:foreach(fun mnesia:delete_object/1, Domain)
        end,

    {atomic, _} = mnesia:transaction(G).



%%%% RECORDS

%% Input: A record record with at least Host and Type set.
%% Output: Mnesia Response.
%% Behaviour: All matching records will be deleted.
delete_record(R) ->
    G = fun() ->
                Recs = find_record(R),
                lists:foreach(fun mnesia:delete_object/1, Recs),
                io:format("the following has been deleted ~p~n",[Recs])
        end,

    mnesia:transaction(G).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DB Populating
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_db() ->

    %% domain with subdomain
    Domain1 = #domain{name = ["testdomain","com"],ttl=24},
    Domain2 = #domain{name = ["subdomain","testdomain","com"],ttl=24},
    insert_new_domain(Domain1),
    insert_new_domain(Domain2),

    %% SOA Record for top domain
    Record0 = #record{hostname = ["testdomain","com"],
                      domainname = ["testdomain","com"],
                      ttl = 3600,
                      type = ?T_SOA,
                      rdata = #soa{nsname="localhost",
                                   mail="hostmaster.testdomain.com",
                                   serial=1234,
                                   refresh=300,
                                   retry=600,
                                   expire=2419200,
                                   minimum=3600}},
    insert_new_record(Record0),

    %% SOA Record for subdomain
    Record0a = #record{hostname = ["subdomain","testdomain","com"],
                       domainname = ["subdomain","testdomain","com"],
                       ttl = 3600,
                       type = ?T_SOA,
                       rdata = #soa{nsname="localhost",
                                    mail="hostmaster.testdomain.com",
                                    serial=1234,
                                    refresh=300,
                                    retry=600,
                                    expire=2419200,
                                    minimum=3600}},
    insert_new_record(Record0a),



    %% two NS servers for the delegated subdomain
    Record1 = #record{hostname = ["delegated","testdomain","com"],
                      domainname = ["testdomain","com"],
                      ttl = 3600,
                      type = ?T_NS,
                      rdata = #ns{nsname="ns1.delegated.testdomain.com"}},
    insert_new_record(Record1),

    Record2 = #record{hostname = ["delegated","testdomain","com"],
                      domainname = ["testdomain","com"],
                      ttl = 3600,
                      type = ?T_NS,
                      rdata = #ns{nsname="ns2.delegated.testdomain.com"}},
    insert_new_record(Record2),


    %% one glue entry for the NS for the delegated subdomain
    Record4 = #record{hostname = ["ns1","delegated","testdomain","com"],
                      domainname = ["testdomain","com"],
                      ttl = 3600,
                      type = ?T_A,
                      rdata = #a{ip1=127,
                                 ip2=0,
                                 ip3=0,
                                 ip4=1}},
    insert_new_record(Record4),


    %% one entry for the authoritative subdomain
    Record5 = #record{hostname = ["test","subdomain","testdomain","com"],
                      domainname = ["subdomain","testdomain","com"],
                      ttl = 3600,
                      type = ?T_A,
                      rdata = #a{ip1=127,
                                 ip2=0,
                                 ip3=0,
                                 ip4=1}},
    insert_new_record(Record5).
