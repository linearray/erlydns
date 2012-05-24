%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : erlydnsadmin.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Admin interface for erlydns
%%%-------------------------------------------------------------------

-module(erlydnsadmin).


%% -------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------

-include("./records.hrl").
-include("./dns.hrl").

-import(types).


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([loop/0]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

loop() ->
    licence(),
    mainhelp(),
    case io:get_line('> ') of
        eof ->
            loop();

        Cmd ->
            Cmd2 = clean(Cmd),

            if
                Cmd2 =:= "help" ->
                    mainhelp(),
                    loop();
                Cmd2 =:= "exit" ->
                    io:format("bye~n");
                Cmd2 =:= "domains" ->
                    domains(),
                    loop();
                Cmd2 =:= "records" ->
                    records(),
                    loop();
                true ->
                    loop()
            end
    end.


mainhelp() ->
    io:format("~n"),
    io:format(" -------------------------------------- ~n"),
    io:format("| domains - manage domains (aka zones) |~n"),
    io:format("| records - manage records (aka RRs)   |~n"),
    io:format(" -------------------------------------- ~n"),
    io:format("| exit    - exit this program          |~n"),
    io:format(" -------------------------------------- ~n~n").

licence() ->
    io:format("======================================================================~n"),
    io:format("Copyright 2008-2012 Max Amanshauser.~n~n"),

    io:format("erlydns comes without any warranty and is licensed under the~n"),
    io:format("Simplified BSD License; see the file named LICENSE for details.~n"),
    io:format("======================================================================~n~n").


%%      _                       _
%%   __| | ___  _ __ ___   __ _(_)_ __  ___
%%  / _` |/ _ \| '_ ` _ \ / _` | | '_ \/ __|
%% | (_| | (_) | | | | | | (_| | | | | \__ \
%%  \__,_|\___/|_| |_| |_|\__,_|_|_| |_|___/

domains() ->
    domainhelp(),

    case io:get_line('> ') of
        eof ->
            loop();

        Cmd ->
            Cmd2 = clean(Cmd),

            if
                Cmd2 =:= "help" ->
                    domainhelp(),
                    domains();

                Cmd2 =:= "back" ->
                    io:format("ok~n");

                Cmd2 =:= "show" ->
                    domainshow(),
                    domains();

                Cmd2 =:= "del" ->
                    domaindel(),
                    domains();

                Cmd2 =:= "add" ->
                    domainadd(),
                    domains();

                true ->
                    domains()
            end
    end.


domainshow() ->
    Suffix = read_default("Suffix","."),
    DomainList = lib_mnesia:find_domains_ending_in(
                   #domain{name=string:tokens(Suffix,".")}),

    enum_domains(DomainList).


domaindel() ->
    Domain = read("Domain: "),      % empty value?
    lib_mnesia:delete_domain(#domain{name=string:tokens(Domain,".")}).


domainadd() ->
    Domain = clean(io:get_line('Domain: ')),   % empty?
    DN = string:tokens(Domain,"."),
    case lib_mnesia:find_domain(#domain{name=DN}) of
        {} ->
            TTL = read_integer_default_stubborn("Default TTL",3600),
            lib_mnesia:insert_new_domain(#domain{name=DN,ttl=TTL}),
            SOA = read_default("Add SOA entry now?","y"),

            if
                SOA =:= "y" ->
                    Rdata = get_rdata(soa),

                    lib_mnesia:insert_new_record(
                      #record{hostname=DN,
                              domainname=DN,
                              ttl=TTL,
                              type=?T_SOA,
                              rdata=Rdata});

                true ->
                    ok

            end;

        _ ->
            io:format("Domain already exists!~n")
    end.


domainhelp() ->
    io:format("~n"),
    io:format(" ----------------------------------------------- ~n"),
    io:format("| show - search and show existing domains       |~n"),
    io:format("| add  - add a new domain                       |~n"),
    io:format("| del  - delete a domain and all associated RRs |~n"),
    io:format(" ----------------------------------------------- ~n"),
    io:format("| back - go back to the main menu               |~n"),
    io:format(" ----------------------------------------------- ~n~n").


%%                             _
%%  _ __ ___  ___ ___  _ __ __| |___
%% | '__/ _ \/ __/ _ \| '__/ _` / __|
%% | | |  __/ (_| (_) | | | (_| \__ \
%% |_|  \___|\___\___/|_|  \__,_|___/

records() ->
    recordhelp(),

    case io:get_line('> ') of
        eof ->
            loop();

        Cmd ->
            Cmd2 = clean(Cmd),

            if
                Cmd2 =:= "help" ->
                    recordhelp(),
                    records();

                Cmd2 =:= "back" ->
                    io:format("ok~n");

                Cmd2 =:= "show" ->
                    recordshow(),
                    records();

                Cmd2 =:= "del" ->
                    recorddel(),
                    records();

                Cmd2 =:= "add" ->
                    recordadd(),
                    records();

                true ->
                    records()
            end
    end.


recordshow() ->
    Domain = read("Domain: "),
    case Domain of
        [] ->
            ok;
        _ ->

            RecordList = lib_mnesia:find_record_by_domain(
                           #record{domainname=string:tokens(Domain,".")}),

            case RecordList of
                [] ->
                    ok;
                _ ->
                    enum_records(RecordList)
            end
    end.


recorddel() ->
    Domain = read("Record: "),
    case Domain of
        [] ->
            recorddel();
        _ ->
            Type = read_type(),

            Test = lib_mnesia:delete_record(
                     #record{hostname=string:tokens(Domain,"."),type=Type}),
            io:format("~p~n",[Test])
    end.


%%
%% INPUT HELPERS

recordadd() ->
    Record = read("Record: "),
    case Record of
        [] ->
            ok;
        _ ->
            DN = string:tokens(Record,"."),     % find a suitable domain entry, this will
                                                % always select the most specific domain. 
            case lib_mnesia:find_recurse_labels(
                   #record{hostname=DN,
                           type=?T_SOA}) of
                [] ->
                    io:format("No suitable domain for this record found! "),
                    io:format("Please create it first!~n");

                SOARecord ->
                    [#record{hostname=Domain,
                            ttl = TTL}] = SOARecord,

                    case lib_mnesia:find_domain(#domain{name=Domain}) of
                        [] ->
                            erlang:error("SOA found, but no domain. Inconsistent database.");
                        DomainRec ->
                            #domain{ttl=DefaultTTL} = DomainRec,

                            io:format("Auto-selected domain ~p~n",[Domain]),

                            Type = read_type(),

                            Rdata = get_rdata(types:type_numeral_to_atom(Type)),

                            TTL2 = read_ttl(DefaultTTL,TTL), % works?

                            lib_mnesia:insert_new_record(
                              #record{hostname=
                                      string:tokens(Record,"."),
                                      domainname=Domain,
                                      ttl=TTL2,
                                      type=Type,
                                      rdata=Rdata})
                    end
            end
    end.


recordhelp() ->
    io:format("~n"),
    io:format(" ----------------------------------------------- ~n"),
    io:format("| show - search and show existing records       |~n"),
    io:format("| add  - add a new record                       |~n"),
    io:format("| del  - delete a record                        |~n"),
    io:format(" ----------------------------------------------- ~n"),
    io:format("| back - go back to the main menu               |~n"),
    io:format(" ----------------------------------------------- ~n~n").


get_rdata(a) ->
    IP = read_stubborn("IP (e.g. 127.0.0.1): "),
    IpList = string:tokens(IP,"."),
    case length(IpList) of
        4 ->                     % theoretically the numbers in the IP
                                 % could be > 255, but if the user
                                 % really hates us so much, he deserves it.
            [IP1,IP2,IP3,IP4] = lists:map(fun get_rdata_a_list_to_integer/1,
                                          IpList),

            #a{ip1=IP1,ip2=IP2,ip3=IP3,ip4=IP4};
        _ ->
            get_rdata_error(a)
    end;

%% Sanity checks on hostnames are difficult and don't appear to be worthwhile.
get_rdata(ns) ->
    NSName = read_stubborn("NS Server (e.g. ns.foo.org): "),
    #ns{nsname=NSName};

get_rdata(mx) ->
    PrefStr = read_stubborn("Preference value (MX with _lowest_ preference value is where the first delivery attempt is made, if it fails the next one is used, etc.): "),
    Pref = get_rdata_mx_list_to_integer(PrefStr),
    MX = read_stubborn("MX host (e.g. mail.foo.com): "),
    #mx{pref=Pref,exchange=MX};

get_rdata(soa) ->
    NSName = read_stubborn("Hostname of an authoritative nameserver for this domain
(most likely you want the hostname of THIS computer, e.g. ns.domain.com): "),
    Mail = read_stubborn(lists:flatten(["Email Address of the person responsible for this domain (e.g. hostmaster@domain.com): "])),
    RNAME = string:join(string:tokens(Mail,"@"),"."),    % no validation yet

    Serial = get_serial(0),
    Refresh = read_integer_default_stubborn("Refresh",10800),
    Retry = read_integer_default_stubborn("Retry", 3600),
    Expire = read_integer_default_stubborn("Expire", 3600000),
    Minimum = read_integer_default_stubborn("Minimum (do not set this higher than the default TTL of the domain)", 3600),

    #soa{nsname=NSName,
         mail=RNAME,
         serial=Serial,
         refresh=Refresh,
         retry=Retry,
         expire=Expire,
         minimum=Minimum}.


get_rdata_error(a) ->
    io:format("Not a valid IP address.~n"),
    get_rdata(a);

get_rdata_error(mx) ->
    io:format("Not a valid Preference value.~n"),
    get_rdata(mx).

get_rdata_a_list_to_integer(String) ->
    try list_to_integer(String)
    catch
        error:_ ->
            get_rdata_error(a)
    end.

get_rdata_mx_list_to_integer(String) ->
    try list_to_integer(String)
    catch
        error:_ ->
            get_rdata_error(mx)
    end.


%%  _          _
%% | |__   ___| |_ __   ___ _ __ ___
%% | '_ \ / _ \ | '_ \ / _ \ '__/ __|
%% | | | |  __/ | |_) |  __/ |  \__ \
%% |_| |_|\___|_| .__/ \___|_|  |___/
%%              |_|

enum_domains([]) ->
    [];
enum_domains([Domain|DomainList]) ->

    {_,Labels,TTL} = Domain,


    io:format("Domain: ~p~n",[string:join(Labels,".")]),
    io:format("TTL: ~p~n~n",[integer_to_list(TTL)]),

    enum_domains(DomainList).


enum_records([]) ->
    ok;
enum_records([Record|RecordList]) ->

    {record,RecordName, DomainName, TTL, Type, Rest} = Record,

    io:format("Record: ~p~n", [string:join(RecordName,".")]),
    io:format("Domain: ~p~n", [string:join(DomainName,".")]),
    io:format("Type: ~p~n", [atom_to_list(types:type_numeral_to_atom(Type))]),
    io:format("TTL: ~p~n", [integer_to_list(TTL)]),
    io:format("RData: ~p~n~n",[Rest]),

    enum_records(RecordList).


% This returns a type numeral
read_type() ->
    Type = string:to_lower(read_default("Type","a")),

    Type2 = list_to_atom(Type),

    try types:type_atom_to_numeral(Type2)
    catch
        _:_ ->
            io:format("This type is unknown or not supported.~n"),
            read_type()
    end.


clean(Data) ->
    string:strip(Data, both, $\n).


read(Prompt) ->
    clean(io:get_line(Prompt)).


%% Input: String, String
%% Output: String
read_default(Prompt,Default) ->

    DisplayString = lists:flatten([Prompt,
                                   " [",
                                   Default,
                                   "]: "]),
    case clean(io:get_line(DisplayString)) of
        [] ->
            Default;
        Response ->
            Response
    end.


%% Input: String, String
%% Output: String
read_stubborn(Prompt) ->
    case clean(io:get_line(Prompt)) of
        [] ->
            read_stubborn(Prompt);
        Response ->
            Response
    end.


%% Input: (String, Integer)
%% Output: Integer
read_integer_default_stubborn(Prompt,Default) ->

    try list_to_integer(read_default(Prompt,integer_to_list(Default)))
    catch
        _:_ ->
            io:format("Not a number.~n"),
            read_integer_default_stubborn(Prompt,Default)
    end.


%% Input: Integer, Integer
%% Output: Integer
read_ttl(DefaultTTL, Min) ->

    ReadTTL = read_integer_default_stubborn("TTL",Min), % works?

    case (ReadTTL < Min) of
        true ->
            io:format("TTL is lower than the minimum for this Domain.~n"),
            read_ttl(DefaultTTL, Min);
        false ->
            ReadTTL
    end.


%%
%% SERIALS
get_serial(OldSerial) ->
    {Date,_Time} = calendar:universal_time(),
    {Year,Month,Day} = Date,

    NYear = lists:flatten(io_lib:format("~4.10.0B",[Year])),
    NMonth = lists:flatten(io_lib:format("~2.10.0B",[Month])),
    NDay = lists:flatten(io_lib:format("~2.10.0B",[Day])),

    SerialStr = lists:flatten([NYear,NMonth,NDay,"00"]),

    compare_serial(list_to_integer(SerialStr),OldSerial).


compare_serial(ComputedSerial,OriginalSerial) ->
    if
        ComputedSerial > OriginalSerial ->
            ComputedSerial;
        true ->
            OriginalSerial + 1
    end.
