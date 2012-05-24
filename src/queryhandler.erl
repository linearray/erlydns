%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : queryhandler.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Handles one single request
%%%-------------------------------------------------------------------

-module(queryhandler).


%% -------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------

-import(lib_misc).

-include("./records.hrl").
-include("./dns.hrl").


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([loop/0]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

loop() ->
    receive
        {dnsquery,Socket,Host,Port,Bin} ->
            SS = spawn_link(fun() -> suffix_server:start() end),

            Message = analyze_query(Bin,SS),
            Result = magic(Message),          % magic takes a look at the
                                              % message and finds out what the
                                              % client really wants from us.

            %% DEBUG
            %% io:format("Magic Result: ~p~n",[Result]),
            reply(Socket,Host,Port,Result),
            lib_misc:rpc(SS,{terminate})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% QUERY ANALYZER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% takes a dns packet and returns the input message
analyze_query(PacketBin,SS) ->
    {<<HeadBin/binary>>,
     <<BodyBin/binary>>} = split_binary(PacketBin,12),

%% DEBUG
%% io:format("HeadBin is: ~p~nBodyBin is: ~p~n",[HeadBin,BodyBin]),

    Header = #header{numberQuestionEntries=QDCOUNT} =
        lib_packet:headerBin_to_header(HeadBin),
    #message{header=Header,
             question=lib_packet:questionsectionBin_to_question(QDCOUNT,
                                                                BodyBin,SS),
             answer=[],
             authority=[],
             additional=[]}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MAGIC - Looks at the question and finds answers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

magic(Message = #message{header=#header{querykind=?QUERY},
                         question=#question{name=Name,
                                            type=Type,
                                            class=_Class}}) ->

    %% exact match?
    case lib_mnesia:find_record(#record{hostname=string:tokens(Name,"."),
                                        type=Type}) of

        %% not found. authoritative?
        []  ->
            case lib_mnesia:find_recurse_labels(
                   #record{hostname=string:tokens(Name,"."),
                           type=?T_SOA}) of

                %% not authoritative -> SERVFAIL
                %% nothing to do here
                [] ->
                    not_authoritative(Message);

                %% authoritative. delegated?
                [_SOA] ->
                    case lib_mnesia:find_recurse_labels(
                           #record{hostname=string:tokens(Name,"."),
                                   type=?T_NS}) of

                        %% not delegated -> NXDOMAIN
                        %% nothing to do here
                        [] ->
                            not_delegated(Message);

                        %% delegated.
                        NSList ->
                            delegated(Message,NSList)
                    end;
                Error -> erlang:error(Error)
            end;



        AnswerList when is_list(AnswerList) ->
            exact_match(Message,AnswerList);

        Error -> erlang:error(Error)
    end;


%% We do not support anything besides standard queries. Reply with NOTIMP.
magic(#message{header=#header{
                 id=ID,
                 querykind=QueryKind,
                 isRecursionDesired=IsRecursionDesired,
                 numberQuestionEntries=NumberQuestionEntries},
               question=Q}) ->
    #message{header=#header{
               id=ID,
               isResponse=1,
               querykind=QueryKind,
               responseCode=?NOTIMP,
               isRecursionDesired=IsRecursionDesired,
               numberQuestionEntries=NumberQuestionEntries},
             question=Q}.


%% 1
exact_match(#message{header=#header{
                       id=ID,
                       querykind=?QUERY,
                       isRecursionDesired=IsRecursionDesired,
                       numberQuestionEntries=NumberQuestionEntries},
                     question = Q},
            RecordList) ->

    #message{header=#header{id=ID,
                            isResponse=1,
                            querykind=?QUERY,
                            isAuthoritative=1,
                            isTruncated=0,
                            isRecursionDesired=IsRecursionDesired,
                            isRecursionAvailable=0,
                            responseCode=?NOERROR,
                            numberQuestionEntries=NumberQuestionEntries,
                            numberAnswerRRs=1,
                            numberAuthorityRRs=0,
                            numberAdditionalRRs=0},
             question=Q,
             answer=record_list_to_aaa_list(RecordList),
             authority=[],
             additional=[]}.


%% 2
not_authoritative(#message{header=#header{
                             id=ID,
                             querykind=?QUERY,
                             isRecursionDesired=IsRecursionDesired,
                             numberQuestionEntries=NumberQuestionEntries},
                           question=Q}) ->

    %% We are not authoritative -> SERVFAIL
    #message{header=#header{id=ID,
                            isResponse=1,
                            querykind=?QUERY,
                            isAuthoritative=1,
                            isTruncated=0,
                            isRecursionDesired=
                            IsRecursionDesired,
                            isRecursionAvailable=0,
                            responseCode=?SERVFAIL,
                            numberQuestionEntries=
                            NumberQuestionEntries,
                            numberAnswerRRs=0,
                            numberAuthorityRRs=0,
                            numberAdditionalRRs=0},
             question=Q,
             answer=[],
             authority=[],
             additional=[]}.


%% 3
%% The requested RR is not present. -> NXDOMAIN
not_delegated(#message{header=#header{
                         id=ID,
                         querykind=?QUERY,
                         isRecursionDesired=IsRecursionDesired,
                         numberQuestionEntries=NumberQuestionEntries},
                       question=Q}) ->
    #message{header=#header{id=ID,
                            isResponse=1,
                            querykind=?QUERY,
                            isAuthoritative=1,
                            isTruncated=0,
                            isRecursionDesired=IsRecursionDesired,
                            isRecursionAvailable=0,
                            responseCode=?NXDOMAIN,
                            numberQuestionEntries=NumberQuestionEntries,
                            numberAnswerRRs=0,
                            numberAuthorityRRs=0,
                            numberAdditionalRRs=0},
             question=Q,
             answer=[],
             authority=[],
             additional=[]}.


%% 4
%% Find out for each NSRec whether we have glue, or not.

%% Put nothing into the answer section, put relevant NS entries into authority
%% section, and if glue is available, put it into additional section.

delegated(#message{header=#header{
                     id=ID,
                     querykind=?QUERY,
                     isRecursionDesired=IsRecursionDesired,
                     numberQuestionEntries=NumberQuestionEntries},
                   question=Q},NSList) ->
    %% DEBUG
    %% io:format("delegated: NSList is: ~p~n",[NSList]),

    {A1,A2} = delegated2(NSList,[],[]),

    #message{header=
             #header{id=ID,
                     isResponse=1,
                     querykind=?QUERY,
                     isAuthoritative=1,
                     isTruncated=0,
                     isRecursionDesired=
                     IsRecursionDesired,
                     isRecursionAvailable=0,
                     responseCode=?NOERROR,
                     numberQuestionEntries=
                     NumberQuestionEntries,
                     numberAnswerRRs=0,
                     numberAuthorityRRs=length(A1),
                     numberAdditionalRRs=length(A2)},
             question=Q,
             answer=[],
             authority=record_list_to_aaa_list(A1),
             additional=record_list_to_aaa_list(A2)}.


delegated2([],A1,A2) ->
    {A1,A2};

delegated2([NS|NSList],Auth,Additional) ->

    case delegated3(NS) of
        {noglue,NS1} ->
            delegated2(NSList,[NS1|Auth],Additional);
        {glue,NS1,GlueList} ->
            %% DEBUG
            %% io:format("delegated2 found glue"),
            delegated2(NSList,[NS1|Auth],lists:flatten([Additional,GlueList]))
    end.


delegated3(NSRecord = #record{hostname=_NSHost,
                              ttl=_NSTTL,
                              type=_NSType,
                              rdata=NSRdata}) ->
    %% DEBUG
    %% io:format("wir sind in delegated3, wir suchen nach glue zu ~p~n",[NSHost]),

    % Glue means having an A record for the NS.
    case lib_mnesia:find_record(#record{hostname=string:tokens(NSRdata#ns.nsname,"."),
                                type=?T_A}) of
        [] ->
            {noglue, NSRecord};

        GlueList ->
            {glue,NSRecord,GlueList}
    end.


record_list_to_aaa_list([]) ->
    [];

record_list_to_aaa_list([Rec|Recs]) ->
    record_list_to_aaa_list2([Rec|Recs],[]).

record_list_to_aaa_list2([],Acc) ->
    Acc;
record_list_to_aaa_list2([Rec|Recs],Acc) ->
    record_list_to_aaa_list2(Recs,[record_to_aaa(Rec)|Acc]).

record_to_aaa(#record{ttl = TTL,
                      hostname = Hostname,
                      type = Type,
                      rdata = Rdata}) ->
    #aaa{name=Hostname,
         type=Type,
         ttl=TTL,
         data=Rdata}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PACKET CONSTRUCTOR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reply(Socket,Host,Port,ReplyMessage) ->
    %% DEBUG
    %% io:format("~p~n",[ReplyMessage]),
    ReplyPacket = lib_packet:message_to_packet(ReplyMessage),
    gen_udp:send(Socket,Host,Port,ReplyPacket).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% print_header(#header{id=ID,
%%                 isResponse=IsResponse,
%%                 querykind=QUERY,
%%                 isAuthoritative=IsAuthoritative,
%%                 isTruncated=IsTruncated,
%%                 isRecursionDesired=IsRecursionDesired,
%%                 isRecursionAvailable=IsRecursionAvailable,
%%                 responseCode=ResponseCode,
%%                 numberQuestionEntries=NumQuestionEntries,
%%                 numberAnswerRRs=NumAnswerRRs,
%%                 numberAuthorityRRs=NumAuthorityRRs,
%%                 numberAdditionalRRs=NumAdditionalRRs}) ->
%%     io:format("id = ~p~nisResponse = ~p~nquerykind = ~p~nisAuthoritative = ~p~nisTruncated = ~p~nis
%%    RecursionDesired = ~p~nisRecursionAvailable = ~p~nresponseCode = ~p~nnumberQuestionEntries = ~p~nnumberAnswerRRs = ~p~nnumberAuthorityRRs = ~p~nnumberAdditionalRRs = ~p~nEND~n~n",[ID,IsResponse,QUERY,IsAuthoritative,IsTruncated,IsRecursionDesired,IsRecursionAvailable,ResponseCode,NumQuestionEntries,NumAnswerRRs,NumAuthorityRRs,NumAdditionalRRs]).
