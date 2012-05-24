%% ===================================================================
%% Copyright 2008-2010 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : lib_packet.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Conversion between Messages and Packets
%%%-------------------------------------------------------------------

-module(lib_packet).


%% -------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------

-include("./records.hrl").
-include("./dns.hrl").

-import(lib_misc,[rpc/2]).
-import(lib_string,[domainname_to_string/1,
                    string_to_binary/1]).


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([headerBin_to_header/1,
        questionsectionBin_to_question/3,
        message_to_packet/1,
        question_to_binary/1,
        aaa_list_to_binary/1]).


%%  _____                    ____  _
%% |  ___| __ ___  _ __ ___ | __ )(_)_ __   __ _ _ __ _   _
%% | |_ | '__/ _ \| '_ ` _ \|  _ \| | '_ \ / _` | '__| | | |
%% |  _|| | | (_) | | | | | | |_) | | | | | (_| | |  | |_| |
%% |_|  |_|  \___/|_| |_| |_|____/|_|_| |_|\__,_|_|   \__, |
%%                                                    |___/

%% Input: Binary Header of a DNS packet (trailing data is ok)
%% Output: #header record of data
headerBin_to_header(<<ID:16,
               IsResponse:1,
               QUERY:4,
               IsAuthoritative:1,
               IsTruncated:1,
               IsRecursionDesired:1,
               IsRecursionAvailable:1,
               _Zero:3,
               ResponseCode:4,
               NumQuestionEntries:16,
               NumAnswerRRs:16,
               NumAuthorityRRs:16,
               NumAdditionalRRs:16,
               _Rest/binary>>) ->
    X = #header{id=ID,
                isResponse=IsResponse,
                querykind=QUERY,
                isAuthoritative=IsAuthoritative,
                isTruncated=IsTruncated,
                isRecursionDesired=IsRecursionDesired,
                isRecursionAvailable=IsRecursionAvailable,
                responseCode=ResponseCode,
                numberQuestionEntries=NumQuestionEntries,
                numberAnswerRRs=NumAnswerRRs,
                numberAuthorityRRs=NumAuthorityRRs,
                numberAdditionalRRs=NumAdditionalRRs},
    X.


%% Input: Number of Questions and a Binary containing said questions
%% (trailing data is ok).
%% Output: #question record
questionsectionBin_to_question(1,Bin,SS) ->
    %% DEBUG
    %% io:format("questionsectionBin entered~n."),
    questionBin_to_question(Bin,[],92,SS);
questionsectionBin_to_question(_,_,_) ->
    error.



%% Input: Binary starting with 1 (one) question entry
%% Output: #question record

%% When this clause is triggered, there is nothing else to do.
%% We need to:
%% 1) add new suffices to SS
%% 2) convert LabelPos and all the other stuff to a #question
questionBin_to_question(<<0:2,Length:6,
                         Type:16,
                         Class:16,
                         _RestBin/binary>>,
                        LabelPosBackw,
                        _Pos,
                        SS)
  when Length =:= 0 ->
    %% DEBUG
    %% io:format("We are in the last clause.~n"),
    LabelPosForw = lists:reverse(LabelPosBackw),
    labelPosEntry(LabelPosForw,SS),            % all new suffices entered
    DN = lists:map(fun({_A,B,C}) -> {B,C} end,LabelPosForw),
    DomainString = domainname_to_string(DN),
    %% DEBUG
    %% io:format("Now leaving last clause, domain name is: ~p~n",[DomainString]),
    #question{name=DomainString,
              type=Type,
              class=Class};


%% This is not a pointer.
questionBin_to_question(<<0:2,Length:6,RestBin/binary>>,
                        LabelPos,
                        Pos,
                        SS) ->
    %% DEBUG
    %% io:format("We are in the first clause, Bin: ~p.~n",[RestBin]),
    <<NewLabel:Length/binary,NewRest/binary>> = RestBin,
    questionBin_to_question(NewRest,
                            [{Pos,
                              Length,
                              binary_to_list(NewLabel)}|LabelPos],
                           Pos+Length+1,
                           SS);


%% This IS a pointer
%% We know that a pointer is always the last entry in a DN.
questionBin_to_question(<<3:2,Offset:14,RestBin/binary>>,
                        LabelPos,
                        Pos,
                        SS) ->
    %% DEBUG
    %% io:format("We are in the pointer clause, Bin: ~p.~n",[RestBin]),

    %% get us the Domainname that the pointer points to
    case rpc(SS,{get,Offset}) of
        {dn,DN} -> % this DN is forward, however LabelPos is backwards
            RevDN = lists:reverse(DN),
            questionBin_to_question(RestBin,   % this MUST call the next clause
                                    lists:append(RevDN,LabelPos),
                                    Pos+2,
                                    SS);
        _Other ->
            error
    end.


labelPosEntry([],_) ->
    ok;
labelPosEntry([{Pos,Len,Label}|Rest],SS) ->
    TwoThree = fun({_A,B,C}) -> {B,C} end,
    rpc(SS,{set,{Pos,[{Len,Label}|lists:map(TwoThree,Rest)]}}),
    labelPosEntry(Rest,SS).



%%  _____     ____  _
%% |_   _|__ | __ )(_)_ __   __ _ _ __ _   _
%%   | |/ _ \|  _ \| | '_ \ / _` | '__| | | |
%%   | | (_) | |_) | | | | | (_| | |  | |_| |
%%   |_|\___/|____/|_|_| |_|\__,_|_|   \__, |
%%                                     |___/

%% Input: #message and converts it to a DNS packet
%% Output: DNS packet
message_to_packet_notrunc(
  #message{header=#header{id=ID,
                          querykind=QueryKind,
                          isTruncated=IsTruncated,
                          isRecursionDesired=IsRecursionDesired,
                          responseCode=ResponseCode,
                          numberQuestionEntries=NumQuestionEntries},
           question=Question,
           answer=Answer,
           authority=Authority,
           additional=Additional}) ->

    %% io:format("Question: ~p~n",[Question]),
    %% io:format("Answer: ~p~n",[Answer]),
    %% io:format("~p~n",[Foo]),
    NumAnswerRRs = length(Answer),
    NumAuthorityRRs = length(Authority),
    NumAdditionalRRs = length(Additional),
    QuestionBin = question_to_binary(Question),
    AnswerBin = aaa_list_to_binary(Answer),
    AuthorityBin = aaa_list_to_binary(Authority),
    AdditionalBin = aaa_list_to_binary(Additional),
    %% io:format("QuestionBin ist: ~p~n",[QuestionBin]),
    %% io:format("AnswerBin ist: ~p~n",[AnswerBin]),
    %% io:format("AuthorityBin ist: ~p~n",[AuthorityBin]),
    list_to_binary([<<ID:16,
                     2#1:1,                           % isResponse
                     QueryKind:4,                     % OpCode
                     2#1:1,                           % isAuthoritative
                     IsTruncated:1,                   % isTruncated
                     IsRecursionDesired:1,            % isRecursionDesired
                     2#0:1,                           % isRecursionAvailable
                     2#0:3,                           % Zeros
                     ResponseCode:4,                  % ResponseCode
                     NumQuestionEntries:16,
                     NumAnswerRRs:16,
                     NumAuthorityRRs:16,
                     NumAdditionalRRs:16>>,
                    QuestionBin,
                    AnswerBin,
                    AuthorityBin,
                    AdditionalBin]).


%% If the resulting packet is larger than 512 bytes we transmit no RRs at all.
%% A proper client will then retry with TCP.
message_to_packet(#message{
header=#header{id=ID,
               querykind=QueryKind,
               isRecursionDesired=IsRecursionDesired,
               responseCode=ResponseCode}}=Message) ->
    Packet = message_to_packet_notrunc(Message),

    if
        size(Packet) > 512 ->
            list_to_binary([<<ID:16,
                             2#1:1,                     % isResponse
                             QueryKind:4,               % OpCode
                             2#1:1,                     % isAuthoritative
                             2#1:1,                     % isTruncated
                             IsRecursionDesired:1,      % isRecursionDes
                             2#0:1,                     % isRecursionAvail
                             2#0:3,                     % Zeros
                             ResponseCode:4,            % ResponseCode
                             0:16,                      % NumQEntries
                             0:16,                      % NumAnsRRs
                             0:16,                      % NumAuthRRs
                             0:16>>]);                  % NumAddRRs
        true ->
            Packet
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Converts Questions to binaries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

question_to_binary(#question{name=Name,
                          type=Type,
                          class=Class}) ->
    NameBin = string_to_binary(Name),
    list_to_binary([NameBin,<<Type:16,Class:16>>]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Converts the AAA sections to binaries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
aaa_list_to_binary(AAA) ->
    aaa_list_to_binary2(AAA,[]).


aaa_list_to_binary2([],Binaries) ->
    list_to_binary(Binaries);

aaa_list_to_binary2([First|Remaining],Binaries) ->
    Res = aaa_to_binary(First),
    aaa_list_to_binary2(Remaining,[Res|Binaries]).

%%
aaa_to_binary(#aaa{name=Name,
                type=Type,
                class=Class,
                ttl=TTL,
                data=Data}) ->
    %% DEBUG
    %% io:format("aaa_to_binary: ~p~n",[A]),
    NameBin = string_to_binary(string:join(Name,".")),
    DataBin = rdata_to_binary(Data),
    DataBinSize = size(DataBin),
    list_to_binary([NameBin,<<Type:16,
                             Class:16,
                             TTL:32,
                             DataBinSize:16,
                             DataBin/binary>>]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Converts RDATA to binaries
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rdata_to_binary(#a{ip1=Ip1,ip2=Ip2,ip3=Ip3,ip4=Ip4}) ->
    <<Ip1:8,Ip2:8,Ip3:8,Ip4:8>>;

rdata_to_binary(#ns{nsname=Nsname}) ->
    lib_string:string_to_binary(Nsname);

rdata_to_binary(#mx{pref=Pref,exchange=Exchange}) ->
    ExBin = lib_string:string_to_binary(Exchange),
    PrefBin = <<Pref:16>>,
    list_to_binary([PrefBin,ExBin]);

rdata_to_binary(#soa{nsname=NSName,
                     mail=Mail,
                     serial=Serial,
                     refresh=Refresh,
                     retry=Retry,
                     expire=Expire,
                     minimum=Minimum}) ->
    NSNameBin = lib_string:string_to_binary(NSName),
    MailBin = lib_string:string_to_binary(Mail),
    RestBin = <<Serial:32,Refresh:32,Retry:32,Expire:32,Minimum:32>>,
    list_to_binary([NSNameBin,MailBin,RestBin]).
