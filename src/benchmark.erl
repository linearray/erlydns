%% ===================================================================
%% Copyright 2008-2012 Max Amanshauser.
%%
%% This file is part of erlydns.
%%
%% erlydns comes without any warranty and is licensed under the
%% Simplified BSD License; see the file named LICENSE for details.
%% ===================================================================


%%%-------------------------------------------------------------------
%%% File    : benchmark.erl
%%% Author  : Max Amanshauser <max at lambdalifting dot org>
%%% Description : Used to benchmark DNS servers
%%%-------------------------------------------------------------------

%% This simple benchmark suite assumes that you have entered data into
%% your nameserver. See populate/0.

%% You need to have created the Mnesia tables using lib_mnesia:create(),
%% afterwards you need to call lib_mnesia:populate().

-module(benchmark).


%% -------------------------------------------------------------------
%% Include files
%% -------------------------------------------------------------------

-include("./records.hrl").
-include("./dns.hrl").


%% -------------------------------------------------------------------
%% Exports
%% -------------------------------------------------------------------

-export([help/0,
         main/3]).


%% -------------------------------------------------------------------
%% Functions
%% -------------------------------------------------------------------

help() ->
    io:format("~n"),
    io:format(" ------------------------------------------ ~n"),
    io:format("| if you are benchmarking erlydns,         |~n"),
    io:format("| call lib_mnesia:populate/0 from within   |~n"),
    io:format("| your server (or set -mnesia dir          |~n"),
    io:format("| accordingly).                            |~n"),
    io:format("| THIS WILL DELETE YOUR DB!                |~n"),
    io:format(" ------------------------------------------ ~n"),
    io:format("| call main/3 with arguments:              |~n"),
    io:format("| main(IP, Port, Iterations)               |~n"),
    io:format("| e.g.                                     |~n"),
    io:format("| main({127,0,0,1}, 1053, 10000)           |~n"),
    io:format(" ------------------------------------------ ~n~n").


main(IP, Port, Iterations) ->
    {MegaSec,Sec,MicroSec} = erlang:now(),
    random:seed(MegaSec,Sec,MicroSec),
    perform_tests(IP,Port,Iterations).


perform_tests(IP,Port,Iterations) ->
    Processes = 40,
    ProcList = spawn_processes(Processes),
    Times = Iterations div Processes,


    statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! {self(),{dnsquery,Times,IP,Port,fun not_existent/0}} end,ProcList),
    io:format("waiting for replies...."),
    wait_for_reply(Processes),
    {_,Time1} = statistics(wall_clock),
    io:format("First test (unknown domain):~n~p queries, ~p milliseconds, ~p queries/sec~n",
              [Times*Processes,Time1,round((Iterations/Time1)*1000)]),

    statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! {self(),{dnsquery,Times,IP,Port,fun direct_hit/0}} end,ProcList),
    io:format("waiting for replies...."),
    wait_for_reply(Processes),
    {_,Time2} = statistics(wall_clock),
    io:format("Second test (A record):~n~p queries, ~p milliseconds, ~p queries/sec~n",
              [Times*Processes,Time2,round((Times*Processes/Time2)*1000)]),

    statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! {self(),{dnsquery,Times,IP,Port,fun nxdomain/0}} end,ProcList),
    io:format("waiting for replies...."),
    wait_for_reply(Processes),
    {_,Time3} = statistics(wall_clock),
    io:format("Third test (NXDOMAIN):~n~p queries, ~p milliseconds, ~p queries/sec~n",
              [Times*Processes,Time3,round((Times*Processes/Time3)*1000)]),

    statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! {self(),{dnsquery,Times,IP,Port,fun redirected_with_glue/0}} end,ProcList),
    io:format("waiting for replies...."),
    wait_for_reply(Processes),
    {_,Time4} = statistics(wall_clock),
    io:format("Fourth test (redirected with glue):~n~p queries, ~p milliseconds, ~p queries/sec~n",
              [Times*Processes,Time4,round((Times*Processes/Time4)*1000)]),

    lists:foreach(fun(Pid) -> Pid ! stop end,ProcList).


wait_for_reply(0) ->
    ok;
wait_for_reply(Processes) ->
    receive
        done ->
            wait_for_reply(Processes-1)
    end.


spawn_processes(Processes) ->
    spawn_processes2(Processes,[]).

spawn_processes2(0,List) ->
    List;
spawn_processes2(Processes,List) ->
    Pid = spawn_link(fun() -> workerloop() end),
    spawn_processes2(Processes-1,[Pid|List]).


workerloop() ->
    receive
        {Parent,{dnsquery,Times,IP,Port,PacketFun}} ->
            send_packet(Times,IP,Port,PacketFun),
            Parent ! done,
            workerloop();
        stop ->
            ok
    end.


send_packet(0,_,_,_) ->
    ok;
send_packet(Times,IP,Port,PacketFun) ->
    {ok, Socket} = gen_udp:open(0),
    gen_udp:send(Socket,IP,Port,PacketFun()),

    receive
        {udp, Socket, _IP, _InPortNo, _} ->
            ok
    after 
        10000 ->
            io:format("I was waiting for response, but to no avail.")
    end,

    gen_udp:close(Socket),
    send_packet(Times-1,IP,Port,PacketFun).



%% This creates a query for a nonexistent domain.
not_existent() ->
    Header = #header{isResponse=0,
                     querykind=?QUERY,
                     isAuthoritative=0,
                     isTruncated=0,
                     isRecursionDesired=1,
                     isRecursionAvailable=0,
                     responseCode=?NOERROR,
                     numberQuestionEntries=1,
                     numberAnswerRRs=0,
                     numberAuthorityRRs=0,
                     numberAdditionalRRs=0},

    Question = #question{name="Idonotexist.com",
                         type=?T_A,
                         class=1},

    Message = #message{header=Header,
                       question=Question,
                       answer=[],
                       authority=[],
                       additional=[]},

    query_message_to_packet(Message).

%% This creates a query for a A record.
direct_hit() ->
    Header = #header{isResponse=0,
                     querykind=?QUERY,
                     isAuthoritative=0,
                     isTruncated=0,
                     isRecursionDesired=1,
                     isRecursionAvailable=0,
                     responseCode=?NOERROR,
                     numberQuestionEntries=1,
                     numberAnswerRRs=0,
                     numberAuthorityRRs=0,
                     numberAdditionalRRs=0},

    Question = #question{name="test.subdomain.testdomain.com",
                         type=?T_A,
                         class=1},

    Message = #message{header=Header,
                       question=Question,
                       answer=[],
                       authority=[],
                       additional=[]},

    query_message_to_packet(Message).


%% This creates a query for a non-existent record in an authoritative domain.
nxdomain() ->
    Header = #header{isResponse=0,
                     querykind=?QUERY,
                     isAuthoritative=0,
                     isTruncated=0,
                     isRecursionDesired=1,
                     isRecursionAvailable=0,
                     responseCode=?NOERROR,
                     numberQuestionEntries=1,
                     numberAnswerRRs=0,
                     numberAuthorityRRs=0,
                     numberAdditionalRRs=0},

    Question = #question{name="tohuwabohu.testdomain.com",
                         type=?T_A,
                         class=1},

    Message = #message{header=Header,
                       question=Question,
                       answer=[],
                       authority=[],
                       additional=[]},

    query_message_to_packet(Message).


%% This creates a query for a record in a redirected subdomain.
redirected_with_glue() ->
    Header = #header{isResponse=0,
                     querykind=?QUERY,
                     isAuthoritative=0,
                     isTruncated=0,
                     isRecursionDesired=1,
                     isRecursionAvailable=0,
                     responseCode=?NOERROR,
                     numberQuestionEntries=1,
                     numberAnswerRRs=0,
                     numberAuthorityRRs=0,
                     numberAdditionalRRs=0},

    Question = #question{name="host.delegated.testdomain.com",
                         type=?T_A,
                         class=1},

    Message = #message{header=Header,
                       question=Question,
                       answer=[],
                       authority=[],
                       additional=[]},

    query_message_to_packet(Message).


%% HELPERS
query_message_to_packet(
  #message{header=#header{querykind=QueryKind,
                          isTruncated=IsTruncated,
                          isRecursionDesired=IsRecursionDesired,
                          responseCode=ResponseCode},
           question=Question,
           answer=Answer,
           authority=Authority,
           additional=Additional}) ->

    ID = random:uniform(65535),

    QuestionBin = lib_packet:question_to_binary(Question),
    AnswerBin = lib_packet:aaa_list_to_binary(Answer),
    AuthorityBin = lib_packet:aaa_list_to_binary(Authority),
    AdditionalBin = lib_packet:aaa_list_to_binary(Additional),
    list_to_binary([<<ID:16,
                     2#0:1,                           % isResponse
                     QueryKind:4,                     % OpCode
                     2#1:1,                           % isAuthoritative
                     IsTruncated:1,                   % isTruncated
                     IsRecursionDesired:1,            % isRecursionDesired
                     2#0:1,                           % isRecursionAvailable
                     2#0:3,                           % Zeros
                     ResponseCode:4,                  % ResponseCode
                     1:16,                            % Questions
                     0:16,                            % Answers
                     0:16,                            % Authority
                     0:16>>,                          % Additional
                    QuestionBin,
                    AnswerBin,
                    AuthorityBin,
                    AdditionalBin]).
