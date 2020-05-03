1)To print to the console use the ‘io’ module:
-module(hello_word).
-compile(export_all).
hello() ->
   io:format(“hello world~n”).
   
2)Finding the maximum value of the elements in a list:
-module(tut6).
-export([list_max/1]).

list_max([Head|Rest]) ->
   list_max(Rest, Head).

list_max([], Res) ->
    Res;
list_max([Head|Rest], Result_so_far) when Head > Result_so_far ->
    list_max(Rest, Head);
list_max([Head|Rest], Result_so_far)  ->
    list_max(Rest, Result_so_far).
   

3) Testing Tools for the Erlang Ecosystem:
% Testing the max_distance property
prop_max_distance(N) ->
   ?FORALL (G, graph(N) ,
               begin 
               D = lists:max(distance_to_sink(G)) , 
               D < (N div 2)
             end) .
% Targeted max_distance property
prop_max_distance(N) ->
?FORALL_SA(G,  ?TARGET(#(gen => graph(N) ) ,
       begin
       D = lists:max(distance_to_sink(G)) , 
      ?MAXIMIZE(D) ,
      D < (N div 2)
end).


4) An Erlang function to calculate the area of a shape:
area({square, Side}) ->
   Side * Side ;
area({circle , Radius}) ->
  Math:pi() * Radius * Radius;
area({triangle, A , B, C}) ->
S = (A + B + C)/2,
math:sqrt(S*(S-A)*(S-B)*(S-C));
area(Other) ->
{error, invalid_object}.


5) Each time an error occurs in the erlang shell, the shell process exits and a new one is started:
1 > self().
	<0.23.0>
	2> x - y.
	** exited: {badarith,[{erl_eval,eval_op,3},
                      {erl_eval,exprs,4},
                      {shell,eval_loop,2}]} **
 	3> self().
	<0.40.0>
  

6) A list of temperature readings from a number of cities in the world:
%% This module is in file tut5.erl

-module(tut5).
-export([format_temps/1]).

%% Only this function is exported
format_temps([])->                        % No output for an empty list
    ok;
format_temps([City | Rest]) ->
    print_temp(convert_to_celsius(City)),
    format_temps(Rest).

convert_to_celsius({Name, {c, Temp}}) ->  % No conversion needed
    {Name, {c, Temp}};
convert_to_celsius({Name, {f, Temp}}) ->  % Do the conversion
    {Name, {c, (Temp - 32) * 5 / 9}}.

print_temp({Name, {c, Temp}}) ->
    io:format("~-15w ~w c~n", [Name, Temp]).
    
  
7) The ping pong example using links to terminate "pong":
% In this way you can connect all processes in a transaction together using links. 
% If one of the processes exits abnormally, all the processes in the transaction are killed.

-module(tut20).
-export([start/1,  ping/2, pong/0]).

ping(N, Pong_Pid) ->
    link(Pong_Pid),
    ping1(N, Pong_Pid).

ping1(0, _) ->
    exit(ping);

ping1(N, Pong_Pid) ->
    Pong_Pid ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping1(N - 1, Pong_Pid).

pong() ->
    receive
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

start(Ping_Node) ->
    PongPID = spawn(tut20, pong, []),
    spawn(Ping_Node, tut20, ping, [3, PongPID]).
    

8) To implement your own behaviors:
-module(gen_happy).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{get_happy, 0}, {get_ecstatic, 0}];
behavior_info(_) ->
    undefined.
    

9) Shows how to calculate alpha blending using maps to reference color and alpha channels:

-module(color).
-export([new/4, blend/2]).

-define(is_channel(V), (is_float(V) andalso V >= 0.0 andalso V =< 1.0)).

new(R,G,B,A) when ?is_channel(R), ?is_channel(G),
                  ?is_channel(B), ?is_channel(A) ->
    #{red => R, green => G, blue => B, alpha => A}.

blend(Src,Dst) ->
    blend(Src,Dst,alpha(Src,Dst)).

blend(Src,Dst,Alpha) when Alpha > 0.0 ->
    Dst#{
        red   := red(Src,Dst) / Alpha,
        green := green(Src,Dst) / Alpha,
        blue  := blue(Src,Dst) / Alpha,
        alpha := Alpha
    };
blend(_,Dst,_) ->
    Dst#{
        red   := 0.0,
        green := 0.0,
        blue  := 0.0,
        alpha := 0.0
    }.

alpha(#{alpha := SA}, #{alpha := DA}) ->
    SA + DA*(1.0 - SA).

red(#{red := SV, alpha := SA}, #{red := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
green(#{green := SV, alpha := SA}, #{green := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
blue(#{blue := SV, alpha := SA}, #{blue := DV, alpha := DA}) ->
    SV*SA + DV*DA*(1.0 - SA).
   
10)If conditions:
-module(tut9).
-export([test_if/2]).

 test_if(A, B) ->
    if 
        A == 5 ->
            io:format("A == 5~n", []),
            a_equals_5;
        B == 6 ->
            io:format("B == 6~n", []),
            b_equals_6;
        A == 2, B == 3 ->                      %That is A equals 2 and B equals 3
            io:format("A == 2, B == 3~n", []),
            a_equals_2_b_equals_3;
        A == 1 ; B == 7 ->                     %That is A equals 1 or B equals 7
            io:format("A == 1 ; B == 7~n", []),
            a_equals_1_or_b_equals_7
    end.
    

11)The length of a month, given the year:
-module(tut11).
-export([month_length/2]).

  month_length(Year, Month) ->
    %% All years divisible by 400 are leap
    %% Years divisible by 100 are not leap (except the 400 rule above)
    %% Years divisible by 4 are leap (except the 100 rule above)
    Leap = if
        trunc(Year / 400) * 400 == Year ->
            leap;
        trunc(Year / 100) * 100 == Year ->
            not_leap;
        trunc(Year / 4) * 4 == Year ->
            leap;
        true ->
            not_leap
    end,  
    case Month of
        sep -> 30;
        apr -> 30;
        jun -> 30;
        nov -> 30;
        feb when Leap == leap -> 29;
        feb -> 28;
        jan -> 31;
        mar -> 31;
        may -> 31;
        jul -> 31;
        aug -> 31;
        oct -> 31;
        dec -> 31
    end.
    
    
 12)Constants in Erlang can be created through the use of macros:
 module(constants).
-compile(export_all).
-define(N, 123).
-define(M, "what").
-define(SQUARED (X), X*X).
 showConstants() ->
    io:format("N = ~p ~n", [?N]),
    io:format("M = ~p ~n", [?M]),
    io:format("~p ~n", [?SQUARED(5)]).

13)
%% qsort:qsort(List)
%% Sort a list of items
-module(qsort).     % This is the file 'qsort.erl'
-export([qsort/1]). % A function 'qsort' with 1 parameter is exported (no type, no name)

  qsort([]) -> []; % If the list [] is empty, return an empty list (nothing to sort)
  qsort([Pivot|Rest]) ->
    % Compose recursively a list with 'Front' for all elements that should be before 'Pivot'
    % then 'Pivot' then 'Back' for all elements that should be after 'Pivot'
    qsort([Front || Front <- Rest, Front < Pivot]) ++ 
    [Pivot] ++
    qsort([Back || Back <- Rest, Back >= Pivot]).
    


 
