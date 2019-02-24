chop_up([],[]).

chop_up([Head|Tail], [Head|NewList]) :-
	is_next(Head, Tail),
	chop_up(Tail, NewList).
	
chop_up([Head|Tail], NewList) :-
	not(is_next(Head, Tail)),
	chop_up(Tail, NewList).

% chop_list(Head|) :-
	

is_next(Num,[Head|_]) :-
	Num =:= Head - 1.


% ————————————————————————————————————————————————-----
% Question_4
% Any list of integers can (uniquely) be broken into "parity runs" where each run is a (maximal) sequence of consecutive even or odd numbers within the original list. For example, the list
List = [8,0,4,3,7,2,-1,9,9]
can be broken into [8, 0, 4], [3, 7], [2] and [-1, 9, 9]
Write a predicate paruns(List, RunList) that converts a list of numbers into the corresponding list of parity runs. For example:

?- paruns([8,0,4,3,7,2,-1,9,9], RunList).
RunList = [[8, 0, 4], [3, 7], [2], [-1, 9, 9]] 

 %judge whether the number is odd or even.
odd(Num) :-
    integer(Num),
    1 is Num mod 2.

even(Num) :-
    integer(Num),
    0 is Num mod 2.

%judge from the first number, if it is odd, put in L1, and put L2 into the result if L2 is not empty, then clear L2.
paruns_compute([Head|Tail], ResultList, L1, L2) :-
    odd(Head),
    L2 == [],
    paruns_compute(Tail, ResultList, [Head|L1], L2).

paruns_compute([Head|Tail], [L2|ResultList], L1, L2) :-
    odd(Head),
    L2 \= [],
    paruns_compute(Tail, ResultList, [Head|L1], []).

%judge from the first number, if it is even, put in L2, and put L1 into the result if L1 is not empty, then clear L1.
paruns_compute([Head|Tail], ResultList, L1, L2) :-
    even(Head),
    L1 == [],
    paruns_compute(Tail, ResultList, L1, [Head|L2]).

paruns_compute([Head|Tail], [L1|ResultList], L1, L2) :-
    even(Head),
    L1 \= [],
    paruns_compute(Tail, ResultList, [], [Head|L2]).

% put the L1 and L2 in the end of the resultlist if(L1 or L2 not empty).
paruns_compute([], [L1], L1, _) :-
 L1 \= [].

paruns_compute([], [L2], _, L2) :-
 L2 \= [].

% reverse the list
reverse([],Z,Z).

reverse([H|T],Z,Acc) :-
    reverse(T,Z,[H|Acc]).

%reverse the final list since it is like [[X],[X,X,X]]..
paruns_reverse([],[]).

paruns_reverse([Head|Tail],[Rev|Result]) :-
    reverse(Head,Rev,[]),
    paruns_reverse(Tail,Result).

paruns(List,RunList) :-
    paruns_compute(List,Temp_List,[],[]),
    paruns_reverse(Temp_List,RunList).


% ————————————————————————————————————————————————-----


even(N) :- 0 is N mod 2.

sumsq_even([], 0).
sumsq_even([N | Ns], Sum) :- even(N), sumsq_even(Ns, S), Sum is S + N * N.
sumsq_even([_ | Ns], Sum) :- sumsq_even(Ns, Sum).

————————————————————————————————————————————————————————————————————
runs([_,_],[]).
 
runs(List,RunList):-
  List = [A|OtherList],
  1 is A mod 2,
  runs(OtherList,RunList).
 
runs(List,RunList):-
  List = [A,B,C|_],
  List = [_|OtherList],
  0 is A mod 2,
  1 is C mod 2,
  runs(OtherList,RunList).
 
runs(List,RunList):-
  List = [A,B,C|_],
  List = [_|OtherList],
  0 is A mod 2,
  0 is C mod 2,
  runs(OtherList,OtherRunList),
  RunList=[B|OtherRunList].

——————————————————————————————————————————————————————————————————————

chop_up([], []).

% chop_up([X], X).
% chop_up(X, [X]).

% chop_up([X,Y],[X,Y]).
% chop_up([X,Y|T],[X,Z]) :-
%    chop_up([Y|T],[Y,Z]).

chop_up(List, [SubNum | Chopped]) :-
	chop_list(List, SubNum, Rest),
	chop_up(Rest, Chopped).

%
chop_list([A],[A],[]).
chop_list([Num, SecondNum | RestList], [Num], [SecondNum | RestList]) :-
	Num + 1 =\= SecondNum.
chop_list([Num, SecondNum | RestList], [Num | Successive], Rest) :-
	Num + 1 =:= SecondNum,
	chop_list([SecondNum | RestList], Successive, Rest).

accRev([], V, V).
accRev([H|T], V, R) :- accRev(T, [H|V], R).
rev(L, R) :- accRev(L, [], R).

flat([X], X).
flat([X,Y],[X,Y]).
flat([X,Y|T],[X,Z]) :-
    flat([Y|T],[Y,Z]).

______________________________________________________________________

accRev([], A, A).
accRev([H|T], A, R) :- accRev(T, [H|A], R).
rev(L, R) :- accRev(L, [], R).

