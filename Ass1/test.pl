

accRev([], A, A).
accRev([H|T], A, R) :- accRev(T, [H|A], R).
rev(L, R) :- accRev(L, [], R).

reverse([], Z, Z).
reverse([H | T], Z, Acc) :-
reverse(T, Z, [H | Acc]).

len([], 0).
len([_|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.

chop_del([],[]).

chop_del([Head|Tail],NewList) :-
len(Tail,Len),
Len =\= 0,
last(Tail,Last),
append([],[Head,Last],NewList).

chop_del([Head|Tail],NewList) :-
len(Tail,Len),
Len == 0,
append([],Head,NewList).

chop_up().

chop_process([], []).

chop_process(List, [SubNum | Chopped]) :-
	chop_list(List, SubNum, Rest),
	chop_process(Rest, Chopped).

%
chop_list([A],[A],[]).
chop_list([Num, SecondNum | RestList], [Num], [SecondNum | RestList]) :-
	Num + 1 =\= SecondNum.
chop_list([Num, SecondNum | RestList], [Num | Successive], Rest) :-
	Num + 1 =:= SecondNum,
	chop_list([SecondNum | RestList], Successive, Rest).
