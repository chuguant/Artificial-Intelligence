% CHUGUAN 
% TIAN 
% z5145006 
% COMP9414 Assignment 1 - Prolog Programming

% Question 1
% sumsq_neg(Numbers, Sum) :- 
% sums the squares of only the negative numbers in a list of numbers.

% base case
sumsq_neg([], 0).

% when the num less than 0, calculate
sumsq_neg([First|Rest],Sum) :-
	First < 0,
	sumsq_neg(Rest, SubTotal),
	Sum is First * First + SubTotal.

% when the num is not negative, do nothing
sumsq_neg([First|Rest], Sum) :-
	First >= 0,
	sumsq_neg(Rest, SubTotal),
	Sum is SubTotal.

% Question 2
% all_like_all(Who_List, What_List) that takes a list of people Who_List and a list of items What_List and succeeds if every person in Who_List likes every item in What_List, according to the predicate likes(Who, What).

% base case
all_like_all([],_).
all_like_all(_,[]).

% Person loop
all_like_all([Person|Rest],What) :-
	likeAll(Person,What),
	all_like_all(Rest,What).

% Thing loop
likeAll(_, []).
likeAll(Person,[Head|Tail]) :-
	likes(Person, Head),
	likeAll(Person, Tail).

% Question 3
% sqrt_table(N, M, Result) that binds Result to the list of pairs consisting of a number and its square root, from N down to M, where N and M are non-negative integers, and N >= M

%base case when N = M
sqrt_table(N, M, Res) :-
	M > 0,
	N = M,
	S is sqrt(N),
	Res = [[M,S]].

% N > M
sqrt_table(N, M, Res) :-
	M > 0,
	N > M,
	S is sqrt(N),
	NextN is N-1,
	sqrt_table(NextN,M,NextRes),
	Res = [[N,S]|NextRes].
	
% Question 4
% chop_up(List, NewList) that takes List and binds NewList to List with all sequences of successive increasing whole numbers replaced by a two-item list containing only the first and last number in the sequence. An example of successive increasing whole numbers is: 19,20,21,22. (Note that the numbers have to be successive in the sense of increasing by exactly 1 at each step.) 

% base case, empty list
chop_up([], []).

%
chop_up(List, [Delled | Chopped]) :-
	chop_list(List, SubNum, Rest),
	chop_del(SubNum, Delled),
	chop_up(Rest, Chopped).

% find the sequence
chop_list([A],[A],[]).
chop_list([Num, SecondNum | RestList], [Num], [SecondNum | RestList]) :-
	Num + 1 =\= SecondNum.
chop_list([Num, SecondNum | RestList], [Num | Successive], Rest) :-
	Num + 1 =:= SecondNum,
	chop_list([SecondNum | RestList], Successive, Rest).

% delete useless part
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

% find the length
len([], 0).
len([_|Y], LenResult):-
    len(Y, L),
    LenResult is L + 1.

% Question 5
% tree_eval(Value, Tree, Eval) that binds Eval to the result of evaluating the expression-tree Tree, with the variable z set equal to the specified Value. 

% base case: when the end branch is z, z = value
tree_eval(Value, tree(empty, X, empty), Eval) :-
	X = z,
	Eval = Value.

% base case: when the end branch is num, num = _
tree_eval(_, tree(empty, X, empty), Eval) :-
	number(X),
	Eval = X.

% calculate: add
tree_eval(Value, tree(Left, '+', Right), Eval) :-
	tree_eval(Value, Left, L),
	tree_eval(Value, Right, R),
	Eval is L + R.

% calculate: sub
tree_eval(Value, tree(Left, '-', Right), Eval) :-
	tree_eval(Value, Left, L),
	tree_eval(Value, Right, R),
	Eval is L - R.

% calculate: multi
tree_eval(Value, tree(Left, '*', Right), Eval) :-
	tree_eval(Value, Left, L),
	tree_eval(Value, Right, R),
	Eval is L * R.

% calculate: divide
tree_eval(Value, tree(Left, '/', Right), Eval) :-
	tree_eval(Value, Left, L),
	tree_eval(Value, Right, R),
	Eval is L / R.

