/*
COMP9414 Assignment3
Option 2: Prolog (BDI Agent)
hw3Group788: Jiongqian Wu(z5180610), Chuguan Tian (z5145006)
*/


%Dynamically produce s
:- dynamic s/3.


% Question One: _________________________________________________________________________
initial_intentions(intents(L, [])):-

	% define the current position of the agent
    agent_at(X0, Y0),

	% add the provisional goal state
    assert(goal(goal(X0, Y0))),

	% set the position of the monster
    monster(MonsterX, MonsterY),

	% initiate path search
    solve(goal(MonsterX, MonsterY), Path, _, _),

	% collect a list of all the locations where stones can be dropped 
    findall(X,(member(goal(Goal1,Goal2),Path),not(land(Goal1,Goal2)),X = [goal(Goal1,Goal2),[]]),L),

	% dynamically remove the provisional goal state
    retract(goal(goal(X0, Y0))),

    retractall(s(_,_,1000)).


% Question Two: _________________________________________________________________________
% takes a list of percepts, each of the form stone(X,Y), and converts it into a corresponding list of goals, each of the form goal(X,Y).
trigger(Percepts,Goals) :- 
    findall(X, (X = goal(Goal1,Goal2), member(stone(Goal1,Goal2), Percepts)), Goals).




% Question Three: _________________________________________________________________________
% terminate, if the list is empty.
incorporate_goals([], Intentions, Intentions):-
    !.


% take in a set of goals in the form of a list [goal(X1,Y1), ... , goal(Xn,Yn)]
incorporate_goals([Head|Tail], Intentions, Intentions1) :-
    agent_at(X, Y),
    assert(goal(goal(X, Y))),
    insert_intent(Head, Intentions, IntentionsNew),
    retract(goal(goal(X, Y))),
    incorporate_goals(Tail, IntentionsNew, Intentions1), !.


% add the intentions of the agent in the form of drop and pick
% if we CAN NOT find a path to the goal state, do NOT insert
insert_intent(Goal,intents(Int_Drop,Int_Pick),intents(Int_Drop,Int_Pick)):-
    not(solve(Goal, _, _, _)),!.



% if we CAN find a path to the goal state, 
insert_intent(Goal,intents(Int_Drop,Int_Pick),intents(Int_Drop,Int_Pick1)):-
    solve(Goal, _, G, _),
    insert_intent_recursion(Goal, G, Int_Pick, Int_Pick1),!.

insert_intent_recursion(Goal,_,[],[[Goal,[]]]).
insert_intent_recursion(Goal,_,[[Goal,Plan]|Tail],[[Goal,Plan]|Tail]).



% if goal not already in the list, INSERT!
insert_intent_recursion(Goal,G,[[RecGoal, RecPlan]|Tail],[[RecGoal,RecPlan]|Tail1]):-
    solve(RecGoal,_,LengthOfPath,_),
    G >= LengthOfPath,
    insert_intent_recursion(Goal, G, Tail, Tail1).

% if goal already in the list, do not insert!
insert_intent_recursion(Goal,G,[[RecGoal,RecPlan]|Tail],[[Goal,[]],[RecGoal, RecPlan]|Tail]):-
    solve(RecGoal,_,LengthOfPath,_),
    G < LengthOfPath,!.


% Question Four: _________________________________________________________________________

% the agent is holding a stone, it needs to drop the stone it currently holds by selecting the first item in the Int_Drop list
get_action(intents(Int_Drop,Int_Pick),intents(Int_Drop1,Int_Pick),Action):-
	
    agent_stones(1),
    agent_at(X,Y),
    assert(goal(goal(X,Y))),
    collect_drop(Int_Drop,Int_Drop1,Action),
    retract(goal(goal(X,Y))),!.


% the agent is NOT holding a stone, it needs to pick up a stone by selecting the first item in the Int_Pick list
get_action(intents(Int_Drop,Int_Pick),intents(Int_Drop,Int_Pick1),Action):-
    agent_stones(0),
    agent_at(X,Y),
    assert(goal(goal(X,Y))),
    collect_pick(Int_Pick,Int_Pick1,Action),

	% remove the goal to avoid errors associated with the solver
    retract(goal(goal(X,Y))),!.

% if no intention is selected, the agents intend to stay where it is
collect_drop([],[],move(X0,Y0)):- agent_at(X0,Y0).

%  check if an action is applicable
collect_drop([[goal(X,Y),[PlanHead|TailPlan]]|PickTail],[[goal(X,Y),TailPlan]|PickTail],PlanHead):- applicable(PlanHead).

% if the first action in the selected plan is indeed applicable:
collect_drop([[goal(X,Y),[]]|PickTail],[[goal(X,Y),NewPlanTail]|PickTail],Action):-
    solve(goal(X,Y),Path,_,_),
    append([_|TailInitPath],[_],Path),
    findall(XX,(XX = move(G1,G2),member(goal(G1,G2),TailInitPath)),TailInitPlan),
    append(TailInitPlan,[drop(X,Y)],[Action|NewPlanTail]).


% if the first action in the selected plan from the Int_Drop list is NOT applicable, the agent needs to
% construct a new plan to go from its current position to the goal position before dropping a stone.
collect_drop([[goal(X,Y),[PlanHead|_]]|PickTail],[[goal(X,Y),NewPlanTail]|PickTail],Action):-
    not(applicable(PlanHead)),
    solve(goal(X,Y),Path,_,_),
    append([_|TailInitPath],[_],Path),
    findall(XX,(XX = move(G1,G2),member(goal(G1,G2),TailInitPath)),TailInitPlan),
    append(TailInitPlan,[drop(X,Y)],[Action|NewPlanTail]).

% if no intention is selected, the agents intend to stay where it is
collect_pick([],[],move(X0,Y0)):-
    agent_at(X0,Y0).

%  check if an action is applicable
collect_pick([[goal(X,Y),[PlanHead|TailPlan]]|PickTail],[[goal(X,Y),TailPlan]|PickTail],PlanHead):-
    applicable(PlanHead).

% if the first action in the selected plan is indeed applicable:
collect_pick([[goal(X,Y),[]]|PickTail],[[goal(X,Y),NewPlanTail]|PickTail],Action):-
    solve(goal(X,Y),Path,_,_),
    append([_|TailInitPath],[_],Path),
    findall(Pos,(Pos = move(G1,G2),member(goal(G1,G2),TailInitPath)),TailInitPlan),
    append(TailInitPlan,[pick(X,Y)],[Action|NewPlanTail]).

% if the first action in the selected plan from the Int_Pick list is NOT applicable, the agent needs to
% construct a new plan to go from its current position to the goal position before picking up a stone.
collect_pick([[goal(X,Y),[PlanHead|_]]|PickTail],[[goal(X,Y),NewPlanTail]|PickTail],Action):-
    not(applicable(PlanHead)),
    solve(goal(X,Y),Path,_,_),
    append([_|TailInitPath],[_],Path),
    findall(XX,(XX = move(G1,G2),member(goal(G1,G2),TailInitPath)),TailInitPlan),
    append(TailInitPlan,[pick(X,Y)],[Action|NewPlanTail]).



% Question 5:___________________________________________________________________________

% update agent's intention based upon observation at(_,_)
update_intentions(at(_,_),Intentions,Intentions).
update_intentions(picked(_,_),intents(Int_Drop,[_|TailInt_Pick]),intents(Int_Drop,TailInt_Pick)).
update_intentions(dropped(_,_),intents([_|TailInt_Drop],Int_Pick),intents(TailInt_Drop,Int_Pick)).

/*
add some constraints for the movement of the agent over land
the first argument ----> state 1
the 2nd arguemtn ----> state 2
agent is moving from state 1 to state 2 over land, third argument is the cost.
*/
s(goal(X1,Y1),goal(X2,Y2),1):-
    land_or_dropped(X2,Y2),
    distance((X1,Y1),(X2,Y2),1).


/*
add some constraints for the movement of the agent over water
the first argument ----> state 1
the 2nd arguemtn ----> state 2
agent is moving from state 1 to state 2,  third argument is the cost, the cost for moving over the water is much much higher

The effect of setting a much higher cost is that the agent will attempt to remain over land unless it is absolutely necessary.
*/
s(goal(X1,Y1),goal(X2,Y2),1000):-

% the agent is allowed to move one step at a time
    XP1 is X1 + 1,
    XD1 is X1 - 1,
    YP1 is Y1 + 1,
    YD1 is Y1 - 1,
    between(YD1,YP1,Y2),
    between(XD1,XP1,X2),
    distance((X1,Y1),(X2,Y2),1).



/*
solve(Start, Solutions, G, N)
Solution is a path (in reverse order) from start node to a goal state.
G is the length of the path, N is the number of nodes expanded.
*/

solve(goal(X1,Y1),Solution,G,N):-
    goal(goal(X1,Y1)),
    XP1 is X1 + 1,
    XD1 is X1 - 1,
    YP1 is Y1 + 1,
    YD1 is Y1 - 1,
    between(YD1,YP1,Y2),
    between(XD1,XP1,X2),
    land_or_dropped(X2,Y2),
    distance((X1,Y1),(X2,Y2),1),
    Solution = [goal(X2,Y2),goal(X1,Y1)],
    G = 2,
    N = 1.






%############################################################################
% Uniform Cost Search, using Dijkstras Algorithm

% COMP3411/9414/9814 Artificial Intelligence, UNSW, Alan Blair

% solve(Start, Solution, G, N)
% Solution is a path (in reverse order) from start node to a goal state.
% G is the length of the path, N is the number of nodes expanded.

solve(Start, Solution, G, N)  :-
    % consult(pathsearch), % insert_legs(), head_member(), build_path()
    ucsdijkstra([[Start,Start,0]], [], Solution, G, 1, N).

% ucsdijkstra(Generated, Expanded, Solution, L, N)
%
% The algorithm builds a list of generated "legs" in the form
% Generated = [[Node1,Prev1,G1],[Node2,Prev2,G2],...,[Start,Start,0]]
% The path length G from the start node is stored with each leg,
% and the legs are listed in increasing order of G.
% The expanded nodes are moved to another list (G is discarded)
%  Expanded = [[Node1,Prev1],[Node2,Prev2],...,[Start,Start]]

% If the next leg to be expanded reaches a goal node,
% stop searching, build the path and return it.
ucsdijkstra([[Node,Pred,G]|_Generated], Expanded, Path, G, N, N)  :-
    goal(Node),
    build_path([[Node,Pred]|Expanded], Path).

% Extend the leg at the head of the queue by generating the
% successors of its destination node.
% Insert these newly created legs into the list of generated nodes,
% keeping it sorted in increasing order of G; and continue searching.
ucsdijkstra([[Node,Pred,G]| Generated], Expanded, Solution, G1, L, N) :-
    extend(Node, G, Expanded, NewLegs),
    M is L + 1,
    insert_legs(Generated, NewLegs, Generated1),
    ucsdijkstra(Generated1, [[Node,Pred]|Expanded], Solution, G1, M, N).

% Find all successor nodes to this node, and check in each case
% that the new node has not previously been expanded.
extend(Node, G, Expanded, NewLegs) :-
    % write(Node),nl,   % print nodes as they are expanded
    findall([NewNode, Node, G1], (s(Node, NewNode, C)
    , not(head_member(NewNode, Expanded))
    , G1 is G + C
    ), NewLegs).

% base case: insert leg into an empty list.
insert_one_leg([], Leg, [Leg]).

% If we already knew a shorter path to the same node, discard the new one.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated]) :-
    Leg  = [Node,_Pred, G ],
    Leg1 = [Node,_Pred1,G1],
    G >= G1, ! .

% Insert the new leg in its correct place in the list (ordered by G).
insert_one_leg([Leg1|Generated], Leg, [Leg,Leg1|Generated]) :-
    Leg  = [_Node, _Pred, G ],
    Leg1 = [_Node1,_Pred1,G1],
    G < G1, ! .

% Search recursively for the correct place to insert.
insert_one_leg([Leg1|Generated], Leg, [Leg1|Generated1]) :-
    insert_one_leg(Generated, Leg, Generated1).

% insert_legs(Generated, Legs, Generated1).
% insert new legs into list of generated legs,
% by repeatedly calling insert_one_leg()

% base case: no legs to be inserted
insert_legs(Generated, [], Generated).

% Insert the first leg using insert_one_leg(); and continue.
insert_legs(Generated, [Leg|Legs], Generated2) :-
   insert_one_leg(Generated, Leg, Generated1),
   insert_legs(Generated1, Legs, Generated2).

% head_member(Node, List)
% check whether Node is the head of a member of List.

% base case: node is the head of first item in list.
head_member(Node,[[Node,_]|_]).

% otherwise, keep searching for node in the tail.
head_member(Node,[_|Tail]) :-
  head_member(Node,Tail).

% build_path(Expanded, [[Node,Pred]], Path).

% build_path(Legs, Path)
% Construct a path from a list of legs, by joining the ones that match.

% base case: join the last two legs to form a path of one step.
build_path([[Next,Start],[Start,Start]], [Next,Start]).

% If the first two legs match, add to the front of the path.
build_path([[C,B],[B,A]|Expanded],[C,B,A|Path]) :-
   build_path([[B,A]|Expanded],[B,A|Path]), ! .

% If the above rule fails, we skip the next leg in the list.
build_path([Leg,_SkipLeg|Expanded],Path) :-
   build_path([Leg|Expanded],Path).
%____________________________________________________________________________