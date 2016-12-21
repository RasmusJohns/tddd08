%-----------------------------------------------------------------------
% 2.1
%-----------------------------------------------------------------------

% Inserts a number in a sorted list
insertNumber(X, [], [X]).
insertNumber(X, [Y|Tail], [X,Y|Tail]):-
  X < Y, !.
insertNumber(X, [Y|Tail], [Y|TailOut]):-
  insertNumber(X, Tail, TailOut).

% Insertion sort
isort([],[]).

isort([Element|Unsorted], Sorted):-
  isort(Unsorted, SortedSubList),
  insertNumber(Element, SortedSubList, Sorted).

% Körexempel
%  ?- isort([2,5,4,2,67,8,4], X).
%  X = [2, 2, 4, 4, 5, 8, 67] ;
%  false.

% Quick sort
qsort([],[]).

qsort([Head|List], Result):-
  split(Head, List, Lower, Upper),
  qsort(Lower, LowerList),
  qsort(Upper, UpperList),
  append(LowerList, [Head|UpperList], Result), !.

% Körexempel
%  ?- qsort([2,5,4,2,67,8,4], X).
%  X = [2, 2, 4, 4, 5, 8, 67].

% Splits a list
split(_, [], _, _).

split(Ref, [Head|List], Lower, [Head|Upper]):-
  Ref =< Head,
  split(Ref, List, Lower, Upper).

split(Ref, [Head|List], [Head|Lower], Upper):-
  Ref > Head,
  split(Ref, List, Lower, Upper).


  %-----------------------------------------------------------------------
  % 2.2
  %-----------------------------------------------------------------------

% X is the middle element in the list Xs
middle(X, [X]).
middle(X, [_|Xs]) :-
  append(Middle, [_], Xs),
  middle(X, Middle).


%-----------------------------------------------------------------------
% 2.3
%-----------------------------------------------------------------------

% KÖREXEMPEL
%  ?- execute([[x,3]], seq(set(id(y),num(1)),while(id(x) > num(1),seq(set(id(y), id(y) * id(x)),set(id(x), id(x) - num(1))))), Sn).
%  Sn = [[x, 1], [y, 6]] ;
%  false.

id(_).
num(X):- number(X).


% set_env will add or update a variable
set_env([], I, E, [[I, E]]).

set_env([[ICurrent, _]|T], I, E, [[ICurrent, E]|T]):-
  ICurrent==I.

set_env([[ICurrent, ECurrent]|T], I, E, [[ICurrent, ECurrent]|Y]):-
  ICurrent\=I,
  set_env(T, I, E, Y).

execute(X, skip, X).

% set I to value of expression E
execute(X, set(id(I), E), Y):-
  eval(X, E, EV),
  set_env(X, I, EV, Y).

% Run seq C1 and C2
execute(X, seq(C1, C2), Y):-
  execute(X, C1, Y1),
  execute(Y1, C2, Y).

% Run C1 if B=true
execute(X, if(B,C1,_), Y):-
  eval_bool(X, B, Test),
  Test,
  execute(X, C1, Y).

% Run C2 if B\=true
execute(X, if(B,_,C2), Y):-
  eval_bool(X, B, Test),
  not(Test),
  execute(X, C2, Y).

% Run C as long as B=true
execute(X, while(B, C), Y):-
  eval_bool(X, B, Test),
  Test,
  execute(X, C, Y1),
  execute(Y1, while(B,C), Y).

execute(X, while(B, _), X):-
  eval_bool(X, B, Test),
  not(Test).

% eval will evaluate all kind of expressions (number, identifier, boolean expressions, arithmetics)
eval(X, id(I), E):-
  member([I,E], X).

eval(X, id(I), _):-
  not(member([I,_],X)),
  write("WARNING: using unset identifier").

eval(_, num(N), N).

eval(_, true, true).
eval(_, false, false).

eval(X, A1 + B1, Y):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  Y is A2 + B2.

eval(X, A1 - B1, Y):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  Y is A2 - B2.

eval(X, A1 * B1, Y):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  Y is A2 * B2.

eval(X, A1 / B1, Y):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  Y is A2 / B2.

eval_bool(X, A1 = B1, Y):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  Y = (A2 == B2).

eval_bool(X, A1 > B1, Y):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  Y = (A2 > B2).

eval_bool(X, A1 >= B1, Y):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  Y = (A2 >= B2).

%-----------------------------------------------------------------------
% 2.4
%-----------------------------------------------------------------------

union([],[],[]).
union([], [H|T], [H|T]).

% recursive union, will add L2 and everything that's not L2
union([H|T], L2, Out):-
  member(H, L2),
  union(T, L2, Out).

union([H|T], L2, [H|Out]):-
  not(member(H, L2)),
  union(T, L2, Out).

% Körexempel

% Base case
intersect([], _, []).
% recursively add all H in L2 to Out
intersect([H|T], L2, [H|Out]):-
  member(H, L2),
  intersect(T, L2, Out).

intersect([H|T], L2, Out):-
  not(member(H, L2)),
  intersect(T, L2, Out).

% Körexempel
%  ?- intersect([a,b],[b,c,d], X).
%  X = [b] ;
%  false.

% Out will be a powerset of In
powerset(In, Out):-
  findall(OutTemp, ps(OutTemp, In), Out).

% Körexempel
%  ?- powerset([a,b,c], X).
%  X = [[], [a], [a, b], [a, b, c], [a, c], [b], [b, c], [c]].

ps([], []). % Base case
ps([], [_|_]). % Not empty list
ps([H|Tail], [H|In]):- % Recursive destruction and return
  ps(Tail, In).
ps([H|Tail], [_|List]):- % Recursive destruction with append to List
  append(_, [H|In], List),
  ps(Tail, In).
