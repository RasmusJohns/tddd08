:- use_module(library(clpfd)).

container(a,2,2).
container(b,4,1).
container(c,2,2).
container(d,1,1).

on(a,d).
on(b,c).
on(c,d).


%% KÖREXEMPEL %%
% schedule(X).
% X = 16.
%
% För att ändra körningen får man helt sonika justrera container(...)
% och on(...), alltså ändra världen.

%Compares all solutions and sets Sum to the solution with minimal cost.
schedule(Sum):-
  findall(X, all_solution_cost(X), AME),
  min_list(AME, Sum).

% Monster predicate that solves everything
all_solution_cost(Sum):-
  findall([B,M,D], container(_,M,D), Containers), % adds container()s to a list
  findall([A,B], on(A,B), Ons), % adds on()s to a list

  length(Containers, CL),
  length(S, CL),

  split_list(Containers, _, Mlist, Dlist),
  min_list(Mlist, MinM),
  sum_list(Mlist, SumM),
  sum_list(Dlist, SumD),

  S ins 0..SumD,

  [Workers] ins MinM..SumM, labeling([max(Workers)],[Workers]),

  create_tasks(Containers, Tasks, S, EndTimes),
  order_tasks(Containers, Ons, S),

  cumulative(Tasks, [limit(Workers)]),
  label(S),
  max_list(EndTimes, TotalTime), % Sets TotalTime to end time for last container
  Sum #= Workers*TotalTime. % Sets Sum to total cost

% Creates a task() for every container.
create_tasks([], [], [], []).
create_tasks([[_,M,D]|Containers], [task(ST,D,ET,M,_)|Tasks], [ST|STs], [ET|ETs]):-
  create_tasks(Containers, Tasks, STs, ETs).

% Set all on(A,B) relations to start times
order_tasks(_, [], _).
order_tasks(Containers, [[A,B]|Ons], S):-
  indexOf(Containers, [A,_,D], IndexA),
  indexOf(Containers, [B,_,_], IndexB),
  nth0(IndexA, S, S1),
  nth0(IndexB, S, S2),
  order(S1, S2, D),
  order_tasks(Containers, Ons, S).

% Set constrains of containers above each other
order(S1, S2, D):-
  S1 + D #=< S2.

% Finds the index for Element in a list.
indexOf([Element|_], Element, 0):- !.
indexOf([_|Tail], Element, Index):-
  indexOf(Tail, Element, Index1),
  !,
  Index is Index1+1.

% transform the list of containers to list of identifiers, persons required, duration to unload.
split_list([], [], [], []).
split_list([[B,M,D]|Cs], [B|Blist], [M|Mlist], [D|Dlist]):-
  split_list(Cs, Blist, Mlist, Dlist).
