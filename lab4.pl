% The data structure is represented by a list of three elements [A,B,C].
% A is the number of missionaries on the wrong side of the rivier.
% B is the number of cannibals on the wrong side.
% C is 1 if the boat is on the wrong side and C is 0 otherwise.


% Körexempel

%  ?- dfs([3,3,1], [], X).
%  X = [[3, 3, 1], [3, 1, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2|...], [0|...], [...|...]|...] [write]
%  X = [[3, 3, 1], [3, 1, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2, 1], [0, 2, 0], [0, 3, 1], [0, 1, 0], [1, 1, 1], [0, 0, 0]] ;
%  X = [[3, 3, 1], [3, 1, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2, 1], [0, 2, 0], [0, 3, 1], [0, 1, 0], [0, 2, 1], [0, 0, 0]] ;
%  X = [[3, 3, 1], [2, 2, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2, 1], [0, 2, 0], [0, 3, 1], [0, 1, 0], [1, 1, 1], [0, 0, 0]] ;
%  X = [[3, 3, 1], [2, 2, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2, 1], [0, 2, 0], [0, 3, 1], [0, 1, 0], [0, 2, 1], [0, 0, 0]] ;
%  false.


%  ?- bfs([[[3,3,1], [], []]], X).
%  X = [[3, 3, 1], [3, 1, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2|...], [0|...], [...|...]|...] [write]
%  X = [[3, 3, 1], [3, 1, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2, 1], [0, 2, 0], [0, 3, 1], [0, 1, 0], [1, 1, 1], [0, 0, 0]] ;
%  X = [[3, 3, 1], [3, 1, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2, 1], [0, 2, 0], [0, 3, 1], [0, 1, 0], [0, 2, 1], [0, 0, 0]] ;
%  X = [[3, 3, 1], [2, 2, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2, 1], [0, 2, 0], [0, 3, 1], [0, 1, 0], [1, 1, 1], [0, 0, 0]] ;
%  X = [[3, 3, 1], [2, 2, 0], [3, 2, 1], [3, 0, 0], [3, 1, 1], [1, 1, 0], [2, 2, 1], [0, 2, 0], [0, 3, 1], [0, 1, 0], [0, 2, 1], [0, 0, 0]] ;
%  false.

% All possible actions which can be done each "turn".

action(X):-
  X = [1,0,1].
action(X):-
  X = [2,0,1].
action(X):-
  X = [0,1,1].
action(X):-
  X = [0,2,1].
action(X):-
  X = [1,1,1].
action(X):-
  X = [-1,0,-1].
action(X):-
  X = [-2,0,-1].
action(X):-
  X = [0,-1,-1].
action(X):-
  X = [0,-2,-1].
action(X):-
  X = [-1,-1,-1].

% Final DFS case. Answer found.
dfs([0,0,0], _, [[0,0,0]]).

% Actual DFS search.
dfs(State, Visited, [State|Result]):-

  % Get an action
  action(A),

  % Subtract the action's impact from the current state,
  % resulting in a NewState
  sub(State, A, NewState),

  % Check that it is a valid action.
  check(NewState, Visited),

  % Go down the search tree.
  dfs(NewState, [State|Visited], Result).


% Final case. [0,0,0] needs to be added to the result.
bfs([[[0,0,0], _, Result]|_], Final_Result):-
  append(Result, [[0,0,0]], Final_Result).

% Even if we found one result, we want BFS to keep searching.
bfs([[[0,0,0], _, _]|States], Temp_Result):-
  bfs(States, Temp_Result).

% Even if BFS checks one failing scenario, we want it to keep looking
% for a solution.
bfs([[State, Visited, _]|States], Result):-
  not(check(State, Visited)),
  bfs(States, Result).

% The actual branching of the search.
bfs([[State, Visited, Result]|States], Temp_Result):-

  % We do not want to search from [0,0,0], as that equals goal.
  State \= [0,0,0],

  % Update our list of visited nodes.
  append(Visited, [State], NewVisited),

  % Check that we are in a good state and have not visited this node.
  check(State, Visited),

  % All new branches are starting to get initialized.
  sub(State, [1,0,1], NewState1),
  sub(State, [2,0,1], NewState2),
  sub(State, [0,1,1], NewState3),
  sub(State, [0,2,1], NewState4),
  sub(State, [1,1,1], NewState5),
  sub(State, [-1,0,-1], NewState6),
  sub(State, [-2,0,-1], NewState7),
  sub(State, [0,-1,-1], NewState8),
  sub(State, [0,-2,-1], NewState9),
  sub(State, [-1,-1,-1], NewState10),

  % We add our current state to the end of the result.
  append(Result, [State], Y),

  % All our new searches are queued. Each search inherits its parents visited and result lists.
  append(States, [[NewState1, NewVisited, Y], [NewState2, NewVisited, Y], [NewState3, NewVisited, Y], [NewState4, NewVisited, Y], [NewState5, NewVisited, Y],
                  [NewState6, NewVisited, Y], [NewState7, NewVisited, Y], [NewState8, NewVisited, Y], [NewState9, NewVisited, Y], [NewState10, NewVisited, Y]], NewStates),
  bfs(NewStates, Temp_Result).

% All the allowed states. Used by the testing-function "check".
testState([3,3]).
testState([2,2]).
testState([1,1]).
testState([0,0]).
testState([3,2]).
testState([3,1]).
testState([3,0]).
testState([0,1]).
testState([0,2]).
testState([0,3]).

% Checks that a state is valid and it has not been visited.
check(State, Visited):-
  nth0(0, State, Element0),
  nth0(1, State, Element1),
  nth0(2, State, Element2),

  Element2 >= 0,
  Element2 =< 1,

  not(member(State, Visited)),

  testState([Element0, Element1]), !.

% Used to perform actions on states (subracting action values from
% state values .
sub([], [], []).
sub([H1|T1], [H2|T2], [X|Result]):-
  X is H1 - H2,
  sub(T1, T2, Result).
