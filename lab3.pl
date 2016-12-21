%Tokens = [id(x),:=,num(3),;,id(y),:=,num(1),;,while,id(x),>,num(1),do,id(y),:=,id(y),*,id(x),;,id(x),:=,id(x),-,num(1),od].
%Tokens = [id(x),:=,num(3),;,id(y),:=,num(1),;,if,id(x),>,num(1),do,id(y),:=,id(y),*,id(x),;,id(x),:=,id(x),-,num(1),od].

%-----------------------------------------------------------------------
% 3.1
%-----------------------------------------------------------------------

% run([x=3], "y:=1; z:=0; while x>z do z:=z+1; y:=y*z od", Res).
% run([x=3], "y:=1; z:=0; if x>z do z:=z+1; y:=y*z od", Res).

% Körexempel
%  ?- run([x=3], "y:=1; z:=0; while x>z do z:=z+1; y:=y*z od", X).
%  X = [[x, 1], [y, 6]] ;
%  false.


run(In, String, Out):-
  scan(String, Tokens),
  parse(Tokens, AbstStx),
  execute(In, AbstStx, Out).

parse(Tokens, AbstStx):-
  pgm(AbstStx, Tokens, []).

% A program can be a command
pgm(C) --> cmd(C).

% A program can be a sequence of commands
pgm(seq(C1,C2)) --> cmd(C1), [;], pgm(C2).

% A command can be skip, set(), If() or while()
cmd(skip) --> [skip].
cmd(set(I,E)) --> identifier(I), [:=], expr(E).
cmd(if(B,C1,C2)) --> [if], bool(B), [then], pgm(C1), [else], pgm(C2), [fi].
cmd(while(B,C1)) --> [while], bool(B), [do], pgm(C1), [od].

% Define boolean expressions
bool(E1 = E2) --> expr(E1), [=], expr(E2).
bool(E1 > E2) --> expr(E1), [>], expr(E2).
bool(E1 >= E2) --> expr(E1), [>=], expr(E2).

% Define Expressions
expr(F * E) --> factor(F), [*], expr(E).
expr(F) --> factor(F).

% Define factors
factor(T + F) --> term(T), [+], factor(F).
factor(T - F) --> term(T), [-], factor(F).
factor(T) --> term(T).

% Define terms
term(I) --> identifier(I).
term(N) --> numb(N).

% Define atoms
identifier(id(X))--> [id(X)].
numb(num(X))--> [num(X)].



%-----------------------------------------------------------------------
% 2.3
%-----------------------------------------------------------------------

% TEST PROGRAMS
%execute([[x,3]], while(id(x)>num(1), seq(set(id(x), id(x) - num(1)), skip)), Sn).
%execute([[x,3]], seq(set(id(y),num(1)),while(id(x) > num(1),seq(set(id(y), id(y) * id(x)),set(id(x), id(x) - num(1))))), Sn).

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
  eval(X,B),
  execute(X, C1, Y).

% Run C2 if B\=true
execute(X, if(B,_,C2), Y):-
  not(eval(X,B)),
  execute(X, C2, Y).

% Run C as long as B=true
execute(X, while(B, C), Y):-
  eval(X,B),
  execute(X, C, Y1),
  execute(Y1, while(B,C), Y).

execute(X, while(B, _), X):-
  not(eval(X,B)).

% eval will evaluate all kind of expressions (number, identifier, boolean expressions, arithmetics)
eval(X, id(I), E):-
  member([I,E], X).

eval(X, id(I), _):-
  not(member([I,_],X)),
  write("WARNING: using unset identifier").

eval(_, num(N), N).

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

eval(X, A1 = B1):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  A2 == B2.

eval(X, A1 > B1):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  A2 > B2.

eval(X, A1 >= B1):-
  eval(X, A1, A2),
  eval(X, B1, B2),
  A2 >= B2.



% Scanner for assignment 3
% TDDD08 Logic Programming
%
% top predicate:
% scan(+String, -Tokens)
%
% try: scan("x:=3; y:=1; while x>1 do y := y*x; x := x-1 od",Tokens).
%
% NOTE: strings are lists of ASCII codes, i.e.
% "Prolog" = [80,114,111,108,111,103]

scan([],[]).
scan([C|Cs],[';'|Ts]) :-
	semicolon(C),!,
	scan(Cs,Ts).
scan([C|Cs],Ts) :-
	space(C),!,
	scan(Cs,Ts).
scan([C|Cs],[num(T)|Ts]) :-
	digit(C),!,
	scan_number(Cs,Cs1,CNum),
	name(T,[C|CNum]),
	scan(Cs1,Ts).
scan([C1,C2|Cs],[T|Ts]) :-
	name(T,[C1,C2]),
	operator(T),!,
	scan(Cs,Ts).
scan([C|Cs],[T|Ts]):-
	name(T,[C]),
	operator(T),!,
	scan(Cs,Ts).
scan([C|Cs],[T|Ts]) :-
	letter(C),
	scan_key_or_id(Cs,Cs1,CWord),
	name(Word,[C|CWord]),
	classify(Word,T),
	scan(Cs1,Ts).

% scaning a number
% scan_number(+In, -Out, -Num)
% Num is a string of digits from front of In,
% Out is the remaining string

scan_number([C|Cs],Cs1,[C|CN]) :-
	digit(C),!,
	scan_number(Cs,Cs1,CN).
scan_number(Cs,Cs,[]).

% scaning a keyword or an identifier
% scan_key_or_id(+In, -Out, -Word)
% Word is a string from front of In,
% Out is the remaining string

scan_key_or_id([C|Cs],Cs1,[C|CW]) :-
	(letter(C)
	 ;
	 digit(C)
	),!,
	scan_key_or_id(Cs,Cs1,CW).
scan_key_or_id(Cs,Cs,[]).

% distinguishing keywords from identifiers

classify(W,T) :-
	keyword(W),!,
	T = W.
classify(W,id(W)).


digit(C) :-
	C >= "0", C =< "9".


letter(C) :-
	C >= "a", C =< "z"
	;
	C >= "A", C =< "Z".


semicolon(59).


operator('*').
operator('+').
operator('/').
operator('-').
operator('>').
operator('<').
operator('=').
operator('=<').
operator('>=').
operator(':=').


space(32).


keyword(skip).
keyword(if).
keyword(then).
keyword(else).
keyword(fi).
keyword(while).
keyword(do).
keyword(od).
keyword(true).
keyword(false).
