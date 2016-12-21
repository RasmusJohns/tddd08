% Definitions
woman(ulrika).
woman(bettan).

man(nisse).
man(peter).
man(bosse).

beautiful(ulrika).
beautiful(nisse).
beautiful(peter).

rich(nisse).
rich(bettan).

strong(bettan).
strong(peter).
strong(bosse).

kind(bosse).


% All men likes beautiful women
like(X,Y):-
  man(X),
  woman(Y),
  beautiful(Y).

% Nisse likes all women who likes him
like(nisse, Y):-
  woman(Y),
  like(Y, nisse).

% Ulrika likes rich and kind men.
like(ulrika, X):-
  man(X),
  rich(X),
  kind(X),
  like(X, ulrika).

% Ulrika likes beautiful and strong men.
like(ulrika, X):-
  man(X),
  beautiful(X),
  strong(X),
  like(X, ulrika).

% The happy predicates can not be combined, seeing how some
% people these days do not define their gender. They
% might not be happy when they are rich.

% Rich men are happy men.
happy(X):-
  man(X),
  rich(X).

% Rich women are happy women.
happy(X):-
  woman(X),
  rich(X).

% A man is happy is he likes a woman who likes him.
happy(X):-
  man(X),
  woman(Y),
  like(X, Y),
  like(Y, X).

% A woman is happy if she likes a man who likes her.
happy(Y):-
  woman(Y),
  man(X),
  like(Y, X),
  like(X, Y).

%
% 1.1
%
% Questions asked to prolog in order to answer example questions in lab:
%
% Who is happy?
% happy(X).
%
% Who likes who?
% like(X,Y).
%
% How many persons like Ulrika?
% findall(X,like(X,ulrika),Y), length(Y,Z) ,write(Z),nl,fail.


%
% 1.2
%

% Definitions
edge(a,b).
edge(a,c).
edge(b,c).
edge(c,d).
edge(c,e).
edge(d,f).
edge(d,h).
edge(e,g).
edge(e,f).
edge(f,g).

% Base path/2.
path(X,Y):-
  edge(X,Y).

% Recursive path/2 used to answer:
% "Is there a path from X to Y?".
path(X,Y):-
  edge(X,Z),
  path(Z,Y).

% Base path/3.
path(X,Y,[Y]):-
  edge(X,Y).

% Recursive path/3 used to answer:
% "What is the way from X to Y?".
path(X,Y,[Z|Output]):-
  edge(X,Z),
  path(Z,Y,Output).

% Recursive npath/3 used to answer:
% "How long is the path from X to Y?".
npath(X,Y,Output):-
  path(X,Y,Length),
  length(Length,Output).


  %-----------------------------------------------------------------------
  % Körexempel
  %-----------------------------------------------------------------------

%?- happy(X).
%X = nisse ;
%X = bettan ;
%X = peter ;
%X = ulrika ;
%false.

%?- like(X,Y).
%X = nisse,
%Y = ulrika ;
%X = peter,
%Y = ulrika ;
%X = bosse,
%Y = ulrika ;
%X = ulrika,
%Y = peter ;
%false.

%?- findall(X,like(X,ulrika),Y), length(Y,Z) ,write(Z),nl,fail.
%3
%false.
