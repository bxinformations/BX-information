%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                     %
% CSE 307: Constraint Logic Programming - F. Fages    %
%                                                     %
%                                                     %
% TP2: Symbolic differentiation in Prolog             %
%                                                     %
%      List processing and road map navigation        %
%                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART I. Symbolic Differentiation %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 1. Write a symbolic differentiation predicate differentiate(Expression, Variable, Derivative)


differentiate(X, Y, Z) :- atom(X), (X = Y -> Z = 1; Z = 0).
differentiate(X, Y, Z) :- number(X), Z = 0.
differentiate(X, Y, Z) :- X = A + B, differentiate(A, Y, C), differentiate(B, Y, D), Z = C + D.
differentiate(X, Y, Z) :- X = A - B, differentiate(A, Y, C), differentiate(B, Y, D), Z = C - D.
differentiate(X, Y, Z) :- X = A * B, differentiate(A, Y, C), differentiate(B, Y, D), Z = C * B + D * A.




% 2. Write an algebraic simplification predicate simplify(Expression, SimplifiedExpression).


simplify(C, C) :- atom(C), !.
simplify(X + 0, Y) :- simplify(X, Y).
simplify(0 + X, Y) :- simplify(X, Y).
simplify(X - 0, Y) :- simplify(X, Y).
simplify(0 - X, -(Y)) :- simplify(X, Y).
simplify(A + B, C) :- number(A), number(B), C is A + B.
simplify(A - A, 0).
simplify(A - B, C) :- number(A), number(B), C is A - B.
simplify(A * B, C) :- number(A), number(B), C is A * B.
simplify(X * 0, 0).
simplify(0 * X, 0).
simplify(1 * X, X).
simplify(X * 1, X).
simplify(A * X + B * X, Z) :- simplify(A + B, C), simplify(C * X, Z).
simplify(X + Y, Z) :- simplify(X, C), simplify(Y, D), (X \== C; Y \== D), simplify(C + D, Z).
simplify(X * Y, Z) :- simplify(X, C), simplify(Y, D), (X \== C; Y \== D), simplify(C * D, Z).
simplify(X - Y, Z) :- simplify(X, C), simplify(Y, D), (X \== C; Y \== D), simplify(C - D, Z).
simplify(X, X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART II. List Processing      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The member/2 predicate can be defined by

member(X, [X|_]).

member(X, [_ | L]):-
  member(X, L).

% 3. Represent the cyclic graph of the TP with predicate arc/2 
%    and define a non looping predicate path/3 where the third argument is a list of forbidden intermediate cities 
%    (that list may be empty initially






arc(amsterdam, paris).
arc(paris, lyon).
arc(lausanne, verone).
arc(lyon, verone).
arc(lyon, rome).
arc(verone, rome).
arc(rome, paris).

path(X, Y, L) :- arc(X, Y);
                 arc(X, Z), \+(member(Z, L)), path(Z, Y, [Z | L]).




% Now, let us use a bit of arithmetic constraints (instead of evaluable predicates) to count elements

:- use_module(library(clpfd)). 

% One can define the length of a list as follows

listlength([], 0).

listlength([_|L], N) :-
  N #> 0,
  N1#= N-1,
  listlength(L, N1).

% 4. Define the predicate enumerate(Min, Max, L) true if L is the list of integers from Min to Max


enumerate(M, M, [M]).
enumerate(Min, Max, []) :-
  Min #> Max.
enumerate(Min, Max, [Min | L]) :-
  Min #< Max,
  Min1 #= Min + 1,
  enumerate(Min1, Max, L).


    


% 5. Label the arcs of the graph with such travel times using a ternary  arc/3 predicate, and add the total expected time it will take, as fourth argument to path(X, Y, L, Time).

arc(amsterdam, paris, 1).
arc(paris, lyon, 1).
arc(lausanne, verone, 4).
arc(lyon, verone, 3).
arc(lyon, rome, 2).
arc(verone, rome, 2).
arc(rome, paris, 1).

path(X, X, L, 0).
path(X, Y, L, Time) :- arc(X, Z, C), \+(member(Z, L)), path(Z, Y, [Z | L], D), Time is C + D.








% In the course we have explained the following predicate for solving the Hanoi tower puzzle
% hanoi(N, X, Y, Z) moves N disks from the top of X to the top of Z using the top of Y

hanoi(N,X,_,Z) :-
  N #= 1,
  format('Move top disk from ~w to ~w~n', [X,Z]). 

hanoi(N,X,Y,Z) :- 
  N #> 1, 
  N1 #= N-1, 
  hanoi(N1,X,Z,Y), 
  format('Move top disk from ~w to ~w~n', [X,Z]), 
  hanoi(N1,Y,X,Z).

% 6. Write a predicate hanoiviz(N, X, Y, Z, X2, Y2, Z2) 
%    where X, Y, Z, X2, Y2, Z2 are lists of the form [left, disks...], [middle, disks...], [right, disks...]
%    in order to print the evolution of the pegs after each move
%
%    ? hanoiviz(4,[left,1,2,3,4],[middle],[right],L,M,R)



hanoiviz(N, X, Y, Z, X2, Y2, Z2) :-
  N #= 1,
  X = [P | [NOW | L]],
  Y = Y2,
  X2 = [P | L],
  Z = [P1 | L1],
  Z2 = [P1 | [NOW | L1]],
  sort([X2, Y2, Z2], L2),
  format('~w~n~w~n~w~n~n', L2).

hanoiviz(N, X, Y, Z, X2, Y2, Z2) :-
  N #> 1, 
  N1 #= N-1, 
  hanoiviz(N1, X, Z, Y, X1, Z1, Y1), 
  X1 = [P | [NOW | L]],
  Y3 = Y1,
  X3 = [P | L],
  Z1 = [P1 | L1],
  Z3 = [P1 | [NOW | L1]],
  sort([X3, Y3, Z3], L2),
  format('~w~n~w~n~w~n~n', L2),
  hanoiviz(N1, Y3, X3, Z3, Y2, X2, Z2).




% following is the result for ? hanoiviz(4,[left,1,2,3,4],[middle],[right],L,M,R)
%[left,2,3,4]
%[middle,1]
%[right]
%
%[left,3,4]
%[middle,1]
%[right,2]
%
%[left,3,4]
%[middle]
%[right,1,2]
%
%[left,4]
%[middle,3]
%[right,1,2]
%
%[left,1,4]
%[middle,3]
%[right,2]
%
%[left,1,4]
%[middle,2,3]
%[right]
%
%[left,4]
%[middle,1,2,3]
%[right]
%
%[left]
%[middle,1,2,3]
%[right,4]
%
%[left]
%[middle,2,3]
%[right,1,4]
%
%[left,2]
%[middle,3]
%[right,1,4]
%
%[left,1,2]
%[middle,3]
%[right,4]
%
%[left,1,2]
%[middle]
%[right,3,4]
%
%[left,2]
%[middle,1]
%[right,3,4]
%
%[left]
%[middle,1]
%[right,2,3,4]
%
%[left]
%[middle]
%[right,1,2,3,4]
%
% L = [left],
% M = [middle],
% R = [right, 1, 2, 3, 4] ;
% false.