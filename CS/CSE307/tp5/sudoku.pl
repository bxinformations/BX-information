:- use_module(library(clpfd)).

example2 :-
   Sudoku = [[_,_,_, _,_,_, _,_,_],
             [_,_,_, _,_,3, _,8,5],
             [_,_,1, _,2,_, _,_,_],

             [_,_,_, 5,_,7, _,_,_],
             [_,_,4, _,_,_, 1,_,_],
             [_,9,_, _,_,_, _,_,_],

             [5,_,_, _,_,_, _,7,3],
             [_,_,2, _,1,_, _,_,_],
             [_,_,_, _,4,_, _,_,9]],
   sudoku(Sudoku),
   maplist(writeln, Sudoku).


example1 :-
   Sudoku = [[5,3,_, _,7,_, _,_,_],
             [6,_,_, 1,9,5, _,_,_],
             [_,9,8, _,_,_, _,6,_],

             [8,_,_, _,6,_, _,_,3],
             [4,_,_, 8,_,3, _,_,1],
             [7,_,_, _,2,_, _,_,6],

             [_,6,_, _,_,_, 2,8,_],
             [_,_,_, 4,1,9, _,_,5],
             [_,_,_, _,8,_, _,7,9]],
   sudoku(Sudoku),
   maplist(writeln, Sudoku).

three_blocks([[], [], []], []).
three_blocks([[L11, L12, L13 | L1S], [L21, L22, L23 | L2S], [L31, L32, L33 |L3S]], [X | XS]) :-
   X = [L11, L12, L13, L21, L22, L23, L31, L32, L33],
   three_blocks([L1S, L2S, L3S], XS).

sudoku([L1, L2, L3, L4, L5, L6, L7, L8, L9]) :-
   flatten([L1, L2, L3, L4, L5, L6, L7, L8, L9], L),
   L ins 1..9,
   maplist(all_distinct, [L1, L2, L3, L4, L5, L6, L7, L8, L9]),
   transpose([L1, L2, L3, L4, L5, L6, L7, L8, L9], LP),
   maplist(all_distinct, LP),
   three_blocks([L1, L2, L3], X),
   maplist(all_distinct, X),
   three_blocks([L4, L5, L6], X1),
   maplist(all_distinct, X1),
   three_blocks([L7, L8, L9], X2),
   maplist(all_distinct, X2).

% for example1 it works without label, since it has only one solution
% for example2 it won't give one pricise result, since it has more than one solutions

% with all_distinct it will give a pricise solution for example2 without label