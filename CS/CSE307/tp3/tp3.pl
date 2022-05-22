% CLP over Rational numbers
:- use_module(library(clpq)).
% use natural display in print (X/Y instead of XrY)
:- set_prolog_flag(rational_syntax, natural).
% lambda notation >>
:- use_module(library(yall)).

%%% Q1

%! spreadsheet(+NRows:int, +NColumns:int, ?Spreadsheet:list) is det.
%
% Spreadsheet is a list of NRows rows, each is a list of NColumns elements.

% spreadsheet(NRows, NColumns, list)
spreadsheet(1, NC, [L]) :- length(L, NC).
spreadsheet(NR, NC, [X | L]) :-  
  {N1 = NR-1},
  length(X, NC), 
  spreadsheet(N1, NC, L).

%%% Q3

%! element(?Spreadsheet:list, +Cell:atom, ?Elt) is det.
%
% Return the element Elt from Spreadsheet, at address given by atom Cell

nth1([E | L], 1, X) :- X = E.
nth1([E | L], N, X) :-
  {N1 = N - 1},
  nth1(L, N1, X).

element(L, C, E) :-
  cell_xy(C, X, Y),
  nth1(L, Y, L1),
  nth1(L1, X, E).

%%% Q2

%! cell_xy(+Cell:atom, -X:int, -Y:int) is det.
%
% Decompose a cell name into integer coordinates.

cell_xy(Cell, X, Y) :- 
  atom_chars(Cell, L),
  L = [L0, L1],
  char_code(L0, X1),
  char_code(L1, Y1),
  {X = X1 - 96},
  {Y = Y1 - 48}.

%! cell_xy_rev(-Cell:atom, +X:int, +Y:int) is det.
cell_xy_rev(Cell, X, Y) :- 
  {X = X1 - 96},
  {Y = Y1 - 48},
  char_code(L0, X1),
  char_code(L1, Y1),
  L = [L0, L1],
  atom_chars(Cell, L).
%%% Q5

%! elements(?Spreadsheet:list, Rectangle, Elts:list) is det.
%
% Return all the list of elements in the Rectangle = Cell1:Cell2
% where Cell1 is upper-left and Cell2 is lower-right

elements(L, R, E) :-
  UL:LR = R,
  spreadsheet(NR, NC, L),
  trans2(NR, NC, UL, LR, C),
  elements-help(L, C, E).

% elements-help(+Spreadsheet:list, +Cell:list, Elts:List)
elements-help(L, [R], E) :- element(L, R, ES), E = [ES].
elements-help(L, [R | RL], E) :- element(L, R, E1), elements-help(L, RL, EL), E = [E1 | EL].


% trans2(NR:int, NC:int, +upper-left, +lower-right, -Cell:list)
trans2(NR, NC, UL, LR, C) :-
  cell_xy(UL, X1, Y1),
  cell_xy(LR, X2, Y2),
  {X2 =< NC},
  {Y2 =< NR},
  {Y1 = Y2},
  {X1 = X2},
  C = [UL].

trans2(NR, NC, UL, LR, C) :-
  cell_xy(UL, X1, Y1),
  cell_xy(LR, X2, Y2),
  {X2 =< NC},
  {Y2 =< NR},
  {Y1 < Y2},
  {X1 < X2},
  {X3 = X1 + 1},
  cell_xy_rev(UL1, X3, Y1),
  trans2(NR, NC, UL1, LR, C1),
  C = [UL | C1].

trans2(NR, NC, UL, LR, C) :-
  cell_xy(UL, X1, Y1),
  cell_xy(LR, X2, Y2),
  {X2 =< NC},
  {Y2 =< NR},
  {Y1 < Y2},
  {X1 = X2},
  {X3 = 1},
  {Y3 = Y1 + 1},
  cell_xy_rev(UL1, X3, Y3),
  trans2(NR, NC, UL1, LR, C1),
  C = [UL | C1].

trans2(NR, NC, UL, LR, C) :-
  cell_xy(UL, X1, Y1),
  cell_xy(LR, X2, Y2),
  {X2 =< NC},
  {Y2 =< NR},
  {Y1 = Y2},
  {X1 < X2},
  {X3 = X1 + 1},
  cell_xy_rev(UL1, X3, Y1),
  trans2(NR, NC, UL1, LR, C1),
  C = [UL | C1].

%%% Q4 and Q6

%! formula(?Spreadsheet:list, +Formula) is nondet.
%
% Parse the term Formula, convert cell names to their contents and add the corresponding constraint.

sum(L, R) :- foldl([X,Y,Z]>>{Z=X+Y}, L, 0, R).

parse(L, X , XX) :- 
  number(X),
  XX = X.

parse(L, X, XX) :-
  atom(X),
  element(L, X, XX).

parse(L, X*Y, XX) :-
  parse(L, X, X1),
  parse(L, Y, Y1),
  XX = X1 * Y1.

parse(L, X+Y, XX) :-
  parse(L, X, X1),
  parse(L, Y, Y1),
  XX = X1 + Y1.

parse(L, X=Y, XX) :- 
  element(L, X, X1),
  parse(L, Y, Y1),
  XX = (X1 = Y1).

parse(L, sum(Rec), XX) :-
  elements(L, Rec, E),
  sum(E, XX).

parse(L, mean(Rec), XX) :-
  elements(L, Rec, E),
  sum(E, X),
  length(E, LEN),
  XX = X / LEN.

formula(S, X) :-
  parse(S, X, XX),
  format('~w~n', X),
  {XX}.

%% ?- spreadsheet(2, 3, S), formula(S, a2 = 2), formula(S, c1 = 6), formula(S, c1 = b1 * a2), formula(S, a1 = mean(a1:c1)), display_sheet(S).
%% a2=2
%% c1=6
%% c1=b1*a2
%% a1=mean(a1:c1)
%%   |       a |        b |        c |
%%   ---------------------------------
%%  1|     9/2 |        3 |        6 |
%%  2|       2 |    _5248 |    _5254 |
%%   ---------------------------------

%% S = [[9/2, 3, 6], [2, _5248, _5254]] .



%%% Provided

%! display_sheet(?Spreadsheet:list) is det.
%
% pretty print Spreadsheet
display_sheet(Spreadsheet) :-
  header_line(Spreadsheet, HeaderString, LineFormat),
  write(HeaderString),
  format(LineFormat, []),
  rows_to_string(Spreadsheet, S),
  write(S),
  format(LineFormat, []),
  nl.


%! header_line(?Spreadsheet:list, -HeaderString:string, -LineFormat:atom) is det.
%
% Builds a header string and a format-string for a line for the given Spreadsheet
header_line(Spreadsheet, HeaderString, LineFormat) :-
  Spreadsheet = [Row | _],
  length(Row, NCols),
  {Total = NCols * 11},
  format(atom(LineFormat), '  ~~|~~`-t~~~w+~~n', [Total]),
  header(Row, Header),
  row_to_string('  ', Header, HeaderString).


%! cell_to_atom(?Cell, -Atom) is det.
%
% Makes an atom of length 8, right aligned for the displayed content of a cell
cell_to_atom(Cell, Atom) :-
  format(atom(Atom), '~|~t~p~8+', [Cell]).


%! row_to_string(+Prefix, ?Row, -String) is det.
%
% Build a string for a whole row Row with given prefix Prefix
row_to_string(Prefix, Row, String) :-
  maplist(cell_to_atom, Row, AtomRow),
  atomics_to_string(AtomRow, ' | ', Joined),
  atomics_to_string([Prefix, '|', Joined, ' |\n'], String).


%! rows_to_string(?Spreadsheet:list, -String:string) is det.
%
% Build a string for all rows of a Spreadsheet
rows_to_string(Spreadsheet, String) :-
  rows_to_strings(Spreadsheet, Strings, 1),
  atomics_to_string(Strings, String).


%! rows_to_strings(?Rows:list, -Strings:list, +N:int) is det
%
% Build a list of Strings with increasing prefix numbered from N
rows_to_strings([], [], _).

rows_to_strings([Row | Rows], [String | Strings], N) :-
  format(atom(Prefix), '~|~t~d~2+', [N]),
  row_to_string(Prefix, Row, String),
  {NN = N + 1},
  rows_to_strings(Rows, Strings, NN).


%! header(?Row:list, -Header:list) is det.
%
% Build a header row from an example row (for size)
header(Row, Header) :-
  header(Row, Header, 'a').

%! header(?Row:list, -Header:list, +Column:char) is det.
%
% Build a header from an example row (for size) starting with given Column name
header([], [], _).

header([_ | Row], [C | Header], C) :-
  char_code(C, Code),
  {CCode = Code + 1},
  char_code(CC, CCode),
  header(Row, Header, CC).
