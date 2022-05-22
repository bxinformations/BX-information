#!prolog
/*
CSE307 - Constraint Logic Programming
Natural Language Processing
*/

%%%%%%%%%% Definition of the Grammar of the Language with construction of a Syntax Tree Term %%%%%%%%%%

sentence(s(N, V)):-
   nounphrase(N, Num),
   verbphrase(V, Num).

nounphrase(np(D, N)):-
   determiner(D, Num),
   noun(N, Num).

nounphrase(np(N)):-
   noun(N, Num).

verbphrase(vp(V)):-
   verb(V, Num).

verbphrase(vp(V, N)):-
   verb(V, Num),
   nounphrase(N).

verb(eats).
verb(eat).
verb(is).

determiner(my).
determiner(the).
determiner(a).


noun(monkey).
noun(monkeys).
noun(father).
noun(banana).
noun(bananas).
noun(fruit).


% You can enumerate the syntax trees of the sentences of the language with the query sentence(T)

test_sentence:-
  sentence(T),
  writeln(T).


% We want to compute the leaves of such syntax trees in a list of strings to get the list of words of a sentence
% You will use
%
% f(a,g(b)) =.. [f, a, g(b)] to decompose a term in the list of its head function symbol and arguments (subterms)
%
% compound(Term) true iff T is a compound term
%
% and the following utilities

string_to_word_list(String, Word_List) :-
	split_string(String," ","",String_Word_List),
	maplist(atom_string, Word_List, String_Word_List).

test_leaves :-
   string_to_word_list("the monkey eats a banana", Word_List),
   sentence(Tree),
   leaves(Tree, Word_List),
   writeln(Tree).

test_number_t :-
   string_to_word_list("the monkey is a banana", Word_List),
   sentence(Tree),
   leaves(Tree, Word_List),
   writeln(Tree).

test_number_f :-
   string_to_word_list("monkeys eats a bananas", Word_List),
   sentence(Tree),
   leaves(Tree, Word_List),
   writeln(Tree).

test_question :-
   string_to_word_list("does the monkey eat", Word_List),
   sentence(Tree),
   leaves(Tree, Word_List),
   writeln(Tree).

test_squestion :-
   string_to_word_list("what does the monkey eat", Word_List),
   sentence(Tree),
   leaves(Tree, Word_List),
   writeln(Tree).

test_linear_leaves :-
   string_to_word_list("banana is a fruit", Word_List),
   sentence(Tree),
   linear_leaves(Tree, Word_List),
   writeln(Tree).

test_freeze_leaves :-
   string_to_word_list("my father eats a fruit", Word_List),
   leaves_freeze(Tree, Word_List),
   sentence(Tree),
   writeln(Tree).

%%%%%%%%%% Part to complete %%%%%%%%%%

%! leaves(Term, Leaves)
%
% this relation is true iff Leaves if the list of words at the leaves of the tree Term
%
% Ex: leaves(f(a,g(b)), [a,b]).

leaves(T, L) :-
   atom(T),
   L = [T].

leaves(T, L) :-
   compound(T),
   T =.. [F | LT],
   maplist(leaves, LT, L2),
   append(L2, L).

%! linear_leave(Term, Leaves)
%
% Use an accumulator to improve the performance of leaves
% and achieve linear time complexity in the number of nodes in the tree

linear_leaves(T, L) :- linear_leaves(T, [], L).
linear_leaves(T, LP, L) :-
   atom(T),
   L = [T | LP].
linear_leaves(T, LP, L) :-
   compound(T),
   T =.. [F | LT],
   reverse(LT, LT1),
   foldl(linear_leaves, LT1, LP, L).


%! leaves_freeze(Term, Leaves)
%
% computes the list of leaves that are not variables
% and freezes the computation on the variables of the term until they get instanciated

leaves_freeze(T, L) :- leaves_freeze(T, [], L).
leaves_freeze(T, LP, L) :-
   var(T),
   freeze(T, leaves_freeze(T, LP, L)).
leaves_freeze(T, LP, L) :-
   atom(T),
   L = [T | LP].
leaves_freeze(T, LP, L) :-
   compound(T),
   T =.. [F | LT],
   reverse(LT, LT1),
   foldl(leaves_freeze, LT1, LP, L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%   More questions    %%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Augment the grammar to include numbers (singular, plural)

nounphrase(np(D, N), Num):-
   determiner(D, Num),
   noun(N, Num).

nounphrase(np(N), Num):-
   noun(N, Num).

verbphrase(vp(V), Num):-
   verb(V, Num).

verbphrase(vp(V, N), Num):-
   verb(V, Num),
   nounphrase(N).

noun(monkey, singular).
noun(monkeys, plural).
noun(father, singular).
noun(banana, singular).
noun(bananas, plural).
noun(fruit, singular).

determiner(my, _).
determiner(the, _).
determiner(a, singular).

verb(eats, singular).
verb(eat, plural).
verb(is, singular).

% Augment the grammar to include questions like "Does the monkey eat a banana", "What does eat the monkey"

sentence(q(D, s(N, V))):-
   question(D),
   sentence(s(N, V), plural).

sentence(qs(D, q(D1, s(N, V)))) :-
   squestion(D),
   sentence(q(D1, s(N, V))).

sentence(s(N, V), plural) :-
   nounphrase(N),
   verbphrase(V, plural).

question(does).
question(do).
squestion(what).
squestion(who).
squestion(why).
squestion(how).