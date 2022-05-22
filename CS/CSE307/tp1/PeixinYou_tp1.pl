%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                     %
% CSE 307: Constraint Logic Programming - F. Fages    %
%                                                     %
%                                                     %
% TP1: initiation to SWI-Prolog                       %
%                                                     %
%      Relational Databases in Datalog                %
%                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is a Prolog file, i.e. containing Prolog facts and Prolog rules 

% Comments are lines starting with % or blocks between /*  ...  */

% You can download (and compile) it in the Prolog interpreter at top level (command swipl)

% We ask you to write all answers to the questions in this file (either textual comments or Prolog code)
% and upload your file on the Moodle at the end of the TP

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART I. Relational Database %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog facts                                           %
% used below to represent a family database in extension %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

man(pierre).
man(jean).
man(robert).
man(michel).
man(david).
man(benjamin).
man(joel).

woman(catherine).
woman(paule).
woman(lucie).
woman(magali).
woman(deborah).
woman(claudine).
woman(vanessa).

parent(jean, david).
parent(jean, benjamin).
parent(robert, joel).
parent(robert, deborah).
parent(michel, claudine).
parent(michel, vanessa).
parent(pierre, jean).
parent(pierre, lucie).
parent(pierre, michel).
parent(paule, david).
parent(paule, benjamin).
parent(lucie, joel).
parent(lucie, deborah).
parent(magali, claudine).
parent(magali, vanessa).
parent(catherine, jean).
parent(catherine, lucie).
parent(catherine, michel).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prolog rules                                    %
% used below to define new relations in intension %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% p :- q reads as p is implied by q
% p :- q, r reads as p is implied by q and r


% X is a father of Y if X is a man and a parent of Y

father(X, Y):- parent(X, Y), man(X).

% the disequality predicate dif(X,Y) constrains X and Y to be different
% it is used below to define the relation X is the brother of Y

brother(X, Y) :- parent(Z, Y), dif(X, Y), parent(Z, X), man(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% QUESTIONS on Relational Databases in Datalog %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
  Let us first play with the interpreter.

  Write Prolog queries for answering the following questions
  and copy them with all Prolog answers below

  Be careful that in Prolog an indentifier starting with a upper case letter denotes a variable ! 
  Constant names must start with a lower case letter (not a good convention for our database)
  
1.	Are David and Benjamin brothers ?

?-brother(benjamin, david).
true
  
2.	Who are the brothers of Lucie ?

?-brother(X, lucie).
X=jean ;
X= michel.

So jean and michel are brothers of lucie
  
3.	Who are the two parents of David ?

?-parent(X, david), dif(X, Y), parent(Y, david).
X = jean
Y = paule.

So the two parents of David are Jean and Paule
  
4.	Are there somebody with two fathers in this family ?

?-father(Y, X), dif(Y, Z), father(Z, X).
false.

No

*/

%  Let us now program in Prolog by defining new predicates in this file

%  You will try your definitions by reloading this file in the Prolog interpreter with the query [tp1].

%  Write Prolog rules for defining the following relations


% 5.    mother/2

mother(X, Y):- parent(X, Y), woman(X).

% 6.    sister/2

sister(X, Y) :- parent(Z, Y), dif(X, Y), parent(Z, X), woman(X).

% 7.	grandparent/2

grandparent(X, Y) :- parent(Z, Y), parent(X, Z).
  
% 8.	grandparent2/2 (alternative definition)

grandparent2(X, Y) :- mother(Z, Y), parent(X, Z); father(Z, Y), parent(X, Z).

  
% 9.	grandparent3/2 (yet another equivalent definition)

grandparent3(X, Y) :- parent(Z, Y), mother(X, Z); parent(Z, Y), father(X, Z).




/*
  10.   Are the answers given in the same order ? explain why.


No. (Actually I think this depends on the oder we list the parent relationship. Since in this
tp, we list all the father of some one, and then mother. So if we define grandparent2
as grandparent2(X, Y) :- father(Z, Y), parent(X, Z); mother(Z, Y), parent(X, Z). I believe 
the order of the output should be the same)

For the grandparent, follow by the rewriting rules of Prolog, it will first
try to find a parent of Y, let's denote by Z, then find the parent of Z.

But for the grandparent2, we will first find a mother of Y, and this will have a
different order with grandparent. Since for example, when we try
to find out the grandparent of david, we will first pick Z = jean when we use grandparent
since parent(jean, david) is the first one. But since jean is not the mother of david,
so for parent2 we will not pick jean as the first one, but paule.

The idea of grandparent3 is the samilar. Just in this case we first find a parent of X, let's denote 
this by Z, in this step, grandparent3 rewrite as same as grandparent and different with grandparent2.
After that, we try to fid out a parent of Z in grandparent, but in granparent3 we are trying to 
find out a mother of Z first and then find a father of Z which will return in a different order.


*/
  
% 11.	uncle/2

uncle(X, Y) :- parent(Z, Y), brother(Z, X).

  
% 12.   aunt/2

aunt(X, Y) :- parent(Z, Y), sister(Z, X).

% 13.	ancestor/2

ancestor(X, Y) :- parent(X, Y);
                  parent(Z, Y), ancestor(X, Z).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART II. Graphs in Datalog %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% 14.	Represent the graph of TP1.pdf in Prolog with a predicate arc/2.




arc(amsterdam, paris).
arc(paris, lyon).
arc(lausanne, verone).
arc(lyon, verone).
arc(lyon, rome).
arc(verone, rome).



% 15.	Define the transitive closure relation path/2 that checks the existence of a path from one vertex to another one in an acyclic graph.


path(X, Y) :- arc(X, Y); 
              arc(Z, Y), path(X, Z).





/*
  16.	Add a cycle in the graph and give one example of a query where the predicate path does loop


arc(paris, amsterdam).

?-path(paris, amsterdam).



  
  17.	Use the trace/0 directive to trace the resolution of the query and explain what happens 


So first the program will check if there is a arc(paris, amsterdam), yes it exists.
And then it will check if there is a arc from a arbitary vertex Z to amsterdam
and a path from paris to Z.

Well yes, there exists a arc from paris to amsterdam. So the program will ask
if there is a path(paris, paris)

In this trun, the program will check if there is a arc from arbitary vertex Z to paris,
and a path from paris to Z.

And yes, there is a arc from amsterdam to paris. So now we want to check if path(pais, amsterdam)
which back to the initial state.

So this will loop forever.

  18.	Prove that your program path/2 cannot loop on an acyclic graph.

Assuming this is an acyclic graph, when we ask path(X, Y) each time we will first
ask about if there exists a arc from some vertex Z to Y and then check if there
is a path from X to this vertex Z.

and in order to get path(X, Z), we will also first check if there exists a vertex Z1
such that arc(Z1, Z) and path(X, Z1).

Let's prove this will terminate by contradiction.
If this process will not terminate, which means there exits an infinite sequnce of vertexes
(Z0, Z1, Z2, ..., Zn, ...) such that for all i we have arc(Zi, Zi-1).

And this kind of sequnce has two cases

1. This sequence only has finite different vertex, and since there is only finite many different arc
so there has to be some vertex Z exists more then one times in this sequence.but in this case, 
the graph has a loop. But we have the graph is acyclic so this is a contradiction.

2. this sequnce has infinite many different vertex. Well we asuume this is a finite graph so this case
is impossible.
  
For not looping on a cyclic graph, some data structure is needed to memorize the vertexes that have been visited.
  
This is not possible in Datalog but possible in Prolog using function symbols for representing lists.

The topic of the next Course and TP ...

*/
