# SAT solver

Lazar Milikic and Peixin You

# Running of the SAT solver

To run the solver one needs cd to the directory where `Project` was unzipped. Then

next one shall enter command

> `ghc -O2 MySat.hs`

which will create an executable called `MySat`, where finally entering command

> `./MySat Tests/<filename>.cnf`

which will produce wanted output.

# Understanding output

Running a problem which is SAT e.g. enter command

> `./MySat Tests/random5.cnf`

will produce the following 3 lines output:

SAT

4 1 -2 3 5

Valide : True

First, line says that given formula is SAT. Second line gives the satisfying solution. Finally, the third line is the output of the function **test** of the module `Tests.Tester` applied at the end of the main procedure of module `MySat` and that confirms validity of the solution (with output of value `True`).

Running a problem which is not SAT e.g. enter command `./MySat Tests/php4.cnf` will produce the following line of the output:

UNSAT

saying that formula cannot be satisfied.

# Structure of the Project

File `MySat.hs` contains the **main** procedure of the project which for solving SAT problem calls function `solution` of the Module

**`Solver.Backtracking`**

and if the given is indeed an SAT then **main** also calls function `test` located in the module

**`Tests.Tester`**

which confirms if provided solution is indeed a correct solution of the given formula. Other modified/updated files are:

- CNF.hs

- Eval.hs

- Problems.hs

## Module Solver.Backtracking

Function **`solution`** represents the main function of the Module. For the given CNF it returns `Nothing` if formula is not SAT or solution which can satisfy the given formula.

The function **`solution`** calls **`solve`** which checks the formula for SAT. As arguments it takes `fc` which is a descending sorted the array of clauses by length, an empty array which is an accumulator for the solution, and a Maybe Lit which forms a single clause (unit clause). We will discuss in more detail why we decide to take descending sorted the array of clauses by length instead of other options in the following section.

It is also important to mention that we if needed we perform a completion for missing variables in function `solution` giving them all true polarization (see **`giveSub`**).

In the function **`solve`** (when the set of clauses is non-empty) we distinguish 2 cases. When we can find `unit clause` and when unit clause is `Nothing`.

- If unit clause is not `Nothing` we just apply `condition` of unit clause as it is described in the **Improvements** section

- Otherwise, we are checking if there exists a `pure literal` (as it is described in the **Improvements**)

- Last, if there are no pure literals nor unit clauses, we try applying condition on the first litteral of the first clause

Function **`condition`** conditions a list of clauses by a literal as suggested in the solution, with a small note that it while doing that it looks for a potential `unit clause`. If it finds a `unit clause` (literal that constructs it) it returns it together with new conditioned array of clauses or if it does not it returns nothing with the described array.

Pure literals are found using **`findPureLit`** function which uses a function `litFrm` we defined in CNF which returns list of sorted by value without duplicates literals in given array of clauses.

## Module Tests.Tester

Function **`test`** of the module takes proposed solution and CNF formula and checks whether the formula is indeed satisfied for the given solution outputting `True` and `False` accordingly.

# Evaluations and approaches tried

We have evaluated our progress using two different approaches. First before every executed command we have typed command `time` which evaluates time for execution. E.g.

> `time ./MySat Tests/random5.cnf`

Gives us additional information regarding the time elapsed for execution.

Other approach included instructions for measuring performance given for GHC. It requires linking with **-rtsopts** e.g.

> `ghc -O2 MySat.hs -rtsopts`

And then adding option `+RTS -sstderr` when running the code. E.g.

> `./MySat Tests/colorK8_7.cnf +RTS -sstderr`

This approach gives us more precision regarding the performance, including the memory and temporal cost.

Also, we have created 16 new examples for measuring performance and correctness. We have used given `php` function in `Problems.hs` to create Propositional pigeon-hole principal examples. However, with not knowing that `kcolor` function was already provided we have typed `kcolorOurV` for creating graph-coloring CNFs derived from graph-coloring problems which one can find in `Problems.hs`. In addition, there are two sudoku example provided.

## Approches tried for optimizations

We have applied two optmiazations regarding `unit propagation`: we followed instructions of the first given bulletpoint, where we did not have to consider negation of the found literal and apply method for quickly identifying unit clauses (where essentially using additional argument we know in O(1) if there is an unit clause available).

Next, we tried couple of approaches with `pure literal elimination`. We did implement **`findPureLit`** to be O(nlogn) thanks to function `litFrm` in `CNF.hs` and it showed some improvements for couple of functions.

Then, we took different approach - computing all `pure literals` at the beginning and passing that array as argument. We decided to do this with having in mind that it is not rare that just one polarization is present in the formula and that performing this calculations just once at the beginning is cheap and it can still bring some benefits. For example for `php_12_87.cnf` we can clearly see benefits of this approach. However, we have had some correctness issues, so we want back to first implementation.

We tried different heuristics for picking a literal to branch on. We have tried picking the most frequent literal in the clauses (so branching on it will eliminate most of them) - functions **`findVariableHeu`** and **`mostCommon`**. However, we have noticed that it is very cotsly to perform this technique for each function call and that it just had worsen the results. So we decided to try with sorting. As clauses are commutative their order doesn't matter. After experimentation, we deduced that descending sorting of clauses with respect to their length will influence a very small increase in precision (around 0.02s). Logic for trying this that as we use heuristic of picking first literal of first clause, then we will eliminate clause with the largest number of elements, and possibly increase speed.

We tried applying subsumption rule (function `cancelImply` in `Backtracking.hs`) however, as that function has complexity of O(n^2) its use showed unbeneficial for bigger and more computationally harder examples, although it showed some improvements on smaller ones.

Finally, we play around with Haskell built in function which were advised to use for increase of optimization and tend to use `(:)` instead of `++` where ever was possible (as we found that operator (:) is much faster).

We provide our first version of executable file **`MySat-old`** for comparison of speed before and after improvements.

E.g.

File: php9.cnf | old time 5.7s | new time 0.5s
