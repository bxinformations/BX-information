:- use_module(library(clpfd)).

%! data(Data)
%
% fact describing the data as a list of jobs
% each job is a list of tasks
% each task is a triple (machine, duration, start_variable)
data([
   [(0, 3, _), (1, 2, _), (2, 2, _)],
   [(0, 2, _), (2, 1, _), (1, 4, _)],
   [(1, 4, _), (2, 3, _)]
]).


%! job_shop(Data, Makespan)
%
% computes the minimal makespan of the job-shop scheduling problem defined by
% Data
job_shop(Data, Makespan) :-
   get_variables(Data, Variables),
   [Makespan | Variables] ins 0..100,
   maplist(precedences(Makespan), Data),
   non_overlap(Data),
   label([Makespan | Variables]).


%! get_variables(Data, Variables)
%
% extract the list of variables from the problem structure Data
get_variables(Data, Variables) :-
   foldl(get_vars_aux, Data, [], Variables).


%! get_vars_aux(Job, Accumulator, Variables)
%
% extract variables from Job, add them to the accumulator and unify Variables
% with the result
get_vars_aux([], Vars, Vars).

get_vars_aux([(_, _, V) | Tasks], Acc, Vars) :-
   get_vars_aux(Tasks, [V | Acc], Vars).


%! precedences(Makespan, Job)
%
% add the constraints than in each job the previous task must be finished
% before the next one starts, and the last one should be finished before the
% Makespan

precedences(Makespan, [(M, D, S)]) :-
   D + S #=< Makespan.
precedences(Makespan, [(M1, D1, S1), (M2, D2, S2) | LS]) :-
   D1 + S1 #=< S2,
   precedences(Makespan, [(M2, D2, S2) | LS]).
   

%! non_overlap(Data)
%
% impose non-overlapping constraints on each pair of tasks sharing the same
% machine
non_overlap([]).

non_overlap([Job | Jobs]) :-
   non_overlap_rec(Job, Jobs),
   non_overlap(Jobs).


%! non_overlap_rec(Job, Jobs)
%
% for each task of Job, impose the constraints that it does not overlap with
% any of the tasks of any job in Jobs if they share the same machine
non_overlap_rec([], _).

non_overlap_rec([Task | Tasks], Jobs) :-
   maplist(maplist(non_overlap(Task)), Jobs),
   non_overlap_rec(Tasks, Jobs).


%! non_overlap((Machine1, Duration1, Start1), (Machine2, Duration2, Start2))
%
% impose the constraint that the two tasks don't overlap (i.e., one is before
% the other, in either direction) if they share the same machine

non_overlap((Machine1, Duration1, Start1), (Machine2, Duration2, Start2)) :-
   Machine1 #= Machine2,
   Duration1 + Start1 #=< Start2 #\/ Duration2 + Start2 #=< Start1.

non_overlap((Machine1, Duration1, Start1), (Machine2, Duration2, Start2)) :-
   Machine1 #\= Machine2.