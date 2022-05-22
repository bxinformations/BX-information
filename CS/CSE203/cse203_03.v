(* -------------------------------------------------------------------- *)
From mathcomp Require Import ssreflect ssrbool.

(* Counting points at Belote *)

(* Belote is a 32-card (A, K, Q, J, 10, 9, 8, 7), trick-taking,
 * Ace-Ten game. We won't explain the rules here, but only give the
 * scoring basics.
 *
 * In Belote, each card rank has a specific scoring value; for Jacks and
 * Nines the value depends on whether the suit is trump or not.
 *
 * We give below the scoring for each card:
 *
 *
 *    |  Trump | Normal |  
 * ============|========|
 *  A |     11 |    11  |  
 *  7 |      0 |     0  |  
 *  8 |      0 |     0  |  
 *  9 |     14 |     0  |  
 * 10 |     10 |    10  |  
 *  J |     20 |     2  |  
 *  Q |      3 |     3  |  
 *  K |      4 |     4  |  
 *)

(* We first define an inductive type for representing cards *)

Inductive card := A | K | Q | J | F10 | F9 | F8 | F7.

(* Define a function [score (c : card) (trump : bool) : nat]
 * that computes the score of card [c] and the Trump Suite flag
 * [trump]. *)

Definition score (c : card) (trump : bool) :=
  match c, trump with
  | A  , _     => 11
  | F7 , _     => 0
  | F8 , _     => 0
  | F9 , true  => 14
  | F9 , false => 0
  | F10, _     => 10
  | J  , true  => 20
  | J  , false => 2
  | Q  , _     => 3
  | K  , _     => 4
  end.

(* Prove the following lemmas *)

(* In these proofs, the tactics discriminate and contradiction
 * will allow you to take care of hypotheses of the form
 *  true <> true, resp.  true = false   *)

Lemma L1 : forall (c : card),
  c <> J -> c <> F9 -> score c true = score c false.
Proof. 
  move => c.
  by case: c.
Qed.

Lemma L2 : forall (b : bool), score F9 b <> 0 -> b = true.
Proof.
  move => b.
  by case: b.
Qed.

Lemma L3 : forall (c1 c2 : card) (b : bool),
  score c1 b + score c2 b = 25 -> b = true.
Proof.
  move => c1 c2 b.
  by case: c1; case c2; case b.
Qed.

(* ==================================================================== *)
(* Let's have a look on the definition of addition *)
Print Nat.add.

(* We can use Coq for computing closed formula *)
Eval compute in 2+2.

Parameter x : nat.

(* And let's try to computer with open terms *)
Eval compute in 0+x.
Eval compute in x+0.

(* A proof by reflexivity *)
Lemma calc : 200 + 200 = 400.
Proof.
(* 200 + 200 computes to 400 *)
reflexivity.
Qed.

(* Easy... and should be done by computation *)
Lemma add0n : forall n:nat, 0 + n = n.
Proof. 
  reflexivity.
Qed.

(* This one needs an induction *)
Lemma addn0 : forall n, n + 0 = n.
Proof. 
  move => n.
  elim: n => [|p hp].
  +
  reflexivity.
  +
  simpl.
  rewrite hp.
  reflexivity.
Qed.

(* More lemmas on natural numbers... *)
Lemma addSn : forall n m, S n + m = S(n + m).
Proof. 
  move => n m.
  elim: m => [|m pm].
  +
  reflexivity.
  +
  simpl.
  reflexivity.
Qed.

Lemma addnS : forall n m, n + S m = S (n + m).
Proof. 
  move => n m.
  elim: n => [|n pn].
  reflexivity.
  simpl.
  rewrite pn.
  reflexivity.
Qed.

Lemma addnC : forall n m, n + m = m + n.
Proof. 
  move => n m.
  elim: n => [|n pn].
  rewrite addn0.
  reflexivity.
  simpl.
  rewrite addnS.
  rewrite pn.
  reflexivity.
Qed.

Lemma addnA : forall n m p, n + (m + p) = (n + m) + p.
Proof. 
  move => n m p.
  elim: n => [|n pn].
  reflexivity.
  simpl.
  rewrite pn.
  reflexivity.
Qed.

(* ==================================================================== *)
(* We know want to compare natural numbers with the following predicate *)
Fixpoint le (x y : nat) :=
  match x, y with
  | 0  , _   => true
  | S _, 0   => false
  | S x, S y => le x y
  end.

(* Note that booleans are projected to propositions :
 * if b is a boolean, the proposition b stands for b = false  *)

(* Check this with :  *)

Lemma b1 : true.
Proof.
reflexivity.
Qed.

Lemma b2 : false -> False.
Proof.
move => h.
discriminate.
Qed.


(* Let's first prove that [comp] is defining an proper order *)
Lemma le_refl : forall n, le n n.
Proof. 
  move => n.
  elim: n => [|n pn].
  reflexivity.
  simpl.
  apply pn.
Qed.

Lemma le_trans : forall n m p, le n m -> le m p -> le n p.
Proof. 
  move => n.

  elim: n=> [|n pn].
  reflexivity.
  simpl.
  move => [|m] [|p]//=.
  simpl.
  apply pn.
Qed.

Lemma le_antisym : forall n m, le n m -> le m n -> n = m.
Proof. 
  move => n.
  elim: n=> [|n pn].
  move => [|m]//=.
  simpl.
  move => [|m]//=.
  move => p1 p2.
  rewrite (pn m).
  done.
  assumption.
  assumption.
Qed.

(* 0 is the bottom element of this order *)
Lemma le0n : forall n, le 0 n.
Proof. 
  move => n.
  reflexivity.
Qed.

(* Let's give a specification for le *)
Lemma leP: forall n m, le n m -> exists p, m = n + p.
Proof.
  move => n.
  elim: n => [|n pn].
  move => [| m].
  move => p.
  exists 0.
  reflexivity.
  move => p.
  exists (S m).
  reflexivity.
  move => [|m].
  simpl.
  discriminate.
  simpl.
  move => p.
  destruct (pn m p).
  exists x0.
  rewrite H.
  reflexivity.
Qed.

(* ==================================================================== *)
(* THIS PART IS NOT MANDATORY *)

(* We define the type for list over natural numbers *)

Inductive list : Type :=
| nil  : list
| cons : nat -> list -> list.

(* We define a function for concatenating lists *)
Fixpoint cat (l1 l2 : list) : list :=
  match l1 with
  | nil       => l2
  | cons x tl => cons x (cat tl l2)
  end.

(* Prove the following properties: *)

Lemma cat0s : forall (l : list), cat nil l = l.
Proof. 
  move => l.
  done.
Qed.

Lemma cats0 : forall (l : list), cat l nil = l.
Proof. 
  move => l.
  elim: l => [|l pl].
  done.
  simpl.
  move => p.
  rewrite p.
  done.
Qed.

(* Define a function for computing the length of a list... *)
Fixpoint size (s : list) : nat :=
  match s with
  | nil => 0
  | cons x t1 => S (size t1)
  end.

(* ...and prove the following property *)
Lemma length_cat : forall (s1 s2 : list), size (cat s1 s2) = size s1 + size s2.
Proof. 
  move => s1.
  elim: s1 => [|s1 ps1].
  done.
  move => p.
  move => s2.
  simpl.
  rewrite p.
  reflexivity.
Qed.
