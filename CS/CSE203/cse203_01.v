(* ====================================================================
 * We start by loading a few libraries and declaring some
 * propositional variables.
 * ==================================================================== *)

Require Import ssreflect.

Parameter A B C D : Prop.

(* ====================================================================
 * Introducing the "move" tactic
 * ==================================================================== *)

(* `move` allows giving a name to the first (top) assumption of
 * the current goal. For example: *)

Lemma move_ex : A -> B -> A.
Proof.
(* Introduce the assumptions `A` & `B` with respective names
 * `hA` and `hB`. *)
move=> hA hB.
Abort.

(* ====================================================================
 * Introducing the "assumption" tactic
 * ==================================================================== *)

(* `assumption` closes a goal when it can be discharged from an
 * assumption. For example: *)

Lemma assumption_ex : A -> B -> A.
Proof.
(* Introduce the assumptions `A` & `B` with respective
 * names `hA` and `hB`. *)
move=> hA hB.
(* The goal can be solved by `hA` *)
assumption.
Qed.

(* It is also possible to close the goal by explicitly giving the name
 * of the assumption, using `apply`: *)

Lemma apply_ex : A -> B -> A.
Proof.
(* Introduce the assumptions `A` & `B` with respective names
 * `hA` and `hB`. *)
move=> hA hB.
(* The goal can be solved by `hA` *)
apply hA.
Qed.

(* ====================================================================
 * Some basic propositional reasonning
 * ==================================================================== *)

Lemma ex0 : A -> A.
Proof.
move=> hA.
apply hA.
Qed.

Lemma ex1 : forall A : Prop, A -> A.
Proof.
intros a H.
apply H.
Qed.
  
Lemma ex2 : (A -> B) -> (B -> C) -> A -> C.
Proof.
move=> ab.
move=> bc.
move=> a.
apply bc.
apply ab.
apply a.
Qed.

Lemma ex3 : (A -> B -> C) -> (B -> A) -> B -> C.
Proof.
move=> abc.
move=> ba.
move=> b.
apply abc.
apply ba.
apply b.
apply b.
Qed.

(* ====================================================================
 * With conjunctions
 * ==================================================================== *)

(* examples *)

Lemma demo_conj1 : (A /\ B) -> A.
Proof.
move=> h. case: h => [a b]. exact a.
Qed.

Lemma demo_conj2 : A -> B -> A /\ B.
Proof.
move=> a b; split.
+ trivial.
+ trivial.
Qed.

(* your turn *)

Lemma conj_ex1: A /\ B <-> B /\ A.
Proof.
split.
+ move=> ab. case ab => [a b]. split. apply b. apply a.
+ move=> ba. case ba => [b a]. split. apply a. apply b.
Qed.

(* ====================================================================
 * With disjunctions
 * ==================================================================== *)

(* examples *)

Lemma demo_disj1 : A -> A \/ B.
Proof.
move=> a. left. trivial.
Qed.

Lemma demo_disj2 : B -> A \/ B.
Proof.
move=> a. right. trivial.
Qed.

Lemma demo_disj3 : A \/ B -> C.
move=> h. case: h => [a | b].    (* gives two subgoals *)
Abort.

(* Your turn *)

Lemma disj_ex1 :  A \/ B <-> B \/ A.
Proof.
split.
+ move=> ab. case: ab => [a | b]. right. trivial. left. trivial.
+ move=> ba. case: ba => [b | a]. right. trivial. left. trivial.
Qed.

Lemma disj_ex2 : A /\ B -> A \/ B.
Proof.
move => ab.
case: ab => [a b].
left. trivial.
Qed.

(* ====================================================================
 * For negations
 * ==================================================================== *)

Print not.  (* not A (or ~A) is a shorthand for (A -> False) *)

(* examples *)

Lemma demo_not1 : False -> A.
Proof.
(* We can prove any goal from False *)
move=> h. case: h.
Qed.

(* Your turn *)

Lemma not_ex1 : A -> ~(~A).
Proof.
move => a.
move => na. 
apply na.
apply a.
Qed.

Lemma not_ex2 :  (A -> B) -> ~B -> ~A.
Proof.
move => ab.
move => nb.
move => h.
apply nb.
apply ab.
exact h.
Qed.

Lemma not_ex3 : ~ ~(A \/ ~A).
Proof.
move => a.
apply a.
right.
move => c.
apply a.
left.
apply c.
Qed.

Lemma not_ex4 :  (A \/ B) /\ C <-> (A /\ C) \/ (B /\ C).
Proof. 
split.
+ 
move => abc.
case: abc => [ab c].
case: ab => [a | b].
- left. split. apply a. apply c.
- right. split. apply b. apply c.
+
move => acbc.
case: acbc => [ac | bc].
- case: ac => [a c]. split. left. apply a. apply c.
- case: bc => [b c]. split. right. apply b. apply c.
Qed.

Lemma not_ex5 : (A /\ B) \/ C <-> (A \/ C) /\ (B \/ C).
Proof. 
split.
+
move => abc.
case: abc => [ab | c].
- case: ab => [a b]. split. left. apply a. left. apply b.
- split. right. apply c. right. apply c. 
+
move => acbc.
case: acbc => [ac bc].
case: ac => [a | c].
case bc => [b | c].
left.
split.
apply a.
apply b.
right.
apply c.
right.
apply c.
Qed.
