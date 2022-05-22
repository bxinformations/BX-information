(* -------------------------------------------------------------------- *)
Require Import ssreflect ssrbool List.

Set Implicit Arguments.

Axiom todo : forall {A}, A.
Ltac todo := by apply: todo.

(* ==================================================================== *)
(* This template contains incomplete definitions that you have to       *)
(* fill. We always used the keyword `Definition` for all of them but    *)
(* you are free to change for a `Fixpoint` or an `Inductive`.           *)
(*                                                                      *)
(* If needed, it is perfectly fine to add intermediate definitions and  *)
(* local lemmas.                                                        *)

(* ==================================================================== *)
(* In this project, we are going to develop and prove correct an        *)
(* algorithm for deciding the membership of a word w.r.t. a given       *)
(* regular language - all these terms are going to be defined below     *)

(* This project lies in the domain of *formal languages*. The study     *)
(* of formal languages is a branch of theoretical computer science and  *)
(* is about that is interested in the purely syntactical aspects of     *)
(* of languages and as applications in different domains, ranging from  *)
(* the definition of  the grammar of programming languages to the field *)
(* of automated translation.                                            *)

(* As with natural languages, we first need to fix an alphabet. In our  *)
(* case, we are simply going to declare a type `A : Type` - i.e. we     *)
(* will use the same alphabet for all the formal languages we are going *)
(* to study. Inhabitants of `A` are called `letters`.                   *)

Parameter (A : Type).

(* -------------------------------------------------------------------- *)
(* A `word` is then simply a finite sequence of letters of `A`. We      *)
(* denote by A* the set of words over `A`. In Coq, we are going to      *)
(* represent words as lists whose elements are inhabitants of `A`. This *)
(* naturally leads to the following definition:                         *)

Notation word := (list A).

(* -------------------------------------------------------------------- *)
(* In this setting, a `language` is simply a subset of A*. Assuming     *)
(* that `x` & `y` are letters of A, we can define the following         *)
(* languages:                                                           *)
(*                                                                      *)
(*  - the empty language: `L = ∅`;                                      *)
(*                                                                      *)
(*  - the language that contains only the empty word ε (i.e. the only   *)
(*    (word of length 0): L = {ε}`;                                     *)
(*                                                                      *)
(*  - the language that contains all the words solely composed of the   *)
(*    letter `x`: L = { ε, x, xx, xxx, ... } = { xⁿ | n ∈ ℕ } (here,    *)
(*    xⁿ stands for the word x…x, where x is repeated n times);         *)
(*                                                                      *)
(*  - the language that contains all the words of the form xⁿyⁿ:        *)
(*    L = { xⁿyⁿ | n ∈ ℕ };                                             *)
(*                                                                      *)
(*  - if we assume that A contains the letter '[' & ']', we can         *)
(*    define the language of well-balanced words for '[' & ']':         *)
(*    L = { w ∈ { [, ] }* | s.t. P(w) && Q(w) }, where                  *)
(*      - P(w) = any prefix of w contain no more ]'s then ['s           *)
(*      - Q(w) = the number of ['s and ]'s of w are equal.              *)

(* -------------------------------------------------------------------- *)
(* We can also combine languages to form other languages. For example,  *)
(* if L and G are languages, we can define:                             *)
(*                                                                      *)
(*  - the union of L & G            L ∪ G                               *)
(*  - the concatenation of L & G    { w1 · w2 | w1 ∈ L, w2 ∈ G }        *)
(*  - the intersection of L & G     L ∩ G                               *)
(*  - the complement of L           A* \ L                              *)
(*  - the Kleene closure of L       L* = { wⁿ | w ∈ L, n ∈ ℕ }          *)
(*  - the mirror of L               rev(L) = { rev(w) | w ∈ L }         *)

(* -------------------------------------------------------------------- *)
(* To define languages in Coq, we need a way to represent subsets       *)
(* of A*, i.e. subsets of the set of `word`'s. To that end, we are      *)
(* going to represent a set using its indicator function. (We remind    *)
(* that, given a subset F of an ambient set E, the indicator function   *)
(* of F is the function f_E from E to { ⊤, ⊥ } s.t.                     *)
(*                                                                      *)
(*                     f_E(x) = ⊤ iff x ∈ E                             *)

(* In Coq, the codomain of its indicator function is going to be a      *)
(* proposition: given a function `F : E -> Prop`, we say that x belongs *)
(* to `x` iff `f x` holds.                                              *)

Notation language := (word -> Prop).

(* -------------------------------------------------------------------- *)
(* From now use, we assume that L, G, H denote languages, x, y denote   *)
(* letters and that and w denotes a word.                               *)

Implicit Types (L G H : language) (x y : A) (w : word).

(* -------------------------------------------------------------------- *)
(* From there, we can define the following languages                    *)

(* The empty language: no words belong to it.                           *)
(* (its indicator function always return `False`)                       *)
Definition lang0 : language :=
  fun w => False.

(* The language that only contains the empty word.                      *)
Definition lang1 : language :=
  fun w => w = nil.

(* Q1. We now ask you to define the following languages                 *)

(*  Given a word `w0`, the language that only contains the word `w0`.   *)
Definition langW w0 : language := 
  fun w => w = w0.

(* Given a sequence `ws` of words, the language that contains all the   *)
(* the words `ws` and only these words.                                 *)
Definition langF (ws : list word) : language := 
  fun w => (In w ws).

(* Given a letter `x`, the language that only contains the letter `x`   *)
(* seen as a word of length 1.                                          *)
Definition langA x : language := 
  fun w => w = (cons x nil).

(* The union of the two languages `L` and `G`.                          *)
Definition langU L G : language := 
  fun w => (L w) \/ (G w).

(* The intersection of the two languages `L` and `G`.                   *)
Definition langI L G : language := 
  fun w => (L w) /\ (G w).

(* The concatenation of the two languages `L` and `G`.                  *)
Definition langS L G : language :=
  fun w => (exists s c : word, (s ++ c = w) /\ (L s) /\ (G c)).

(* The Kleene closure of the language `L`                               *)
Inductive langK L : language := 
  | kleene0 : (langK L) nil
  | kleeneA : forall w, L w -> langK L w
  | kleeneS : forall s c : word, (langK L) s -> ((langK L) c) -> ((langK L) (s ++ c)).

(* The mirror of the language `L` (You can use the `rev`, that reversed *)
(* a list, from the standard library. *)
Definition langM L : language := 
  fun w => (L (rev w)).

(* -------------------------------------------------------------------- *)
(* Given two languages, we will consider `L` & `G` equal iff they       *)
(* contain the same words:                                              *)

Definition eqL L G := forall w, L w <-> G w.

Infix "=L" := eqL (at level 90).

(* Q2. Prove the following equivalances:                                *)

Lemma concat0L L : langS lang0 L =L lang0.
Proof. 
  move => w.
  split.
  move => h.
  case h.
  move => x h1.
  inversion h1.
  move: H => [h2 h3].
  move: h3 => [h4 h5].
  case h4.
  move => h.
  case h.
Qed.

Lemma concatL0 L : langS L lang0 =L lang0.
Proof. 
  move => w.
  split.
  move => h.
  case h.
  move => x h1.
  inversion h1.
  move: H => [h2 h3].
  move: h3 => [h4 h5].
  case h5.
  move => h.
  case h.
Qed.

Lemma concat1L L : langS lang1 L =L L.
Proof.
  move => w; split.
  move => h.
  case h.
  move => x h1.
  inversion h1.
  move: H => [h2 h3].
  move: h3 => [h4 h5].
  inversion h4.
  inversion h2.
  rewrite H.
  simpl.
  apply h5.
  move => h.
  exists nil.
  exists w.
  split.
  simpl; reflexivity.
  split.
  reflexivity.
  apply h.
Qed.

Lemma concatL1 L : langS L lang1 =L L.
Proof. 
  move => w; split.
  move => h.
  case h.
  move => x h1.
  inversion h1.
  move: H => [h2 h3].
  move: h3 => [h4 h5].
  inversion h5.
  inversion h2.
  rewrite H.
  rewrite app_nil_r.
  apply h4.
  move => H.
  exists w.
  exists nil.
  split.
  apply app_nil_r.
  split.
  apply H.
  reflexivity.
Qed.

Lemma concatA L G H : langS (langS L G) H =L langS L (langS G H).
Proof. 
  move => w.
  split.
  move => h.
  inversion h.
  inversion H0.
  move: H1 => [h2 h3].
  move: h3 => [h4 h5].
  inversion h4.
  inversion H1.
  move: H2 => [h6 h7].
  move: h7 => [h8 h9].
  exists x1.
  exists (x2 ++ x0).
  split.
  rewrite app_assoc.
  rewrite h6.
  by rewrite h2.
  split.
  apply h8.
  exists x2.
  exists x0.
  split.
  reflexivity.
  split.
  apply h9.
  apply h5.
  move => h.
  inversion h.
  inversion H0.
  move: H1 => [h2 h3].
  move: h3 => [h4 h5].
  inversion h5.
  inversion H1.
  move: H2 => [h6 h7].
  move: h7 => [h8 h9].
  exists (x ++ x1).
  exists x2.
  split.
  rewrite app_assoc_reverse.
  rewrite h6.
  by rewrite h2.
  split.
  exists x.
  exists x1.
  split.
  trivial.
  split; auto.
  auto.
Qed.

Lemma unionC L G : langU L G =L langU G L.
Proof. 
  move => w.
  split.
  move => H.
  inversion H.
  right.
  apply H0.
  left.
  apply H0.
  move => H.
  inversion H.
  right; done.
  left; done.
Qed.



Lemma interC L G : langI L G =L langI G L.
Proof. 
  move => w.
  split.
  move => [h1 h2].
  split.
  apply h2.
  apply h1.
  move => [h1 h2].
  split.
  apply h2.
  apply h1.
Qed.

Lemma langKK L : langK (langK L) =L langK L.
Proof. 
  move => w.
  split.
  elim.
  apply kleene0.
  move => w0 h.
  apply h.
  move => s c h1 h2 h3 h4.
  apply kleeneS.
  apply h2.
  apply h4.
  move => h.
  apply kleeneA.
  apply h.
Qed.

(* Note that, since languages are represented as indicator functions    *)
(* over `Prop`, we cannot assess that `L =L G` implies `L = G`.         *)

(* ==================================================================== *)
(*                          REGULAR LANGUAGES                           *)

(* We are now interested in a subclass of languages called "regular     *)
(* languages": a language `L` is said to be regular iff one of the      *)
(* following holds:                                                     *)
(*                                                                      *)
(*  - L = ∅ or L = {ε} or L = {x} for some x ∈ A ;                      *)
(*  - L = L1 ∪ L2 for L1, L2 regular languages ;                        *)
(*  - L = L1 · L2 for L1, L2 regular languages ;                        *)
(*  - L = G* for G a regular language.                                  *)

(* This kind of inductive definitions can be encoded in Coq using       *)
(* an inductive predicate `regular : language -> Prop` s.t.             *)
(*                                                                      *)
(*             L is regular iff `regular L` holds                       *)

(* Q3. complete the following definition of the predicate `regular`:    *)

Inductive regular : language -> Prop :=
  (* Any language equivalent to a regular language is regular *)
| REq L G of regular L & G =L L : regular G

  (* The empty language is regular *)
| REmpty : regular lang0
| R1 : regular lang1
| RA : forall x, regular (langA x)
| RUnion : forall L1 L2 : language, regular L1 -> regular L2 -> regular (langU L1 L2)
| RS : forall L1 L2 : language, regular L1 -> regular L2 -> regular (langS L1 L2)
| RKleene : forall L, regular L -> regular (langK L)
.

(* -------------------------------------------------------------------- *)
(* Q4. prove that `langW w` is regular.                                 *)

Lemma equalSaw a w : langW ((cons a nil) ++ w) =L langS (langA a) (langW w).
Proof.
  move => c.
  split.
  move => h.
  exists (a::nil).
  exists w.
  split.
  inversion h.
  reflexivity.
  split.
  reflexivity.
  reflexivity.
  move => h.
  inversion h.
  inversion H.
  move: H0 => [h1 h2].
  move: h2 => [h3 h4].
  inversion h3.
  inversion h4.
  symmetry in h1.
  rewrite h1.
  rewrite H0.
  rewrite H1.
  reflexivity.
Qed.

Lemma regularW w : regular (langW w).
Proof. 
  induction w.
  apply R1.
  apply REq with (langS (langA a) (langW w)).
  apply RS.
  apply RA.
  apply IHw.
  apply (equalSaw a w).
Qed.

(* -------------------------------------------------------------------- *)
(* Q5. prove that `langM L` is regular, given that L is regular.        *)

Lemma EqlangM L G: L =L G -> langM L =L langM G.
Proof.
  move => h w.
  split.
  move => h1.
  apply h.
  apply h1.
  move => h1.
  apply h.
  apply h1.
Qed.


Lemma EqlangM0 : langM lang0 =L lang0.
Proof.
  move => w.
  split.
  move => h.
  case h.
  move => h.
  case h.
Qed.

Lemma EqlangM1 : langM lang1 =L lang1.
Proof.
  move => w.
  split.
  move => h.
  inversion h.
  move: (rev_eq_app w nil nil H0) => h1.
  simpl in h1.
  rewrite h1.
  reflexivity.
  move => h.
  inversion h.
  reflexivity.
Qed.

Lemma EqlangMA x: langM (langA x) =L langA x.
Proof.
  move => w.
  split.
  move => h.
  inversion h.
  move: (rev_eq_app w (x :: nil) nil H0) => h1.
  simpl in h1.
  rewrite h1.
  reflexivity.
  move => h.
  inversion h.
  reflexivity.
Qed.

Lemma EqlangMS L1 L2 : langM (langS L1 L2) =L langS (langM L2) (langM L1).
Proof.
  move => w.
  split.
  move => h.
  inversion h.
  inversion H.
  move: H0 => [h1 h2].
  move: h2 => [h3 h4].
  symmetry in h1.
  move: (rev_eq_app (w) x x0 h1) => h5.
  rewrite h5.
  exists (rev x0).
  exists (rev x).
  split.
  reflexivity.
  split.
  rewrite /langM rev_involutive.
  apply h4.
  rewrite /langM rev_involutive.
  apply h3.
  move => h.
  inversion h.
  inversion H.
  move: H0 => [h1 h2].
  move: h2 => [h3 h4].
  symmetry in h1.
  rewrite h1.
  unfold langM.
  exists (rev x0).
  exists (rev x).
  split.
  move: (rev_app_distr x x0) => h5.
  symmetry in h5.
  apply h5.
  split.  
  apply h4.
  apply h3.
Qed.

Lemma langKTolangKM L w: langK L w -> langK (langM L) (rev w).
Proof.
  move => h.
  induction h.
  apply kleene0.
  apply kleeneA.
  rewrite /langM rev_involutive.
  apply H.
  rewrite (rev_app_distr s c).
  apply kleeneS.
  apply IHh2.
  apply IHh1.
Qed.


Lemma EqlangKM L : langM (langK L) =L langK (langM L).
Proof.
  move => w.
  split.
  move => h.
  move: (rev_involutive w) => h1.
  symmetry in h1.
  rewrite h1.
  apply langKTolangKM.
  apply h.
  move => h.
  induction h.
  apply kleene0.
  apply kleeneA.
  apply H.
  unfold langM.
  rewrite (rev_app_distr s c).
  apply kleeneS.
  apply IHh2.
  apply IHh1.
Qed.

Lemma regularM L : regular L -> regular (langM L).
Proof. 
  move => h.
  induction h.
  apply REq with (langM L).
  apply IHh.
  move: (EqlangM H)=> h1.
  apply h1.
  apply REq with (lang0).
  apply REmpty.
  apply EqlangM0.
  apply REq with (lang1).
  apply R1.
  apply EqlangM1.
  apply REq with (langA x).
  apply RA.
  apply EqlangMA.
  apply RUnion.
  apply IHh1.
  apply IHh2.
  apply REq with (langS (langM L2) (langM L1)).
  apply RS.
  apply IHh2.
  apply IHh1.
  apply EqlangMS.
  apply REq with (langK (langM L)).
  apply RKleene.
  apply IHh.
  apply (EqlangKM L).
Qed.

(* ==================================================================== *)
(*                        REGULAR EXPRESSIONS                           *)

(* Related to regular languages is the notion of regular expressions.   *)
(* A regular expression is a formal, syntactic expression that can      *)
(* latter be interpreted as a regular language. Regular expressions are *)
(* pervasive in computer science, e.g. for searching for some text in   *)
(* a file, as it is possible with the `grep` command line tool.         *)
(*                                                                      *)
(* For instance, the command:                                           *)
(*                                                                      *)
(*    grep -E 'ab*a' foo.txt                                            *)
(*                                                                      *)
(* is going to print all the lines of `foo.txt` that contains a word    *)
(* of the form ab⋯ba (where the letter b can be repeated 0, 1 or more   *)
(* time. I.e., grep is going to find all the lines of `foo.txt` that    *)
(* contains a word that belongs in the formal language:                 *)
(*                                                                      *)
(*    L = { abⁿa | n ∈ ℕ }                                              *)
(*                                                                      *)
(* If you need to convince yourself that L is regular, note that:       *)
(*                                                                      *)
(*    L = { a } ∪ { b }* ∪ { a }                                        *)
(*                                                                      *)
(* In some sense, a regular expression is just a compact way to         *)
(* represent a regular language, and its definition is going to be      *)
(* close to the one of regular languages.                               *)
(*                                                                      *)
(* A regular expression is either:                                      *)
(*                                                                      *)
(*  - the constant ∅ or the constant ε or one letter from A             *)
(*  - the disjunction r1 | r2 of two regular expressions                *)
(*  - the concatenation r1 · r2 of two regular expressions              *)
(*  - the Kleene r* of some regular expression                          *)

(* We can represent regular expressions as a inductive type in Coq.     *)

(* Q6. complete the following definition:                               *)

Inductive regexp : Type :=
| RE_Empty : regexp
| RE_Void  : regexp
| RE_Atom  : A -> regexp
| RE_Cat : regexp -> regexp -> regexp
| RE_Or : regexp -> regexp -> regexp
| RE_Kleene : regexp -> regexp
.

Implicit Types (r : regexp).

(* We now have to formally related regular expressions to regular       *)
(* languages. For that purpose, we are going to interpret a regular     *)
(* expression as a languages. If r is a regular expression, then we     *)
(* denote by language [r] as follows:                                   *)
(*                                                                      *)
(*   - [∅]       = ∅                                                    *)
(*   - [ε]       = ε                                                    *)
(*   - [a]       = { a } for a ∈ A                                      *)
(*   - [r₁ ∪ r₂] = [r₁] ∪ [r₂]                                          *)
(*   - [r₁ · r₂] = [r₁] · [r₂]                                          *)
(*   - [r*]      = [r]*                                                 *)

(* Q7. implement the Coq counterpart of the above definition:           *)

Fixpoint interp (r : regexp) {struct r} : language :=
  match r with
  | RE_Empty => lang0
  | RE_Void => lang1
  | RE_Atom a => langA a
  | RE_Cat r1 r2 => langS (interp r1) (interp r2)
  | RE_Or r1 r2 => langU (interp r1) (interp r2)
  | RE_Kleene r1 => langK (interp r1)
  end.

(* Q8. show that the interpretation of a regular expression is a        *)
(*     regular language:                                                *)

Lemma regular_regexp r : regular (interp r).
Proof. 
  induction r.
  apply REmpty.
  apply R1.
  apply RA.
  apply (RS IHr1 IHr2).
  apply (RUnion IHr1 IHr2).
  apply RKleene.
  apply IHr.
Qed.

(* Q9. show that any regular language can be interpreted as a           *)
(*     regular expression:                                              *)

Lemma EqlangU L1 L2 G1 G2 : L1 =L G1 -> L2 =L G2 -> langU L1 L2 =L langU G1 G2.
Proof.
  move => h1 h2.
  move => w.
  split.
  move => [H1 | H2].
  left.
  apply h1.
  apply H1.
  right.
  apply h2.
  apply H2.
  move => [H1 | H2].
  left.
  apply h1.
  apply H1.
  right.
  apply h2.
  apply H2.
Qed.

Lemma EqlangS L1 L2 G1 G2 : L1 =L G1 -> L2 =L G2 -> langS L1 L2 =L langS G1 G2.
Proof.
  move => h1 h2.
  move => w.
  unfold langS.
  split.
  move => h.
  move: h => [s [c [H1 [H2 H3]]]].
  exists s.
  exists c.
  split.
  auto.
  split.
  apply h1.
  apply H2.
  apply h2.
  apply H3.
  move => h.
  move: h => [s [c [H1 [H2 H3]]]].
  exists s.
  exists c.
  split.
  auto.
  split.
  apply h1.
  apply H2.
  apply h2.
  apply H3.
Qed.

Lemma EqlangK L G : L =L G -> langK L =L langK G.
Proof.
  move => H.
  move => w.
  split.
  move => h.
  induction h.
  apply kleene0.
  apply kleeneA.
  apply H.
  apply H0.
  apply kleeneS.
  apply IHh1.
  apply IHh2.
  move => h.
  induction h.
  apply kleene0.
  apply kleeneA.
  apply H.
  apply H0.
  apply kleeneS.
  apply IHh1.
  apply IHh2.
Qed.

Lemma regexp_regular L : regular L -> exists r, L =L interp r.
Proof.
  move => h.
  induction h.
  inversion IHh.
  exists x.
  move => w.
  split.
  move => h1.
  apply H0.
  apply H.
  apply h1.
  move => h1.
  apply H.
  apply H0.
  apply h1.
  exists RE_Empty.
  move => w.
  split.
  move => x.
  case x.
  move => x.
  case x.
  exists RE_Void.
  move => w.
  split.
  move => h.
  apply h.
  move => h.
  apply h.
  exists (RE_Atom x).
  move => w.
  split.
  move => h.
  apply h.
  move => h.
  apply h.
  move: IHh1 => [r1 H1].
  move: IHh2 => [r2 H2].
  exists (RE_Or r1 r2).
  simpl.
  apply (EqlangU H1 H2).
  move: IHh1 => [r1 H1].
  move: IHh2 => [r2 H2].
  exists (RE_Cat r1 r2).
  simpl.
  apply (EqlangS H1 H2).
  move: IHh => [r H].
  exists (RE_Kleene r).
  simpl.
  apply (EqlangK H).
Qed.

(* Of course, it may happen that two regular expressions represent      *)
(* the same language: r1 ~ r2 iff [r1] = [r2].                          *)

(* Q10. write a binary predicate eqR : regexp -> regexp -> Prop s.t.    *)
(*      eqR r1 r2 iff r1 and r2 are equivalent regexp.                  *)

Definition eqR (r1 r2 : regexp) : Prop := interp r1 =L interp r2.

Infix "~" := eqR (at level 90).

(* Q11. state and prove the following regexp equivalence:               *)
(*           (a|b)* ~ ( a*b* )*                                         *)

Lemma KleeneC L G : (forall w, (L w -> G w)) -> (forall w, langK L w -> langK G w).
Proof.
  move => H.
  move => w.
  move => h.
  induction h.
  apply kleene0.
  apply kleeneA.
  apply H.
  apply H0.
  apply kleeneS.
  apply IHh1.
  apply IHh2.
Qed.

Lemma eqExample a b : RE_Kleene (RE_Or (RE_Atom a) (RE_Atom b)) ~ RE_Kleene (RE_Cat (RE_Kleene (RE_Atom a)) (RE_Kleene (RE_Atom b))).
Proof.
  split.
  move => h.
  induction h.
  apply kleene0.
  case : H.
  move => h.
  apply kleeneA.
  exists w.
  exists nil.
  split.
  rewrite app_nil_r.
  reflexivity.
  split.  
  apply kleeneA.
  apply h.
  apply kleene0.
  move => h.
  apply kleeneA.
  exists nil.
  exists w.
  split.
  simpl.
  reflexivity.
  split.
  apply kleene0.
  apply kleeneA.
  apply h.
  apply kleeneS.
  apply IHh1.
  apply IHh2.
  move => h.
  induction h.
  apply kleene0.
  case : H.
  move => x.
  move => [c [h1 [h2 h3]]].
  symmetry in h1.
  rewrite h1.
  apply kleeneS.
  case h2.  
  apply kleene0.
  move => w0 h.
  apply kleeneA.
  left.
  apply h.
  move => s c0.
  move => H1 H2.
  apply kleeneS.
  apply (@KleeneC (langA a) (langU (langA a) (langA b))).
  move => w0.
  move => H.
  left.
  apply H.
  apply H1.
  apply (@KleeneC (langA a) (langU (langA a) (langA b))).
  move => w0.
  move => H.
  left.
  apply H.
  apply H2.
  apply (@KleeneC (langA b) (langU (langA a) (langA b))).
  move => w0.
  move => H.
  right.
  apply H.
  apply h3.
  apply kleeneS.
  apply IHh1.
  apply IHh2.
Qed.

(* ==================================================================== *)
(*                          REGEXP MATCHING                             *)

(* We now want to write a algorithm for deciding if a given word `w`    *)
(* matches a regular expression `r`, i.e. for deciding wether `w ∈ [r]` *)
(*                                                                      *)
(* For that purpose, we are going to use Brzozowski's derivatives.      *)
(*                                                                      *)
(* The idea of the algorithm is the following:                          *)
(*                                                                      *)
(* Given a letter `x` and an regular expression `r`, the Brzozowski's   *)
(* derivatives of `x` w.r.t. `r` is a regular expression x⁻¹·r s.t.     *)
(*                                                                      *)
(*    x · w ∈ [r] ⇔ w ∈ [x⁻¹·r]                                       *)
(*                                                                      *)
(* Assuming that we can compute a Brzozowski's derivative for any       *)
(* letter `x` and regular expression `r`, one can check that a word `w` *)
(* matches a regexp `r` as follows:                                     *)
(*                                                                      *)
(*   - if w = x · w' for some letter x and word w', we recursively      *)
(*     check that `w` matches `x⁻¹·r`; otherwise                        *)
(*   - if w = ε, then we directly check that [r] contains the empty     *)
(*     word - a property that is deciable.                              *)

(* Q12. write a nullity test `contains0` : regexp -> bool s.t.          *)
(*                                                                      *)
(*      ∀ r, contains0 r ⇔ ε ∈ [e]                                    *)

Fixpoint contains0 (r : regexp) : bool := 
  match r with
  | RE_Empty => false
  | RE_Void => true
  | RE_Atom c => false
  | RE_Cat r1 r2 => (contains0 r1 && contains0 r2)%bool
  | RE_Or r1 r2 => (contains0 r1 || contains0 r2)%bool
  | RE_Kleene r => true
  end.

(* Q13. prove that your definition of `contains0` is correct:           *)

Lemma contains0_ok r : contains0 r <-> interp r nil.
Proof. 
  split.
  move => h.
  induction r.
  simpl in h.
  discriminate h.
  simpl.
  reflexivity.
  simpl in h.
  discriminate h.
  simpl in h.
  simpl.
  exists nil.
  exists nil.
  split.
  done.
  move/andP : h => [h1 h2].
  split.
  apply IHr1.
  apply h1.
  apply IHr2.
  apply h2.
  simpl in h.
  move/orP : h => [h1 | h2].
  simpl.
  left.
  apply IHr1.
  apply h1.
  simpl.
  right.
  apply IHr2.
  apply h2.
  apply kleene0.
  move => h.
  induction r.
  case : h.
  simpl.
  trivial.
  simpl in h.
  unfold langA in h.
  discriminate h.
  simpl.
  apply /andP.
  simpl in h.
  move : h => [s [c [h1 [h2 h3]]]].
  move: (app_eq_nil s c h1) => [h4 h5].
  split.
  apply IHr1.
  rewrite h4 in h2.
  apply h2.
  rewrite h5 in h3.
  apply IHr2.
  apply h3.
  simpl.
  apply /orP.
  simpl in h.
  move: h => [h1 | h2].
  left.
  apply IHr1.
  apply h1.
  right.
  apply IHr2.
  apply h2.
  simpl.
  done.
Qed.

(* We give below the definition of the Brzozowski's derivative:         *)
(*                                                                      *)
(*   - x⁻¹ · x         = ε                                              *)
(*   - x⁻¹ · y         = ∅ if x ≠ y                                     *)
(*   - x⁻¹ · ε         = ∅                                                     *)
(*   - x⁻¹ · ∅         = ∅                                                      *)
(*   - x⁻¹ · (r₁ | r₂) = (x⁻¹ · r₁) | (x⁻¹ · r₂)                         *)
(*   - x⁻¹ · (r₁ · r₂) = (x⁻¹ · r₁) · r₂ | E(r₁) · (x⁻¹ · r₂)            *)
(*   - x⁻¹ · r*        = (x⁻¹ · r) · r*                                 *)
(*                                                                      *)
(* where E(r) = ε if ε ∈ [r] & E(r) = ∅ otherwise.                      *)

(* Q14. write a function `Brzozowski` that computes a Brzozowski's      *)
(*      derivative as defined above.                                    *)
(*                                                                      *)
(* For that purpose, you may need to have a decidable equality over     *)
(* `A`. The parameter `Aeq` along with the axioms `Aeq_dec` give        *)
(* you such a decidable equality.                                       *)

Parameter Aeq : A -> A -> bool.

(* Here, `Aeq x y` has to be read as `Aeq x y = true`                   *)
Axiom Aeq_dec : forall (x y : A), Aeq x y <-> x = y.

Notation "a ++ b" := (RE_Cat a b).
Notation "a || b" := (RE_Or a b).

Fixpoint Brzozowski (x : A) (r : regexp) : regexp := 
  match r with
  | RE_Empty => RE_Empty
  | RE_Void => RE_Empty
  | RE_Atom c => match (Aeq c x) with
              | true => RE_Void
              | false => RE_Empty
              end
  | RE_Cat r s => match (contains0 r) with
               | true => ((Brzozowski x r) ++ s) || (Brzozowski x s)
               | false => (Brzozowski x r) ++ s
               end
  | RE_Or r s => (Brzozowski x r) || (Brzozowski x s)
  | RE_Kleene r => (Brzozowski x r) ++ (RE_Kleene r) 
end.

Notation "r / x" := (Brzozowski x r).

(* Q15. write a function `rmatch` s.t. `rmatch r w` checks wether a     *)
(*      word `w` matches a given regular expression `r`.                *)

Fixpoint rmatch (r : regexp) (w : word) : bool := 
  match w with
  | nil => contains0 r
  | cons a s => rmatch (r / a) s
  end.

(* Q16. show that the `Brzozowski` function is correct.                 *)

Lemma Brzozowski_correct (x : A) (w : word) (r : regexp) :
  interp (Brzozowski x r) w -> interp r (x :: w).
Proof. 
  move : w.
  induction r.
  move => w h.
  simpl in h.
  case : h.
  move => w h.
  simpl in h.
  case : h.
  move => w h.
  case e: (Aeq a x).
  unfold Brzozowski in h.
  rewrite e in h.
  simpl in h.
  unfold lang1 in h.
  rewrite h.
  simpl.
  move : (Aeq_dec a x) => [h1 h2].
  move : (h1 e) => h3.
  symmetry in h3.
  rewrite h3.
  reflexivity.
  unfold Brzozowski in h.
  rewrite e in h.
  case h.
  move => w h.
  simpl in h.
  case e: (contains0 (r1)).
  rewrite e in h.
  simpl.
  move : h => /= [h1 | h2].
  move : h1 => [s [c [h1 [h2 h3]]]].
  exists (x :: s).
  exists c.
  split.
  simpl.
  rewrite h1.
  done.
  split.
  apply IHr1.
  apply h2.
  apply h3.
  exists nil.
  exists (x :: w).
  split.
  simpl.
  done.
  split.
  apply contains0_ok.
  apply e.
  apply IHr2.
  apply h2.
  rewrite e in h.
  move : h => /= [s [c [h1 [h2 h3]]]].
  exists (x :: s).
  exists c.
  split.
  simpl.
  rewrite h1.
  done.
  split.
  apply IHr1.
  apply h2.
  apply h3.
  move => w h.
  move : h => /= [h1 | h2].
  left.
  apply IHr1.
  apply h1.
  right.
  apply IHr2.
  apply h2.
  move => w h.
  move : h => /= [s [c [h1 [h2 h3]]]].
  symmetry in h1.
  rewrite h1.
  rewrite app_comm_cons.
  apply kleeneS.
  apply kleeneA.
  apply IHr.
  apply h2.
  apply h3.
Qed.

(* Q17. show that `rmatch` is correct.                                  *)

Lemma rmatch_correct (r : regexp) (w : word):
  rmatch r w -> interp r w.
Proof. 
  move : r.
  induction w.
  move => r h.
  apply contains0_ok.
  apply h.
  move => r h.
  apply Brzozowski_correct.
  apply IHw.
  apply h.
Qed.

(* Q18. (HARD - OPTIONAL) show that `rmatch` is complete.               *)

Lemma Brzozowski_complete (x : A) (w : word) (r : regexp) :
   interp r (x :: w) -> interp (Brzozowski x r) w.
Proof.
  move : w.
  move : x.
  induction r.
  move => x w h.
  case : h.
  move => x w h.
  simpl in h.
  done.
  move => x w h.
  simpl in h.
  move: h => [e1 e2].
  rewrite e1.
  rewrite e2.
  simpl.
  move : (Aeq_dec a a) => e.
  move : e => [h1 h2].
  rewrite h2.
  reflexivity.
  done.
  move => x w h.
  simpl in h.
  move: h => [s [c [h1 [h2 h3]]]].
  simpl.
  case e : (contains0 r1).
  case: s h1 h2.
  simpl.
  move => h1 h2.
  right.
  apply IHr2.
  rewrite h1 in h3.
  apply h3.
  move => a l.
  move => h1 h2.
  simpl in h1.
  case : h1.
  move => h1 h4.
  symmetry in h4.
  rewrite h1 in h2.
  rewrite h4.
  left.
  exists l.
  exists c.
  split.
  done.
  split.
  apply IHr1.
  apply h2.
  apply h3.
  case : s h1 h2.
  simpl.
  move => h h1.
  move : (contains0_ok r1) => [h2 h4].
  move : (h4 h1) => h5.
  rewrite e in h5.
  done.
  move => a l.
  move => h1 h2.
  simpl in h1.
  case : h1.
  move => h1 h4.
  rewrite h1 in h2.
  exists l.
  exists c.
  split.
  apply h4.
  split.
  apply IHr1.
  apply h2.
  apply h3.
  move => x w h.
  move : h => /= [h1 | h2].
  left.
  apply IHr1.
  apply h1.
  right.
  apply IHr2.
  apply h2.
  move => x w h.
  move: {-2}(x :: w) (Logic.eq_refl (x :: w)) h => /= w' w'E h; elim: h x w w'E => {w'} //.
  move => s h.
  move => x w.
  move => e1.
  exists w.
  exists nil.
  split.
  apply app_nil_r.
  split.
  apply IHr.
  symmetry in e1.
  rewrite e1.
  apply h.
  apply kleene0.
  move => s c h.
  move => h1 h2.
  move => h3.
  move => x w.
  move => e1.
  case e : s => [ | a l].
  rewrite e in e1.
  simpl in  e1.
  apply h3.
  apply e1.
  rewrite e in e1.
  simpl in e1.
  case : e1.
  move => e1 e2.
  move: (h1 a l e) => h4.
  move: h4 => [s1 [c1 [e3 [h5 h6]]]].
  exists s1.
  exists (c1 ++ c)%list.
  split.
  rewrite app_assoc.
  rewrite e3.
  rewrite e2.
  done.
  split.
  rewrite e1 in h5.
  apply h5.
  apply kleeneS.
  apply h6.
  apply h2.
Qed.

Lemma rmatch_complete (r : regexp) (w : word):
  interp r w -> rmatch r w.
Proof.
  move : r.
  induction w.
  move => r h.
  simpl.
  apply contains0_ok.
  apply h.
  move => r h.
  simpl.
  apply IHw.
  apply Brzozowski_complete.
  apply h.
Qed.
