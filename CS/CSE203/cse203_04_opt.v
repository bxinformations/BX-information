(* There are several things which one may do
then :
A - Proving that heap_sort returns a sorted list
    (like done for insertion sort)

B - Proving that heap_sort preserves the content of 
    input list (like done for insertion sort)

C - Proving a stronger correction property for insertion
    sort, namely that the result is really a permuation
    of the input.


We give some starting points for each.
 *)




(*
A Proving that heap_sort returns a sorted list

The proof is similar than the one for insertion sort.
But one needs to prove that insertion in a tree preserves
the heap property.

Like for the correctness of insertion, this is best
done by proving a stronger induction hypothesis.
*)

(* A1 : proving merge preserves sorting *)

(* The main induction *)
Lemma merge_sorted_aux :
  forall l1 l2,
    sorted l1 ->
    sorted l2 ->
       sorted (merge l1 l2)
       /\forall x, le_head x l1 ->
                   le_head x l2 ->
                   le_head x (merge l1 l2).
Proof.
elim =>[| n1 l1 hl1]; elim => [| n2 l2 hl2]//= [s1 h1] [s2 h2].
case n12 : (_ <=? _); simpl.
  by split; auto; split; apply hl1;auto.
split; trivial; split; trivial; case: (hl2) => //= h hh; trivial.
by apply hh; trivial; apply leb_anti.
Qed.


Lemma to_list_sorted :
  forall t, heap t ->
            sorted (to_list t)
          /\forall m, le_tree m t -> le_head m (to_list t). 
Proof.
elim => [|n t1 iht1 t2 iht2]//=.
move => [nt1 [nt2 [ht1 ht2]]].
move: (iht1 ht1) => [iht11 iht12].
move: (iht2 ht2) => [iht21 iht22].
case: (merge_sorted_aux _ _ iht11 iht21) => [h1 h2].
split; auto.
Qed.


(* A2 proving that insertion in the heap preserves
the heap property *)

(* quite easy technical lemma; requires no induction *)
Lemma le_tree_trans : forall n m t,
    n <=? m -> le_tree m t -> le_tree n t.
Proof.
move => n m [| r t1 t2]//=.
apply leb_trans.
Qed.

(* the main induction *)
Lemma ins_heap_aux :
  forall t n, heap t ->
              heap (ins_tree n t)
              /\forall m, m <=? n = true ->
                          le_tree m t  ->
                          le_tree m (ins_tree n t).
Proof.
elim => [| m t1 iht1 t2 iht2] //= n [mt1 [mt2 [ht1 ht2]]].
move: (iht2 n ht2) => [iht11 iht12].
move: (iht2 m ht2) => [iht21 iht22].
case nm: (_ <=? _) => /=; do 4 (try split; auto).
- apply iht22; trivial.
  by apply le_tree_trans with m.
- by apply le_tree_trans with m.
- apply iht12; trivial.
  by apply leb_anti.
Qed.


Lemma ins_heap :
  forall t n, heap t ->
              heap (ins_tree n t).
Proof.
by move => t n ht; case: (ins_heap t n ht).
Qed.


Lemma to_heap_heap : forall l,
    heap (to_heap l).
Proof.
elim => [| n l hl]//=.
by apply ins_heap.
Qed.

(* A3 - pasting it together *)
Lemma heap_sorted :
  forall l, sorted (heap_sort l).
Proof.
move => l; unfold heap_sort; simpl.
case: (to_list_sorted (to_heap l)); trivial.
by apply to_heap_heap.
Qed.


(* B - Proving that heapsort preserves the content *)

(* Proving the properties about merge *)
Lemma merge_elem1 : forall l1 l2 n, elem n l1 -> elem n (merge l1 l2).
Proof.
elim => [| n1 l1 hl1]; elim => [| n2 l2 hl2] n //=.
move => [ -> | e] /=; case: (_ <=? _) => //=; auto.
Show.
  right.
  apply hl2; simpl; auto.
right.
apply hl2; simpl; auto.
Qed.

Lemma merge_elem2 : forall l1 l2 n, elem n l2 -> elem n (merge l1 l2).
Proof.
elim => [| n1 l1 hl1]; elim => [| n2 l2 hl2] n //=.
move => [ -> | e] /=; case: (_ <=? _) => //=; auto.
  right.
  apply hl1; simpl; auto.
right.
apply hl1; simpl; auto.
Qed.

Lemma merge_elem3 :  forall l1 l2 n, elem n (merge l1 l2) ->
                                     (elem n l1)\/(elem n l2).
elim => [| n1 l1 hl1]; elim => [| n2 l2 hl2] n //=.
  move => [e|e]; auto.
  move => [e|e]; auto.
case: (_ <=? _) => //=; auto.
  move => [e|e]; auto.
  case: (hl1 _ n e); auto.
  move => [e|e]; auto.
fold merge in e.  
case (hl2 n e); simpl; auto.
Qed.


(* elements of a tree *)

Fixpoint telem n t :=
  match t with
  | Leaf => False
  | Node m t1 t2 => n=m \/ telem n t1 \/ telem n t2
  end.

(* Provving the properties about ins_tree *)

Lemma inst_elem1 : forall t n, telem n (ins_tree n t).
Proof.
elim => [| m t1 ht1 t2 ht2] n //=.
  auto.
case: (_ <=? _) => //=; auto.
Qed.

Lemma inst_elem2 : forall t n m, telem n t -> telem n (ins_tree m t).
Proof.
elim => [| m t1 ht1 t2 ht2] n //= p.
case:  (_ <=? _) => //=; auto;
 move => [->|[e|e]]; auto; right; left;
 apply inst_elem1.
Qed.

Lemma inst_elem3 : forall t n m, telem n (ins_tree m t) ->
                                 n = m \/ telem n t.
Proof.
elim => [| p t1 ht1 t2 ht2] n m //=.
 move =>[->|[f|f]]//=; auto.
 case:  (_ <=? _) => //=; auto;
    move => [->|[e|e]]; auto; case: (ht2 _ _ e); auto.  
Qed.

(* Proving them for to_heap *)
Lemma to_heap_elem1 : forall l n,
    elem n l -> telem n (to_heap l).
Proof.
elim => [|m l hl]n//=.
move =>[->|e].
 apply inst_elem1.
apply inst_elem2.
by apply hl. 
Qed.

Lemma to_list_elem1 : forall t n,
    telem n t -> elem n (to_list t).
Proof.
elim => [|m t1 ht1 t2 ht2]n//=.
move=>[e|[e|e]]; auto; right.  
 apply merge_elem1; auto.      
apply merge_elem2; auto.
Qed.

(* Pasting it together *)
Lemma heapsort_elem1 : forall n l,
    elem n l -> elem n (heap_sort l).
Proof.
move => n l e; apply to_list_elem1.  
apply to_heap_elem1; auto.
Qed.

Lemma to_heap_elem2 : forall l n, telem n (to_heap l) -> elem n l.
Proof.
elim => [|m l hl]n//= e.
case: (inst_elem3 _ _ _ e); auto.
Qed.

Lemma to_list_elem2 : forall t n, elem n (to_list t) -> telem n t.
Proof.
elim => [|m t1 ht1 t2 ht2]n //=.  
move => [e|e]; auto.  
case: (merge_elem3 _ _ _ e); auto.
Qed.

Lemma heap_sort_elem2 : forall l n, elem n (heap_sort l) -> elem n l.
Proof.
move => l n e.
apply to_heap_elem2.
by apply to_list_elem2.
Qed.






(* Possible additional work *)
(* Show that insertion sort returns a permutation of the original list *)

(* We fist axiomatize permuations *)

Parameter permutation : forall A, list A -> list A -> Prop.

Axiom perm_refl : forall A (l : list A), permutation l l.

Axiom perm_app : forall A (l1 : list A) l2 x,
    permutation (cons x (app l1 l2))(app l1 (cons x l2)).

Axiom perm_cons : forall A (l1 : list A) l2 x,
    permutation l1 l2 -> permutation (cons x l1)(cons x l2).

Axiom perm_trans : forall A (l1 : list A) l2 l3,
    permutation l1 l2 -> permutation l2 l3 -> permutation l1 l3.

Axiom perm_sym : forall A (l1 : list A) l2, permutation l1 l2 -> permutation l2 l1.

Lemma ins_perm : forall n l, permutation (cons n l) (insert n l).
move => n; elim => [| m l hl]//=.
 apply perm_refl.
case (_ <=? _).
 apply perm_refl.
apply perm_trans with (cons m (cons n l)).
 apply (perm_app (cons m (nil _)) l n).
apply perm_cons; assumption.
Qed.

Lemma sort_perm : forall l, permutation l (insertion_sort l).
Proof.
elim => [| n l hl] //=.
 apply perm_refl.
apply perm_trans with (cons n (insertion_sort l)).
  apply perm_cons; assumption.
 apply ins_perm.
Qed.


(* C - Show that insertion sort returns a permutation of the original list *)

(* We fist axiomatize permuations *)

Parameter permutation : forall A, list A -> list A -> Prop.

Axiom perm_refl : forall A (l : list A), permutation l l.

Axiom perm_app : forall A (l1 : list A) l2 x,
    permutation (cons x (app l1 l2))(app l1 (cons x l2)).

Axiom perm_cons : forall A (l1 : list A) l2 x,
    permutation l1 l2 -> permutation (cons x l1)(cons x l2).

Axiom perm_trans : forall A (l1 : list A) l2 l3,
    permutation l1 l2 -> permutation l2 l3 -> permutation l1 l3.

Axiom perm_sym : forall A (l1 : list A) l2, permutation l1 l2 -> permutation l2 l1.


Lemma ins_perm : forall n l, permutation (cons n l) (insert n l).
move => n; elim => [| m l hl]//=.
 apply perm_refl.
case (_ <=? _).
 apply perm_refl.
apply perm_trans with (cons m (cons n l)).
 apply (perm_app (cons m (nil _)) l n).
apply perm_cons; assumption.
Qed.

Lemma sort_perm : forall l, permutation l (insertion_sort l).
Proof.
elim => [| n l hl] //=.
 apply perm_refl.
apply perm_trans with (cons n (insertion_sort l)).
  apply perm_cons; assumption.
 apply ins_perm.
Qed.
