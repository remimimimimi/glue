From Coq Require Import List Bool.Bool Arith.Arith Arith.EqNat.
From Coq Require Import Program.Program.
From Coq Require Import ssreflect.

From Hammer Require Import Hammer Tactics.

Require Import String.
Import ListNotations.

Inductive aexp : Type :=
  | ANum (n : nat)
  | APlus (a1 a2 : aexp)
  | AMinus (a1 a2 : aexp)
  | AMult (a1 a2 : aexp).

Inductive bexp : Type :=
  | BTrue
  | BFalse
  | BEq (a1 a2 : aexp)
  | BLe (a1 a2 : aexp)
  | BNot (b : bexp)
  | BAnd (b1 b2 : bexp).

Fixpoint aeval (a : aexp) : nat :=
  match a with
  | ANum n => n
  | APlus a1 a2 => (aeval a1) + (aeval a2)
  | AMinus a1 a2 => (aeval a1) - (aeval a2)
  | AMult a1 a2 => (aeval a1) * (aeval a2)
  end.

Example test_aeval1:
  aeval (APlus (ANum 2) (ANum 2)) = 4.
Proof. done. Qed.

Fixpoint beval (b : bexp) : bool :=
  match b with
  | BTrue => true
  | BFalse => false
  | BEq a1 a2 => (aeval a1) =? (aeval a2)
  | BLe a1 a2 => (aeval a1) <=? (aeval a2)
  | BNot b1 => negb (beval b1)
  | BAnd b1 b2 => andb (beval b1) (beval b2)
  end.

Fixpoint optimize_0plus (a:aexp) : aexp :=
  match a with
  | ANum n => ANum n
  | APlus (ANum 0) e2 => optimize_0plus e2
  | APlus e1 e2 => APlus (optimize_0plus e1) (optimize_0plus e2)
  | AMinus e1 e2 => AMinus (optimize_0plus e1) (optimize_0plus e2)
  | AMult e1 e2 => AMult (optimize_0plus e1) (optimize_0plus e2)
  end.

Example test_optimize_0plus:
  optimize_0plus (APlus (ANum 2)
                        (APlus (ANum 0)
                               (APlus (ANum 0) (ANum 1))))
  = APlus (ANum 2) (ANum 1).
Proof. done. Qed.

Theorem optimize_0plus_sound: forall a,
  aeval (optimize_0plus a) = aeval a.
Proof.
  elim => [| a1 IHa1 ? ? | | ];
    by[| elim : a1 IHa1 => /= [n IHn | | |]; first elim : n IHn => /=; by auto
       | sfirstorder].
Qed.

Reserved Notation "e '==>' n" (at level 90, left associativity).

Inductive aevalR : aexp -> nat -> Prop :=
  | E_ANum (n : nat) :
      (ANum n) ==> n
  | E_APlus (e1 e2 : aexp) (n1 n2 : nat) :
      (e1 ==> n1) -> (e2 ==> n2) -> (APlus e1 e2) ==> (n1 + n2)
  | E_AMinus (e1 e2 : aexp) (n1 n2 : nat) :
      (e1 ==> n1) -> (e2 ==> n2) -> (AMinus e1 e2) ==> (n1 - n2)
  | E_AMult (e1 e2 : aexp) (n1 n2 : nat) :
      (e1 ==> n1) -> (e2 ==> n2) -> (AMult e1 e2) ==> (n1 * n2)

  where "e '==>' n" := (aevalR e n) : type_scope.

Theorem aeval_iff_aevalR : forall a n, (a ==> n) <-> aeval a = n.
Proof.
  move => a n.
  split.
  - by elim => /= [| ? ? ? ? ? -> ? -> | ? ? ? ? ? -> ? -> | ? ? ? ? ? -> ? ->].
  - move : n; elim : a => /=; first (move => ? ? ->; constructor);
    by [move => ? IHa1 ? IHa2 ? <-; constructor; by [apply IHa1 | apply IHa2]].
Qed.

Reserved Notation "e '==>b' b" (at level 90, left associativity).

Inductive bevalR: bexp -> bool -> Prop :=
  | E_BTrue : BTrue ==>b true
  | E_BFalse : BFalse ==>b false
  | E_BEq (e1 e2 : aexp) (n1 n2 : nat) :
      (e1 ==> n1) -> (e2 ==> n2) -> (BEq e1 e2) ==>b (n1 =? n2)
  | E_BLe (e1 e2 : aexp) (n1 n2 : nat) :
      (e1 ==> n1) -> (e2 ==> n2) -> (BLe e1 e2) ==>b (n1 <=? n2)
  | E_BNot (e : bexp) (b : bool) : (e ==>b b) -> (BNot e) ==>b negb b
  | E_BAnd (e1 e2 : bexp) (b1 b2 : bool) :
      (e1 ==>b b1) -> (e2 ==>b b2) -> (BAnd e1 e2) ==>b (andb b1 b2)
  where "e '==>b' b" := (bevalR e b) : type_scope.

Theorem beval_iff_bevalR : forall e b, (e ==>b b) <-> beval e = b.
Proof.
  split.
  - elim;
      by [| move => e1 e2 n1 n2 /= H1 H2;
            rewrite aeval_iff_aevalR in H1; rewrite aeval_iff_aevalR in H2;
            auto
          | sfirstorder].
  - move => <-; elim : e; rewrite //=; constructor;
      by [rewrite aeval_iff_aevalR | rewrite -/beval; exact].
Qed.
