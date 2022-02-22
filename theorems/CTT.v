From Coq Require Import List.
From Coq Require Import Program.Program.
From Coq Require Import String.
From Coq Require Import Lia.

From Coq Require Import ssreflect ssrfun ssrbool.
Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

From Coq Require derive.Derive.
Import ListNotations.

From Hammer Require Import Hammer Tactics.

Require Import Arith.PeanoNat Arith.Minus Arith.Plus.

(* Record loc := mkloc { loc_file : string ; loc_pos : nat * nat }. *)

(* Definition ident := string. *)
(* Definition lident := string. *)

(* (* Telescope (x1 : A 1) .. (xn : An) *) *)
(* Definition telescope := list (ident * term)%type. *)
(* Inductive label := *)
(*   | object_label : lident -> telescope -> label *)
(*   | path_label : lident -> telescope -> list name ->  -> label *)

