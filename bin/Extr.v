Require Extraction.
(* NOTE: For proper string extraction *)
(* XXX: Not working for `dune build @check` for some reason *)
(* From Coq Require Import extraction.ExtrOcamlNativeString. *)

Require Import Theory.Bozon.

Extraction "extr.ml" type_of.
