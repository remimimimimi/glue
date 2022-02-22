open Format
(* open Extr *)

(* (\* val nat_to_int : nat -> int *\) *)
(* let nat_to_int n = *)
(*     let rec nat_to_int' (p : nat * int) : (nat * int) = *)
(*         match fst p with *)
(*         | O -> (O, snd p) *)
(*         | S n -> nat_to_int' (n, snd p + 1) *)
(*     in snd (nat_to_int' (n, 0)) *)

(* let () = printf "Hello %d!" (nat_to_int (summ (S O) (S (S (S O))))) *)
let () = printf "Hello World!"
