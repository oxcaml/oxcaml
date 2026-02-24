(* TEST
 flags += "-dlambda -dno-unique-ids -extension mode_polymorphism_alpha";
 expect;
*)

(* Check that the code produced by TMC reads reasonably well. *)
let[@tail_mod_cons] rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs
;;
[%%expect{|
Uncaught exception: File "lambda/tmc.ml", line 734, characters 10-16: Assertion failed

|}]

(* check that TMC works for records as well *)
type 'a cell = { hd : 'a; tl : 'a rec_list }
and 'a rec_list = 'a cell option
[%%expect{|
0
type 'a cell = { hd : 'a; tl : 'a rec_list; }
and 'a rec_list = 'a cell option
|}]

let[@tail_mod_cons] rec rec_map f = function
  | None -> None
  | Some {hd; tl} -> Some { hd = f hd; tl = rec_map f tl }
;;
[%%expect{|
Uncaught exception: File "lambda/tmc.ml", line 734, characters 10-16: Assertion failed

|}]

(* check the case where several constructors are nested;
   we want to avoid creating an intermediate destination
   for each constructor.  *)
let[@tail_mod_cons] rec trip = function
  | [] -> []
  | x :: xs -> (x, 0) :: (x, 1) :: (x, 2) :: trip xs
;;
[%%expect{|
Uncaught exception: File "lambda/tmc.ml", line 734, characters 10-16: Assertion failed

|}]

(* check nested-constructors whose arguments
   are effectful: they need to be let-bound appropriately
   (ideally, only in the DPS version) *)
let[@tail_mod_cons] rec effects f = function
  | [] -> []
  | (x, y) :: xs -> f x :: f y :: effects f xs
;;
[%%expect{|
Uncaught exception: File "lambda/tmc.ml", line 734, characters 10-16: Assertion failed

|}]

(* Check the case where several constructors
   are nested across a duplicating context: the [f None ::]
   part should not be duplicated in each branch. *)
let[@tail_mod_cons] rec map_stutter f xs =
  f None :: (
    match xs with
    | [] -> []
    | x :: xs -> f (Some x) :: map_stutter f xs
  )
;;
[%%expect{|
Uncaught exception: File "lambda/tmc.ml", line 734, characters 10-16: Assertion failed

|}]

(* Check the case where several constructors
   are nested across a non-duplicating context;
   the [f None :: .] part can be delayed below the let..in,
   buts it expression argument must be let-bound
   before the let..in is evaluated. *)
type 'a stream = { hd : 'a; tl : unit -> 'a stream }
let[@tail_mod_cons] rec smap_stutter f xs n =
  if n = 0 then []
  else f None :: (
    let v = f (Some xs.hd) in
    v :: smap_stutter f (xs.tl ()) (n - 1)
  )
;;
[%%expect{|
0
type 'a stream = { hd : 'a; tl : unit -> 'a stream; }
Uncaught exception: File "lambda/tmc.ml", line 734, characters 10-16: Assertion failed

|}]
