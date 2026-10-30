(* TEST
 flags = "-extension layouts_beta";
 { expect; }
 { expect.opt; }
*)

type ('a : any) seq = Nil | Cons of 'a * 'a seq
[%%expect{|
type ('a : any) seq = Nil | Cons of 'a * 'a seq
|}]

(* Requires the TMC [delay_impure] transformation to know the layout of the
   elements of the [seq]. *)
let[@tail_mod_cons] rec copy (xs : #(int * int) seq) =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (x, (copy [@tailcall]) xs)
[%%expect{|
val copy : #(int * int) seq -> #(int * int) seq = <fun>
|}]

(* Should return (11, 22). The TMC code that writes the recursive result to the
   hole must know that hole is the _third_ thing in the block because of the
   unboxed pair. *)
let[@tail_mod_cons] rec repeat n (x : #(int * int)) =
  if n = 0 then Nil else Cons (x, (repeat [@tailcall]) (n - 1) x)

let _ =
  match repeat 1 #(11, 22) with
  | Nil -> failwith "expected Cons"
  | Cons (#(a, b), _) -> a, b
[%%expect{|
val repeat : int -> #(int * int) -> #(int * int) seq = <fun>
- : int * int = (11, 22)
|}]

(* The value tail precedes the unboxed float in native code. *)
let[@tail_mod_cons] rec copy_float (xs : float# seq) =
  match xs with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (x, (copy_float [@tailcall]) xs)
[%%expect{|
val copy_float : float# seq -> float# seq = <fun>
|}]

external box_float : float# -> float = "%box_float"
let _ =
  match copy_float (Sys.opaque_identity (Cons (#1.5, Cons (#2.5, Nil)))) with
  | Cons (a, Cons (b, Nil)) ->
    box_float a, box_float b
  | _ -> failwith "unexpected sequence"
[%%expect{|
external box_float : float# -> float = "%box_float"
- : float * float = (1.5, 2.5)
|}]

(* Boxed and unboxed fields before and after the hole *)
type t = Nil
       | Cons of { a : string;
                   b : float#;
                   t : t;
                   c : int;
                   d : #(float * int16#) }

let[@tail_mod_cons] rec repeat n a b c d =
  if n = 0
  then Nil
  else Cons { a; b; t = (repeat [@tailcall]) (n - 1) a b c d; c; d }

external box_int16 : int16# -> int16 = "%int16_of_int16#"
let _ =
  match repeat 3 "hi" #3.14 42 #(3.15, #43S) with
  | Cons { t = Cons { t = Cons { a; b; t = Nil; c; d = #(d1, d2)} } } ->
    a, box_float b, c, d1, box_int16 d2
  | _ -> failwith "expected shape"
[%%expect{|
type t =
    Nil
  | Cons of { a : string; b : float#; t : t; c : int; d : #(float * int16#);
    }
val repeat : int -> string -> float# -> int -> #(float * int16#) -> t = <fun>
external box_int16 : int16# -> int16 = "%int16_of_int16#"
- : string * float * int * float * int16 = ("hi", 3.14, 42, 3.15, 43S)
|}]

(* A value hole nested inside an unboxed tuple is not supported. *)
type t = Nil
       | Cons of { a : float#;
                   b : #(float# * t * float#);
                   c : float#; }

let[@tail_mod_cons] rec repeat n a b1 b2 c =
  if n = 0
  then Nil
  else Cons { a; b = #(b1, (repeat [@tailcall]) (n - 1) a b1 b2 c, b2); c }

let _ =
  match repeat 3 #1.11 #1.01 #2.02 #2.22 with
  | Cons { b = #(_, Cons { b = #(_, Cons { a; b = #(b1, Nil, b2); c }, _) }, _) } ->
    box_float a, box_float b1, box_float b2, box_float c
  | _ -> failwith "expected shape"
[%%expect{|
type t =
    Nil
  | Cons of { a : float#; b : #(float# * t * float#); c : float#; }
Lines 6-9, characters 31-75:
6 | ...............................n a b1 b2 c =
7 |   if n = 0
8 |   then Nil
9 |   else Cons { a; b = #(b1, (repeat [@tailcall]) (n - 1) a b1 b2 c, b2); c }
Warning 71 [unused-tmc-attribute]: This function is marked "@tail_mod_cons"
  but is never applied in TMC position.

Line 9, characters 27-65:
9 |   else Cons { a; b = #(b1, (repeat [@tailcall]) (n - 1) a b1 b2 c, b2); c }
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall

Line 9, characters 27-65:
9 |   else Cons { a; b = #(b1, (repeat [@tailcall]) (n - 1) a b1 b2 c, b2); c }
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 51 [wrong-tailcall-expectation]: expected tailcall

val repeat : int -> float# -> float# -> float# -> float# -> t = <fun>
- : float * float * float * float = (1.11, 1.01, 2.02, 2.22)
|}]

(* Products before and after *)
type t = Nil
       | Cons of { a : float#;
                   b : #(float * int16#);
                   t : t;
                   c : float#;
                   d : #(float * int16#) }

let[@tail_mod_cons] rec repeat n a b c d =
  if n = 0
  then Nil
  else Cons { a; b; t = (repeat [@tailcall]) (n - 1) a b c d; c; d }

let _ =
  match repeat 3 #6.28 #(2.85, #41S) #3.14 #(3.15, #43S) with
  | Cons { t = Cons { t = Cons { a; b = #(b1, b2); t = Nil; c; d = #(d1, d2)} } } ->
    box_float a, b1, box_int16 b2, box_float c, d1, box_int16 d2
  | _ -> failwith "expected shape"
[%%expect{|
type t =
    Nil
  | Cons of { a : float#; b : #(float * int16#); t : t; c : float#;
      d : #(float * int16#);
    }
val repeat :
  int -> float# -> #(float * int16#) -> float# -> #(float * int16#) -> t =
  <fun>
- : float * float * int16 * float * float * int16 =
(6.28, 2.85, 41S, 3.14, 3.15, 43S)
|}]

(* Nested placeholder under boxed tuple *)
type t = Nil
       | Cons of { a : float#;
                   b : float * t * float;
                   c : float#; }

let[@tail_mod_cons] rec repeat n a b1 b2 c =
  if n = 0
  then Nil
  else Cons { a; b = (b1, (repeat [@tailcall]) (n - 1) a b1 b2 c, b2); c }

let _ =
  match repeat 3 #1.11 1.01 2.02 #2.22 with
  | Cons { b = _, Cons { b = _, Cons { a; b = (b1, Nil, b2); c }, _ }, _ } ->
    box_float a, b1, b2, box_float c
  | _ -> failwith "expected shape"
[%%expect{|
type t = Nil | Cons of { a : float#; b : float * t * float; c : float#; }
val repeat : int -> float# -> float -> float -> float# -> t = <fun>
- : float * float * float * float = (1.11, 1.01, 2.02, 2.22)
|}]

(* Nested placeholder under boxed record *)
type t = Nil
       | Cons of { a : float#;
                   b : s;
                   c : float#; }
and s = { x : float#; t : t; y : float# }

let[@tail_mod_cons] rec repeat n a x y c =
  if n = 0
  then Nil
  else Cons { a; b = { x; t = (repeat [@tailcall]) (n - 1) a x y c; y }; c }

let _ =
  match repeat 3 #1.11 #1.01 #2.02 #2.22 with
  | Cons { b = { t = Cons { b = { t = Cons { a; b = { x; t = Nil; y }; c } } } } } ->
    box_float a, box_float x, box_float y, box_float c
  | _ -> failwith "expected shape"
[%%expect{|
type t = Nil | Cons of { a : float#; b : s; c : float#; }
and s = { x : float#; t : t; y : float#; }
val repeat : int -> float# -> float# -> float# -> float# -> t = <fun>
- : float * float * float * float = (1.11, 1.01, 2.02, 2.22)
|}]
