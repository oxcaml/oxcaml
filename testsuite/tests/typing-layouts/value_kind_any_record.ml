(* TEST
 flags = "-extension layouts_alpha -dlambda -dno-unique-ids";
 expect;
*)

type ('a : any) t = { x : 'a; y : int }
[%%expect{|
0
type ('a : any) t = { x : 'a; y : int; }
|}]

let ints (r : int t) = r
[%%expect{|
(let
  (ints =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : (consts ()) (non_consts ([0: value<int>, value<int>])) r))
  (apply (field_imm 1 (global Toploop!)) "ints" ints))
val ints : int t -> int t = <fun>
|}]

let floats (r : float t) = r
[%%expect{|
(let
  (floats =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: value<float>, value<int>]))>]
       : (consts ()) (non_consts ([0: value<float>, value<int>])) r))
  (apply (field_imm 1 (global Toploop!)) "floats" floats))
val floats : float t -> float t = <fun>
|}]

let unboxed_floats (r : float# t) = r
[%%expect{|
(let
  (unboxed_floats =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: float64, value<int>]))>]
       : (consts ()) (non_consts ([0: float64, value<int>])) r))
  (apply (field_imm 1 (global Toploop!)) "unboxed_floats" unboxed_floats))
val unboxed_floats : float# t -> float# t = <fun>
|}]

let products (r : #(int * float#) t) = r
[%%expect{|
(let
  (products =
     (function {nlocal = 0}
       r[value<
          (consts ())
           (non_consts ([0: product value<int>, float64, value<int>]))>]
       : (consts ())
          (non_consts ([0: product value<int>, float64, value<int>]))
       r))
  (apply (field_imm 1 (global Toploop!)) "products" products))
val products : #(int * float#) t -> #(int * float#) t = <fun>
|}]

let voids (r : unit# t) = r
[%%expect{|
(let
  (voids =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: product , value<int>]))>]
       : (consts ()) (non_consts ([0: product , value<int>])) r))
  (apply (field_imm 1 (global Toploop!)) "voids" voids))
val voids : unit# t -> unit# t = <fun>
|}]

let opaque (type a : any) (r : a t) = r
[%%expect{|
(let (opaque = (function {nlocal = 0} r r))
  (apply (field_imm 1 (global Toploop!)) "opaque" opaque))
val opaque : ('a : any). 'a t -> 'a t = <fun>
|}]

type ('a : any) single = { field : 'a }
[%%expect{|
0
type ('a : any) single = { field : 'a; }
|}]

(* Refining [any] to [float] does not produce a flat float record. *)
let boxed_floats (r : float single) = r
[%%expect{|
(let
  (boxed_floats =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: value<float>]))>]
       : (consts ()) (non_consts ([0: value<float>])) r))
  (apply (field_imm 1 (global Toploop!)) "boxed_floats" boxed_floats))
val boxed_floats : float single -> float single = <fun>
|}]

let all_void (r : unit# single) = r
[%%expect{|
(let
  (all_void =
     (function {nlocal = 0}
       r[value<(consts ()) (non_consts ([0: product ]))>]
       : (consts ()) (non_consts ([0: product ])) r))
  (apply (field_imm 1 (global Toploop!)) "all_void" all_void))
val all_void : unit# single -> unit# single = <fun>
|}]

type ('a : any) mutable_record = { mutable field : 'a }
[%%expect{|
0
type ('a : any) mutable_record = { mutable field : 'a; }
|}]

let mutable_record (r : int mutable_record) = r
[%%expect{|
(let (mutable_record = (function {nlocal = 0} r r))
  (apply (field_imm 1 (global Toploop!)) "mutable_record" mutable_record))
val mutable_record : int mutable_record -> int mutable_record = <fun>
|}]

type ('a : any) inline = A | B of { x : 'a; y : int }
[%%expect{|
0
type ('a : any) inline = A | B of { x : 'a; y : int; }
|}]

let inline (r : float# inline) =
  match r with
  | A -> 0
  | B r -> r.y
[%%expect{|
(let
  (inline =
     (function {nlocal = 0}
       r[value<(consts (0)) (non_consts ([0: float64, value<int>]))>] : int
       (if r (mixedfield 1  (float64,value<int>) r) 0)))
  (apply (field_imm 1 (global Toploop!)) "inline" inline))
val inline : float# inline -> int = <fun>
|}]
