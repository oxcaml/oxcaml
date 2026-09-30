(* Values are accumulated in 63 bits by [combine], a polynomial fold that is
   cheap, order-sensitive and a bijection in each argument. The bits are only
   mixed when the 32-bit hash is taken by [to_int32]. *)
type t = int

(* The constants below assume 63-bit ints. *)
let () = assert (Sys.int_size = 63)

let combine a b = (a * 0x9e3779b97f4a7c1) + b

let int n = n

(* Byte by byte (FNV-1a), so the result does not depend on the OCaml version. *)
let string s =
  let h = ref 0x4bf29ce484222325 in
  String.iter (fun c -> h := !h lxor Char.code c * 0x100000001b3) s;
  !h

(* A murmur3-style finalizer, with constants that fit an OCaml int. *)
let mix x =
  let x = x lxor (x lsr 33) in
  let x = x * 0x3f51afd7ed558ccd in
  let x = x lxor (x lsr 33) in
  let x = x * 0x34ceb9fe1a85ec53 in
  x lxor (x lsr 33)

(* The low 32 bits after mixing. *)
let to_int32 x = Int32.of_int (mix x)
