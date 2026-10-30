(* TEST
 readonly_files = "stubs.c";
 native-compiler;
 native-dynlink;
 shared-libraries;
 flambda2;
 {
   setup-ocamlopt.opt-build-env;
   program = "stubs.cmxs";
   flags = "-shared";
   all_modules = "stubs.c";
   ocamlopt.opt;
   flags = "-I ocamlopt.opt stubs.cmxs";
   {
     expect.opt;
   }{
     flags += " -Oclassic";
     expect.opt;
   }{
     flags += " -O3";
     expect.opt;
   }
 }{
   setup-ocamlc.byte-build-env;
   program = "stubs.cma";
   flags = "-a -dllib ./ocamlc.byte/dllstubs";
   all_modules = "stubs.c";
   ocamlc.byte;
   program = "stubs";
   modules = "stubs.o";
   ocamlmklib;
   modules = "";
   flags = "-I ocamlc.byte stubs.cma";
   expect;
 }
*)

(* Arguments marked [@elide_void] are not passed to externals. The stubs
   concatenate their int arguments as decimal digits, so an unexpected void
   argument passed in bytecode (as unit, i.e. 0) changes the result. Each
   ordinary binding uses a bytecode stub that accepts the unit arguments. *)

type w : void
external w : unit -> w = "%unbox_unit"

let print_int x = Format.printf "%d@." x
let print_float x = Format.printf "%g@." x
let print_bool x = Format.printf "%b@." x
let note name = Format.printf "%s@." name
let[@inline never] noisy_w name = note name; w ()
let raise_exit () = if Sys.opaque_identity true then raise Exit
let value name x = note name; x
[%%expect {|
type w : void
external w : unit -> w = "%unbox_unit"
val print_int : int -> unit = <fun>
val print_float : float -> unit = <fun>
val print_bool : bool -> unit = <fun>
val note : string -> unit = <fun>
val noisy_w : string -> w = <fun>
val raise_exit : unit -> unit = <fun>
val value : string -> 'a -> 'a = <fun>
|}]

(* C stubs: direct calls, followed by shared eta-expansion tests. Keep the
   direct calls explicit so they do not just test a higher-order wrapper. *)

external first : w -> int -> int -> int
  = "elide_first_normal" "elide_digits2"
external first_elide : (w [@elide_void]) -> int -> int -> int = "elide_digits2"
[%%expect {|
external first : w -> int -> int -> int = "elide_first_normal"
  "elide_digits2"
external first_elide : (w [@elide_void]) -> int -> int -> int
  = "elide_digits2"
|}]

let () = print_int (first (noisy_w "w") (value "a" 1) (value "b" 2))
[%%expect {|
b
a
w
12
|}]

let () = print_int (first_elide (noisy_w "w") (value "a" 1) (value "b" 2))
[%%expect {|
b
a
w
12
|}]

external mid : int -> w -> int -> int
  = "elide_mid_normal" "elide_digits2"
external mid_elide : int -> (w [@elide_void]) -> int -> int = "elide_digits2"
[%%expect {|
external mid : int -> w -> int -> int = "elide_mid_normal" "elide_digits2"
external mid_elide : int -> (w [@elide_void]) -> int -> int = "elide_digits2"
|}]

let () = print_int (mid (value "a" 1) (noisy_w "w") (value "b" 2))
[%%expect {|
b
w
a
12
|}]

let () = print_int (mid_elide (value "a" 1) (noisy_w "w") (value "b" 2))
[%%expect {|
b
w
a
12
|}]

external last : int -> int -> w -> int
  = "elide_last_normal" "elide_digits2"
external last_elide : int -> int -> (w [@elide_void]) -> int = "elide_digits2"
[%%expect {|
external last : int -> int -> w -> int = "elide_last_normal" "elide_digits2"
external last_elide : int -> int -> (w [@elide_void]) -> int
  = "elide_digits2"
|}]

let () = print_int (last (value "a" 1) (value "b" 2) (noisy_w "w"))
[%%expect {|
w
b
a
12
|}]

let () = print_int (last_elide (value "a" 1) (value "b" 2) (noisy_w "w"))
[%%expect {|
w
b
a
12
|}]

external lab : a:int -> w:w -> b:int -> int
  = "elide_mid_normal" "elide_digits2"
external lab_elide : a:int -> w:(w [@elide_void]) -> b:int -> int
  = "elide_digits2"
[%%expect {|
external lab : a:int -> w:w -> b:int -> int = "elide_mid_normal"
  "elide_digits2"
external lab_elide : a:int -> w:(w [@elide_void]) -> b:int -> int
  = "elide_digits2"
|}]

(* Labels are deliberately supplied out of order. *)
let () =
  print_int (lab ~b:(value "b" 2) ~w:(noisy_w "w") ~a:(value "a" 1))
[%%expect {|
b
w
a
12
|}]

let () =
  print_int (lab_elide ~b:(value "b" 2) ~w:(noisy_w "w") ~a:(value "a" 1))
[%%expect {|
b
w
a
12
|}]

external mixed : w -> w -> int -> int
  = "elide_mixed_normal" "elide_digits1"
(* The unannotated void argument is still passed in bytecode. *)
external mixed_elide : (w [@elide_void]) -> w -> int -> int
  = "elide_digits2" "elide_digits1"
[%%expect {|
external mixed : w -> w -> int -> int = "elide_mixed_normal" "elide_digits1"
external mixed_elide : (w [@elide_void]) -> w -> int -> int = "elide_digits2"
  "elide_digits1"
|}]

let () = print_int (mixed (noisy_w "w1") (noisy_w "w2") (value "a" 6))
[%%expect {|
a
w2
w1
6
|}]

let () = print_int (mixed_elide (noisy_w "w1") (noisy_w "w2") (value "a" 6))
[%%expect {|
a
w2
w1
6
|}]

external seven_last :
  int -> int -> int -> int -> int -> int -> w -> int
  = "elide_seven_last_normal" "elide_digits6"
(* The bytecode stub receives [argn], which the result encodes. The ordinary
   stub checks the extra unit argument before returning the same result. *)
external seven_last_elide :
  int -> int -> int -> int -> int -> int -> (w [@elide_void]) -> int
  = "elide_digits_argv" "elide_digits6"
[%%expect {|
external seven_last : int -> int -> int -> int -> int -> int -> w -> int
  = "elide_seven_last_normal" "elide_digits6"
external seven_last_elide :
  int -> int -> int -> int -> int -> int -> (w [@elide_void]) -> int
  = "elide_digits_argv" "elide_digits6"
|}]

let () = print_int (seven_last 1 2 3 4 5 6 (noisy_w "w"))
[%%expect {|
w
60123456
|}]

let () = print_int (seven_last_elide 1 2 3 4 5 6 (noisy_w "w"))
[%%expect {|
w
60123456
|}]

let () =
  print_int (seven_last (value "a" 1) (value "b" 2) (value "c" 3)
               (value "d" 4) (value "e" 5) (value "f" 6) (noisy_w "w"))
[%%expect {|
w
f
e
d
c
b
a
60123456
|}]

let () =
  print_int (seven_last_elide (value "a" 1) (value "b" 2) (value "c" 3)
               (value "d" 4) (value "e" 5) (value "f" 6) (noisy_w "w"))
[%%expect {|
w
f
e
d
c
b
a
60123456
|}]

external six_source : int -> w -> int -> w -> int -> w -> int
  = "elide_six_source_normal" "elide_digits3"
(* Six source arguments but three runtime ones, so one name suffices. *)
external six_source_elide :
  int -> (w [@elide_void]) -> int -> (w [@elide_void]) -> int
  -> (w [@elide_void]) -> int
  = "elide_digits3"
[%%expect {|
external six_source : int -> w -> int -> w -> int -> w -> int
  = "elide_six_source_normal" "elide_digits3"
external six_source_elide :
  int ->
  (w [@elide_void]) ->
  int -> (w [@elide_void]) -> int -> (w [@elide_void]) -> int
  = "elide_digits3"
|}]

let () =
  print_int (six_source (value "a" 1) (noisy_w "w1") (value "b" 2)
               (noisy_w "w2") (value "c" 3) (noisy_w "w3"))
[%%expect {|
w3
c
w2
b
w1
a
123
|}]

let () =
  print_int (six_source_elide (value "a" 1) (noisy_w "w1") (value "b" 2)
               (noisy_w "w2") (value "c" 3) (noisy_w "w3"))
[%%expect {|
w3
c
w2
b
w1
a
123
|}]

external hypot :
  (float [@unboxed]) -> w -> (float [@unboxed]) -> (float [@unboxed])
  = "elide_hypot_normal" "caml_hypot" [@@noalloc]
(* Global [@@unboxed] does not apply to the elided argument. *)
external hypot_elide : float -> (w [@elide_void]) -> float -> float
  = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
[%%expect {|
external hypot :
  (float [@unboxed]) -> w -> (float [@unboxed]) -> (float [@unboxed])
  = "elide_hypot_normal" "caml_hypot" [@@noalloc]
external hypot_elide : float -> (w [@elide_void]) -> float -> float
  = "caml_hypot_float" "caml_hypot" [@@unboxed] [@@noalloc]
|}]

let () = print_float (hypot (value "a" 3.) (noisy_w "w") (value "b" 4.))
[%%expect {|
b
w
a
5
|}]

let () = print_float (hypot_elide (value "a" 3.) (noisy_w "w") (value "b" 4.))
[%%expect {|
b
w
a
5
|}]

(* Shared tests for eta-expanded and partial applications. *)
let[@inline never] test_mid (f : int -> w -> int -> int) =
  let f = Sys.opaque_identity f in
  print_int (f (value "a" 1) (noisy_w "w") (value "b" 2));
  note "partial before void";
  let before_void = Sys.opaque_identity (f (value "a" 1)) in
  print_int (before_void (noisy_w "w") (value "b" 2));
  note "partial after void";
  let after_void = Sys.opaque_identity (f (value "a" 1) (noisy_w "w")) in
  print_int (after_void (value "b" 2))
[%%expect {|
val test_mid : (int -> w -> int -> int) -> unit = <fun>
|}]

let () = test_mid mid
[%%expect {|
b
w
a
12
partial before void
a
b
w
12
partial after void
w
a
b
12
|}]

let () = test_mid mid_elide
[%%expect {|
b
w
a
12
partial before void
a
b
w
12
partial after void
w
a
b
12
|}]

(* Also partially apply the external itself, not just an OCaml function.
   Keep these inside functions: the native toplevel raises [Not_found] for
   some partial applications at initialization. *)
let[@inline never] test_partial () =
  let before_void = Sys.opaque_identity (mid 1) in
  let after_void = Sys.opaque_identity (mid 1 (w ())) in
  let labelled = Sys.opaque_identity (lab ~w:(w ())) in
  print_int (before_void (w ()) 2);
  print_int (after_void 2);
  print_int (labelled ~a:1 ~b:2)
let () = test_partial ()
[%%expect {|
val test_partial : unit -> unit = <fun>
12
12
12
|}]

let[@inline never] test_partial () =
  let before_void = Sys.opaque_identity (mid_elide 1) in
  let after_void = Sys.opaque_identity (mid_elide 1 (w ())) in
  let labelled = Sys.opaque_identity (lab_elide ~w:(w ())) in
  print_int (before_void (w ()) 2);
  print_int (after_void 2);
  print_int (labelled ~a:1 ~b:2)
let () = test_partial ()
[%%expect {|
val test_partial : unit -> unit = <fun>
12
12
12
|}]

let[@inline never] test_lab (f : a:int -> w:w -> b:int -> int) =
  let f = Sys.opaque_identity f in
  let partial = Sys.opaque_identity (f ~w:(noisy_w "w")) in
  print_int (partial ~a:(value "a" 1) ~b:(value "b" 2))
let[@inline never] test_mixed f =
  let f = Sys.opaque_identity f in
  print_int (f (noisy_w "w1") (noisy_w "w2") (value "a" 6))
let[@inline never] test_seven_last f =
  let f = Sys.opaque_identity f in
  print_int (f 1 2 3 4 5 6 (noisy_w "w"))
let[@inline never] test_six_source f =
  let f = Sys.opaque_identity f in
  print_int (f (value "a" 1) (noisy_w "w1") (value "b" 2)
               (noisy_w "w2") (value "c" 3) (noisy_w "w3"))
let[@inline never] test_hypot f =
  let f = Sys.opaque_identity f in
  print_float (f (value "a" 3.) (noisy_w "w") (value "b" 4.))
[%%expect {|
val test_lab : (a:int -> w:w -> b:int -> int) -> unit = <fun>
val test_mixed : (w -> w -> int -> int) -> unit = <fun>
val test_seven_last :
  (int -> int -> int -> int -> int -> int -> w -> int) -> unit = <fun>
val test_six_source : (int -> w -> int -> w -> int -> w -> int) -> unit =
  <fun>
val test_hypot : (float -> w -> float -> float) -> unit = <fun>
|}]

let () = test_lab lab
[%%expect {|
w
b
a
12
|}]

let () = test_lab lab_elide
[%%expect {|
w
b
a
12
|}]

let () = test_mixed mixed
[%%expect {|
a
w2
w1
6
|}]

let () = test_mixed mixed_elide
[%%expect {|
a
w2
w1
6
|}]

let () = test_seven_last seven_last
[%%expect {|
w
60123456
|}]

let () = test_seven_last seven_last_elide
[%%expect {|
w
60123456
|}]

let () = test_six_source six_source
[%%expect {|
w3
c
w2
b
w1
a
123
|}]

let () = test_six_source six_source_elide
[%%expect {|
w3
c
w2
b
w1
a
123
|}]

let () = test_hypot hypot
[%%expect {|
b
w
a
5
|}]

let () = test_hypot hypot_elide
[%%expect {|
b
w
a
5
|}]

(* Direct calls agree with staged applications. *)
let[@inline never] test_staged () =
  let f = mid (value "a" 1) (noisy_w "w") in
  print_int (f (value "b" 2))
let () = test_staged ()
[%%expect {|
val test_staged : unit -> unit = <fun>
w
a
b
12
|}]

let[@inline never] test_staged () =
  let f = mid_elide (value "a" 1) (noisy_w "w") in
  print_int (f (value "b" 2))
let () = test_staged ()
[%%expect {|
val test_staged : unit -> unit = <fun>
w
a
b
12
|}]

(* A retained read must happen before a witness mutates its source. *)
let () =
  let mutable b = 2 in
  print_int (mid 1 (b <- 3; w ()) b);
  print_int b
[%%expect {|
12
3
|}]

let () =
  let mutable b = 2 in
  print_int (mid_elide 1 (b <- 3; w ()) b);
  print_int b
[%%expect {|
12
3
|}]

(* Exceptions stop evaluation at the same point. *)
let () =
  match mid (value "a" 1) (note "w"; raise_exit (); w ())
          (value "b" 2) with
  | result -> print_int result
  | exception Exit -> note "Exit"
[%%expect {|
b
w
Exit
|}]

let () =
  match mid_elide (value "a" 1) (note "w"; raise_exit (); w ())
          (value "b" 2) with
  | result -> print_int result
  | exception Exit -> note "Exit"
[%%expect {|
b
w
Exit
|}]

(* Intrinsics *)

(* The native C stub aborts if called: these calls must be lowered to the
   intrinsic. Bytecode uses the boxed C stubs. The witness's effects must
   survive the intrinsic's purity annotations. *)
external select :
  bool -> (int64 [@unboxed]) -> w -> (int64 [@unboxed]) -> (int64 [@unboxed])
  = "elide_select_normal" "caml_csel_int64_unboxed"
  [@@builtin] [@@noalloc] [@@no_effects] [@@no_coeffects]
external select_elide :
  bool -> (int64 [@unboxed]) -> (w [@elide_void]) -> (int64 [@unboxed])
  -> (int64 [@unboxed])
  = "elide_select" "caml_csel_int64_unboxed"
  [@@builtin] [@@noalloc] [@@no_effects] [@@no_coeffects]
[%%expect {|
external select :
  bool -> (int64 [@unboxed]) -> w -> (int64 [@unboxed]) -> (int64 [@unboxed])
  = "elide_select_normal" "caml_csel_int64_unboxed" [@@no_coeffects]
  [@@no_effects] [@@builtin] [@@noalloc]
external select_elide :
  bool ->
  (int64 [@unboxed]) ->
  (w [@elide_void]) -> (int64 [@unboxed]) -> (int64 [@unboxed])
  = "elide_select" "caml_csel_int64_unboxed" [@@no_coeffects] [@@no_effects]
  [@@builtin] [@@noalloc]
|}]

let () =
  Format.printf "%Ld@."
    (select (value "cond" false) (value "ifso" 17L) (noisy_w "w")
       (value "ifnot" 42L))
[%%expect {|
ifnot
w
ifso
cond
42
|}]

let () =
  Format.printf "%Ld@."
    (select_elide (value "cond" false) (value "ifso" 17L) (noisy_w "w")
       (value "ifnot" 42L))
[%%expect {|
ifnot
w
ifso
cond
42
|}]

let[@inline never] test_select f =
  let f = Sys.opaque_identity f in
  Format.printf "%Ld@."
    (f (value "cond" true) (value "ifso" 17L) (noisy_w "w")
       (value "ifnot" 42L))
[%%expect {|
val test_select : (bool -> int64 -> w -> int64 -> int64) -> unit = <fun>
|}]

let () = test_select select
[%%expect {|
ifnot
w
ifso
cond
17
|}]

let () = test_select select_elide
[%%expect {|
ifnot
w
ifso
cond
17
|}]

let[@inline never] test_select_partial () =
  note "before witness";
  let before =
    Sys.opaque_identity (select (value "cond" true) (value "ifso" 17L))
  in
  Format.printf "%Ld@." (before (noisy_w "w") (value "ifnot" 42L));
  note "after witness";
  let after =
    Sys.opaque_identity
      (select (value "cond" false) (value "ifso" 17L) (noisy_w "w"))
  in
  Format.printf "%Ld@." (after (value "ifnot" 42L))
let () = test_select_partial ()
[%%expect {|
val test_select_partial : unit -> unit = <fun>
before witness
ifso
cond
ifnot
w
17
after witness
w
ifso
cond
ifnot
42
|}]

let[@inline never] test_select_partial () =
  note "before witness";
  let before =
    Sys.opaque_identity (select_elide (value "cond" true) (value "ifso" 17L))
  in
  Format.printf "%Ld@." (before (noisy_w "w") (value "ifnot" 42L));
  note "after witness";
  let after =
    Sys.opaque_identity
      (select_elide (value "cond" false) (value "ifso" 17L) (noisy_w "w"))
  in
  Format.printf "%Ld@." (after (value "ifnot" 42L))
let () = test_select_partial ()
[%%expect {|
val test_select_partial : unit -> unit = <fun>
before witness
ifso
cond
ifnot
w
17
after witness
w
ifso
cond
ifnot
42
|}]

(* Primitives have fixed arities: unlike C stubs, they cannot accept ordinary
   void arguments. We test both direct and partial/eta-expanded calls below.
   Direct short-circuiting calls can skip the RHS; an eta-expanded function
   receives already-evaluated arguments. *)

external id_last : string -> (w [@elide_void]) -> string = "%identity"
external add_mid : int -> (w [@elide_void]) -> int -> int = "%addint"
external opaque : (w [@elide_void]) -> 'a -> 'a = "%opaque"
external equal : 'a -> (w [@elide_void]) -> 'a -> bool = "%equal"
external file : (w [@elide_void]) -> string = "%loc_FILE"
external argv : (w [@elide_void]) -> string array = "%sys_argv"
external ( &&& ) : bool -> (w [@elide_void]) -> bool -> bool = "%sequand"
external ( ||| ) : (w [@elide_void]) -> bool -> bool -> bool = "%sequor"
[%%expect {|
external id_last : string -> (w [@elide_void]) -> string = "%identity"
external add_mid : int -> (w [@elide_void]) -> int -> int = "%addint"
external opaque : (w [@elide_void]) -> 'a -> 'a = "%opaque"
external equal : 'a -> (w [@elide_void]) -> 'a -> bool = "%equal"
external file : (w [@elide_void]) -> string = "%loc_FILE"
external argv : (w [@elide_void]) -> string array = "%sys_argv"
external ( &&& ) : bool -> (w [@elide_void]) -> bool -> bool = "%sequand"
external ( ||| ) : (w [@elide_void]) -> bool -> bool -> bool = "%sequor"
|}]

let () =
  let s = "hello" in
  let id = id_last s (noisy_w "id witness") in
  Format.printf "%s (same object: %b)@." id (id == s);
  let opaque = opaque (noisy_w "opaque witness") (value "value" s) in
  Format.printf "%s (same object: %b)@." opaque (opaque == s)
[%%expect {|
id witness
hello (same object: true)
value
opaque witness
hello (same object: true)
|}]

let () = print_int (add_mid (value "a" 1) (noisy_w "w") (value "b" 2))
[%%expect {|
b
w
a
3
|}]

let () =
  print_bool (equal 1 (w ()) 1);
  print_bool (equal "a" (w ()) "b");
  (* Compare these with the ordinary primitives rather than snapshotting
     the test runner's filename and arguments. *)
  Format.printf "file matches __FILE__: %b@." (file (w ()) = __FILE__);
  Format.printf "argv matches Sys.argv: %b@." (argv (w ()) = Sys.argv)
[%%expect {|
true
false
file matches __FILE__: true
argv matches Sys.argv: true
|}]

(* All witnesses are evaluated first, then the LHS, then lazily the RHS. The choice of how
   to do the ordering here is somewhat arbitrary. *)
let () =
  let t = Sys.opaque_identity true and f = Sys.opaque_identity false in
  print_bool (( &&& ) (value "lhs" f) (noisy_w "w") (value "rhs" t))
[%%expect {|
w
lhs
false
|}]

let () =
  let t = Sys.opaque_identity true in
  print_bool (( &&& ) (value "lhs" t) (noisy_w "w") (value "rhs" t))
[%%expect {|
w
lhs
rhs
true
|}]

let () =
  let t = Sys.opaque_identity true and f = Sys.opaque_identity false in
  print_bool (( &&& ) t (w ()) f)
[%%expect {|
false
|}]

let () =
  let t = Sys.opaque_identity true and f = Sys.opaque_identity false in
  print_bool (( ||| ) (noisy_w "w") (value "lhs" t) (value "rhs" f))
[%%expect {|
w
lhs
true
|}]

let () =
  let t = Sys.opaque_identity true and f = Sys.opaque_identity false in
  print_bool (( ||| ) (noisy_w "w") (value "lhs" f) (value "rhs" t))
[%%expect {|
w
lhs
rhs
true
|}]

let () =
  let f = Sys.opaque_identity false in
  print_bool (( ||| ) (w ()) f f)
[%%expect {|
false
|}]

let[@inline never] test_primitive_eta () =
  let add_one = Sys.opaque_identity (add_mid 1 (w ())) in
  let equal' = Sys.opaque_identity equal in
  let and' = Sys.opaque_identity ( &&& ) in
  print_int (add_one 2);
  print_bool (equal' 1 (w ()) 1);
  print_bool (and' (value "lhs" false) (noisy_w "w") (value "rhs" true))
let () = test_primitive_eta ()
[%%expect {|
val test_primitive_eta : unit -> unit = <fun>
3
true
rhs
w
lhs
false
|}]

let () =
  match add_mid (value "a" 1) (note "w"; raise_exit (); w ())
          (value "b" 2) with
  | result -> print_int result
  | exception Exit -> note "Exit"
[%%expect {|
b
w
Exit
|}]

(* The coercion is eta-expanded at the signature's type. *)
module M : sig
  type t
  val of_array : float# array -> t
  val get : w -> t -> w -> int -> w -> float#
end = struct
  type t = float# array
  let of_array a = a
  external get :
    (w [@elide_void]) -> t -> (w [@elide_void]) -> int
    -> (w [@elide_void]) -> float#
    = "%array_safe_get"
end
[%%expect {|
module M :
  sig
    type t
    val of_array : float# array -> t
    val get : w -> t -> w -> int -> w -> float#
  end
|}]

