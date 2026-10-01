(* TEST
   expect.opt;
*)

(* Define a clearly allocating function, to be used later: *)
let state = ref (ref 0)
let allocate () = state := ref 42
[%%expect{|
val state : int ref ref = {contents = {contents = 0}}
val allocate : unit -> unit = <fun>
|}]

(* Immediates do not allocate: *)
let za_unit = zero_alloc_ ()
let za_int = zero_alloc_ 42
[%%expect{|
val za_unit : unit = ()
val za_int : int = 42
|}]

(* Unboxed primitives had better not allocate: *)
let za_u_unit = zero_alloc_ #()
let za_u_int = zero_alloc_ #42L
[%%expect{|
val za_u_unit : unit# = <abstr>
val za_u_int : int64_u = <abstr>
|}]

(* Unboxed tuples do not allocate, but normal (boxed?) tuples do: *)
let za_u_tuple = zero_alloc_ #(Sys.opaque_identity 42, 42)
[%%expect{|
val za_u_tuple : #(int * int) = <abstr>
|}]
let za_tuple = zero_alloc_ (Sys.opaque_identity 42, 42)
[%%expect{|
(* CR wsturgeon for wsturgeon: this needs to fail *)
|}]

(* Unboxed records do not allocate, but normal (boxed?) records do: *)
type r = { a : int; b : int }
let za_u_record = zero_alloc_ Sys.opaque_identity #{ a = 42; b = 42 }
[%%expect{|
type r = { a : int; b : int; }
val za_u_record : r# = <abstr>
|}]
let za_record = zero_alloc_ Sys.opaque_identity { a = 42; b = 42 }
[%%expect{|
(* CR wsturgeon for wsturgeon: this needs to fail *)
|}]

(* Calling an allocating function will clearly allocate: *)
let direct_call = zero_alloc_ allocate ()
[%%expect{|
(* CR wsturgeon for wsturgeon: this needs to fail *)
|}]

(* Returning (but not calling) an allocating function is (maybe deceptively!) fine: *)
let without_invoking = zero_alloc_ allocate
[%%expect{|
val without_invoking : unit -> unit = <fun>
|}]

(* Statically allocated closures are (maybe deceptively!) fine. *)
let in_pure_closure = zero_alloc_ fun () -> allocate ()
[%%expect{|
val in_pure_closure : unit -> unit = <fun>
|}]

(* Closures that capture state will allocate themselves on the heap: *)
let in_stateful_closure x = zero_alloc_ fun () -> (print_string x; allocate ())
[%%expect{|
(* CR wsturgeon for wsturgeon: this needs to fail *)
|}]

(* ...unless you allocate those stateful closures on the stack: *)
let in_stateful_stack_closure x = zero_alloc_ exclave_ fun () -> (print_string x; allocate ())
[%%expect{|
val in_stateful_stack_closure : string -> (unit -> unit) @ local = <fun>
|}]

(* `zero_alloc_` ignores paths that never return (e.g. raise or diverge): *)
exception After_allocating
let[@inline never] sometimes_allocate_then_raise b =
  if b then begin
    allocate ();
    raise After_allocating
  end
[%%expect{|
exception After_allocating
val sometimes_allocate_then_raise : bool -> unit = <fun>
|}]

let allocates_iff_raising b = zero_alloc_ sometimes_allocate_then_raise b
[%%expect{|
val allocates_iff_raising : bool -> unit = <fun>
|}]

(* If paths that raise exceptions are caught, they're checked once again: *)
let catch_after_allocating b =
  zero_alloc_ try sometimes_allocate_then_raise b with After_allocating -> ()
[%%expect{|
(* CR wsturgeon for wsturgeon: this needs to fail *)
|}]

(* However, we can't know which paths raise which exceptions, so this is conservative: *)
let catch_something_unrelated b =
  zero_alloc_ try sometimes_allocate_then_raise b with Not_found -> ()
[%%expect{|
(* CR wsturgeon for wsturgeon: this needs to fail *)
|}]

(* Note that the placement of `zero_alloc_` matters: *)
let inside_try b =
  try zero_alloc_ sometimes_allocate_then_raise b with After_allocating -> ()
[%%expect{|
val inside_try : bool -> unit = <fun>
|}]
