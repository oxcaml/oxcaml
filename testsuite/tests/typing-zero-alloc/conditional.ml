(* TEST
   flags = "-g";
   expect.opt;
*)

(* `List.iter` isn't `zero_alloc`, because `f` could allocate: *)
let[@zero_alloc] iter ~f l = List.iter f l
[%%expect{|
Line 1, characters 5-15:
1 | let[@zero_alloc] iter ~f l = List.iter f l
         ^^^^^^^^^^
Error: Annotation check for zero_alloc failed on function TOP1.iter (camlTOP1__iter_0_1_code).
File "list.ml", line 119, characters 12-15:
Error: called function may allocate (indirect call) (:1,29--42)
|}]

(* However, `List.iter` doesn't *itself* allocate, so it's
   *conditionally* `zero_alloc`: *)
let[@zero_alloc conditional] iter ~f l = List.iter f l
[%%expect{|
val iter : f:('a -> unit) -> 'a list -> unit [@@zero_alloc conditional] = <fun>
|}]

let[@zero_alloc conditional] rec iter ~f : 'a list -> unit = function
  | [] -> ()
  | head :: tail -> (f head; iter ~f tail)
[%%expect{|
val iter : f:('a -> unit) -> 'a list -> unit [@@zero_alloc conditional] = <fun>
|}]

let[@zero_alloc conditional] rec iter_even ~f : 'a list -> unit = function
  | [] -> ()
  | head :: tail -> (f head; iter_odd ~f tail)
and[@zero_alloc conditional] iter_odd ~f : 'a list -> unit = function
  | [] -> ()
  | head :: tail -> (f head; iter_even ~f tail)
[%%expect{|
val iter_even : f:('a -> unit) -> 'a list -> unit [@@zero_alloc conditional] = <fun>
val iter_odd : f:('a -> unit) -> 'a list -> unit [@@zero_alloc conditional] = <fun>
|}]

(* A trivially allocating function, to be used later: *)
let state = ref (ref 42)
let[@inline never] allocate () = state := ref 42
[%%expect{|
val state : int ref ref = {contents = {contents = 42}}
val allocate : unit -> unit = <fun>
|}]

let[@zero_alloc] iter_no_op l = iter ~f:ignore l
[%%expect{|
val iter_no_op : 'a list -> unit [@@zero_alloc] = <fun>
|}]

let[@zero_alloc] iter_alloc l = iter ~f:allocate l
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc] iter_even_alloc l = iter_even ~f:allocate l
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc] iter_odd_alloc l = iter_odd ~f:allocate l
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc conditional] allocate_before_callback f =
  allocate ();
  f ()
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc conditional] allocate_after_callback f =
  f ();
  allocate ()
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc conditional] shadow_allocate f =
  f ();
  let f () = allocate () in
  f ()
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc conditional] rec recursion_must_be_checked f b =
  if b then f () else
  recursion_must_be_checked allocate true
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc conditional] either f g x b =
  if b then f x else g x
let[@zero_alloc] cleverly_do_nothing b = either ignore ignore () b
[%%expect{|
CR wsturgeon: this has to pass
|}]
let[@zero_alloc] allocate_rhs b = either ignore allocate () b
[%%expect{|
CR wsturgeon: this has to fail
|}]
let[@zero_alloc] allocate_lhs b = either allocate ignore () b
[%%expect{|
CR wsturgeon: this has to fail
|}]
let[@zero_alloc] allocate_both b = either allocate allocate () b
[%%expect{|
CR wsturgeon: this has to fail
|}]

(* Exceptions may or may not be assumed: *)
exception After_allocating
let[@zero_alloc] allocate_then_raise () = (allocate (); raise After_allocating)
[%%expect{|
exception After_allocating
val allocate_then_raise : unit -> 'a [@@zero_alloc] = <fun>
|}]

(* Arguments are non-`strict` by default: *)
let[@zero_alloc conditional] catch_nonstrict f = try f () with _ -> ()
[%%expect{|
CR wsturgeon: this has to fail
|}]

(* Note that asking for `conditional strict` does not assume that *arguments*
   are `strict`: *)
let[@zero_alloc conditional strict] catch_strict f = try f () with _ -> ()
[%%expect{|
CR wsturgeon: this has to fail
|}]

(* Arguments are assumed non-strict by default, but they can be made strict: *)
let catch_nonstrict_with_strict_assumption
    (f [@zero_alloc conditional assume strict]) =
  try f () with _ -> ()
[@@zero_alloc conditional]
[%%expect{|
CR wsturgeon: this has to pass
|}]
let[@zero_alloc] catch_ignore () = catch_nonstrict_with_strict_assumption ignore
[%%expect{|
CR wsturgeon: this has to pass
|}]
let[@zero_alloc] catch_after_allocating () =
  catch_nonstrict_with_strict_assumption allocate_then_raise
[%%expect{|
CR wsturgeon: this has to fail
|}]

(* On a function instead of an argument, `conditional assume` does what it says:
   "at call sites, if all arguments won't allocate, this won't allocate." *)
let[@inline never][@zero_alloc conditional assume] secret () = allocate ()
let[@zero_alloc] danger () = secret ()
[%%expect{|
CR wsturgeon: this has to pass
|}]

(* Deceptively trivial-looking functions (e.g. if `'a` is a function): *)
let[@zero_alloc conditional] apply f x = f x
let[@zero_alloc conditional] id x = x
[%%expect{|
val apply : ('a -> 'b) -> 'a -> 'b [@@zero_alloc conditional] = <fun>
val id : 'a -> 'a [@@zero_alloc conditional] = <fun>
|}]

(* Try to use inlining (via `apply`) to break soundness: *)
module Inlining = struct
  let[@inline always] [@zero_alloc conditional] apply f x = f x

  let[@zero_alloc conditional] inlining_cant_expose_assumptions () =
    (* Note that `apply` assumes its `f` argument is non-allocating, but
      we have to be careful about scoping that, since in this invocation,
      `f` is `allocate`, and `apply` is `inline always`, so its inlined body is
      `(allocate [@zero_alloc assume]) ()`, which would be accepted(!). *)
    apply allocate ()
end
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@zero_alloc conditional] apply2 f x y = f x y
[%%expect{|
val apply2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c [@@zero_alloc conditional] = <fun>
|}]

let[@inline never] [@zero_alloc strict] allocator () = allocate
[%%expect{|
CR wsturgeon: this has to pass
|}]

let[@zero_alloc] overapply () = apply2 allocator () ()
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@inline never] [@zero_alloc strict] add_ints x y = x + y
[%%expect{|
CR wsturgeon: this has to pass
|}]

let[@zero_alloc] apply_two_argument_callback x y = apply2 add_ints x y
[%%expect{|
CR wsturgeon: this has to pass
|}]

let[@inline never] [@zero_alloc strict] identity x = x
[%%expect{|
CR wsturgeon: this has to pass
|}]

let[@zero_alloc] allocating_callback_argument x = apply identity (ref x)
[%%expect{|
CR wsturgeon: this has to fail
|}]

(* Without inlining, arguments themselves can require conditional logic: *)
let[@inline never][@zero_alloc conditional] forward2 f x y = f x y
let[@zero_alloc] higher_order () = forward2 apply ignore ()
let[@zero_alloc] higher_order () = forward2 apply allocate ()
[%%expect{|
val forward2 : ('a -> 'b -> 'c) -> 'a -> 'b -> 'c [@@zero_alloc conditional] = <fun>
val higher_order : unit -> unit [@@zero_alloc] = <fun>
CR wsturgeon: the latter has to fail
|}]

let[@inline never][@zero_alloc conditional strict] forward
    (f [@zero_alloc conditional assume strict]) x =
  f x
let[@zero_alloc conditional strict] rec insane () = forward insane ()
[%%expect{|
CR wsturgeon: this has to pass
|}]

let[@zero_alloc conditional strict] rec roundabout b =
  if b then allocate () else
  forward roundabout true
[%%expect{|
CR wsturgeon: this has to fail
|}]

(* Callback arguments are themselves conditional: *)
let[@zero_alloc conditional] arguments_are_conditional
    (f : (unit -> unit) -> unit) g =
  f g
[%%expect{|
CR wsturgeon: this has to pass
|}]
let[@zero_alloc] no_op () =
  arguments_are_conditional (fun f -> f ()) ignore
[%%expect{|
CR wsturgeon: this has to pass
|}]
let[@zero_alloc] alloc () =
  arguments_are_conditional (fun f -> f ()) allocate
[%%expect{|
CR wsturgeon: this has to fail
|}]
let[@zero_alloc] alloc () =
  arguments_are_conditional (fun f -> (allocate (); f ())) ignore
[%%expect{|
CR wsturgeon: this has to fail
|}]

(* Unannotated stdlib functions need to know their conditions: *)
let[@zero_alloc] ignore_ints (ints : int list) =
  (List.iter [@inlined never]) ignore ints
[%%expect{|
val ignore_ints : int list -> unit [@@zero_alloc] = <fun>
|}]
