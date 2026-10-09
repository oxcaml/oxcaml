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
CR wsturgeon: this has to pass
|}]

let[@zero_alloc conditional] catch_nonstrict f =
  try f () with After_allocating -> ()
[%%expect{|
CR wsturgeon: genuinely not sure here
|}]
let[@zero_alloc] alloc_nonstrict () = catch_nonstrict allocate_then_raise
[%%expect{|
CR wsturgeon: this has to fail, assuming the above passed, else delete
|}]

let[@zero_alloc conditional strict] catch_strict f =
  try f () with After_allocating -> ()
[%%expect{|
CR wsturgeon: this has to pass
|}]
let[@zero_alloc] alloc_strict () = catch_strict allocate_then_raise
[%%expect{|
CR wsturgeon: this has to fail
|}]

(* Try to use inlining (via `apply`) to break soundness: *)
let[@inline always] [@zero_alloc conditional] apply f x = f x
[%%expect{|
val apply : ('a -> 'b) -> 'a -> 'b [@@zero_alloc conditional] = <fun>
|}]

let[@zero_alloc conditional] inlining_cant_expose_assumptions () =
  (* Note that `apply` assumes its `f` argument is non-allocating, but
     we have to be careful about scoping that, since in this invocation,
     `f` is `allocate`, and `apply` is `inline always`, so its inlined body is
     `(allocate [@zero_alloc assume]) ()`, which would be accepted(!). *)
  apply allocate ()
[%%expect{|
CR wsturgeon: this has to fail
|}]

let[@inline always] [@zero_alloc conditional] apply2 f x y = f x y
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
