(* TEST
   flags = "-O3 -I ${ocamlsrcdir}/utils";
   only-default-codegen;
   expect.opt;
*)

(* This tests the compiler's ability to infer (a sound over-approximation of)
   the set of callees that may be called at each indirect CFG call. *)

open Cmm

module Callees : sig
  val take : unit -> callee list option list
end = struct
  let calls = ref []

  let () =
    Compiler_hooks.register Cfg_combine (fun cfg_with_layout ->
      let cfg = Cfg_with_layout.cfg cfg_with_layout in
      if not (String.ends_with ~suffix:"__entry" cfg.fun_name) then
        Cfg_with_layout.iter_blocks cfg_with_layout ~f:(fun block ->
          match block.terminator.desc with
          | Call { op = Indirect callees; _ }
          | Tailcall_func (Indirect callees) -> calls := callees :: !calls
          | _ -> ()))

  let take () =
    let result = List.rev !calls in
    calls := [];
    result
end
[%%expect{|
module Callees : sig val take : unit -> Cmm.callee list option list end
|}]

let[@inline never] first_argument f x = f x;;
Callees.take ();;
[%%expect{|
val first_argument : ('a -> 'b) -> 'a -> 'b = <fun>
- : Cmm.callee list option list = [Some [Argument 0]]
|}]

let[@inline never] second_argument x f = f x;;
Callees.take ();;
[%%expect{|
val second_argument : 'a -> ('a -> 'b) -> 'b = <fun>
- : Cmm.callee list option list = [Some [Argument 1]]
|}]

let[@inline never] non_tail_argument f x = f x + 1;;
Callees.take ();;
[%%expect{|
val non_tail_argument : ('a -> int) -> 'a -> int = <fun>
- : Cmm.callee list option list = [Some [Argument 0]]
|}]

let[@inline never] labelled_argument ~x ~f = f x;;
Callees.take ();;
[%%expect{|
val labelled_argument : x:'a -> f:('a -> 'b) -> 'b = <fun>
- : Cmm.callee list option list = [Some [Argument 1]]
|}]

let[@inline never] aliased_argument f x =
  let alias = f in
  alias x;;
Callees.take ();;
[%%expect{|
val aliased_argument : ('a -> 'b) -> 'a -> 'b = <fun>
- : Cmm.callee list option list = [Some [Argument 0]]
|}]

let[@inline never] shadowed_argument f g x =
  let first = f x in
  let f = g in
  f first;;
Callees.take ();;
[%%expect{|
val shadowed_argument : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = <fun>
- : Cmm.callee list option list = [Some [Argument 0]; Some [Argument 1]]
|}]

let[@inline never] selected_argument f g b x =
  (if b then f else g) x;;
Callees.take ();;
[%%expect{|
val selected_argument : ('a -> 'b) -> ('a -> 'b) -> bool -> 'a -> 'b = <fun>
- : Cmm.callee list option list = [None]
|}]

let[@inline never] callback_result make x =
  let f = make x in
  (Sys.opaque_identity f) x;;
Callees.take ();;
[%%expect{|
val callback_result : ('a -> 'a -> 'b) -> 'a -> 'b = <fun>
- : Cmm.callee list option list = [Some [Argument 0]; None]
|}]

let[@inline never] callback_in_ref cell x = !cell x;;
Callees.take ();;
[%%expect{|
val callback_in_ref : ('a -> 'b) ref -> 'a -> 'b = <fun>
- : Cmm.callee list option list = [None]
|}]

let[@inline never] captured_callback f =
  let[@inline never] inner x = f x in
  Sys.opaque_identity inner;;
Callees.take ();;
[%%expect{|
val captured_callback : ('a -> 'b) -> 'a -> 'b = <fun>
- : Cmm.callee list option list = [None]
|}]

let[@inline never][@loop never] rec recursive_callback f n =
  if n <= 0 then f n else recursive_callback f (n - 1);;
Callees.take ();;
[%%expect{|
val recursive_callback : (int -> 'a) -> int -> 'a = <fun>
- : Cmm.callee list option list = [Some [Argument 0]]
|}]

let[@inline never] selected_symbol b x =
  let[@inline never] left y = y + 1 in
  let[@inline never] right y = y - 1 in
  (if b then left else right) x;;
Callees.take ();;
[%%expect{|
val selected_symbol : bool -> int -> int = <fun>
- : Cmm.callee list option list =
[Some
  [Symbol {sym_name = "camlTOP25__right_17_35_code"; sym_global = Local};
   Symbol {sym_name = "camlTOP25__left_16_34_code"; sym_global = Local}]]
|}]

let[@inline never] inlined_argument x f =
  let[@inline always] apply callback value = callback value in
  apply f x;;
Callees.take ();;
[%%expect{|
val inlined_argument : 'a -> ('a -> 'b) -> 'b = <fun>
- : Cmm.callee list option list = [Some [Argument 1]]
|}]

let[@inline never] inlined_non_argument cell x =
  let[@inline always] apply callback value = callback value in
  apply !cell x;;
Callees.take ();;
[%%expect{|
val inlined_non_argument : ('a -> 'b) ref -> 'a -> 'b = <fun>
- : Cmm.callee list option list = [None]
|}]

let[@inline never] tupled_argument (x, f) = f x;;
Callees.take ();;
[%%expect{|
val tupled_argument : 'a * ('a -> 'b) -> 'b = <fun>
- : Cmm.callee list option list = [Some [Argument 1]]
|}]
