(* TEST
 reference = "${test_source_directory}/matching.reference";
 flambda2;
 include stdlib_stable;
 include stdlib_upstream_compatible;
 {
   bytecode;
 } {
   native;
 } {
   flags = "-Oclassic";
   native;
 } {
   flags = "-O3";
   native;
 }
*)

open Stdlib_stable
open Stdlib_upstream_compatible

let print_int_ln x = Printf.printf "%d\n" x
let print_i64_ln x = Printf.printf "%Ld\n" (Int64_u.to_int64 x)
let print_float_ln x = Printf.printf "%.1f\n" (Float_u.to_float x)

type pair = #{ a : int64_u; b : int64_u }

type t =
  { mutable m_int : int;
    i_int : int;
    mutable m_str : string;
    i_str : string;
    mutable m_float : float#;
    i_float : float#;
    mutable m_pair : pair;
    i_pair : pair;
    mutable m_opt : int option;
    i_opt : int option;
    mutable m_addr : int addr
  }

let make () =
  { m_int = 1;
    i_int = 5;
    m_str = "abcd";
    i_str = "efgh";
    m_float = #1.5;
    i_float = #2.5;
    m_pair = #{ a = #3L; b = #2L };
    i_pair = #{ a = #7L; b = #6L };
    m_opt = Some 4;
    i_opt = None;
    m_addr = Addr.of_idx (ref 7) (.contents)
  }

(* Reading values of various layouts *)

let get_int (addr_ x) = x
let get_str (addr_ x) = x
let get_float (addr_ (x : float#)) = x
let get_i64 (addr_ (x : int64_u)) = x
let get_pair (addr_ #{ a; b }) = #(a, b)
let get_b (addr_ #{ b; _ }) = b

let get_int_imm (addr_imm_ x) = x
let get_str_imm (addr_imm_ x) = x
let get_float_imm (addr_imm_ (x : float#)) = x
let get_pair_imm (addr_imm_ #{ a; b }) = #(a, b)

let () =
  print_endline "read mutable";
  let r = make () in
  print_int_ln (get_int (Addr.of_idx r (.m_int)));
  print_endline (get_str (Addr.of_idx r (.m_str)));
  print_float_ln (get_float (Addr.of_idx r (.m_float)));
  let #(a, b) = get_pair (Addr.of_idx r (.m_pair)) in
  print_i64_ln a;
  print_i64_ln b;
  print_i64_ln (get_b (Addr.of_idx r (.m_pair)));
  print_i64_ln (get_i64 (Addr.of_idx r (.m_pair.#a)));
  print_newline ()

let () =
  print_endline "read immutable";
  let r = make () in
  print_int_ln (get_int_imm (Addr_imm.of_idx r (.i_int)));
  print_endline (get_str_imm (Addr_imm.of_idx r (.i_str)));
  print_float_ln (get_float_imm (Addr_imm.of_idx r (.i_float)));
  let #(a, b) = get_pair_imm (Addr_imm.of_idx r (.i_pair)) in
  print_i64_ln a;
  print_i64_ln b;
  print_newline ()

(* Address patterns in different positions *)

type 'a addr_variant = Mut of 'a addr | Imm of 'a addr_imm

let variant = function Mut (addr_ x) | Imm (addr_imm_ x) -> x

let let_bound a =
  let addr_ x = a in
  let addr_imm_ y = Addr_imm.of_idx (make ()) (.i_str) in
  x ^ y

let nested (addr_ (addr_ x)) = x

let unboxed_tuple #(addr_ x, addr_imm_ y) = x + y

let () =
  print_endline "positions";
  let r = make () in
  print_endline (variant (Mut (Addr.of_idx r (.m_str))));
  print_endline (variant (Imm (Addr_imm.of_idx r (.i_str))));
  print_endline (let_bound (Addr.of_idx r (.m_str)));
  print_int_ln (nested (Addr.of_idx r (.m_addr)));
  print_int_ln
    (unboxed_tuple #(Addr.of_idx r (.m_int), Addr_imm.of_idx r (.i_int)));
  print_endline (get_str (Addr.of_imm (Addr_imm.of_idx_read r (.i_str))));
  print_newline ()

(* Matching on the value at an address *)

let describe = function
  | addr_ (Some 0) -> "zero"
  | addr_ (Some x) -> string_of_int x
  | addr_ None -> "none"

let describe_imm = function
  | addr_imm_ (Some 0) -> "zero"
  | addr_imm_ (Some x) -> string_of_int x
  | addr_imm_ None -> "none"

(* Wildcards in between address patterns *)
let with_wildcard a b =
  match #(a, b) with
  | #(addr_ (Some x), true) -> string_of_int x
  | #(_, false) -> "false"
  | #(addr_ None, _) -> "none"

let with_wildcard_imm a b =
  match #(a, b) with
  | #(addr_imm_ (Some x), true) -> string_of_int x
  | #(_, false) -> "false"
  | #(addr_imm_ None, _) -> "none"

let () =
  print_endline "matching";
  let r = make () in
  let a = Addr.of_idx r (.m_opt) in
  print_endline (describe a);
  Addr.set a (Some 0);
  print_endline (describe a);
  Addr.set a None;
  print_endline (describe a);
  print_endline (with_wildcard a true);
  print_endline (with_wildcard a false);
  Addr.set a (Some 3);
  print_endline (with_wildcard a true);
  print_endline (with_wildcard a false);
  let a = Addr_imm.of_idx r (.i_opt) in
  print_endline (describe_imm a);
  print_endline (with_wildcard_imm a true);
  print_endline (with_wildcard_imm a false);
  let a = Addr_imm.of_idx { r with i_opt = Some 0 } (.i_opt) in
  print_endline (describe_imm a);
  print_endline (with_wildcard_imm a true);
  print_newline ()

(* Mutation *)

let read_twice a =
  let addr_ x = a in
  Addr.set a (x + 1);
  let addr_ y = a in
  x, y

let read_then_write a =
  let addr_ x = a in
  Addr.set a 0;
  x

let read_in_loop a =
  let acc = ref 0 in
  for _ = 1 to 3 do
    match a with
    | addr_ x ->
      acc := !acc + x;
      Addr.set a (x * 2)
  done;
  !acc

let () =
  print_endline "mutation";
  let r = make () in
  let a = Addr.of_idx r (.m_int) in
  let x, y = read_twice a in
  print_int_ln x;
  print_int_ln y;
  print_int_ln r.m_int;
  print_int_ln (read_in_loop a);
  print_int_ln r.m_int;
  let x = get_int a in
  r.m_int <- 100;
  let y = get_int a in
  print_int_ln x;
  print_int_ln y;
  Addr.set a 5;
  print_int_ln (read_then_write a);
  print_int_ln r.m_int;
  print_newline ()

(* Local addresses *)

let () =
  print_endline "local";
  let r =
    stack_
      { m_int = 1;
        i_int = 5;
        m_str = "abcd";
        i_str = "efgh";
        m_float = #1.5;
        i_float = #2.5;
        m_pair = #{ a = #3L; b = #2L };
        i_pair = #{ a = #7L; b = #6L };
        m_opt = Some 4;
        i_opt = None;
        m_addr = Addr.of_idx (ref 8) (.contents)
      }
  in
  let addr_ x = Addr.of_idx_local r (.m_int) in
  print_int_ln x;
  let addr_ #{ a; _ } = Addr.of_idx_local r (.m_pair) in
  print_i64_ln a;
  (match Addr.of_idx_local r (.m_opt) with
   | addr_ (Some x) -> print_int_ln x
   | addr_ None -> print_endline "none");
  let addr_imm_ s = Addr_imm.of_idx_local r (.i_str) in
  print_endline s;
  print_newline ()

(* Dereference patterns *)

type v = { mutable vx : int; vy : string }

let () =
  print_endline "deref records";
  let r = make () in
  (match r with
   | { m_int = deref_ a; i_str = deref_ b; _ } ->
     Addr.set a 10;
     print_int_ln r.m_int;
     print_endline (Addr_imm.get b));
  (match r with
   | { m_float = deref_ a; m_pair = #{ b = deref_ pb; _ }; _ } ->
     Addr.set a #4.5;
     Addr.set pb #9L;
     print_float_ln r.m_float;
     print_i64_ln r.m_pair.#b;
     print_i64_ln r.m_pair.#a);
  let v = { vx = 1; vy = "vy" } in
  (match v with
   | { vx = deref_ a; vy = deref_ b } ->
     Addr.set a 2;
     print_int_ln v.vx;
     print_endline (Addr_imm.get b));
  print_newline ()

type ub = { ub : int } [@@unboxed]
type s1 = #{ s : int }
type ut = { ut : #(int * string); mutable mut_ut : #(string * int) }
type wrap = { mutable wub : ub; mutable ws1 : s1 }

let () =
  print_endline "deref unboxed";
  let w = { wub = { ub = 1 }; ws1 = #{ s = 2 } } in
  (match w with
   | { wub = { ub = deref_ a }; ws1 = #{ s = deref_ b } } ->
     Addr.set a 3;
     Addr.set b 4;
     print_int_ln w.wub.ub;
     print_int_ln w.ws1.#s);
  let u = { ut = #(5, "five"); mut_ut = #("six", 6) } in
  (match u with
   | { ut = #(deref_ a, deref_ b); mut_ut = #(_, deref_ c) } ->
     print_int_ln (Addr_imm.get a);
     print_endline (Addr_imm.get b);
     Addr.set c 7;
     let #(_, c) = u.mut_ut in
     print_int_ln c);
  print_newline ()

let () =
  print_endline "deref tuples";
  let t = (1, "one") in
  (match t with
   | (deref_ a, deref_ b) ->
     print_int_ln (Addr_imm.get a);
     print_endline (Addr_imm.get b));
  let nested = ((2, "two"), 3) in
  (match nested with
   | ((_, deref_ a), deref_ b) ->
     print_endline (Addr_imm.get a);
     print_int_ln (Addr_imm.get b));
  print_newline ()

(* Syntactic tuples are not always allocated *)

let tupled (deref_ a, b) = Addr_imm.get a ^ b

let multiple x y =
  match x, y with
  | deref_ a, 0 -> Addr_imm.get a
  | (deref_ a, deref_ b) as t ->
    let (_, y') = t in
    Addr_imm.get a ^ string_of_int (Addr_imm.get b + y')

let let_tuple x y =
  let (deref_ a, deref_ b) = (x, y) in
  Addr_imm.get a ^ Addr_imm.get b

let let_tuple_branch c x y =
  let (deref_ a, b) = if c then (x, y) else (y, x) in
  Addr_imm.get a ^ b

let () =
  print_endline "deref syntactic tuples";
  print_endline (tupled ("ab", "cd"));
  print_endline (multiple "x" 0);
  print_endline (multiple "x" 1);
  print_endline (let_tuple "y" "z");
  print_endline (let_tuple_branch true "p" "q");
  print_endline (let_tuple_branch false "p" "q");
  print_newline ()

(* Dereference patterns below address patterns *)

let () =
  print_endline "deref below addr";
  let r = make () in
  (match Addr.of_idx r (.m_pair) with
   | addr_ #{ a = deref_ pa; b } ->
     Addr.set pa #11L;
     print_i64_ln r.m_pair.#a;
     print_i64_ln b);
  (match Addr.of_idx r (.m_int) with
   | addr_ (deref_ a) ->
     Addr.set a 12;
     print_int_ln r.m_int);
  let u = { ut = #(13, "thirteen"); mut_ut = #("fourteen", 14) } in
  (match Addr_imm.of_idx u (.ut) with
   | addr_imm_ #(_, deref_ b) -> print_endline (Addr_imm.get b));
  print_newline ()

(* Choosing between positions *)

type two = { mutable p : int; mutable q : int; sel : bool }

let pick = function
  | { sel = true; p = deref_ a; _ } | { sel = false; q = deref_ a; _ } -> a

let guarded r =
  match r with
  | { vx = deref_ a; _ } when (Addr.set a 42; false) -> 0
  | _ -> r.vx

let () =
  print_endline "deref choice";
  let t = { p = 1; q = 2; sel = true } in
  Addr.set (pick t) 3;
  Addr.set (pick { t with sel = false }) 4;
  let t' = { t with sel = false } in
  Addr.set (pick t') 5;
  print_int_ln t.p;
  print_int_ln t.q;
  print_int_ln t'.q;
  print_int_ln (guarded { vx = 0; vy = "" });
  print_newline ()

(* Local containers *)

let () =
  print_endline "deref local";
  let r = stack_ { vx = 1; vy = "local" } in
  (match r with
   | { vx = deref_ a; _ } -> Addr.set a 2);
  print_int_ln r.vx;
  let x = stack_ (3, "three") in
  (match x with
   | (deref_ a, _) -> print_int_ln (Addr_imm.get a));
  print_newline ()

(* More positions *)

type inner = #{ ia : int; ib : float# }
type outer = #{ oa : string; ob : inner }
type deep = { mutable mo : outer; mutable mub : ub }

let choose b x y =
  match b, x, y with
  | true, deref_ a, _ | false, _, deref_ a -> Addr_imm.get a

let nested_in_syntactic x y =
  match (x, 1), y with
  | ((deref_ a, _), _) -> Addr_imm.get a

let () =
  print_endline "deref more positions";
  print_endline (choose true "x" "y");
  print_endline (choose false "x" "y");
  let v = { vx = 2; vy = "" } in
  print_int_ln v.vx;
  let { vx = deref_ a; _ } = v in
  Addr.set a 7;
  print_int_ln v.vx;
  (fun { vx = deref_ a; _ } -> Addr.set a 8) v;
  print_int_ln v.vx;
  print_endline (nested_in_syntactic "nested" 0);
  let d =
    { mo = #{ oa = "oa"; ob = #{ ia = 1; ib = #1.5 } }; mub = { ub = 1 } }
  in
  (match Addr.of_idx d (.mo) with
   | addr_ #{ ob = #{ ib = deref_ x; _ }; _ } -> Addr.set x #2.5);
  print_float_ln d.mo.#ob.#ib;
  print_int_ln d.mo.#ob.#ia;
  (match Addr.of_idx d (.mub) with
   | addr_ { ub = deref_ a } -> Addr.set a 9);
  print_int_ln d.mub.ub;
  print_newline ()
