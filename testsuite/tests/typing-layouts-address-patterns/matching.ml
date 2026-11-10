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
