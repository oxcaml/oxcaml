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

let print_pair_ln #{ a; b } =
  Printf.printf "%Ld %Ld\n" (Int64_u.to_int64 a) (Int64_u.to_int64 b)

(* Positions: record fields *)

(* Unlike [t], [v] is not a mixed record *)
type v = { mutable vx : int; vy : string }

let () =
  print_endline "deref record fields";
  let v = { vx = 1; vy = "vy" } in
  (match v with
   | { vx = deref_ vx; vy = deref_ vy } ->
     print_int_ln (Addr.get vx);
     print_endline (Addr_imm.get vy);
     Addr.set vx 2);
  print_int_ln v.vx;
  print_endline v.vy;
  let r = make () in
  (match r with
   | { m_int = deref_ m_int; i_str = deref_ i_str; m_float = deref_ m_float;
       i_pair = deref_ i_pair; m_addr = deref_ m_addr; _ } ->
     print_int_ln (Addr.get m_int);
     print_endline (Addr_imm.get i_str);
     print_float_ln (Addr.get m_float);
     print_pair_ln (Addr_imm.get i_pair);
     print_int_ln (Addr.get (Addr.get m_addr));
     Addr.set m_int 10;
     Addr.set m_float #10.5;
     Addr.set m_addr (Addr.of_idx (ref 70) (.contents)));
  print_int_ln r.m_int;
  print_endline r.i_str;
  print_float_ln r.m_float;
  print_pair_ln r.i_pair;
  print_int_ln (Addr.get r.m_addr);
  print_newline ()

(* Positions: below unboxed records and tuples in record fields *)

type ub = { ub : int } [@@unboxed]
type s1 = #{ s : int }
type inner = #{ ia : int; ib : float# }
type outer = #{ oa : string; ob : inner }

(* Unlike [u], [w] is not a mixed record *)
type w = { mutable w_ub : ub; mutable w_s1 : s1 }

type u =
  { mutable m_ub : ub;
    i_ub : ub;
    mutable m_s1 : s1;
    mutable m_ut : #(int * string);
    i_ut : #(int * string);
    mutable m_outer : outer;
    i_outer : outer
  }

let make_u () =
  { m_ub = { ub = 3 };
    i_ub = { ub = 4 };
    m_s1 = #{ s = 5 };
    m_ut = #(6, "six");
    i_ut = #(7, "seven");
    m_outer = #{ oa = "eight"; ob = #{ ia = 9; ib = #10.5 } };
    i_outer = #{ oa = "eleven"; ob = #{ ia = 12; ib = #13.5 } }
  }

let print_ut_ln #(i, s) = Printf.printf "%d %s\n" i s

let print_outer_ln #{ oa; ob = #{ ia; ib } } =
  Printf.printf "%s %d %.1f\n" oa ia (Float_u.to_float ib)

let () =
  print_endline "deref unboxed";
  let w = { w_ub = { ub = 1 }; w_s1 = #{ s = 2 } } in
  (match w with
   | { w_ub = { ub = deref_ ub }; w_s1 = #{ s = deref_ s } } ->
     print_int_ln (Addr.get ub);
     print_int_ln (Addr.get s);
     Addr.set ub 10;
     Addr.set s 20);
  print_int_ln w.w_ub.ub;
  print_int_ln w.w_s1.#s;
  let u = make_u () in
  (match u with
   | { m_ub = { ub = deref_ m_ub }; i_ub = { ub = deref_ i_ub };
       m_s1 = #{ s = deref_ m_s1 };
       m_ut = #(deref_ m_ut_i, deref_ m_ut_s);
       i_ut = #(deref_ i_ut_i, deref_ i_ut_s);
       m_outer =
         #{ oa = deref_ m_oa; ob = #{ ia = deref_ m_ia; ib = deref_ m_ib } };
       i_outer =
         #{ oa = deref_ i_oa; ob = #{ ia = deref_ i_ia; ib = deref_ i_ib } }
     } ->
     print_int_ln (Addr.get m_ub);
     print_int_ln (Addr_imm.get i_ub);
     print_int_ln (Addr.get m_s1);
     print_ut_ln #(Addr.get m_ut_i, Addr.get m_ut_s);
     print_ut_ln #(Addr_imm.get i_ut_i, Addr_imm.get i_ut_s);
     print_outer_ln
       #{ oa = Addr.get m_oa;
          ob = #{ ia = Addr.get m_ia; ib = Addr.get m_ib } };
     print_outer_ln
       #{ oa = Addr_imm.get i_oa;
          ob = #{ ia = Addr_imm.get i_ia; ib = Addr_imm.get i_ib } };
     Addr.set m_ub 30;
     Addr.set m_s1 50;
     Addr.set m_ut_i 60;
     Addr.set m_ut_s "SIX";
     Addr.set m_oa "EIGHT";
     Addr.set m_ia 90;
     Addr.set m_ib #100.5);
  print_int_ln u.m_ub.ub;
  print_int_ln u.i_ub.ub;
  print_int_ln u.m_s1.#s;
  print_ut_ln u.m_ut;
  print_ut_ln u.i_ut;
  print_outer_ln u.m_outer;
  print_outer_ln u.i_outer;
  print_newline ()

(* Positions: tuple elements *)

let () =
  print_endline "deref tuples";
  let t = (1, "one") in
  (match t with
   | (deref_ a, deref_ b) ->
     print_int_ln (Addr_imm.get a);
     print_endline (Addr_imm.get b));
  let nested = ((2, "two"), 3) in
  (match nested with
   | ((deref_ a, deref_ b), deref_ c) ->
     print_int_ln (Addr_imm.get a);
     print_endline (Addr_imm.get b);
     print_int_ln (Addr_imm.get c));
  let local = stack_ (4, 5) in
  (match local with
   | (deref_ a, deref_ b) ->
     print_int_ln (Addr_imm.get a);
     print_int_ln (Addr_imm.get b));
  print_newline ()

(* Positions: below address patterns *)

let () =
  print_endline "deref below addr";
  let r = make () in
  (match Addr.of_idx r (.m_int) with
   | addr_ (deref_ a) ->
     print_int_ln (Addr.get a);
     Addr.set a 10);
  (match Addr_imm.of_idx r (.i_int) with
   | addr_imm_ (deref_ a) -> print_int_ln (Addr_imm.get a));
  (match Addr.of_idx r (.m_pair) with
   | addr_ #{ a = deref_ a; b = deref_ b } ->
     print_pair_ln #{ a = Addr.get a; b = Addr.get b };
     Addr.set a #30L;
     Addr.set b #20L);
  (match Addr_imm.of_idx r (.i_pair) with
   | addr_imm_ #{ a = deref_ a; b = deref_ b } ->
     print_pair_ln #{ a = Addr_imm.get a; b = Addr_imm.get b });
  (match Addr.of_idx r (.m_addr) with
   | addr_ (addr_ (deref_ a)) ->
     print_int_ln (Addr.get a);
     Addr.set a 70);
  print_int_ln r.m_int;
  print_int_ln r.i_int;
  print_pair_ln r.m_pair;
  print_pair_ln r.i_pair;
  print_int_ln (Addr.get r.m_addr);
  let u = make_u () in
  (match Addr.of_idx u (.m_ub) with
   | addr_ { ub = deref_ a } ->
     print_int_ln (Addr.get a);
     Addr.set a 30);
  (match Addr.of_idx u (.m_s1) with
   | addr_ #{ s = deref_ a } ->
     print_int_ln (Addr.get a);
     Addr.set a 50);
  (match Addr.of_idx u (.m_ut) with
   | addr_ #(deref_ a, deref_ b) ->
     print_ut_ln #(Addr.get a, Addr.get b);
     Addr.set a 60;
     Addr.set b "SIX");
  (match Addr.of_idx u (.m_outer) with
   | addr_ #{ oa = deref_ oa; ob = #{ ia = deref_ ia; ib = deref_ ib } } ->
     print_outer_ln
       #{ oa = Addr.get oa; ob = #{ ia = Addr.get ia; ib = Addr.get ib } };
     Addr.set oa "EIGHT";
     Addr.set ia 90;
     Addr.set ib #100.5);
  (match Addr_imm.of_idx u (.i_outer) with
   | addr_imm_ #{ oa = deref_ oa; ob = #{ ia = deref_ ia; ib = deref_ ib } } ->
     print_outer_ln
       #{ oa = Addr_imm.get oa;
          ob = #{ ia = Addr_imm.get ia; ib = Addr_imm.get ib } });
  print_int_ln u.m_ub.ub;
  print_int_ln u.m_s1.#s;
  print_ut_ln u.m_ut;
  print_outer_ln u.m_outer;
  print_outer_ln u.i_outer;
  print_newline ()

(* Binding sites, each at [vx] *)

let get_set (local_ a) x =
  let before : int = Addr.get a in
  Addr.set a x;
  before

let param { vx = deref_ a; _ } x = get_set a x

let optional ?v:({ vx = deref_ a; _ } = { vx = 0; vy = "" }) x = get_set a x

let ( let* ) v f = f v

let binding_sites : (string * (v -> int -> int)) list =
  [ ("match", fun v x -> match v with { vx = deref_ a; _ } -> get_set a x);
    ("let", fun v x -> let { vx = deref_ a; _ } = v in get_set a x);
    ("parameter", param);
    ( "function",
      fun v x -> (function { vx = deref_ a; _ } -> get_set a x) v );
    ("optional argument default", fun v x -> optional ~v x);
    ("let-operator", fun v x -> let* { vx = deref_ a; _ } = v in get_set a x);
    ( "match with an exception case",
      fun v x ->
        match v with
        | { vx = deref_ a; _ } -> get_set a x
        | exception Exit -> 0 );
    ( "guard",
      fun v x ->
        let before = ref 0 in
        (match v with
         | { vx = deref_ a; _ } when (before := get_set a x; false) -> ()
         | _ -> ());
        !before );
    ( "or-pattern",
      fun v x ->
        match v with
        | { vx = deref_ a; vy = "" } | { vx = deref_ a; _ } -> get_set a x );
    ( "constructor argument",
      fun v x ->
        match Some v with
        | Some { vx = deref_ a; _ } -> get_set a x
        | None -> 0 );
    ( "syntactic tuple element",
      fun v x -> match v, 0 with { vx = deref_ a; _ }, _ -> get_set a x )
  ]

let () =
  print_endline "deref binding sites";
  List.iter
    (fun (name, bind) ->
      let v = { vx = 1; vy = "" } in
      let before = bind v 2 in
      Printf.printf "%s: %d -> %d\n" name before v.vx)
    binding_sites;
  let v = stack_ { vx = 1; vy = "" } in
  (match v with
   | { vx = deref_ a; _ } -> Printf.printf "local: %d -> " (get_set a 2));
  print_int_ln v.vx;
  print_newline ()

(* Syntactic tuples are usually matched without allocating them. With
   dereference patterns on their elements, they are allocated. *)

let get2 (addr_imm_ a) (addr_imm_ b) = a ^ b

let tupled (deref_ a, deref_ b) = get2 a b

let syntactic_tuples : (string * (string -> string -> string)) list =
  [ ("match", fun x y -> match x, y with deref_ a, deref_ b -> get2 a b);
    ( "match with an exception case",
      fun x y ->
        match x, y with deref_ a, deref_ b -> get2 a b | exception Exit -> "" );
    ( "match with several rows",
      fun x y ->
        match x, y with
        | deref_ a, "" -> Addr_imm.get a
        | deref_ a, deref_ b -> get2 a b );
    ( "match with an alias",
      fun x y ->
        match x, y with (deref_ a, deref_ b) as t -> get2 a b ^ fst t ^ snd t );
    ( "match on a nested tuple",
      fun x y -> match (x, 0), y with (deref_ a, _), deref_ b -> get2 a b );
    ("let", fun x y -> let (deref_ a, deref_ b) = (x, y) in get2 a b);
    ( "let of a nested tuple",
      fun x y ->
        let ((deref_ a, _), deref_ b) = ((x, 0), y) in
        get2 a b );
    ( "let of a constant",
      fun _ _ -> let (deref_ a, deref_ b) = ("x", "y") in get2 a b );
    ("tupled function", fun x y -> tupled (x, y))
  ]

let () =
  print_endline "deref syntactic tuples";
  List.iter
    (fun (name, f) -> Printf.printf "%s: %s\n" name (f "x" "y"))
    syntactic_tuples;
  print_newline ()

(* Choosing between positions *)

type two = { mutable p : int; mutable q : int; sel : bool }

let pick = function
  | { sel = true; p = deref_ a; _ } | { sel = false; q = deref_ a; _ } -> a

let choose b x y =
  match b, x, y with
  | true, deref_ a, _ | false, _, deref_ a -> Addr_imm.get a

(* The second alternative of the first case is kept in the default matrix of
   the other cases. *)
let default = function
  | (1, (deref_ _a, _)) | (_, (deref_ _a, 1)) -> 1
  | (2, (_, 2)) -> 2
  | _ -> 3

let () =
  print_endline "deref choice";
  let t = { p = 1; q = 2; sel = true } in
  Addr.set (pick t) 3;
  print_int_ln t.p;
  print_int_ln t.q;
  let t = { p = 1; q = 2; sel = false } in
  Addr.set (pick t) 3;
  print_int_ln t.p;
  print_int_ln t.q;
  print_endline (choose true "x" "y");
  print_endline (choose false "x" "y");
  print_int_ln (default (1, (0, 0)));
  print_int_ln (default (0, (0, 1)));
  print_int_ln (default (2, (0, 2)));
  print_int_ln (default (3, (0, 0)));
  print_newline ()

(* Allocating syntactic tuples does not change evaluation order *)

let say s = print_string s

let () =
  print_endline "deref evaluation order";
  (match say "a", say "b" with _ -> ());
  print_string " ";
  (match say "a", say "b" with (deref_ a, _) -> Addr_imm.get a);
  print_newline ();
  (match say "a", say "b" with _ -> () | exception _ -> ());
  print_string " ";
  (match say "a", say "b" with
   | (deref_ a, _) -> Addr_imm.get a
   | exception _ -> ());
  print_newline ();
  (let (_, _) = (say "a", say "b") in ());
  print_string " ";
  (let (deref_ a, _) = (say "a", say "b") in Addr_imm.get a);
  print_newline ();
  print_newline ()
