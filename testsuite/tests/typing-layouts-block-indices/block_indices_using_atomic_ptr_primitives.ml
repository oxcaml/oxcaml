(* TEST
 reference = "${test_source_directory}/block_indices_using_atomic_ptr_primitives.reference";
 include stdlib_stable;
 include stdlib_upstream_compatible;
 flambda2;
 {
   bytecode;
 } {
   native;
 } {
   flags = "-Oclassic";
   native;
 }
*)

(* Coupled with records-and-block-indices/idx_atomic.ml *)

open Stdlib_stable
open Stdlib_upstream_compatible

(* test reading/writing atomic fields using atomic ptrs *)

module Basic = struct
  type t = { mutable x: int [@atomic]; mutable y: string [@atomic] }

  let () =
    Printf.printf "== Basic ptr_atomic ==\n";
    let t = { x = 1; y = "one" } in
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get #(t, (.x)));
    Printf.printf "(.y) = %s\n" (Ptr_atomic.get #(t, (.y)));
    Printf.printf "(.x) <- 2\n"; Ptr_atomic.set #(t, (.x)) 2;
    Printf.printf "(.y) <- two\n"; Ptr_atomic.set #(t, (.y)) "two";
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get #(t, (.x)));
    Printf.printf "(.y) = %s\n" (Ptr_atomic.get #(t, (.y)));
    ()
end

(* test read-modify-write operations using atomic ptrs *)

module Rmw = struct
  type t = { mutable x: int [@atomic]; mutable y: string [@atomic] }

  let () =
    Printf.printf "== ptr_atomic read-modify-write ==\n";
    let t = { x = 1; y = "one" } in
    let px = #(t, (.x)) in
    let py = #(t, (.y)) in
    Printf.printf "exchange (.x) 2 = %d\n" (Ptr_atomic.exchange px 2);
    Printf.printf "exchange (.y) two = %s\n" (Ptr_atomic.exchange py "two");
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "(.y) = %s\n" (Ptr_atomic.get py);
    Printf.printf "compare_and_set (.x) 2 3 = %b\n"
      (Ptr_atomic.compare_and_set px 2 3);
    Printf.printf "compare_and_set (.x) 2 4 = %b\n"
      (Ptr_atomic.compare_and_set px 2 4);
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    let y = Ptr_atomic.get py in
    Printf.printf "compare_and_set (.y) y three = %b\n"
      (Ptr_atomic.compare_and_set py y "three");
    Printf.printf "(.y) = %s\n" (Ptr_atomic.get py);
    Printf.printf "compare_exchange (.x) 3 5 = %d\n"
      (Ptr_atomic.compare_exchange px 3 5);
    Printf.printf "compare_exchange (.x) 3 6 = %d\n"
      (Ptr_atomic.compare_exchange px 3 6);
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "fetch_and_add (.x) 10 = %d\n"
      (Ptr_atomic.fetch_and_add px 10);
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "add (.x) 4\n"; Ptr_atomic.add px 4;
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "sub (.x) 5\n"; Ptr_atomic.sub px 5;
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "logand (.x) 6\n"; Ptr_atomic.logand px 6;
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "logor (.x) 9\n"; Ptr_atomic.logor px 9;
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "logxor (.x) 3\n"; Ptr_atomic.logxor px 3;
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "incr (.x)\n"; Ptr_atomic.incr px;
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    Printf.printf "decr (.x)\n"; Ptr_atomic.decr px;
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    ()
end

(* test read-modify-write operations on a mixed record *)

module RmwMixed = struct
  type t = { x: int64#; mutable y: int [@atomic]; z: int64# }

  let () =
    Printf.printf "== ptr_atomic read-modify-write (mixed record) ==\n";
    let t = { x = #42L; y = 1; z = #67L } in
    let py = #(t, (.y)) in
    Printf.printf "exchange (.y) 2 = %d\n" (Ptr_atomic.exchange py 2);
    Printf.printf "compare_and_set (.y) 2 3 = %b\n"
      (Ptr_atomic.compare_and_set py 2 3);
    Printf.printf "fetch_and_add (.y) 10 = %d\n"
      (Ptr_atomic.fetch_and_add py 10);
    Printf.printf "(.x) = %Ld\n" (Int64_u.to_int64 t.x);
    Printf.printf "(.y) = %d\n" (Ptr_atomic.get py);
    Printf.printf "(.z) = %Ld\n" (Int64_u.to_int64 t.z);
    ()
end

(* test read-modify-write operations on a stack-allocated record *)

module RmwStack = struct
  type t = { mutable x: int [@atomic] }

  let () =
    Printf.printf
      "== ptr_atomic read-modify-write (stack-allocated record) ==\n";
    let t = stack_ { x = 1 } in
    let px = #(t, (.x)) in
    Printf.printf "exchange (.x) 2 = %d\n" (Ptr_atomic.exchange px 2);
    Printf.printf "compare_and_set (.x) 2 3 = %b\n"
      (Ptr_atomic.compare_and_set px 2 3);
    Printf.printf "compare_exchange (.x) 3 4 = %d\n"
      (Ptr_atomic.compare_exchange px 3 4);
    Printf.printf "fetch_and_add (.x) 10 = %d\n"
      (Ptr_atomic.fetch_and_add px 10);
    Printf.printf "(.x) = %d\n" (Ptr_atomic.get px);
    ()
end

(* test reading/writing unboxed singleton record *)

module UnboxedSingleton = struct
  type inner = { y: int }
  type outer = { mutable x: inner# [@atomic] }

  let print_inner ppf (i : inner#) = Printf.fprintf ppf "#{y = %d}" i.#y

  let () =
    Printf.printf "== ptr_atomic with unboxed singleton ==\n";
    let t = { x = #{ y = 1 } } in
    let fst = #(t, (.x)) in
    let snd = #(t, (.x.#y)) in
    Printf.printf "(.x) = %a\n" print_inner (Ptr_atomic.get fst);
    Printf.printf "(.x.#y) = %d\n" (Ptr_atomic.get snd);
    Printf.printf "(.x) <- #{y = 2}\n"; Ptr_atomic.set fst #{y = 2};
    Printf.printf "(.x) = %a\n" print_inner (Ptr_atomic.get fst);
    Printf.printf "(.x.#y) = %d\n" (Ptr_atomic.get snd);
    Printf.printf "(.x.#y) <- 3\n"; Ptr_atomic.set snd 3;
    Printf.printf "(.x) = %a\n" print_inner (Ptr_atomic.get fst);
    Printf.printf "(.x.#y) = %d\n" (Ptr_atomic.get snd);
    ()

  (* test deepening idx_atomic *)
  let () =
    Printf.printf "== deepening ptr_atomic with unboxed singleton ==\n";
    let t = { x = #{ y = 1 } } in
    let fst_idx = (.x) in
    let fst = #(t, fst_idx) in
    let snd = #(t, (.idx_atomic(fst_idx).#y)) in
    Printf.printf "(.x) = %a\n" print_inner (Ptr_atomic.get fst);
    Printf.printf "(.idx_atomic((.x)).#y) = %d\n" (Ptr_atomic.get snd);
    Printf.printf "(.x) <- #{y = 2}\n"; Ptr_atomic.set fst #{y = 2};
    Printf.printf "(.x) = %a\n" print_inner (Ptr_atomic.get fst);
    Printf.printf "(.idx_atomic((.x)).#y) = %d\n" (Ptr_atomic.get snd);
    Printf.printf "(.idx_atomic((.x)).#y) <- 3\n"; Ptr_atomic.set snd 3;
    Printf.printf "(.x) = %a\n" print_inner (Ptr_atomic.get fst);
    Printf.printf "(.idx_atomic((.x)).#y) = %d\n" (Ptr_atomic.get snd);
    ()
end

(* test reading/writing from mixed record *)

module Mixed = struct
  type t = { x: int64#; mutable y: string [@atomic]; z: int64# }

  let () =
    Printf.printf "== Basic ptr_atomic (mixed record) ==\n";
    let t = { x = #42L; y = "two"; z = #67L } in
    Printf.printf "(.x) = %Ld\n" (Int64_u.to_int64 t.x);
    Printf.printf "(.y) = %s\n" (Ptr_atomic.get #(t, (.y)));
    Printf.printf "(.z) = %Ld\n" (Int64_u.to_int64 t.z);
    Printf.printf "(.y) <- three\n"; Ptr_atomic.set #(t, (.y)) "three";
    Printf.printf "(.x) = %Ld\n" (Int64_u.to_int64 t.x);
    Printf.printf "(.y) = %s\n" (Ptr_atomic.get #(t, (.y)));
    Printf.printf "(.z) = %Ld\n" (Int64_u.to_int64 t.z);
end

(* test reading/writing from all-float record *)

module Float = struct
  [@@@warning "-214"]
  type t = { x: float; mutable y: float [@atomic] }

  let () =
    Printf.printf "== Basic ptr_atomic (float record) ==\n";
    let t = { x = 2.0; y = 4.0 } in
    Printf.printf "(.x) = %f\n" t.x;
    Printf.printf "(.y) = %f\n" (Ptr_atomic.get #(t, (.y)));
    Printf.printf "(.y) <- 6.0\n"; Ptr_atomic.set #(t, (.y)) 6.0;
    Printf.printf "(.x) = %f\n" t.x;
    Printf.printf "(.y) = %f\n" (Ptr_atomic.get #(t, (.y)));
    ()
end

(* test reading/writing from record with void fields *)

module Void = struct
  type t = { x: unit#; mutable y: string [@atomic]; z: unit# }

  let () =
    Printf.printf "== Basic ptr_atomic (void record) ==\n";
    let t = { x = #(); y = "hello"; z = #() } in
    Printf.printf "(.y) = %s\n" (Ptr_atomic.get #(t, (.y)));
    Printf.printf "(.y) <- world\n"; Ptr_atomic.set #(t, (.y)) "world";
    Printf.printf "(.y) = %s\n" (Ptr_atomic.get #(t, (.y)));
    ()
end

(* test reading/writing from stack-allocated record *)

module Stack = struct
  type t = { mutable x: string [@atomic] }

  let () =
    Printf.printf "== Basic ptr_atomic (stack-allocated record) ==\n";
    let t = stack_ { x = "hello" } in
    Printf.printf "(.x) = %s\n" (Ptr_atomic.get #(t, (.x)) );
    Printf.printf "(.x) <- world\n"; Ptr_atomic.set #(t, (.x)) "world";
    Printf.printf "(.x) = %s\n" (Ptr_atomic.get #(t, (.x)));
    ()
end
