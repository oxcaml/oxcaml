(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files} stubs.c";
 include stdlib_upstream_compatible;
 arch_amd64;
 flambda2;
 {
   flags = "-extension simd_beta";
   native;
 }
*)
(* Test compilation correctness for array of unboxed int64x4s. General
   tests around type-checking should go to [basics.ml]. *)

module Int64x4_I = struct

  type t = int64x4

  external box : int64x4# -> int64x4 = "%box_vec256"
  external unbox : int64x4 -> int64x4# = "%unbox_vec256"

  external interleave_low_64 : int64x2 -> int64x2 -> int64x2 = "" "caml_simd_vec128_interleave_low_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external interleave_high_64 : int64x2 -> int64x2 -> int64x2 = "" "caml_simd_vec128_interleave_high_64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external insert_128 :
    (int[@untagged]) ->
    (int64x4[@unboxed]) ->
    (int64x2[@unboxed]) ->
    (int64x4[@unboxed]) = "caml_vec256_unreachable" "caml_avx_vec256_insert_128"
    [@@noalloc] [@@builtin]

  external extract_128 :
    (int[@untagged]) -> (int64x4[@unboxed]) -> (int64x2[@unboxed])
    = "caml_vec256_unreachable" "caml_avx_vec256_extract_128"
    [@@noalloc] [@@builtin]

  external low_of : int64 -> int64x2 = "" "caml_int64x2_low_of_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external low_to : int64x2 -> int64 = "" "caml_int64x2_low_to_int64"
    [@@noalloc] [@@unboxed] [@@builtin]

  external const1 : int64 -> t = "" "caml_int64x4_const1"
    [@@noalloc] [@@unboxed] [@@builtin]

  external add : t -> t -> t = "" "caml_avx2_int64x4_add"
    [@@noalloc] [@@unboxed] [@@builtin]

  external sub : t -> t -> t = "" "caml_avx2_int64x4_sub"
    [@@noalloc] [@@unboxed] [@@builtin]

  let neg x = sub (const1 0L) x

  let high_to x =
    let x = interleave_high_64 x x in
    low_to x

  let of_i64s x y z w =
    let x = low_of x in
    let y = low_of y in
    let z = low_of z in
    let w = low_of w in
    let xy = interleave_low_64 x y in
    let zw = interleave_low_64 z w in
    let i = const1 0L in
    let i = insert_128 1 i xy in
    let i = insert_128 0 i zw in
    i

  let mul x y =
    let xl = extract_128 0 x in
    let yl = extract_128 0 y in
    let xh = extract_128 1 x in
    let yh = extract_128 1 y in
    let xll, yll = low_to xl, low_to yl in
    let xlh, ylh = high_to xl, high_to yl in
    let xhl, yhl = low_to xh, low_to yh in
    let xhh, yhh = high_to xh, high_to yh in
    of_i64s Int64.(mul xhh yhh) Int64.(mul xhl yhl)
            Int64.(mul xlh ylh) Int64.(mul xll yll)

  let of_int i = of_i64s (Int64.of_int i) (Int64.of_int i)
                         (Int64.of_int i) (Int64.of_int i)
  let max_val = of_i64s Int64.max_int Int64.max_int
                        Int64.max_int Int64.max_int
  let min_val = of_i64s Int64.min_int Int64.min_int
                        Int64.min_int Int64.min_int
  let rand x =
    let l, h = extract_128 0 x, extract_128 1 x in
    let ll, lh = low_to l, high_to l in
    let hl, hh = low_to h, high_to h in
    of_i64s (Random.int64 hh) (Random.int64 hl)
            (Random.int64 lh) (Random.int64 ll)

  let print x =
    let l, h = extract_128 0 x, extract_128 1 x in
    let ll, lh = low_to l, high_to l in
    let hl, hh = low_to h, high_to h in
    Format.printf "%Ld:%Ld:%Ld:%Ld" hh hl lh ll

  let compare x y =
    let xl = extract_128 0 x in
    let yl = extract_128 0 y in
    let xh = extract_128 1 x in
    let yh = extract_128 1 y in
    let xll, yll = low_to xl, low_to yl in
    let xlh, ylh = high_to xl, high_to yl in
    let xhl, yhl = low_to xh, low_to yh in
    let xhh, yhh = high_to xh, high_to yh in
    let h = Int64.compare xhh yhh in
    if h <> 0 then h else
      let h = Int64.compare xhl yhl in
      if h <> 0 then h else
        let h = Int64.compare xlh ylh in
        if h <> 0 then h else
          Int64.compare xll yll
end

module Int64x4_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = int64x4
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> Int64x4_I.compare x y = 0)
  let mem x a =
    let n = length a in
    let rec loop i =
      if i = n then false
      else if Int64x4_I.compare (unsafe_get a i) x = 0 then true
      else loop (succ i) in
    loop 0

  module I = Int64x4_I
end
module _ = Test_gen_u_array.Test (Int64x4_array)

module Int64x4_u_array0 : Gen_u_array.S0
                        with type element_t = int64x4#
                        and type ('a : any) array_t = 'a array = struct

  type element_t = int64x4#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array
  let max_length = Sys.max_unboxed_vec256_array_length
  external length : ('a : vec256). 'a array -> int = "%array_length"
  external get: ('a : vec256). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : vec256). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : vec256). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : vec256). 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : vec256). int -> 'a array =
    "caml_make_unboxed_vec256_vect_bytecode" "caml_make_unboxed_vec256_vect"
  external unsafe_blit : ('a : vec256).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_unboxed_vec256_vect_blit"
  let empty () = [||]
  external to_boxed : int64x4# -> (int64x4[@local_opt]) = "%box_vec256"
  let compare_element x y = Int64x4_I.compare (to_boxed (x ())) (to_boxed (y ()))
end

module Int64x4_u_array = Gen_u_array.Make (Int64x4_u_array0)
module Int64x4_u_array_boxed = Test_gen_u_array.Make_boxed (struct
  module M = Int64x4_u_array
  module I = Int64x4_I
  module E = struct
    external to_boxed : int64x4# -> (int64x4[@local_opt]) = "%box_vec256"
    external of_boxed : int64x4 -> int64x4# = "%unbox_vec256"
    let to_boxed x = to_boxed (x ())
    let of_boxed x () = of_boxed x
  end
end)
module _ = Test_gen_u_array.Test (Int64x4_u_array_boxed)



(* Extra tests for array expressions and patterns *)
module A = Int64x4_u_array_boxed
module I = Int64x4_u_array_boxed.I

let check_i a =
  let rec check_i_upto a i =
    if i >= 0 then begin
      assert (Int64x4_I.compare (A.get a i) (I.of_int i) = 0);
      check_i_upto a (i - 1);
    end
  in
  check_i_upto a (A.length a - 1)

let check_eq_f f arr = A.iteri (fun i x -> assert (Int64x4_I.compare x (f i) = 0)) arr
let check_all_the_same v arr = A.iter (fun x -> assert (Int64x4_I.compare x v = 0)) arr

let check_inval f arg =
  match f arg with
  | _ -> assert false
  | exception (Invalid_argument _) -> ()
  | exception _ -> assert false

external const : int64# -> int64x4# = "" "caml_int64x4_const1"
  [@@noalloc] [@@unboxed] [@@builtin]

let () =
  (* empty arrays *)
  let test_empty_array arr =
    check_inval (fun a -> A.get a 0) arr;
    check_inval (fun a -> A.get a 1) arr;
    check_inval (fun a -> A.get a (-1)) arr;
    check_inval (fun a -> A.set a 0 (I.of_int 0)) arr;
    check_inval (fun a -> A.set a 1 (I.of_int 0)) arr;
    check_inval (fun a -> A.set a (-1) (I.of_int 0)) arr
  in
  let r : A.t = [||] in
  test_empty_array r;
  let r = A.make 0 (I.of_int 0) in
  test_empty_array r;

  (* static blocks *)
  let r = [|
const #0L; const #1L; const #2L; const #3L; const #4L; const #5L; const #6L;
const #7L; const #8L;  const #9L; const #10L; const #11L; const #12L; const #13L;
const #14L; const #15L; const #16L; const #17L; const #18L; const #19L; const #20L;
const #21L; const #22L; const #23L; const #24L; const #25L; const #26L; const #27L;
const #28L; const #29L; const #30L; const #31L; const #32L; const #33L; const #34L;
const #35L; const #36L; const #37L; const #38L; const #39L; const #40L; const #41L;
const #42L; const #43L; const #44L; const #45L; const #46L; const #47L; const #48L;
const #49L; const #50L |]
  in
  check_i r;
  let r = [|
const #0L; const #1L; const #2L; const #3L; const #4L; const #5L; const #6L;
const #7L; const #8L;  const #9L; const #10L; const #11L; const #12L; const #13L;
const #14L; const #15L; const #16L; const #17L; const #18L; const #19L; const #20L |]
  in
  check_i r;
  let r = [|
const (-#123L); const (-#123L); const (-#123L); const (-#123L); const (-#123L); const (-#123L);
const (-#123L); const (-#123L); const (-#123L); const (-#123L); const (-#123L); |] in
  check_all_the_same (I.of_int (-123)) r;
  let r = [|
const (-#1L); const #1L; const (-#1L); const #1L; const (-#1L); const #1L; const (-#1L);
const #1L; const (-#1L); |]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r = [|
const #1L; const (-#1L); const #1L; const (-#1L); const #1L; const (-#1L); const #1L;
const (-#1L) |]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* static blocks with opaque contents *)
  let[@inline never] f x = x in
  let r = [|
f (const #0L); f (const #1L); f (const #2L); f (const #3L); f (const #4L); f (const #5L);
f (const #6L); f (const #7L); f (const #8L); f (const #9L); f (const #10L); f (const #11L);
f (const #12L); f (const #13L); f (const #14L); f (const #15L); f (const #16L); f (const #17L);
f (const #18L); f (const #19L); f (const #20L); f (const #21L); f (const #22L); f (const #23L);
f (const #24L); f (const #25L); f (const #26L); f (const #27L); f (const #28L); f (const #29L);
f (const #30L); f (const #31L); f (const #32L); f (const #33L); f (const #34L); f (const #35L);
f (const #36L); f (const #37L); f (const #38L); f (const #39L); f (const #40L); f (const #41L);
f (const #42L); f (const #43L); f (const #44L); f (const #45L); f (const #46L); f (const #47L);
f (const #48L); f (const #49L); f (const #50L) |]
  in
  check_i r;
  let r = [|
f (const #0L); f (const #1L); f (const #2L); f (const #3L); f (const #4L); f (const #5L);
f (const #6L); f (const #7L); f (const #8L); f (const #9L); f (const #10L); f (const #11L);
f (const #12L); f (const #13L); f (const #14L); f (const #15L); f (const #16L); f (const #17L);
f (const #18L); f (const #19L); f (const #20L); |]
  in
  check_i r;
  let r = [|
f (const (-#123L)); f (const (-#123L)); f (const (-#123L)); f (const (-#123L)); f (const (-#123L));
f (const (-#123L)); f (const (-#123L)); f (const (-#123L)); f (const (-#123L)) |]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| const #0L; ((fun x -> Int64x4_I.(of_i64s x x x x |> unbox)) 1L) |];
  check_i [| const #0L; ((fun x -> Int64x4_I.(of_i64s x x x x |> unbox)) 1L); const #2L |];
  let r = [|
f (const (-#1L)); f (const #1L); f (const (-#1L)); f (const #1L); f (const (-#1L));
f (const #1L); f (const (-#1L)); f (const #1L); f (const (-#1L)) |]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r = [|
f (const #1L); f (const (-#1L)); f (const #1L); f (const (-#1L)); f (const #1L);
f (const (-#1L)); f (const #1L); f (const (-#1L)) |]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) x y = Int64x4_I.(compare (box x) (box y)) = 0 in
  (* match statement *)
  let d = [| const #1L; const #2L |] in
  (match d with
    | [| a; b |] ->
      assert (a = const #1L);
      assert (b = const #2L)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| const #1L |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = const #1L);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| const #1L |] = const #1L);
  ()
