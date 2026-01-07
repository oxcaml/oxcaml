(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files}";
 include stdlib_upstream_compatible;
 include stdlib_stable;
 flambda2;
 {
   bytecode;
 }{
   native;
 }{
   flags = "-extension layouts_beta";
   bytecode;
 }{
   flags = "-extension layouts_beta";
   native;
 }
*)
(* Test compilation correctness for array of untagged int8s. General
   tests around type-checking should go to [basics.ml]. *)

(* We use "box" instead of "tag" in this file for compatibility with
   [Test_gen_u_array]. *)

open Stdlib_stable

module Int8_I = struct
  include Int8
  let max_val = max_int
  let min_val = min_int
  let rand n = of_int (Random.int (to_int n))
  let print n = Format.printf "%d" (to_int n)
end

module Int8_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = int8
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> x = y)
  module I = Int8_I
end
module _ = Test_gen_u_array.Test (Int8_array)

module Int8_u_array0 : Gen_u_array.S0
                        with type element_t = int8#
                        and type ('a : any) array_t = 'a array = struct

  type element_t = int8#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array
  let max_length = Sys.max_untagged_int8_array_length
  external length : ('a : bits8). 'a array -> int = "%array_length"
  external get: ('a : bits8). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : bits8). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : bits8). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : bits8). 'a array -> int -> 'a -> unit
    = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : bits8). int -> 'a array =
    "caml_make_untagged_int8_vect_bytecode" "caml_make_untagged_int8_vect"
  external unsafe_blit : ('a : bits8).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_untagged_int8_vect_blit"
  let empty () = [||]
  external to_boxed : ('a : bits8) -> (int8[@local_opt]) = "%tag_int8"
  let compare_element x y = Int8.compare (to_boxed (x ())) (to_boxed (y ()))
end

module Int8_u_array = Gen_u_array.Make (Int8_u_array0)
module Int8_u_array_boxed : Test_gen_u_array.S with type t = int8# array = Test_gen_u_array.Make_boxed (struct
  module M = Int8_u_array
  module I = Int8_I
  module E = struct
    open Int8_u
    let to_boxed x = to_int8 (x ())
    let of_boxed x () = of_int8 x
  end
end)
module _ = Test_gen_u_array.Test (Int8_u_array_boxed)

(* Extra tests for array expressions and patterns *)
module A = Int8_u_array_boxed
module I = Int8_u_array_boxed.I

let check_i a =
  let rec check_i_upto a i =
    if i >= 0 then begin
      assert (A.get a i = I.of_int i);
      check_i_upto a (i - 1);
    end
  in
  check_i_upto a (A.length a - 1)

let check_eq_f f arr = A.iteri (fun i x -> assert (x = f i)) arr
let check_all_the_same v arr = A.iter (fun x -> assert (x = v)) arr

let check_inval f arg =
  match f arg with
  | _ -> assert false
  | exception (Invalid_argument _) -> ()
  | exception _ -> assert false

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
  let r = A.make (Sys.opaque_identity 0) (I.of_int 0) in
  test_empty_array r;

  (* static blocks *)
  let r = [|
#0s;#1s;#2s;#3s;#4s;#5s;#6s;#7s;#8s;#9s;#10s;#11s;#12s;#13s;#14s;#15s;#16s;#17s;
#18s;#19s;#20s;#21s;#22s;#23s;#24s;#25s;#26s;#27s;#28s;#29s;#30s;#31s;#32s;#33s;#34s;#35s;
#36s;#37s;#38s;#39s;#40s;#41s;#42s;#43s;#44s;#45s;#46s;#47s;#48s;#49s;#50s;#51s;#52s;#53s;
#54s;#55s;#56s;#57s;#58s;#59s;#60s;#61s;#62s;#63s;#64s;#65s;#66s;#67s;#68s;#69s;#70s;#71s;
#72s;#73s;#74s;#75s;#76s;#77s;#78s;#79s;#80s;#81s;#82s;#83s;#84s;#85s;#86s;#87s;#88s;#89s;
#90s;#91s;#92s;#93s;#94s;#95s;#96s;#97s;#98s;#99s; |]
  in
  check_i r;
  let r = [|
#0s;#1s;#2s;#3s;#4s;#5s;#6s;#7s;#8s;#9s;#10s;#11s;#12s;#13s;#14s;#15s;#16s;#17s;
#18s;#19s;#20s;#21s;#22s;#23s;#24s;#25s;#26s;#27s;#28s;#29s;#30s;#31s;#32s;#33s;#34s;#35s;
#36s;#37s;#38s;#39s;#40s;#41s;#42s;#43s;#44s;#45s;#46s;#47s;#48s;#49s;#50s;#51s;#52s;#53s;
#54s;#55s;#56s;#57s;#58s;#59s;#60s;#61s;#62s;#63s;#64s;#65s;#66s;#67s;#68s;#69s;#70s;#71s;
#72s;#73s;#74s;#75s;#76s;#77s;#78s;#79s;#80s;#81s;#82s;#83s;#84s;#85s;#86s;#87s;#88s;#89s;
#90s;#91s;#92s;#93s;#94s;#95s;#96s;#97s;#98s;#99s;#100s; |]
  in
  check_i r;
  let r = [|-#123s;-#123s;-#123s;-#123s;-#123s;-#123s;-#123s;-#123s;-#123s;-#123s;-#123s;|] in
  check_all_the_same (I.of_int (-123)) r;
  let r =
    [|-#1s; #1s; -#1s; #1s; -#1s; #1s; -#1s; #1s; -#1s;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|#1s; -#1s; #1s; -#1s; #1s; -#1s; #1s; -#1s;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* dynamic blocks *)
  let[@inline never] f x = x in
  let r = [|
    f #0s;f #1s;f #2s;f #3s;f #4s;f #5s;f #6s;f #7s;f #8s;f #9s;f #10s;f #11s;f #12s;f #13s;f #14s;
    f #15s;f #16s;f #17s;f #18s;f #19s;f #20s;f #21s;f #22s;f #23s;f #24s;f #25s;f #26s;f #27s;f #28s;f #29s;
    f #30s;f #31s;f #32s;f #33s;f #34s;f #35s;f #36s;f #37s;f #38s;f #39s;f #40s;f #41s;f #42s;f #43s;f #44s;
    f #45s;f #46s;f #47s;f #48s;f #49s;f #50s;f #51s;f #52s;f #53s;f #54s;f #55s;f #56s;f #57s;f #58s;f #59s;
    f #60s;f #61s;f #62s;f #63s;f #64s;f #65s;f #66s;f #67s;f #68s;f #69s;f #70s;f #71s;f #72s;f #73s;f #74s;
    f #75s;f #76s;f #77s;f #78s;f #79s;f #80s;f #81s;f #82s;f #83s;f #84s;f #85s;f #86s;f #87s;f #88s;f #89s;
    f #90s;f #91s;f #92s;f #93s;f #94s;f #95s;f #96s;f #97s;f #98s;f #99s; |]
  in
  check_i r;
  let r = [|
    f #0s;f #1s;f #2s;f #3s;f #4s;f #5s;f #6s;f #7s;f #8s;f #9s;f #10s;f #11s;f #12s;f #13s;f #14s;
    f #15s;f #16s;f #17s;f #18s;f #19s;f #20s;f #21s;f #22s;f #23s;f #24s;f #25s;f #26s;f #27s;f #28s;f #29s;
    f #30s;f #31s;f #32s;f #33s;f #34s;f #35s;f #36s;f #37s;f #38s;f #39s;f #40s;f #41s;f #42s;f #43s;f #44s;
    f #45s;f #46s;f #47s;f #48s;f #49s;f #50s;f #51s;f #52s;f #53s;f #54s;f #55s;f #56s;f #57s;f #58s;f #59s;
    f #60s;f #61s;f #62s;f #63s;f #64s;f #65s;f #66s;f #67s;f #68s;f #69s;f #70s;f #71s;f #72s;f #73s;f #74s;
    f #75s;f #76s;f #77s;f #78s;f #79s;f #80s;f #81s;f #82s;f #83s;f #84s;f #85s;f #86s;f #87s;f #88s;f #89s;
    f #90s;f #91s;f #92s;f #93s;f #94s;f #95s;f #96s;f #97s;f #98s;f #99s;f #100s; |]
  in
  check_i r;
  let r =
    [|f (-#123s);f (-#123s);f (-#123s);f (-#123s);f (-#123s);f (-#123s);f (-#123s);f (-#123s);f (-#123s);|]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| #0s; ((fun x -> x) #1s)|];
  check_i [| #0s; ((fun x -> x) #1s); #2s|];
  let r =
    [|f (-#1s);f (#1s);f (-#1s);f (#1s);f (-#1s);f (#1s);f (-#1s);f (#1s);f (-#1s);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|f (#1s);f (-#1s);f (#1s);f (-#1s);f (#1s);f (-#1s);f (#1s);f (-#1s);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) = Int8_u.equal in
  (* match statement *)
  let d = [| #1s; #2s |] in
  (match d with
    | [| a; b |] ->
      assert (a = #1s);
      assert (b = #2s)
    | _ -> assert false);

  let d = [: #1s; #2s :] in
  (match d with
    | [: a; b :] ->
      assert (a = #1s);
      assert (b = #2s)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| #1s |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = #1s);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| #1s |] = #1s);
  ()
