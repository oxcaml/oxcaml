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
(* Test compilation correctness for array of untagged int16s. General
   tests around type-checking should go to [basics.ml]. *)

(* We use "box" instead of "tag" in this file for compatibility with
   [Test_gen_u_array]. *)

open Stdlib_stable

module Int16_I = struct
  include Int16
  let max_val = max_int
  let min_val = min_int
  let rand n = of_int (Random.int (to_int n))
  let print n = Format.printf "%d" (to_int n)
end

module Int16_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = int16
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> x = y)
  module I = Int16_I
end
module _ = Test_gen_u_array.Test (Int16_array)

module Int16_u_array0 : Gen_u_array.S0
                        with type element_t = int16#
                        and type ('a : any) array_t = 'a array = struct

  type element_t = int16#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array
  let max_length = Sys.max_untagged_int16_array_length
  external length : ('a : bits16). 'a array -> int = "%array_length"
  external get: ('a : bits16). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : bits16). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : bits16). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : bits16). 'a array -> int -> 'a -> unit
    = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : bits16). int -> 'a array =
    "caml_make_untagged_int16_vect_bytecode" "caml_make_untagged_int16_vect"
  external unsafe_blit : ('a : bits16).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_untagged_int16_vect_blit"
  let empty () = [||]
  external to_boxed : ('a : bits16) -> (int16[@local_opt]) = "%tag_int16"
  let compare_element x y = Int16.compare (to_boxed (x ())) (to_boxed (y ()))
end

module Int16_u_array = Gen_u_array.Make (Int16_u_array0)
module Int16_u_array_boxed : Test_gen_u_array.S with type t = int16# array = Test_gen_u_array.Make_boxed (struct
  module M = Int16_u_array
  module I = Int16_I
  module E = struct
    open Int16_u
    let to_boxed x = to_int16 (x ())
    let of_boxed x () = of_int16 x
  end
end)
module _ = Test_gen_u_array.Test (Int16_u_array_boxed)

(* Extra tests for array expressions and patterns *)
module A = Int16_u_array_boxed
module I = Int16_u_array_boxed.I

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
#0S;#1S;#2S;#3S;#4S;#5S;#6S;#7S;#8S;#9S;#10S;#11S;#12S;#13S;#14S;#15S;#16S;#17S;
#18S;#19S;#20S;#21S;#22S;#23S;#24S;#25S;#26S;#27S;#28S;#29S;#30S;#31S;#32S;#33S;#34S;#35S;
#36S;#37S;#38S;#39S;#40S;#41S;#42S;#43S;#44S;#45S;#46S;#47S;#48S;#49S;#50S;#51S;#52S;#53S;
#54S;#55S;#56S;#57S;#58S;#59S;#60S;#61S;#62S;#63S;#64S;#65S;#66S;#67S;#68S;#69S;#70S;#71S;
#72S;#73S;#74S;#75S;#76S;#77S;#78S;#79S;#80S;#81S;#82S;#83S;#84S;#85S;#86S;#87S;#88S;#89S;
#90S;#91S;#92S;#93S;#94S;#95S;#96S;#97S;#98S;#99S; |]
  in
  check_i r;
  let r = [|
#0S;#1S;#2S;#3S;#4S;#5S;#6S;#7S;#8S;#9S;#10S;#11S;#12S;#13S;#14S;#15S;#16S;#17S;
#18S;#19S;#20S;#21S;#22S;#23S;#24S;#25S;#26S;#27S;#28S;#29S;#30S;#31S;#32S;#33S;#34S;#35S;
#36S;#37S;#38S;#39S;#40S;#41S;#42S;#43S;#44S;#45S;#46S;#47S;#48S;#49S;#50S;#51S;#52S;#53S;
#54S;#55S;#56S;#57S;#58S;#59S;#60S;#61S;#62S;#63S;#64S;#65S;#66S;#67S;#68S;#69S;#70S;#71S;
#72S;#73S;#74S;#75S;#76S;#77S;#78S;#79S;#80S;#81S;#82S;#83S;#84S;#85S;#86S;#87S;#88S;#89S;
#90S;#91S;#92S;#93S;#94S;#95S;#96S;#97S;#98S;#99S;#100S; |]
  in
  check_i r;
  let r = [|-#123S;-#123S;-#123S;-#123S;-#123S;-#123S;-#123S;-#123S;-#123S;-#123S;-#123S;|] in
  check_all_the_same (I.of_int (-123)) r;
  let r =
    [|-#1S; #1S; -#1S; #1S; -#1S; #1S; -#1S; #1S; -#1S;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|#1S; -#1S; #1S; -#1S; #1S; -#1S; #1S; -#1S;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* dynamic blocks *)
  let[@inline never] f x = x in
  let r = [|
    f #0S;f #1S;f #2S;f #3S;f #4S;f #5S;f #6S;f #7S;f #8S;f #9S;f #10S;f #11S;f #12S;f #13S;f #14S;
    f #15S;f #16S;f #17S;f #18S;f #19S;f #20S;f #21S;f #22S;f #23S;f #24S;f #25S;f #26S;f #27S;f #28S;f #29S;
    f #30S;f #31S;f #32S;f #33S;f #34S;f #35S;f #36S;f #37S;f #38S;f #39S;f #40S;f #41S;f #42S;f #43S;f #44S;
    f #45S;f #46S;f #47S;f #48S;f #49S;f #50S;f #51S;f #52S;f #53S;f #54S;f #55S;f #56S;f #57S;f #58S;f #59S;
    f #60S;f #61S;f #62S;f #63S;f #64S;f #65S;f #66S;f #67S;f #68S;f #69S;f #70S;f #71S;f #72S;f #73S;f #74S;
    f #75S;f #76S;f #77S;f #78S;f #79S;f #80S;f #81S;f #82S;f #83S;f #84S;f #85S;f #86S;f #87S;f #88S;f #89S;
    f #90S;f #91S;f #92S;f #93S;f #94S;f #95S;f #96S;f #97S;f #98S;f #99S; |]
  in
  check_i r;
  let r = [|
    f #0S;f #1S;f #2S;f #3S;f #4S;f #5S;f #6S;f #7S;f #8S;f #9S;f #10S;f #11S;f #12S;f #13S;f #14S;
    f #15S;f #16S;f #17S;f #18S;f #19S;f #20S;f #21S;f #22S;f #23S;f #24S;f #25S;f #26S;f #27S;f #28S;f #29S;
    f #30S;f #31S;f #32S;f #33S;f #34S;f #35S;f #36S;f #37S;f #38S;f #39S;f #40S;f #41S;f #42S;f #43S;f #44S;
    f #45S;f #46S;f #47S;f #48S;f #49S;f #50S;f #51S;f #52S;f #53S;f #54S;f #55S;f #56S;f #57S;f #58S;f #59S;
    f #60S;f #61S;f #62S;f #63S;f #64S;f #65S;f #66S;f #67S;f #68S;f #69S;f #70S;f #71S;f #72S;f #73S;f #74S;
    f #75S;f #76S;f #77S;f #78S;f #79S;f #80S;f #81S;f #82S;f #83S;f #84S;f #85S;f #86S;f #87S;f #88S;f #89S;
    f #90S;f #91S;f #92S;f #93S;f #94S;f #95S;f #96S;f #97S;f #98S;f #99S;f #100S; |]
  in
  check_i r;
  let r =
    [|f (-#123S);f (-#123S);f (-#123S);f (-#123S);f (-#123S);f (-#123S);f (-#123S);f (-#123S);f (-#123S);|]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| #0S; ((fun x -> x) #1S)|];
  check_i [| #0S; ((fun x -> x) #1S); #2S|];
  let r =
    [|f (-#1S);f (#1S);f (-#1S);f (#1S);f (-#1S);f (#1S);f (-#1S);f (#1S);f (-#1S);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|f (#1S);f (-#1S);f (#1S);f (-#1S);f (#1S);f (-#1S);f (#1S);f (-#1S);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) = Int16_u.equal in
  (* match statement *)
  let d = [| #1S; #2S |] in
  (match d with
    | [| a; b |] ->
      assert (a = #1S);
      assert (b = #2S)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| #1S |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = #1S);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| #1S |] = #1S);
  ()
