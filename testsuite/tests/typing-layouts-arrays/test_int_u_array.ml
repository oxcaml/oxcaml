(* TEST
 readonly_files = "gen_u_array.ml test_gen_u_array.ml";
 modules = "${readonly_files}";
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
(* Test compilation correctness for array of untagged immediatess. General
   tests around type-checking should go to [basics.ml]. *)

(* We use "box" instead of "tag" in this file for compatibility with
   [Test_gen_u_array]. *)

module Int_I = struct
  include Int
  let of_int = Fun.id
  let max_val = max_int
  let min_val = min_int
  let rand = Random.full_int
  let print = Format.printf "%d"
end

module Int_array : Test_gen_u_array.S = struct
  include Stdlib.Array
  type element_t = int
  type t = element_t array
  let map_to_array f a = map f a
  let map_from_array f a = map f a
  let max_length = Sys.max_array_length
  let equal = for_all2 (fun x y -> x = y)
  module I = Int_I
end
module _ = Test_gen_u_array.Test (Int_array)

module Int_u_array0 : Gen_u_array.S0
                        with type element_t = int#
                        and type ('a : any) array_t = 'a array = struct

  type element_t = int#
  type ('a : any) array_t = 'a array
  type element_arg = unit -> element_t
  type t = element_t array
  let max_length = Sys.max_untagged_int_array_length
  external length : ('a : untagged_immediate). 'a array -> int = "%array_length"
  external get: ('a : untagged_immediate). 'a array -> int -> 'a = "%array_safe_get"
  let get t i = let a = get t i in fun () -> a
  external set: ('a : untagged_immediate). 'a array -> int -> 'a -> unit = "%array_safe_set"
  let set t i e = set t i (e ())
  external unsafe_get: ('a : untagged_immediate). 'a array -> int -> 'a = "%array_unsafe_get"
  let unsafe_get t i = let a = unsafe_get t i in fun () -> a
  external unsafe_set: ('a : untagged_immediate). 'a array -> int -> 'a -> unit = "%array_unsafe_set"
  let unsafe_set t i e = unsafe_set t i (e ())

  external unsafe_create : ('a : untagged_immediate). int -> 'a array =
    "caml_make_untagged_int_vect_bytecode" "caml_make_untagged_int_vect"
  external unsafe_blit : ('a : untagged_immediate).
    'a array -> int -> 'a array -> int -> int -> unit =
    "caml_array_blit" "caml_untagged_int_vect_blit"
  let empty () = [||]
  external to_boxed : ('a : untagged_immediate) -> (int[@local_opt]) = "%tag_int"
  let compare_element x y = Int.compare (to_boxed (x ())) (to_boxed (y ()))
end

module Int_u_array = Gen_u_array.Make (Int_u_array0)
module Int_u_array_boxed : Test_gen_u_array.S with type t = int# array = Test_gen_u_array.Make_boxed (struct
  module M = Int_u_array
  module I = Int_I
  module E = struct
    open Stdlib_stable.Int_u
    let to_boxed x = to_int (x ())
    let of_boxed x () = of_int x
  end
end)
module _ = Test_gen_u_array.Test (Int_u_array_boxed)


(* Extra tests for array expressions and patterns *)
module A = Int_u_array_boxed
module I = Int_u_array_boxed.I

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
#0m;#1m;#2m;#3m;#4m;#5m;#6m;#7m;#8m;#9m;#10m;#11m;#12m;#13m;#14m;#15m;#16m;#17m;
#18m;#19m;#20m;#21m;#22m;#23m;#24m;#25m;#26m;#27m;#28m;#29m;#30m;#31m;#32m;#33m;#34m;#35m;
#36m;#37m;#38m;#39m;#40m;#41m;#42m;#43m;#44m;#45m;#46m;#47m;#48m;#49m;#50m;#51m;#52m;#53m;
#54m;#55m;#56m;#57m;#58m;#59m;#60m;#61m;#62m;#63m;#64m;#65m;#66m;#67m;#68m;#69m;#70m;#71m;
#72m;#73m;#74m;#75m;#76m;#77m;#78m;#79m;#80m;#81m;#82m;#83m;#84m;#85m;#86m;#87m;#88m;#89m;
#90m;#91m;#92m;#93m;#94m;#95m;#96m;#97m;#98m;#99m; |]
  in
  check_i r;
  let r = [|
#0m;#1m;#2m;#3m;#4m;#5m;#6m;#7m;#8m;#9m;#10m;#11m;#12m;#13m;#14m;#15m;#16m;#17m;
#18m;#19m;#20m;#21m;#22m;#23m;#24m;#25m;#26m;#27m;#28m;#29m;#30m;#31m;#32m;#33m;#34m;#35m;
#36m;#37m;#38m;#39m;#40m;#41m;#42m;#43m;#44m;#45m;#46m;#47m;#48m;#49m;#50m;#51m;#52m;#53m;
#54m;#55m;#56m;#57m;#58m;#59m;#60m;#61m;#62m;#63m;#64m;#65m;#66m;#67m;#68m;#69m;#70m;#71m;
#72m;#73m;#74m;#75m;#76m;#77m;#78m;#79m;#80m;#81m;#82m;#83m;#84m;#85m;#86m;#87m;#88m;#89m;
#90m;#91m;#92m;#93m;#94m;#95m;#96m;#97m;#98m;#99m;#100m; |]
  in
  check_i r;
  let r = [|-#123m;-#123m;-#123m;-#123m;-#123m;-#123m;-#123m;-#123m;-#123m;-#123m;-#123m;|] in
  check_all_the_same (I.of_int (-123)) r;
  let r =
    [|-#1m; #1m; -#1m; #1m; -#1m; #1m; -#1m; #1m; -#1m;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|#1m; -#1m; #1m; -#1m; #1m; -#1m; #1m; -#1m;|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  (* dynamic blocks *)
  let[@inline never] f x = x in
  let r = [|
    f #0m;f #1m;f #2m;f #3m;f #4m;f #5m;f #6m;f #7m;f #8m;f #9m;f #10m;f #11m;f #12m;f #13m;f #14m;
    f #15m;f #16m;f #17m;f #18m;f #19m;f #20m;f #21m;f #22m;f #23m;f #24m;f #25m;f #26m;f #27m;f #28m;f #29m;
    f #30m;f #31m;f #32m;f #33m;f #34m;f #35m;f #36m;f #37m;f #38m;f #39m;f #40m;f #41m;f #42m;f #43m;f #44m;
    f #45m;f #46m;f #47m;f #48m;f #49m;f #50m;f #51m;f #52m;f #53m;f #54m;f #55m;f #56m;f #57m;f #58m;f #59m;
    f #60m;f #61m;f #62m;f #63m;f #64m;f #65m;f #66m;f #67m;f #68m;f #69m;f #70m;f #71m;f #72m;f #73m;f #74m;
    f #75m;f #76m;f #77m;f #78m;f #79m;f #80m;f #81m;f #82m;f #83m;f #84m;f #85m;f #86m;f #87m;f #88m;f #89m;
    f #90m;f #91m;f #92m;f #93m;f #94m;f #95m;f #96m;f #97m;f #98m;f #99m; |]
  in
  check_i r;
  let r = [|
    f #0m;f #1m;f #2m;f #3m;f #4m;f #5m;f #6m;f #7m;f #8m;f #9m;f #10m;f #11m;f #12m;f #13m;f #14m;
    f #15m;f #16m;f #17m;f #18m;f #19m;f #20m;f #21m;f #22m;f #23m;f #24m;f #25m;f #26m;f #27m;f #28m;f #29m;
    f #30m;f #31m;f #32m;f #33m;f #34m;f #35m;f #36m;f #37m;f #38m;f #39m;f #40m;f #41m;f #42m;f #43m;f #44m;
    f #45m;f #46m;f #47m;f #48m;f #49m;f #50m;f #51m;f #52m;f #53m;f #54m;f #55m;f #56m;f #57m;f #58m;f #59m;
    f #60m;f #61m;f #62m;f #63m;f #64m;f #65m;f #66m;f #67m;f #68m;f #69m;f #70m;f #71m;f #72m;f #73m;f #74m;
    f #75m;f #76m;f #77m;f #78m;f #79m;f #80m;f #81m;f #82m;f #83m;f #84m;f #85m;f #86m;f #87m;f #88m;f #89m;
    f #90m;f #91m;f #92m;f #93m;f #94m;f #95m;f #96m;f #97m;f #98m;f #99m;f #100m; |]
  in
  check_i r;
  let r =
    [|f (-#123m);f (-#123m);f (-#123m);f (-#123m);f (-#123m);f (-#123m);f (-#123m);f (-#123m);f (-#123m);|]
  in
  check_all_the_same (I.of_int (-123)) r;
  check_i [| #0m; ((fun x -> x) #1m)|];
  check_i [| #0m; ((fun x -> x) #1m); #2m|];
  let r =
    [|f (-#1m);f (#1m);f (-#1m);f (#1m);f (-#1m);f (#1m);f (-#1m);f (#1m);f (-#1m);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (-1) else I.of_int 1) r;
  let r =
    [|f (#1m);f (-#1m);f (#1m);f (-#1m);f (#1m);f (-#1m);f (#1m);f (-#1m);|]
  in
  check_eq_f (fun idx -> if (idx mod 2) = 0 then I.of_int (1) else I.of_int (-1)) r;
  ()


(* expression and patterns *)
let () =
  let ( = ) = Stdlib_stable.Int_u.equal in
  (* match statement *)
  let d = [| #1m; #2m |] in
  (match d with
    | [| a; b |] ->
      assert (a = #1m);
      assert (b = #2m)
    | _ -> assert false);

  let d = [: #1m; #2m :] in
  (match d with
    | [: a; b :] ->
      assert (a = #1m);
      assert (b = #2m)
    | _ -> assert false);

  (* let statement pattern *)
  let a = [||] in
  let b = [| #1m |] in
  let c = A.append a b in
  let[@warning "-8"] [| d |] = c in
  assert (d = #1m);

  (* function argument pattern *)
  let[@warning "-8"] f [| b |] = b in
  assert (f [| #1m |] = #1m);
  ()
