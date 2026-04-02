(* TEST
   flags += "-extension-universe alpha ";
   flags += "-dlambda";
   expect;
*)

(* This file tests the lambda code that is generated for projections out of unique values.
   We need to ensure that if an allocation is used uniquely, all projections out of
   this allocation happen before the unique use and are not pushed down beyond that point.
*)

type record = { x : string; y : string @@ many aliased }
[%%expect{|
0
type record = { x : string; y : string @@ many aliased; }
|}]

let aliased_use x = x
[%%expect{|
(let (aliased_use/294 = (function {nlocal = 0} x/296? x/296))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/294))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/297 = (function {nlocal = 0} x/299? x/299))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/297))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/294 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/300 =
     (function {nlocal = 0}
       r/302[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/303 = (field_imm 1 r/302)
          r/304 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/294 r/302))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/304
           y/303))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/300))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/297 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/305 =
     (function {nlocal = 0}
       r/307[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/308 = (field_mut 1 r/307)
          r/309 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/297 r/307))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/309
           y/308))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/305))
val proj_unique : record @ unique -> record * string = <fun>
|}]

(* This output would be unsound if [aliased_use] was able to overwrite [r]
   because the [field_imm 1 r] read happens after calling [aliased_use]. *)
let match_aliased r =
  match r with
  | { y } ->
    let r = aliased_use r in
    (r, y)
[%%expect{|
(let
  (aliased_use/294 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/310 =
     (function {nlocal = 0}
       r/312[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (r/314 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/294 r/312))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/314
           (field_imm 1 r/312)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/310))
val match_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] before the [unique_use] *)
let match_unique r =
  match r with
  | { y } ->
    let r = unique_use r in
    (r, y)
[%%expect{|
(let
  (unique_use/297 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/316 =
     (function {nlocal = 0}
       r/318[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/319 =o? (field_mut 1 r/318)
          r/320 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/297 r/318))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/320
           y/319))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/316))
val match_unique : record @ unique -> record * string = <fun>
|}]

(* Similarly, this would be unsound since Lambda performs a mini ANF pass. *)
let match_mini_anf_aliased r =
  let y, _ =
    match r with
    | { y } -> (y, 1)
  in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/294 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/322 =
     (function {nlocal = 0}
       r/324[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (*match*/330 =[value<int>] 1
          r/327 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/294 r/324))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/327
           (field_imm 1 r/324)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/322))
val match_mini_anf_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] before the [unique_use] *)
let match_mini_anf_unique r =
  let y, _ =
    match r with
    | { y } -> (y, 1)
  in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/297 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/332 =
     (function {nlocal = 0}
       r/334[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/336 =o? (field_mut 1 r/334)
          *match*/340 =[value<int>] 1
          r/337 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/297 r/334))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/337
           y/336))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/332))
val match_mini_anf_unique : record @ unique -> record * string = <fun>
|}]

let match_anf_aliased r =
  let y, _ =
    match r with
    | { y } when y == "" -> (y, 0)
    | { y } -> (y, 1)
  in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/294 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/342 =
     (function {nlocal = 0}
       r/344[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/346 =a? (field_imm 1 r/344))
           (if (%eq y/346 "")
             (let (*match*/353 =[value<int>] 0) (exit 21 y/346))
             (let (*match*/351 =[value<int>] 1)
               (exit 21 (field_imm 1 r/344)))))
        with (21 y/345)
         (let
           (r/348 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply aliased_use/294 r/344))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/348
             y/345)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/342))
val match_anf_aliased : record -> record * string = <fun>
|}]

(* This is sound since we bind [y] using [field_mut] *)
let match_anf_unique r =
  let y, _ =
    match r with
    | { y } when y == "" -> (y, 0)
    | { y } -> (y, 1)
  in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/297 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/354 =
     (function {nlocal = 0}
       r/356[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/358 =o? (field_mut 1 r/356))
           (if (%eq y/358 "")
             (let (*match*/365 =[value<int>] 0) (exit 29 y/358))
             (let (y/359 =o? (field_mut 1 r/356) *match*/363 =[value<int>] 1)
               (exit 29 y/359))))
        with (29 y/357)
         (let
           (r/360 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply unique_use/297 r/356))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/360
             y/357)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/354))
val match_anf_unique : record @ unique -> record * string = <fun>
|}]

type tree =
  | Leaf
  | Node of { l : tree; x : int; r : tree }
[%%expect{|
0
type tree = Leaf | Node of { l : tree; x : int; r : tree; }
|}]

(* This output would be unsound with overwriting:
   If we naively replaced makeblock with reuseblock,
   then we would first overwrite r to have left child lr.
   But then, the overwrite of l still has to read the left child of r
   (as field_imm 0 *match*/329). But this value has been overwritten and so in fact,
   this code drops the rl and sets lr to be the inner child of both l and r.
*)
let swap_inner (t : tree) =
  match t with
  | Node ({ l = Node ({ r = lr } as l); r = Node ({ l = rl } as r) } as t) ->
    Node { t with l = Node { l with r = rl; }; r = Node { r with l = lr; }}
  | _ -> t
[%%expect{|
(let
  (swap_inner/372 =
     (function {nlocal = 0}
       t/374[value<
              (consts (0))
               (non_consts ([0:
                             value<
                              (consts (0))
                               (non_consts ([0: *, value<int>, *]))>,
                             value<int>,
                             value<
                              (consts (0))
                               (non_consts ([0: *, value<int>, *]))>]))>]
       : (consts (0))
          (non_consts ([0:
                        value<
                         (consts (0)) (non_consts ([0: *, value<int>, *]))>,
                        value<int>,
                        value<
                         (consts (0)) (non_consts ([0: *, value<int>, *]))>]))
       (catch
         (if t/374
           (let (*match*/383 =a? (field_imm 0 t/374))
             (if *match*/383
               (let (*match*/387 =a? (field_imm 2 t/374))
                 (if *match*/387
                   (makeblock 0 (value<
                                  (consts (0))
                                   (non_consts ([0:
                                                 value<
                                                  (consts (0))
                                                   (non_consts ([0: *,
                                                                 value<int>,
                                                                 *]))>,
                                                 value<int>,
                                                 value<
                                                  (consts (0))
                                                   (non_consts ([0: *,
                                                                 value<int>,
                                                                 *]))>]))>,
                     value<int>,value<
                                 (consts (0))
                                  (non_consts ([0:
                                                value<
                                                 (consts (0))
                                                  (non_consts ([0: *,
                                                                value<int>,
                                                                *]))>,
                                                value<int>,
                                                value<
                                                 (consts (0))
                                                  (non_consts ([0: *,
                                                                value<int>,
                                                                *]))>]))>)
                     (makeblock 0 (value<
                                    (consts (0))
                                     (non_consts ([0:
                                                   value<
                                                    (consts (0))
                                                     (non_consts ([0: *,
                                                                   value<int>,
                                                                   *]))>,
                                                   value<int>,
                                                   value<
                                                    (consts (0))
                                                     (non_consts ([0: *,
                                                                   value<int>,
                                                                   *]))>]))>,
                       value<int>,value<
                                   (consts (0))
                                    (non_consts ([0:
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: *,
                                                                  value<int>,
                                                                  *]))>,
                                                  value<int>,
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: *,
                                                                  value<int>,
                                                                  *]))>]))>)
                       (field_imm 0 *match*/383) (field_int 1 *match*/383)
                       (field_imm 0 *match*/387))
                     (field_int 1 t/374)
                     (makeblock 0 (value<
                                    (consts (0))
                                     (non_consts ([0:
                                                   value<
                                                    (consts (0))
                                                     (non_consts ([0: *,
                                                                   value<int>,
                                                                   *]))>,
                                                   value<int>,
                                                   value<
                                                    (consts (0))
                                                     (non_consts ([0: *,
                                                                   value<int>,
                                                                   *]))>]))>,
                       value<int>,value<
                                   (consts (0))
                                    (non_consts ([0:
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: *,
                                                                  value<int>,
                                                                  *]))>,
                                                  value<int>,
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: *,
                                                                  value<int>,
                                                                  *]))>]))>)
                       (field_imm 2 *match*/383) (field_int 1 *match*/387)
                       (field_imm 2 *match*/387)))
                   (exit 36)))
               (exit 36)))
           (exit 36))
        with (36) t/374)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/372))
val swap_inner : tree -> tree = <fun>
|}]

(* CR uniqueness: Update this test once overwriting is fully implemented.
   let swap_inner (t : tree) =
   match t with
   | Node { l = Node { r = lr } as l; r = Node { l = rl } as r } as t ->
   overwrite_ t with
   Node { l = overwrite_ l with Node { r = rl; };
   r = overwrite_ r with Node { l = lr; }}
   | _ -> t
   [%%expect{|

   |}]
*)

(***********************)
(* Barriers for guards *)

let match_guard r =
  match r with
  | { y } when String.equal y "" ->
    let r = aliased_use r in
    (r, y)
  | { y } ->
    let r = unique_use r in
    (r, y)
[%%expect{|
(let
  (unique_use/297 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/294 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/390 =
     (function {nlocal = 0}
       r/392[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let (y/393 =o? (field_mut 1 r/392))
         (if (caml_string_equal y/393 "")
           (let
             (r/466 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply aliased_use/294 r/392))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/466 y/393))
           (let
             (y/394 =o? (field_mut 1 r/392)
              r/467 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply unique_use/297 r/392))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/467 y/394))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/390))
val match_guard : record @ unique -> record * string = <fun>
|}]

let match_guard_unique (unique_ r) =
  match r with
  | { y } when String.equal ((unique_use r).x) "" -> y
  | _ -> ""
[%%expect{|
Line 3, characters 41-42:
3 |   | { y } when String.equal ((unique_use r).x) "" -> y
                                             ^
Error: This value is used here as unique, but it is also being read from at:
Line 3, characters 4-9:
3 |   | { y } when String.equal ((unique_use r).x) "" -> y
        ^^^^^

|}]

(********************************************)
(* Global allocations in overwritten fields *)

type option_record = { x : string option; y : string option }
[%%expect{|
0
type option_record = { x : string option; y : string option; }
|}]

let check_heap_alloc_in_overwrite (unique_ r : option_record) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1168, characters 2-8: Assertion failed

|}]

let check_heap_alloc_in_overwrite (local_ unique_ r : option_record) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1168, characters 2-8: Assertion failed

|}]

(*******************************)
(* Overwrite of mutable fields *)

type mutable_record = { mutable x : string; y : string }
[%%expect{|
0
type mutable_record = { mutable x : string; y : string; }
|}]

let update (unique_ r : mutable_record) =
  let x = overwrite_ r with { x = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { x = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1168, characters 2-8: Assertion failed

|}]

let update (unique_ r : mutable_record) =
  let x = overwrite_ r with { y = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { y = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1168, characters 2-8: Assertion failed

|}]
