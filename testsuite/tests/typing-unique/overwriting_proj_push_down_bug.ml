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
(let (aliased_use/290 = (function {nlocal = 0} x/292? x/292))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/290))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/293 = (function {nlocal = 0} x/295? x/295))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/293))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/290 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/296 =
     (function {nlocal = 0}
       r/298[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/299 = (field_imm 1 r/298)
          r/300 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/290 r/298))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/300
           y/299))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/296))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/293 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/301 =
     (function {nlocal = 0}
       r/303[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/304 = (field_mut 1 r/303)
          r/305 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/293 r/303))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/305
           y/304))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/301))
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
  (aliased_use/290 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/306 =
     (function {nlocal = 0}
       r/308[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (r/310 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/290 r/308))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/310
           (field_imm 1 r/308)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/306))
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
  (unique_use/293 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/312 =
     (function {nlocal = 0}
       r/314[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/315 =o? (field_mut 1 r/314)
          r/316 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/293 r/314))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/316
           y/315))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/312))
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
  (aliased_use/290 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/318 =
     (function {nlocal = 0}
       r/320[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (*match*/326 =[value<int>] 1
          r/323 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/290 r/320))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/323
           (field_imm 1 r/320)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/318))
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
  (unique_use/293 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/328 =
     (function {nlocal = 0}
       r/330[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/332 =o? (field_mut 1 r/330)
          *match*/336 =[value<int>] 1
          r/333 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/293 r/330))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/333
           y/332))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/328))
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
  (aliased_use/290 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/338 =
     (function {nlocal = 0}
       r/340[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/342 =a? (field_imm 1 r/340))
           (if (%eq y/342 "")
             (let (*match*/349 =[value<int>] 0) (exit 8 y/342))
             (let (*match*/347 =[value<int>] 1) (exit 8 (field_imm 1 r/340)))))
        with (8 y/341)
         (let
           (r/344 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply aliased_use/290 r/340))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/344
             y/341)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/338))
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
  (unique_use/293 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/350 =
     (function {nlocal = 0}
       r/352[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/354 =o? (field_mut 1 r/352))
           (if (%eq y/354 "")
             (let (*match*/361 =[value<int>] 0) (exit 14 y/354))
             (let (y/355 =o? (field_mut 1 r/352) *match*/359 =[value<int>] 1)
               (exit 14 y/355))))
        with (14 y/353)
         (let
           (r/356 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply unique_use/293 r/352))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/356
             y/353)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/350))
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
  (swap_inner/368 =
     (function {nlocal = 0}
       t/370[value<
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
         (if t/370
           (let (*match*/379 =a? (field_imm 0 t/370))
             (if *match*/379
               (let (*match*/383 =a? (field_imm 2 t/370))
                 (if *match*/383
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
                       (field_imm 0 *match*/379) (field_int 1 *match*/379)
                       (field_imm 0 *match*/383))
                     (field_int 1 t/370)
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
                       (field_imm 2 *match*/379) (field_int 1 *match*/383)
                       (field_imm 2 *match*/383)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/370)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/368))
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
  (unique_use/293 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/290 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/386 =
     (function {nlocal = 0}
       r/388[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let (y/389 =o? (field_mut 1 r/388))
         (if (caml_string_equal y/389 "")
           (let
             (r/460 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply aliased_use/290 r/388))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/460 y/389))
           (let
             (y/390 =o? (field_mut 1 r/388)
              r/461 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply unique_use/293 r/388))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/461 y/390))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/386))
val match_guard : record @ unique -> record * string = <fun>
|}]

let match_guard_unique (unique_ r) =
  match r with
  | { y } when String.equal ((unique_use r).x) "" -> y
  | _ -> ""
[%%expect{|
Line 3, characters 4-9:
3 |   | { y } when String.equal ((unique_use r).x) "" -> y
        ^^^^^
Error: This value is read from here, but it is already being used as unique:
Line 3, characters 41-42:
3 |   | { y } when String.equal ((unique_use r).x) "" -> y
                                             ^

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
Uncaught exception: File "parsing/location.ml", line 1124, characters 2-8: Assertion failed

|}]

let check_heap_alloc_in_overwrite (local_ unique_ r : option_record) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1124, characters 2-8: Assertion failed

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
Uncaught exception: File "parsing/location.ml", line 1124, characters 2-8: Assertion failed

|}]

let update (unique_ r : mutable_record) =
  let x = overwrite_ r with { y = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { y = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1124, characters 2-8: Assertion failed

|}]
