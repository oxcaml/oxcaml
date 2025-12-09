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
(let (aliased_use/301 = (function {nlocal = 0} x/303? x/303))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/301))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/304 = (function {nlocal = 0} x/306? x/306))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/304))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/301 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/307 =
     (function {nlocal = 0}
       r/309[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/310 = (field_imm 1 r/309)
          r/311 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/301 r/309))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/311
           y/310))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/307))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/304 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/312 =
     (function {nlocal = 0}
       r/314[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/315 = (field_mut 1 r/314)
          r/316 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/304 r/314))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/316
           y/315))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/312))
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
  (aliased_use/301 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/317 =
     (function {nlocal = 0}
       r/319[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (r/321 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/301 r/319))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/321
           (field_imm 1 r/319)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/317))
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
  (unique_use/304 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/323 =
     (function {nlocal = 0}
       r/325[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/326 =o? (field_mut 1 r/325)
          r/327 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/304 r/325))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/327
           y/326))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/323))
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
  (aliased_use/301 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/329 =
     (function {nlocal = 0}
       r/331[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (*match*/337 =[value<int>] 1
          r/334 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/301 r/331))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/334
           (field_imm 1 r/331)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/329))
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
  (unique_use/304 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/339 =
     (function {nlocal = 0}
       r/341[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/343 =o? (field_mut 1 r/341)
          *match*/347 =[value<int>] 1
          r/344 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/304 r/341))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/344
           y/343))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/339))
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
  (aliased_use/301 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/349 =
     (function {nlocal = 0}
       r/351[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/353 =a? (field_imm 1 r/351))
           (if (%eq y/353 "")
             (let (*match*/360 =[value<int>] 0) (exit 8 y/353))
             (let (*match*/358 =[value<int>] 1) (exit 8 (field_imm 1 r/351)))))
        with (8 y/352)
         (let
           (r/355 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply aliased_use/301 r/351))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/355
             y/352)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/349))
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
  (unique_use/304 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/361 =
     (function {nlocal = 0}
       r/363[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/365 =o? (field_mut 1 r/363))
           (if (%eq y/365 "")
             (let (*match*/372 =[value<int>] 0) (exit 14 y/365))
             (let (y/366 =o? (field_mut 1 r/363) *match*/370 =[value<int>] 1)
               (exit 14 y/366))))
        with (14 y/364)
         (let
           (r/367 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply unique_use/304 r/363))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/367
             y/364)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/361))
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
  (swap_inner/379 =
     (function {nlocal = 0}
       t/381[value<
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
         (if t/381
           (let (*match*/390 =a? (field_imm 0 t/381))
             (if *match*/390
               (let (*match*/394 =a? (field_imm 2 t/381))
                 (if *match*/394
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
                       (field_imm 0 *match*/390) (field_int 1 *match*/390)
                       (field_imm 0 *match*/394))
                     (field_int 1 t/381)
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
                       (field_imm 2 *match*/390) (field_int 1 *match*/394)
                       (field_imm 2 *match*/394)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/381)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/379))
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
  (unique_use/304 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/301 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/397 =
     (function {nlocal = 0}
       r/399[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let (y/400 =o? (field_mut 1 r/399))
         (if (caml_string_equal y/400 "")
           (let
             (r/471 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply aliased_use/301 r/399))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/471 y/400))
           (let
             (y/401 =o? (field_mut 1 r/399)
              r/472 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply unique_use/304 r/399))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/472 y/401))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/397))
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
