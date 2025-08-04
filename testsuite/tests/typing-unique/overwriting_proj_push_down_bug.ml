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
(let (aliased_use/289 = (function {nlocal = 0} x/291? x/291))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/289))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (unique_ x) = x
[%%expect{|
(let (unique_use/292 = (function {nlocal = 0} x/294? x/294))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/292))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/289 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/295 =
     (function {nlocal = 0}
       r/297[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/298 = (field_imm 1 r/297)
          r/299 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/289 r/297))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/299
           y/298))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/295))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/292 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/300 =
     (function {nlocal = 0}
       r/302[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/303 = (field_mut 1 r/302)
          r/304 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/292 r/302))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/304
           y/303))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/300))
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
  (aliased_use/289 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/305 =
     (function {nlocal = 0}
       r/307[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (r/309 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/289 r/307))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/309
           (field_imm 1 r/307)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/305))
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
  (unique_use/292 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/311 =
     (function {nlocal = 0}
       r/313[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/314 =o? (field_mut 1 r/313)
          r/315 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/292 r/313))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/315
           y/314))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/311))
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
  (aliased_use/289 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/317 =
     (function {nlocal = 0}
       r/319[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (*match*/325 =[value<int>] 1
          r/322 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/289 r/319))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/322
           (field_imm 1 r/319)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/317))
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
  (unique_use/292 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/327 =
     (function {nlocal = 0}
       r/329[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/331 =o? (field_mut 1 r/329)
          *match*/335 =[value<int>] 1
          r/332 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/292 r/329))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/332
           y/331))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/327))
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
  (aliased_use/289 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/337 =
     (function {nlocal = 0}
       r/339[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/341 =a? (field_imm 1 r/339))
           (if (%eq y/341 "")
             (let (*match*/348 =[value<int>] 0) (exit 8 y/341))
             (let (*match*/346 =[value<int>] 1) (exit 8 (field_imm 1 r/339)))))
        with (8 y/340)
         (let
           (r/343 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply aliased_use/289 r/339))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/343
             y/340)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/337))
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
  (unique_use/292 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/349 =
     (function {nlocal = 0}
       r/351[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/353 =o? (field_mut 1 r/351))
           (if (%eq y/353 "")
             (let (*match*/360 =[value<int>] 0) (exit 14 y/353))
             (let (y/354 =o? (field_mut 1 r/351) *match*/358 =[value<int>] 1)
               (exit 14 y/354))))
        with (14 y/352)
         (let
           (r/355 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply unique_use/292 r/351))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/355
             y/352)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/349))
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
  (swap_inner/367 =
     (function {nlocal = 0}
       t/369[value<
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
         (if t/369
           (let (*match*/378 =a? (field_imm 0 t/369))
             (if *match*/378
               (let (*match*/382 =a? (field_imm 2 t/369))
                 (if *match*/382
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
                       (field_imm 0 *match*/378) (field_int 1 *match*/378)
                       (field_imm 0 *match*/382))
                     (field_int 1 t/369)
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
                       (field_imm 2 *match*/378) (field_int 1 *match*/382)
                       (field_imm 2 *match*/382)))
                   (exit 19)))
               (exit 19)))
           (exit 19))
        with (19) t/369)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/367))
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
  (unique_use/292 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/289 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/385 =
     (function {nlocal = 0}
       r/387[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let (y/388 =o? (field_mut 1 r/387))
         (if (caml_string_equal y/388 "")
           (let
             (r/459 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply aliased_use/289 r/387))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/459 y/388))
           (let
             (y/389 =o? (field_mut 1 r/387)
              r/460 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply unique_use/292 r/387))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/460 y/389))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/385))
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
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

|}]

let check_heap_alloc_in_overwrite (local_ unique_ r : option_record) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

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
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

|}]

let update (unique_ r : mutable_record) =
  let x = overwrite_ r with { y = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { y = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
Uncaught exception: File "parsing/location.ml", line 1107, characters 2-8: Assertion failed

|}]
