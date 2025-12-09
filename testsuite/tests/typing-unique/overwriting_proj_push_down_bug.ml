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
(let (aliased_use/308 = (function {nlocal = 0} x/310? x/310))
  (apply (field_imm 1 (global Toploop!)) "aliased_use" aliased_use/308))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (x @ unique) = x
[%%expect{|
(let (unique_use/311 = (function {nlocal = 0} x/313? x/313))
  (apply (field_imm 1 (global Toploop!)) "unique_use" unique_use/311))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/308 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/314 =
     (function {nlocal = 0}
       r/316[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/317 = (field_imm 1 r/316)
          r/318 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/308 r/316))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/318
           y/317))))
  (apply (field_imm 1 (global Toploop!)) "proj_aliased" proj_aliased/314))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/311 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/319 =
     (function {nlocal = 0}
       r/321[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/322 = (field_mut 1 r/321)
          r/323 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/311 r/321))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/323
           y/322))))
  (apply (field_imm 1 (global Toploop!)) "proj_unique" proj_unique/319))
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
  (aliased_use/308 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/324 =
     (function {nlocal = 0}
       r/326[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (r/328 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/308 r/326))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/328
           (field_imm 1 r/326)))))
  (apply (field_imm 1 (global Toploop!)) "match_aliased" match_aliased/324))
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
  (unique_use/311 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/330 =
     (function {nlocal = 0}
       r/332[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/333 =o? (field_mut 1 r/332)
          r/334 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/311 r/332))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/334
           y/333))))
  (apply (field_imm 1 (global Toploop!)) "match_unique" match_unique/330))
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
  (aliased_use/308 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/336 =
     (function {nlocal = 0}
       r/338[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (*match*/344 =[value<int>] 1
          r/341 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply aliased_use/308 r/338))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/341
           (field_imm 1 r/338)))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/336))
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
  (unique_use/311 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/346 =
     (function {nlocal = 0}
       r/348[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/350 =o? (field_mut 1 r/348)
          *match*/354 =[value<int>] 1
          r/351 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply unique_use/311 r/348))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/351
           y/350))))
  (apply (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/346))
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
  (aliased_use/308 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/356 =
     (function {nlocal = 0}
       r/358[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/360 =a? (field_imm 1 r/358))
           (if (%eq y/360 "")
             (let (*match*/367 =[value<int>] 0) (exit 21 y/360))
             (let (*match*/365 =[value<int>] 1)
               (exit 21 (field_imm 1 r/358)))))
        with (21 y/359)
         (let
           (r/362 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply aliased_use/308 r/358))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/362
             y/359)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/356))
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
  (unique_use/311 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/368 =
     (function {nlocal = 0}
       r/370[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/372 =o? (field_mut 1 r/370))
           (if (%eq y/372 "")
             (let (*match*/379 =[value<int>] 0) (exit 29 y/372))
             (let (y/373 =o? (field_mut 1 r/370) *match*/377 =[value<int>] 1)
               (exit 29 y/373))))
        with (29 y/371)
         (let
           (r/374 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply unique_use/311 r/370))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/374
             y/371)))))
  (apply (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/368))
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
  (swap_inner/386 =
     (function {nlocal = 0}
       t/388[value<
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
         (if t/388
           (let (*match*/397 =a? (field_imm 0 t/388))
             (if *match*/397
               (let (*match*/401 =a? (field_imm 2 t/388))
                 (if *match*/401
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
                       (field_imm 0 *match*/397) (field_int 1 *match*/397)
                       (field_imm 0 *match*/401))
                     (field_int 1 t/388)
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
                       (field_imm 2 *match*/397) (field_int 1 *match*/401)
                       (field_imm 2 *match*/401)))
                   (exit 36)))
               (exit 36)))
           (exit 36))
        with (36) t/388)))
  (apply (field_imm 1 (global Toploop!)) "swap_inner" swap_inner/386))
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
  (unique_use/311 =? (apply (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/308 =? (apply (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/404 =
     (function {nlocal = 0}
       r/406[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let (y/407 =o? (field_mut 1 r/406))
         (if (caml_string_equal y/407 "")
           (let
             (r/480 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply aliased_use/308 r/406))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/480 y/407))
           (let
             (y/408 =o? (field_mut 1 r/406)
              r/481 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply unique_use/311 r/406))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/481 y/408))))))
  (apply (field_imm 1 (global Toploop!)) "match_guard" match_guard/404))
val match_guard : record @ unique -> record * string = <fun>
|}]

let match_guard_unique (r @ unique) =
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

let check_heap_alloc_in_overwrite (r : option_record @ unique) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
>> Fatal error: Location.todo_overwrite_not_implemented
Uncaught exception: Misc.Fatal_error

|}]

let check_heap_alloc_in_overwrite (r : option_record @ local unique) =
  overwrite_ r with { x = Some "" }
[%%expect{|
Line 2, characters 2-35:
2 |   overwrite_ r with { x = Some "" }
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
>> Fatal error: Location.todo_overwrite_not_implemented
Uncaught exception: Misc.Fatal_error

|}]

(*******************************)
(* Overwrite of mutable fields *)

type mutable_record = { mutable x : string; y : string }
[%%expect{|
0
type mutable_record = { mutable x : string; y : string; }
|}]

let update (r : mutable_record @ unique) =
  let x = overwrite_ r with { x = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { x = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
>> Fatal error: Location.todo_overwrite_not_implemented
Uncaught exception: Misc.Fatal_error

|}]

let update (r : mutable_record @ unique) =
  let x = overwrite_ r with { y = "foo" } in
  x.x
[%%expect{|
Line 2, characters 10-41:
2 |   let x = overwrite_ r with { y = "foo" } in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert Translcore: Overwrite not implemented.
>> Fatal error: Location.todo_overwrite_not_implemented
Uncaught exception: Misc.Fatal_error

|}]
