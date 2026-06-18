(* TEST
   flags += "-extension-universe alpha ";
   flags += "-dlambda -dcanonical-ids";
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
(let (aliased_use/0 = (function {nlocal = 0} x/0? x/0))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "aliased_use"
    aliased_use/0))
val aliased_use : 'a -> 'a = <fun>
|}]

let unique_use (x @ unique) = x
[%%expect{|
(let (unique_use/0 = (function {nlocal = 0} x/1? x/1))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "unique_use"
    unique_use/0))
val unique_use : 'a @ unique -> 'a = <fun>
|}]

(* This output is fine with overwriting: The [r.y] is not pushed down. *)
let proj_aliased r =
  let y = r.y in
  let r = aliased_use r in
  (r, y)
[%%expect{|
(let
  (aliased_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "aliased_use")
   proj_aliased/0 =
     (function {nlocal = 0} r/0[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/0 = (field_imm 1 r/0)
          r/1 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply[unyielding] aliased_use/0 r/0))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/1
           y/0))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "proj_aliased"
    proj_aliased/0))
val proj_aliased : record -> record * string = <fun>
|}]

let proj_unique r =
  let y = r.y in
  let r = unique_use r in
  (r, y)
[%%expect{|
(let
  (unique_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "unique_use")
   proj_unique/0 =
     (function {nlocal = 0} r/2[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/1 = (field_mut 1 r/2)
          r/3 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply[unyielding] unique_use/0 r/2))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/3
           y/1))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "proj_unique"
    proj_unique/0))
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
  (aliased_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "aliased_use")
   match_aliased/0 =
     (function {nlocal = 0} r/4[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (r/5 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply[unyielding] aliased_use/0 r/4))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/5
           (field_imm 1 r/4)))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "match_aliased"
    match_aliased/0))
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
  (unique_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "unique_use")
   match_unique/0 =
     (function {nlocal = 0} r/6[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/2 =o? (field_mut 1 r/6)
          r/7 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply[unyielding] unique_use/0 r/6))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/7
           y/2))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "match_unique"
    match_unique/0))
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
  (aliased_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "aliased_use")
   match_mini_anf_aliased/0 =
     (function {nlocal = 0} r/8[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (*match*/0 =[value<int>] 1
          r/9 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply[unyielding] aliased_use/0 r/8))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/9
           (field_imm 1 r/8)))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "match_mini_anf_aliased"
    match_mini_anf_aliased/0))
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
  (unique_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "unique_use")
   match_mini_anf_unique/0 =
     (function {nlocal = 0} r/10[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let
         (y/3 =o? (field_mut 1 r/10)
          *match*/1 =[value<int>] 1
          r/11 =[value<(consts ()) (non_consts ([0: *, *]))>]
            (apply[unyielding] unique_use/0 r/10))
         (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/11
           y/3))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "match_mini_anf_unique"
    match_mini_anf_unique/0))
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
  (aliased_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "aliased_use")
   match_anf_aliased/0 =
     (function {nlocal = 0} r/12[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/4 =a? (field_imm 1 r/12))
           (if (%eq y/4 "") (let (*match*/2 =[value<int>] 0) (exit 21 y/4))
             (let (*match*/3 =[value<int>] 1) (exit 21 (field_imm 1 r/12)))))
        with (21 y/5)
         (let
           (r/13 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply[unyielding] aliased_use/0 r/12))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/13
             y/5)))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "match_anf_aliased"
    match_anf_aliased/0))
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
  (unique_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "unique_use")
   match_anf_unique/0 =
     (function {nlocal = 0} r/14[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (catch
         (let (y/6 =o? (field_mut 1 r/14))
           (if (%eq y/6 "") (let (*match*/4 =[value<int>] 0) (exit 29 y/6))
             (let (y/7 =o? (field_mut 1 r/14) *match*/5 =[value<int>] 1)
               (exit 29 y/7))))
        with (29 y/8)
         (let
           (r/15 =[value<(consts ()) (non_consts ([0: *, *]))>]
              (apply[unyielding] unique_use/0 r/14))
           (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*) r/15
             y/8)))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "match_anf_unique"
    match_anf_unique/0))
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
  (swap_inner/0 =
     (function {nlocal = 0}
       t/0[value<
            (consts (0))
             (non_consts ([0:
                           value<
                            (consts (0)) (non_consts ([0: *, value<int>, *]))>,
                           value<int>,
                           value<
                            (consts (0)) (non_consts ([0: *, value<int>, *]))>]))>]
       : (consts (0))
          (non_consts ([0:
                        value<
                         (consts (0)) (non_consts ([0: *, value<int>, *]))>,
                        value<int>,
                        value<
                         (consts (0)) (non_consts ([0: *, value<int>, *]))>]))
       (catch
         (if t/0
           (let (*match*/6 =a? (field_imm 0 t/0))
             (if *match*/6
               (let (*match*/7 =a? (field_imm 2 t/0))
                 (if *match*/7
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
                       (field_imm 0 *match*/6) (field_int 1 *match*/6)
                       (field_imm 0 *match*/7))
                     (field_int 1 t/0)
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
                       (field_imm 2 *match*/6) (field_int 1 *match*/7)
                       (field_imm 2 *match*/7)))
                   (exit 36)))
               (exit 36)))
           (exit 36))
        with (36) t/0)))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "swap_inner"
    swap_inner/0))
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
  (unique_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "unique_use")
   aliased_use/0 =?
     (apply[unyielding] (field_imm 0 (global Toploop!)) "aliased_use")
   match_guard/0 =
     (function {nlocal = 0} r/16[value<(consts ()) (non_consts ([0: *, *]))>]
       : (consts ())
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>, *]))
       (let (y/9 =o? (field_mut 1 r/16))
         (if (caml_string_equal y/9 "")
           (let
             (r/17 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply[unyielding] aliased_use/0 r/16))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/17 y/9))
           (let
             (y/10 =o? (field_mut 1 r/16)
              r/18 =[value<(consts ()) (non_consts ([0: *, *]))>]
                (apply[unyielding] unique_use/0 r/16))
             (makeblock 0 (value<(consts ()) (non_consts ([0: *, *]))>,*)
               r/18 y/10))))))
  (apply[unyielding] (field_imm 1 (global Toploop!)) "match_guard"
    match_guard/0))
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
