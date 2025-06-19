(* TEST
 flags = "-drawlambda -dlambda";
 expect;
*)

(* Note: the tests below contain *both* the -drawlambda and
   the -dlambda intermediate representations:
   -drawlambda is the Lambda code generated directly by the
     pattern-matching compiler; it contain "alias" bindings or static
     exits that are unused, and will be removed by simplification, or
     that are used only once, and will be inlined by simplification.
   -dlambda is the Lambda code resulting from simplification.

  The -drawlambda output more closely matches what the
  pattern-compiler produces, and the -dlambda output more closely
  matches the final generated code.

  In this test we decided to show both to notice that some allocations
  are "optimized away" during simplification (see "here flattening is
  an optimization" below).
*)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/284 =[int] 3 *match*/285 =[int] 2 *match*/286 =[int] 1)
  (catch
    (catch
      (catch (if (!= *match*/285 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/284 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/284 =[int] 3 *match*/285 =[int] 2 *match*/286 =[int] 1)
  (catch (if (!= *match*/285 3) (if (!= *match*/284 1) 0 (exit 1)) (exit 1))
   with (1) 1))
- : bool = false
|}];;

(* This tests needs to allocate the tuple to bind 'x',
   but this is only done in the branches that use it. *)
match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let (*match*/289 =[int] 3 *match*/290 =[int] 2 *match*/291 =[int] 1)
  (catch
    (catch
      (catch
        (if (!= *match*/290 3) (exit 6)
          (let
            (x/293 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/289 *match*/290 *match*/291))
            (exit 4 x/293)))
       with (6)
        (if (!= *match*/289 1) (exit 5)
          (let
            (x/292 =a[(consts ()) (non_consts ([0: [int], [int], [int]]))]
               (makeblock 0 *match*/289 *match*/290 *match*/291))
            (exit 4 x/292))))
     with (5) 0)
   with (4 x/287[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/287) 1)))
(let (*match*/289 =[int] 3 *match*/290 =[int] 2 *match*/291 =[int] 1)
  (catch
    (if (!= *match*/290 3)
      (if (!= *match*/289 1) 0
        (exit 4 (makeblock 0 *match*/289 *match*/290 *match*/291)))
      (exit 4 (makeblock 0 *match*/289 *match*/290 *match*/291)))
   with (4 x/287[(consts ()) (non_consts ([0: [int], [int], [int]]))])
    (seq (ignore x/287) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function {nlocal = 0} a/294[int] b/295 : int 0)
(function {nlocal = 0} a/294[int] b/295 : int 0)
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during half-simplification,
   then flattened) or inside an or-pattern (handled during simplification).

   We used to have a Cannot_flatten exception that would result in fairly
   different code generated in both cases, but now the compilation strategy
   is fairly similar.
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
(function {nlocal = 0} a/298[int] b/299
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/300 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/298 b/299))
                                              p/300))
(function {nlocal = 0} a/298[int] b/299
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/298 b/299))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function {nlocal = 0} a/302[int] b/303
  [(consts ()) (non_consts ([0: [int], *]))](let
                                              (p/304 =a[(consts ())
                                                        (non_consts (
                                                        [0: [int], *]))]
                                                 (makeblock 0 a/302 b/303))
                                              p/304))
(function {nlocal = 0} a/302[int] b/303
  [(consts ()) (non_consts ([0: [int], *]))](makeblock 0 a/302 b/303))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function {nlocal = 0} a/308[int] b/309
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/310 =a[int] a/308
     p/311 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/308 b/309))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/310
      p/311)))
(function {nlocal = 0} a/308[int] b/309
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/308
    (makeblock 0 a/308 b/309)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function {nlocal = 0} a/314[int] b/315
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/316 =a[int] a/314
     p/317 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/314 b/315))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/316
      p/317)))
(function {nlocal = 0} a/314[int] b/315
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/314
    (makeblock 0 a/314 b/315)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function {nlocal = 0} a/324[int] b/325[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/324
    (let
      (x/326 =a[int] a/324
       p/327 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/324 b/325))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/326
        p/327))
    (let
      (x/328 =a[(consts ()) (non_consts ([0: ]))] b/325
       p/329 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/324 b/325))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/328
        p/329))))
(function {nlocal = 0} a/324[int] b/325[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/324
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/324
      (makeblock 0 a/324 b/325))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) b/325
      (makeblock 0 a/324 b/325))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function {nlocal = 0} a/330[int] b/331[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/330
      (let
        (x/338 =a[int] a/330
         p/339 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/330 b/331))
        (exit 10 x/338 p/339))
      (let
        (x/336 =a[(consts ()) (non_consts ([0: ]))] b/331
         p/337 =a[(consts ()) (non_consts ([0: [int], [int]]))]
           (makeblock 0 a/330 b/331))
        (exit 10 x/336 p/337)))
   with (10 x/332[int] p/333[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/332
      p/333)))
(function {nlocal = 0} a/330[int] b/331[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (catch
    (if a/330 (exit 10 a/330 (makeblock 0 a/330 b/331))
      (exit 10 b/331 (makeblock 0 a/330 b/331)))
   with (10 x/332[int] p/333[(consts ()) (non_consts ([0: [int], [int]]))])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/332
      p/333)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation: the allocation is moved as an
   alias within each branch, and in the first branch it is unused and
   will be removed by simplification, so the final code
   (see the -dlambda output) will not allocate in the first branch. *)
let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
(function {nlocal = 0} a/340[int] b/341[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/340
    (let
      (x/342 =a[int] a/340
       _p/343 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/340 b/341))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/342
        [0: 1 1]))
    (let
      (x/344 =a[int] a/340
       p/345 =a[(consts ()) (non_consts ([0: [int], [int]]))]
         (makeblock 0 a/340 b/341))
      (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) x/344
        p/345))))
(function {nlocal = 0} a/340[int] b/341[int]
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], [int]]))]]))]
  (if a/340
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/340
      [0: 1 1])
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], [int]]))]) a/340
      (makeblock 0 a/340 b/341))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function {nlocal = 0} a/346[int] b/347
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (let
    (x/348 =a[int] a/346
     p/349 =a[(consts ()) (non_consts ([0: [int], *]))]
       (makeblock 0 a/346 b/347))
    (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) x/348
      p/349)))
(function {nlocal = 0} a/346[int] b/347
  [(consts ())
   (non_consts ([0: [int], [(consts ()) (non_consts ([0: [int], *]))]]))]
  (makeblock 0 (int,[(consts ()) (non_consts ([0: [int], *]))]) a/346
    (makeblock 0 a/346 b/347)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function {nlocal = 0} a/359[int]
  b/360[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/359
                                                                    (if b/360
                                                                    (let
                                                                    (p/361 =a
                                                                    (field_imm 0
                                                                    b/360))
                                                                    p/361)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (let
                                                                    (p/362 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/359
                                                                    b/360))
                                                                    p/362)))
(function {nlocal = 0} a/359[int]
  b/360[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (if a/359
                                                                    (if b/360
                                                                    (field_imm 0
                                                                    b/360)
                                                                    (exit 12))
                                                                    (exit 12))
                                                                    with (12)
                                                                    (makeblock 0
                                                                    a/359
                                                                    b/360)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function {nlocal = 0} a/363[int]
  b/364[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/363
                                                                    (if b/364
                                                                    (let
                                                                    (p/368 =a
                                                                    (field_imm 0
                                                                    b/364))
                                                                    (exit 13
                                                                    p/368))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (let
                                                                    (p/367 =a
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))]
                                                                    (makeblock 0
                                                                    a/363
                                                                    b/364))
                                                                    (exit 13
                                                                    p/367)))
                                                                    with (13 p/365
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/365))
(function {nlocal = 0} a/363[int]
  b/364[(consts (0))
        (non_consts ([0: [(consts ()) (non_consts ([0: *, *]))]]))]
  [(consts ())
   (non_consts ([0: [int], [(consts (0)) (non_consts ([0: *]))]]))](catch
                                                                    (catch
                                                                    (if a/363
                                                                    (if b/364
                                                                    (exit 13
                                                                    (field_imm 0
                                                                    b/364))
                                                                    (exit 14))
                                                                    (exit 14))
                                                                    with (14)
                                                                    (exit 13
                                                                    (makeblock 0
                                                                    a/363
                                                                    b/364)))
                                                                    with (13 p/365
                                                                    [(consts ())
                                                                    (non_consts (
                                                                    [0:
                                                                    [int],
                                                                    [(consts (0))
                                                                    (non_consts (
                                                                    [0: *]))]]))])
                                                                    p/365))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
