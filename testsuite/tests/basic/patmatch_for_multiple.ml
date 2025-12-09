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
(let
  (*match*/296 =[value<int>] 3
   *match*/297 =[value<int>] 2
   *match*/298 =[value<int>] 1)
  (catch
    (catch
      (catch (if (%int_notequal *match*/297 3) (exit 3) (exit 1)) with (3)
        (if (%int_notequal *match*/296 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let
  (*match*/296 =[value<int>] 3
   *match*/297 =[value<int>] 2
   *match*/298 =[value<int>] 1)
  (catch
    (if (%int_notequal *match*/297 3)
      (if (%int_notequal *match*/296 1) 0 (exit 1)) (exit 1))
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
(let
  (*match*/301 =[value<int>] 3
   *match*/302 =[value<int>] 2
   *match*/303 =[value<int>] 1)
  (catch
    (catch
      (catch
        (if (%int_notequal *match*/302 3) (exit 6)
          (let
            (x/305 =a[value<
                       (consts ())
                        (non_consts ([0: value<int>, value<int>, value<int>]))>]
               (makeblock 0 *match*/301 *match*/302 *match*/303))
            (exit 4 x/305)))
       with (6)
        (if (%int_notequal *match*/301 1) (exit 5)
          (let
            (x/304 =a[value<
                       (consts ())
                        (non_consts ([0: value<int>, value<int>, value<int>]))>]
               (makeblock 0 *match*/301 *match*/302 *match*/303))
            (exit 4 x/304))))
     with (5) 0)
   with (4 x/299[value<
                  (consts ())
                   (non_consts ([0: value<int>, value<int>, value<int>]))>])
    (seq (ignore x/299) 1)))
(let
  (*match*/301 =[value<int>] 3
   *match*/302 =[value<int>] 2
   *match*/303 =[value<int>] 1)
  (catch
    (if (%int_notequal *match*/302 3)
      (if (%int_notequal *match*/301 1) 0
        (exit 4 (makeblock 0 *match*/301 *match*/302 *match*/303)))
      (exit 4 (makeblock 0 *match*/301 *match*/302 *match*/303)))
   with (4 x/299[value<
                  (consts ())
                   (non_consts ([0: value<int>, value<int>, value<int>]))>])
    (seq (ignore x/299) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function {nlocal = 0} a/306[value<int>] b/307? : int 0)
(function {nlocal = 0} a/306[value<int>] b/307? : int 0)
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
(function {nlocal = 0} a/310[value<int>] b/311?
  : (consts ()) (non_consts ([0: value<int>, ?]))
  (let
    (p/312 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/310 b/311))
    p/312))
(function {nlocal = 0} a/310[value<int>] b/311?
  : (consts ()) (non_consts ([0: value<int>, ?])) (makeblock 0 a/310 b/311))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function {nlocal = 0} a/314[value<int>] b/315?
  : (consts ()) (non_consts ([0: value<int>, ?]))
  (let
    (p/316 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/314 b/315))
    p/316))
(function {nlocal = 0} a/314[value<int>] b/315?
  : (consts ()) (non_consts ([0: value<int>, ?])) (makeblock 0 a/314 b/315))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function {nlocal = 0} a/320[value<int>] b/321?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/322 =a[value<int>] a/320
     p/323 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/320 b/321))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/322 p/323)))
(function {nlocal = 0} a/320[value<int>] b/321?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/320 (makeblock 0 a/320 b/321)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function {nlocal = 0} a/326[value<int>] b/327?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/328 =a[value<int>] a/326
     p/329 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/326 b/327))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/328 p/329)))
(function {nlocal = 0} a/326[value<int>] b/327?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/326 (makeblock 0 a/326 b/327)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function {nlocal = 0} a/336[value<int>] b/337[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/336
    (let
      (x/338 =a[value<int>] a/336
       p/339 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/336 b/337))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/338 p/339))
    (let
      (x/340 =a[value<(consts ()) (non_consts ([0: ]))>] b/337
       p/341 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/336 b/337))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/340 p/341))))
(function {nlocal = 0} a/336[value<int>] b/337[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/336
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/336 (makeblock 0 a/336 b/337))
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      b/337 (makeblock 0 a/336 b/337))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function {nlocal = 0} a/342[value<int>] b/343[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (catch
    (if a/342
      (let
        (x/350 =a[value<int>] a/342
         p/351 =a[value<
                   (consts ()) (non_consts ([0: value<int>, value<int>]))>]
           (makeblock 0 a/342 b/343))
        (exit 10 x/350 p/351))
      (let
        (x/348 =a[value<(consts ()) (non_consts ([0: ]))>] b/343
         p/349 =a[value<
                   (consts ()) (non_consts ([0: value<int>, value<int>]))>]
           (makeblock 0 a/342 b/343))
        (exit 10 x/348 p/349)))
   with (10 x/344[value<int>] p/345[value<
                                     (consts ())
                                      (non_consts ([0: value<int>,
                                                    value<int>]))>])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      x/344 p/345)))
(function {nlocal = 0} a/342[value<int>] b/343[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (catch
    (if a/342 (exit 10 a/342 (makeblock 0 a/342 b/343))
      (exit 10 b/343 (makeblock 0 a/342 b/343)))
   with (10 x/344[value<int>] p/345[value<
                                     (consts ())
                                      (non_consts ([0: value<int>,
                                                    value<int>]))>])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      x/344 p/345)))
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
(function {nlocal = 0} a/352[value<int>] b/353[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/352
    (let
      (x/354 =a[value<int>] a/352
       _p/355 =a[value<
                  (consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/352 b/353))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/354 [0: 1 1]))
    (let
      (x/356 =a[value<int>] a/352
       p/357 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/352 b/353))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/356 p/357))))
(function {nlocal = 0} a/352[value<int>] b/353[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/352
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/352 [0: 1 1])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/352 (makeblock 0 a/352 b/353))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function {nlocal = 0} a/358[value<int>] b/359?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/360 =a[value<int>] a/358
     p/361 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/358 b/359))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/360 p/361)))
(function {nlocal = 0} a/358[value<int>] b/359?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/358 (makeblock 0 a/358 b/359)))
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
(function {nlocal = 0} a/372[value<int>]
  b/373[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (if a/372
      (if b/373 (let (p/374 =a? (field_imm 0 b/373)) p/374) (exit 12))
      (exit 12))
   with (12)
    (let
      (p/375 =a[value<
                 (consts ())
                  (non_consts ([0: value<int>,
                                value<(consts (0)) (non_consts ([0: *]))>]))>]
         (makeblock 0 a/372 b/373))
      p/375)))
(function {nlocal = 0} a/372[value<int>]
  b/373[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch (if a/372 (if b/373 (field_imm 0 b/373) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/372 b/373)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function {nlocal = 0} a/376[value<int>]
  b/377[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (catch
      (if a/376
        (if b/377 (let (p/381 =a? (field_imm 0 b/377)) (exit 13 p/381))
          (exit 14))
        (exit 14))
     with (14)
      (let
        (p/380 =a[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>]
           (makeblock 0 a/376 b/377))
        (exit 13 p/380)))
   with (13 p/378[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>])
    p/378))
(function {nlocal = 0} a/376[value<int>]
  b/377[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (catch
      (if a/376 (if b/377 (exit 13 (field_imm 0 b/377)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/376 b/377)))
   with (13 p/378[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>])
    p/378))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
