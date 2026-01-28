(* TEST
 flags = "-drawlambda -dlambda -dcanonical-ids";
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
<<<<<<< HEAD
(let
  (*match*/285 =[value<int>] 3
   *match*/286 =[value<int>] 2
   *match*/287 =[value<int>] 1)
  (catch
    (catch
      (catch (if (%int_notequal *match*/286 3) (exit 3) (exit 1)) with (3)
        (if (%int_notequal *match*/285 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let
  (*match*/285 =[value<int>] 3
   *match*/286 =[value<int>] 2
   *match*/287 =[value<int>] 1)
  (catch
    (if (%int_notequal *match*/286 3)
      (if (%int_notequal *match*/285 1) 0 (exit 1)) (exit 1))
   with (1) 1))
=======
(let (*match*/0 = 3 *match*/1 = 2 *match*/2 = 1)
  (catch
    (catch
      (catch (if (!= *match*/1 3) (exit 4) (exit 2)) with (4)
        (if (!= *match*/0 1) (exit 3) (exit 2)))
     with (3) 0)
   with (2) 1))
(let (*match*/0 = 3 *match*/1 = 2 *match*/2 = 1)
  (catch (if (!= *match*/1 3) (if (!= *match*/0 1) 0 (exit 2)) (exit 2))
   with (2) 1))
>>>>>>> upstream/5.4
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
<<<<<<< HEAD
(let
  (*match*/290 =[value<int>] 3
   *match*/291 =[value<int>] 2
   *match*/292 =[value<int>] 1)
  (catch
    (catch
      (catch
        (if (%int_notequal *match*/291 3) (exit 6)
          (let
            (x/294 =a[value<
                       (consts ())
                        (non_consts ([0: value<int>, value<int>, value<int>]))>]
               (makeblock 0 *match*/290 *match*/291 *match*/292))
            (exit 4 x/294)))
       with (6)
        (if (%int_notequal *match*/290 1) (exit 5)
          (let
            (x/293 =a[value<
                       (consts ())
                        (non_consts ([0: value<int>, value<int>, value<int>]))>]
               (makeblock 0 *match*/290 *match*/291 *match*/292))
            (exit 4 x/293))))
     with (5) 0)
   with (4 x/288[value<
                  (consts ())
                   (non_consts ([0: value<int>, value<int>, value<int>]))>])
    (seq (ignore x/288) 1)))
(let
  (*match*/290 =[value<int>] 3
   *match*/291 =[value<int>] 2
   *match*/292 =[value<int>] 1)
  (catch
    (if (%int_notequal *match*/291 3)
      (if (%int_notequal *match*/290 1) 0
        (exit 4 (makeblock 0 *match*/290 *match*/291 *match*/292)))
      (exit 4 (makeblock 0 *match*/290 *match*/291 *match*/292)))
   with (4 x/288[value<
                  (consts ())
                   (non_consts ([0: value<int>, value<int>, value<int>]))>])
    (seq (ignore x/288) 1)))
=======
(let (*match*/3 = 3 *match*/4 = 2 *match*/5 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/4 3) (exit 8)
          (let (x/0 =a (makeblock 0 *match*/3 *match*/4 *match*/5))
            (exit 6 x/0)))
       with (8)
        (if (!= *match*/3 1) (exit 7)
          (let (x/1 =a (makeblock 0 *match*/3 *match*/4 *match*/5))
            (exit 6 x/1))))
     with (7) 0)
   with (6 x/2) (seq (ignore x/2) 1)))
(let (*match*/3 = 3 *match*/4 = 2 *match*/5 = 1)
  (catch
    (if (!= *match*/4 3)
      (if (!= *match*/3 1) 0
        (exit 6 (makeblock 0 *match*/3 *match*/4 *match*/5)))
      (exit 6 (makeblock 0 *match*/3 *match*/4 *match*/5)))
   with (6 x/2) (seq (ignore x/2) 1)))
>>>>>>> upstream/5.4
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/295[value<int>] b/296? : int 0)
(function {nlocal = 0} a/295[value<int>] b/296? : int 0)
=======
(function a/0[int] b/0 : int 0)
(function a/0[int] b/0 : int 0)
>>>>>>> upstream/5.4
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
<<<<<<< HEAD
(function {nlocal = 0} a/299[value<int>] b/300?
  : (consts ()) (non_consts ([0: value<int>, ?]))
  (let
    (p/301 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/299 b/300))
    p/301))
(function {nlocal = 0} a/299[value<int>] b/300?
  : (consts ()) (non_consts ([0: value<int>, ?])) (makeblock 0 a/299 b/300))
=======
(function a/1[int] b/1 (let (p/0 =a (makeblock 0 a/1 b/1)) p/0))
(function a/1[int] b/1 (makeblock 0 a/1 b/1))
>>>>>>> upstream/5.4
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/303[value<int>] b/304?
  : (consts ()) (non_consts ([0: value<int>, ?]))
  (let
    (p/305 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/303 b/304))
    p/305))
(function {nlocal = 0} a/303[value<int>] b/304?
  : (consts ()) (non_consts ([0: value<int>, ?])) (makeblock 0 a/303 b/304))
=======
(function a/2[int] b/2 (let (p/1 =a (makeblock 0 a/2 b/2)) p/1))
(function a/2[int] b/2 (makeblock 0 a/2 b/2))
>>>>>>> upstream/5.4
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/309[value<int>] b/310?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/311 =a[value<int>] a/309
     p/312 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/309 b/310))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/311 p/312)))
(function {nlocal = 0} a/309[value<int>] b/310?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/309 (makeblock 0 a/309 b/310)))
=======
(function a/3[int] b/3
  (let (x/3 =a[int] a/3 p/2 =a (makeblock 0 a/3 b/3))
    (makeblock 0 (int,*) x/3 p/2)))
(function a/3[int] b/3 (makeblock 0 (int,*) a/3 (makeblock 0 a/3 b/3)))
>>>>>>> upstream/5.4
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
<<<<<<< HEAD
(function {nlocal = 0} a/315[value<int>] b/316?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/317 =a[value<int>] a/315
     p/318 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/315 b/316))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/317 p/318)))
(function {nlocal = 0} a/315[value<int>] b/316?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/315 (makeblock 0 a/315 b/316)))
=======
(function a/4[int] b/4
  (let (x/4 =a[int] a/4 p/3 =a (makeblock 0 a/4 b/4))
    (makeblock 0 (int,*) x/4 p/3)))
(function a/4[int] b/4 (makeblock 0 (int,*) a/4 (makeblock 0 a/4 b/4)))
>>>>>>> upstream/5.4
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/325[value<int>] b/326[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/325
    (let
      (x/327 =a[value<int>] a/325
       p/328 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/325 b/326))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/327 p/328))
    (let
      (x/329 =a[value<(consts ()) (non_consts ([0: ]))>] b/326
       p/330 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/325 b/326))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/329 p/330))))
(function {nlocal = 0} a/325[value<int>] b/326[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/325
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/325 (makeblock 0 a/325 b/326))
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      b/326 (makeblock 0 a/325 b/326))))
=======
(function a/5[int] b/5[int]
  (if a/5
    (let (x/5 =a[int] a/5 p/4 =a (makeblock 0 a/5 b/5))
      (makeblock 0 (int,*) x/5 p/4))
    (let (x/6 =a b/5 p/5 =a (makeblock 0 a/5 b/5))
      (makeblock 0 (int,*) x/6 p/5))))
(function a/5[int] b/5[int]
  (if a/5 (makeblock 0 (int,*) a/5 (makeblock 0 a/5 b/5))
    (makeblock 0 (int,*) b/5 (makeblock 0 a/5 b/5))))
>>>>>>> upstream/5.4
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/331[value<int>] b/332[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (catch
    (if a/331
      (let
        (x/339 =a[value<int>] a/331
         p/340 =a[value<
                   (consts ()) (non_consts ([0: value<int>, value<int>]))>]
           (makeblock 0 a/331 b/332))
        (exit 10 x/339 p/340))
      (let
        (x/337 =a[value<(consts ()) (non_consts ([0: ]))>] b/332
         p/338 =a[value<
                   (consts ()) (non_consts ([0: value<int>, value<int>]))>]
           (makeblock 0 a/331 b/332))
        (exit 10 x/337 p/338)))
   with (10 x/333[value<int>] p/334[value<
                                     (consts ())
                                      (non_consts ([0: value<int>,
                                                    value<int>]))>])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      x/333 p/334)))
(function {nlocal = 0} a/331[value<int>] b/332[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (catch
    (if a/331 (exit 10 a/331 (makeblock 0 a/331 b/332))
      (exit 10 b/332 (makeblock 0 a/331 b/332)))
   with (10 x/333[value<int>] p/334[value<
                                     (consts ())
                                      (non_consts ([0: value<int>,
                                                    value<int>]))>])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      x/333 p/334)))
=======
(function a/6[int] b/6[int]
  (catch
    (if a/6
      (let (x/7 =a[int] a/6 p/6 =a (makeblock 0 a/6 b/6)) (exit 31 x/7 p/6))
      (let (x/8 =a b/6 p/7 =a (makeblock 0 a/6 b/6)) (exit 31 x/8 p/7)))
   with (31 x/9[int] p/8) (makeblock 0 (int,*) x/9 p/8)))
(function a/6[int] b/6[int]
  (catch
    (if a/6 (exit 31 a/6 (makeblock 0 a/6 b/6))
      (exit 31 b/6 (makeblock 0 a/6 b/6)))
   with (31 x/9[int] p/8) (makeblock 0 (int,*) x/9 p/8)))
>>>>>>> upstream/5.4
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
<<<<<<< HEAD
(function {nlocal = 0} a/341[value<int>] b/342[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/341
    (let
      (x/343 =a[value<int>] a/341
       _p/344 =a[value<
                  (consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/341 b/342))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/343 [0: 1 1]))
    (let
      (x/345 =a[value<int>] a/341
       p/346 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/341 b/342))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/345 p/346))))
(function {nlocal = 0} a/341[value<int>] b/342[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/341
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/341 [0: 1 1])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/341 (makeblock 0 a/341 b/342))))
=======
(function a/7[int] b/7[int]
  (if a/7
    (let (x/10 =a[int] a/7 _p/0 =a (makeblock 0 a/7 b/7))
      (makeblock 0 (int,*) x/10 [0: 1 1]))
    (let (x/11 =a[int] a/7 p/9 =a (makeblock 0 a/7 b/7))
      (makeblock 0 (int,*) x/11 p/9))))
(function a/7[int] b/7[int]
  (if a/7 (makeblock 0 (int,*) a/7 [0: 1 1])
    (makeblock 0 (int,*) a/7 (makeblock 0 a/7 b/7))))
>>>>>>> upstream/5.4
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/347[value<int>] b/348?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/349 =a[value<int>] a/347
     p/350 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/347 b/348))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/349 p/350)))
(function {nlocal = 0} a/347[value<int>] b/348?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/347 (makeblock 0 a/347 b/348)))
=======
(function a/8[int] b/8
  (let (x/12 =a[int] a/8 p/10 =a (makeblock 0 a/8 b/8))
    (makeblock 0 (int,*) x/12 p/10)))
(function a/8[int] b/8 (makeblock 0 (int,*) a/8 (makeblock 0 a/8 b/8)))
>>>>>>> upstream/5.4
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
<<<<<<< HEAD
(function {nlocal = 0} a/360[value<int>]
  b/361[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (if a/360
      (if b/361 (let (p/362 =a? (field_imm 0 b/361)) p/362) (exit 12))
      (exit 12))
   with (12)
    (let
      (p/363 =a[value<
                 (consts ())
                  (non_consts ([0: value<int>,
                                value<(consts (0)) (non_consts ([0: *]))>]))>]
         (makeblock 0 a/360 b/361))
      p/363)))
(function {nlocal = 0} a/360[value<int>]
  b/361[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch (if a/360 (if b/361 (field_imm 0 b/361) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/360 b/361)))
=======
(function a/9[int] b/9
  (catch
    (if a/9 (if b/9 (let (p/11 =a (field_imm 0 b/9)) p/11) (exit 42))
      (exit 42))
   with (42) (let (p/12 =a (makeblock 0 a/9 b/9)) p/12)))
(function a/9[int] b/9
  (catch (if a/9 (if b/9 (field_imm 0 b/9) (exit 42)) (exit 42)) with (42)
    (makeblock 0 a/9 b/9)))
>>>>>>> upstream/5.4
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
<<<<<<< HEAD
(function {nlocal = 0} a/364[value<int>]
  b/365[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (catch
      (if a/364
        (if b/365 (let (p/369 =a? (field_imm 0 b/365)) (exit 13 p/369))
          (exit 14))
        (exit 14))
     with (14)
      (let
        (p/368 =a[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>]
           (makeblock 0 a/364 b/365))
        (exit 13 p/368)))
   with (13 p/366[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>])
    p/366))
(function {nlocal = 0} a/364[value<int>]
  b/365[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (catch
      (if a/364 (if b/365 (exit 13 (field_imm 0 b/365)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/364 b/365)))
   with (13 p/366[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>])
    p/366))
=======
(function a/10[int] b/10
  (catch
    (catch
      (if a/10
        (if b/10 (let (p/13 =a (field_imm 0 b/10)) (exit 46 p/13)) (exit 47))
        (exit 47))
     with (47) (let (p/14 =a (makeblock 0 a/10 b/10)) (exit 46 p/14)))
   with (46 p/15) p/15))
(function a/10[int] b/10
  (catch
    (catch
      (if a/10 (if b/10 (exit 46 (field_imm 0 b/10)) (exit 47)) (exit 47))
     with (47) (exit 46 (makeblock 0 a/10 b/10)))
   with (46 p/15) p/15))
>>>>>>> upstream/5.4
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
