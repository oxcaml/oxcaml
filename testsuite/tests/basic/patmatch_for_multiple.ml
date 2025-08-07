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
  (*match*/289 =[value<int>] 3
   *match*/290 =[value<int>] 2
   *match*/291 =[value<int>] 1)
  (catch
    (catch
      (catch (if (!= *match*/290 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/289 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let
  (*match*/289 =[value<int>] 3
   *match*/290 =[value<int>] 2
   *match*/291 =[value<int>] 1)
  (catch (if (!= *match*/290 3) (if (!= *match*/289 1) 0 (exit 1)) (exit 1))
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
  (*match*/294 =[value<int>] 3
   *match*/295 =[value<int>] 2
   *match*/296 =[value<int>] 1)
  (catch
    (catch
      (catch
        (if (!= *match*/295 3) (exit 6)
          (let
            (x/298 =a[value<
                       (consts ())
                        (non_consts ([0: value<int>, value<int>, value<int>]))>]
               (makeblock 0 *match*/294 *match*/295 *match*/296))
            (exit 4 x/298)))
       with (6)
        (if (!= *match*/294 1) (exit 5)
          (let
            (x/297 =a[value<
                       (consts ())
                        (non_consts ([0: value<int>, value<int>, value<int>]))>]
               (makeblock 0 *match*/294 *match*/295 *match*/296))
            (exit 4 x/297))))
     with (5) 0)
   with (4 x/292[value<
                  (consts ())
                   (non_consts ([0: value<int>, value<int>, value<int>]))>])
    (seq (ignore x/292) 1)))
(let
  (*match*/294 =[value<int>] 3
   *match*/295 =[value<int>] 2
   *match*/296 =[value<int>] 1)
  (catch
    (if (!= *match*/295 3)
      (if (!= *match*/294 1) 0
        (exit 4 (makeblock 0 *match*/294 *match*/295 *match*/296)))
      (exit 4 (makeblock 0 *match*/294 *match*/295 *match*/296)))
   with (4 x/292[value<
                  (consts ())
                   (non_consts ([0: value<int>, value<int>, value<int>]))>])
    (seq (ignore x/292) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function {nlocal = 0} a/299[value<int>] b/300? : int 0)
(function {nlocal = 0} a/299[value<int>] b/300? : int 0)
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
(function {nlocal = 0} a/303[value<int>] b/304?
  : (consts ()) (non_consts ([0: value<int>, ?]))
  (let
    (p/305 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/303 b/304))
    p/305))
(function {nlocal = 0} a/303[value<int>] b/304?
  : (consts ()) (non_consts ([0: value<int>, ?])) (makeblock 0 a/303 b/304))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function {nlocal = 0} a/307[value<int>] b/308?
  : (consts ()) (non_consts ([0: value<int>, ?]))
  (let
    (p/309 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/307 b/308))
    p/309))
(function {nlocal = 0} a/307[value<int>] b/308?
  : (consts ()) (non_consts ([0: value<int>, ?])) (makeblock 0 a/307 b/308))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function {nlocal = 0} a/313[value<int>] b/314?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/315 =a[value<int>] a/313
     p/316 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/313 b/314))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/315 p/316)))
(function {nlocal = 0} a/313[value<int>] b/314?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/313 (makeblock 0 a/313 b/314)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function {nlocal = 0} a/319[value<int>] b/320?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/321 =a[value<int>] a/319
     p/322 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/319 b/320))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/321 p/322)))
(function {nlocal = 0} a/319[value<int>] b/320?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/319 (makeblock 0 a/319 b/320)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function {nlocal = 0} a/329[value<int>] b/330[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/329
    (let
      (x/331 =a[value<int>] a/329
       p/332 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/329 b/330))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/331 p/332))
    (let
      (x/333 =a[value<(consts ()) (non_consts ([0: ]))>] b/330
       p/334 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/329 b/330))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/333 p/334))))
(function {nlocal = 0} a/329[value<int>] b/330[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/329
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/329 (makeblock 0 a/329 b/330))
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      b/330 (makeblock 0 a/329 b/330))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function {nlocal = 0} a/335[value<int>] b/336[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (catch
    (if a/335
      (let
        (x/343 =a[value<int>] a/335
         p/344 =a[value<
                   (consts ()) (non_consts ([0: value<int>, value<int>]))>]
           (makeblock 0 a/335 b/336))
        (exit 10 x/343 p/344))
      (let
        (x/341 =a[value<(consts ()) (non_consts ([0: ]))>] b/336
         p/342 =a[value<
                   (consts ()) (non_consts ([0: value<int>, value<int>]))>]
           (makeblock 0 a/335 b/336))
        (exit 10 x/341 p/342)))
   with (10 x/337[value<int>] p/338[value<
                                     (consts ())
                                      (non_consts ([0: value<int>,
                                                    value<int>]))>])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      x/337 p/338)))
(function {nlocal = 0} a/335[value<int>] b/336[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (catch
    (if a/335 (exit 10 a/335 (makeblock 0 a/335 b/336))
      (exit 10 b/336 (makeblock 0 a/335 b/336)))
   with (10 x/337[value<int>] p/338[value<
                                     (consts ())
                                      (non_consts ([0: value<int>,
                                                    value<int>]))>])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      x/337 p/338)))
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
(function {nlocal = 0} a/345[value<int>] b/346[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/345
    (let
      (x/347 =a[value<int>] a/345
       _p/348 =a[value<
                  (consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/345 b/346))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/347 [0: 1 1]))
    (let
      (x/349 =a[value<int>] a/345
       p/350 =a[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
         (makeblock 0 a/345 b/346))
      (makeblock 0 (value<int>,value<
                                (consts ())
                                 (non_consts ([0: value<int>, value<int>]))>)
        x/349 p/350))))
(function {nlocal = 0} a/345[value<int>] b/346[value<int>]
  : (consts ())
     (non_consts ([0: value<int>,
                   value<
                    (consts ()) (non_consts ([0: value<int>, value<int>]))>]))
  (if a/345
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/345 [0: 1 1])
    (makeblock 0 (value<int>,value<
                              (consts ())
                               (non_consts ([0: value<int>, value<int>]))>)
      a/345 (makeblock 0 a/345 b/346))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function {nlocal = 0} a/351[value<int>] b/352?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (let
    (x/353 =a[value<int>] a/351
     p/354 =a[value<(consts ()) (non_consts ([0: value<int>, ?]))>]
       (makeblock 0 a/351 b/352))
    (makeblock 0 (value<int>,value<
                              (consts ()) (non_consts ([0: value<int>, ?]))>)
      x/353 p/354)))
(function {nlocal = 0} a/351[value<int>] b/352?
  : (consts ())
     (non_consts ([0: value<int>,
                   value<(consts ()) (non_consts ([0: value<int>, ?]))>]))
  (makeblock 0 (value<int>,value<
                            (consts ()) (non_consts ([0: value<int>, ?]))>)
    a/351 (makeblock 0 a/351 b/352)))
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
(function {nlocal = 0} a/365[value<int>]
  b/366[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (if a/365
      (if b/366 (let (p/367 =a? (field_imm 0 b/366)) p/367) (exit 12))
      (exit 12))
   with (12)
    (let
      (p/368 =a[value<
                 (consts ())
                  (non_consts ([0: value<int>,
                                value<(consts (0)) (non_consts ([0: *]))>]))>]
         (makeblock 0 a/365 b/366))
      p/368)))
(function {nlocal = 0} a/365[value<int>]
  b/366[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch (if a/365 (if b/366 (field_imm 0 b/366) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/365 b/366)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function {nlocal = 0} a/369[value<int>]
  b/370[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (catch
      (if a/369
        (if b/370 (let (p/374 =a? (field_imm 0 b/370)) (exit 13 p/374))
          (exit 14))
        (exit 14))
     with (14)
      (let
        (p/373 =a[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>]
           (makeblock 0 a/369 b/370))
        (exit 13 p/373)))
   with (13 p/371[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>])
    p/371))
(function {nlocal = 0} a/369[value<int>]
  b/370[value<
         (consts (0))
          (non_consts ([0: value<(consts ()) (non_consts ([0: *, *]))>]))>]
  : (consts ())
     (non_consts ([0: value<int>, value<(consts (0)) (non_consts ([0: *]))>]))
  (catch
    (catch
      (if a/369 (if b/370 (exit 13 (field_imm 0 b/370)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/369 b/370)))
   with (13 p/371[value<
                   (consts ())
                    (non_consts ([0: value<int>,
                                  value<(consts (0)) (non_consts ([0: *]))>]))>])
    p/371))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
