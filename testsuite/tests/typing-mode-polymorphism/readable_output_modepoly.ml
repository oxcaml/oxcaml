(* TEST
 flags += "-dlambda -dno-unique-ids -extension mode_polymorphism_alpha";
 expect;
*)

(* Check that the code produced by TMC reads reasonably well. *)
let[@tail_mod_cons] rec map f = function
  | [] -> []
  | x :: xs -> f x :: map f xs
;;
[%%expect{|
(letrec
  (map
     (function {nlocal = 1} f[L]
       param[L][value<
                 (consts (0))
                  (non_consts ([0: ?,
                                value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       tail_mod_cons
       : (consts (0))
          (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
       (if param
         (let
           (block =
              (makemutable 0 (?,value<
                                 (consts (0))
                                  (non_consts ([0: ?,
                                                value<
                                                 (consts (0))
                                                  (non_consts ([0: ?, *]))>]))>)
                (apply f (field_imm 0 param)) 24029))
           (seq (apply map_dps block 1 f (field_imm 1 param)) block))
         0))
    map_dps
      (function {nlocal = 1} dst offset[value<int>] f[L]
        param[L][value<
                  (consts (0))
                   (non_consts ([0: ?,
                                 value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
        tail_mod_cons
        : (consts (0))
           (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
        (if param
          (let
            (block0_arg0 =? (apply f (field_imm 0 param))
             block =
               (makemutable 0 (?,value<
                                  (consts (0))
                                   (non_consts ([0: ?,
                                                 value<
                                                  (consts (0))
                                                   (non_consts ([0: ?, *]))>]))>)
                 block0_arg0 24029))
            (seq (setfield_ptr(heap-init)_computed dst offset block)
              (apply map_dps block 1 f (field_imm 1 param) tailcall)))
          (setfield_ptr(heap-init)_computed dst offset 0))))
  (apply (field_imm 1 (global Toploop!)) "map" map))
val map :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o.future & 'p.future & 'mm2 & global many > 'q | 'mm2 | aliased] ->
  ('a list @ [< 'mm1 & 'n > 'mm1] -> 'b list @ [< 'mm0 & global > 'mm0 | 'm]) @ [< global > 'o.future | 'p.future | monadic_to_comonadic_min('q) | nonportable] =
  <fun>
|}]

(* check that TMC works for records as well *)
type 'a cell = { hd : 'a; tl : 'a rec_list }
and 'a rec_list = 'a cell option
[%%expect{|
0
type 'a cell = { hd : 'a; tl : 'a rec_list; }
and 'a rec_list = 'a cell option
|}]

let[@tail_mod_cons] rec rec_map f = function
  | None -> None
  | Some {hd; tl} -> Some { hd = f hd; tl = rec_map f tl }
;;
[%%expect{|
(letrec
  (rec_map
     (function {nlocal = 1} f[L]
       param[L][value<(consts (0)) (non_consts ([0: ?]))>] tail_mod_cons
       : (consts (0)) (non_consts ([0: ?]))
       (if param
         (let (*match* =a? (field_imm 0 param))
           (makeblock 0 (value<
                          (consts ())
                           (non_consts ([0: *,
                                         value<
                                          (consts (0)) (non_consts ([0: ?]))>]))>)
             (let
               (block =
                  (makemutable 0 (*,value<(consts (0)) (non_consts ([0: ?]))>)
                    (apply f (field_imm 0 *match*)) 24029))
               (seq (apply rec_map_dps block 1 f (field_imm 1 *match*))
                 block))))
         0))
    rec_map_dps
      (function {nlocal = 1} dst offset[value<int>] f[L]
        param[L][value<(consts (0)) (non_consts ([0: ?]))>] tail_mod_cons
        : (consts (0)) (non_consts ([0: ?]))
        (if param
          (let
            (*match* =a? (field_imm 0 param)
             block1_arg0 =? (apply f (field_imm 0 *match*))
             block =
               (makemutable 0 (*,value<(consts (0)) (non_consts ([0: ?]))>)
                 block1_arg0 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst offset
                (makeblock 0 (value<
                               (consts ())
                                (non_consts ([0: *,
                                              value<
                                               (consts (0))
                                                (non_consts ([0: ?]))>]))>)
                  block))
              (apply rec_map_dps block 1 f (field_imm 1 *match*) tailcall)))
          (setfield_ptr(heap-init)_computed dst offset 0))))
  (apply (field_imm 1 (global Toploop!)) "rec_map" rec_map))
val rec_map :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o.future & 'p.future & 'mm2 & global many > 'q | 'mm2 | aliased] ->
  ('a rec_list @ [< 'mm1 & 'n > 'mm1] ->
   'b rec_list @ [< 'mm0 & global > 'mm0 | 'm]) @ [< global > 'o.future | 'p.future | monadic_to_comonadic_min('q) | nonportable] =
  <fun>
|}]

(* check the case where several constructors are nested;
   we want to avoid creating an intermediate destination
   for each constructor.  *)
let[@tail_mod_cons] rec trip = function
  | [] -> []
  | x :: xs -> (x, 0) :: (x, 1) :: (x, 2) :: trip xs
;;
[%%expect{|
(letrec
  (trip
     (function {nlocal = 0}
       param[value<
              (consts (0))
               (non_consts ([0: ?,
                             value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       tail_mod_cons
       : (consts (0))
          (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
       (if param
         (let (x =a? (field_imm 0 param))
           (makeblock 0 (value<(consts ()) (non_consts ([0: ?, value<int>]))>,
             value<
              (consts (0))
               (non_consts ([0: ?,
                             value<(consts (0)) (non_consts ([0: ?, *]))>]))>)
             (makeblock 0 (?,value<int>) x 0)
             (makeblock 0 (value<
                            (consts ()) (non_consts ([0: ?, value<int>]))>,
               value<
                (consts (0))
                 (non_consts ([0: ?,
                               value<(consts (0)) (non_consts ([0: ?, *]))>]))>)
               (makeblock 0 (?,value<int>) x 1)
               (let
                 (block =
                    (makemutable 0 (value<
                                     (consts ())
                                      (non_consts ([0: ?, value<int>]))>,
                      value<
                       (consts (0))
                        (non_consts ([0: ?,
                                      value<
                                       (consts (0)) (non_consts ([0: ?, *]))>]))>)
                      (makeblock 0 (?,value<int>) x 2) 24029))
                 (seq (apply trip_dps block 1 (field_imm 1 param)) block)))))
         0))
    trip_dps
      (function {nlocal = 0} dst offset[value<int>]
        param[value<
               (consts (0))
                (non_consts ([0: ?,
                              value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
        tail_mod_cons
        : (consts (0))
           (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
        (if param
          (let
            (x =a? (field_imm 0 param)
             block0_arg0 =? (makeblock 0 (?,value<int>) x 0)
             block1_arg0 =? (makeblock 0 (?,value<int>) x 1)
             block2_arg0 =? (makeblock 0 (?,value<int>) x 2)
             block =
               (makemutable 0 (value<
                                (consts ()) (non_consts ([0: ?, value<int>]))>,
                 value<
                  (consts (0))
                   (non_consts ([0: ?,
                                 value<(consts (0)) (non_consts ([0: ?, *]))>]))>)
                 block2_arg0 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst offset
                (makeblock 0 (value<
                               (consts ()) (non_consts ([0: ?, value<int>]))>,
                  value<
                   (consts (0))
                    (non_consts ([0: ?,
                                  value<
                                   (consts (0)) (non_consts ([0: ?, *]))>]))>)
                  block0_arg0
                  (makeblock 0 (value<
                                 (consts ())
                                  (non_consts ([0: ?, value<int>]))>,
                    value<
                     (consts (0))
                      (non_consts ([0: ?,
                                    value<
                                     (consts (0)) (non_consts ([0: ?, *]))>]))>)
                    block1_arg0 block)))
              (apply trip_dps block 1 (field_imm 1 param) tailcall)))
          (setfield_ptr(heap-init)_computed dst offset 0))))
  (apply (field_imm 1 (global Toploop!)) "trip" trip))
val trip :
  'a list @ [< 'n & 'o & global many > 'o] ->
  ('a * int) list @ [< 'm & global > 'm | 'n | aliased] = <fun>
|}]

(* check nested-constructors whose arguments
   are effectful: they need to be let-bound appropriately
   (ideally, only in the DPS version) *)
let[@tail_mod_cons] rec effects f = function
  | [] -> []
  | (x, y) :: xs -> f x :: f y :: effects f xs
;;
[%%expect{|
(letrec
  (effects
     (function {nlocal = 1} f[L]
       param[L][value<
                 (consts (0))
                  (non_consts ([0: ?,
                                value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       tail_mod_cons
       : (consts (0))
          (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
       (if param
         (let (*match* =a? (field_imm 0 param))
           (makeblock 0 (?,value<
                            (consts (0))
                             (non_consts ([0: ?,
                                           value<
                                            (consts (0))
                                             (non_consts ([0: ?, *]))>]))>)
             (apply f (field_imm 0 *match*))
             (let
               (block =
                  (makemutable 0 (?,value<
                                     (consts (0))
                                      (non_consts ([0: ?,
                                                    value<
                                                     (consts (0))
                                                      (non_consts ([0: ?, *]))>]))>)
                    (apply f (field_imm 1 *match*)) 24029))
               (seq (apply effects_dps block 1 f (field_imm 1 param)) block))))
         0))
    effects_dps
      (function {nlocal = 1} dst offset[value<int>] f[L]
        param[L][value<
                  (consts (0))
                   (non_consts ([0: ?,
                                 value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
        tail_mod_cons
        : (consts (0))
           (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
        (if param
          (let
            (*match* =a? (field_imm 0 param)
             block0_arg0 =? (apply f (field_imm 0 *match*))
             block1_arg0 =? (apply f (field_imm 1 *match*))
             block =
               (makemutable 0 (?,value<
                                  (consts (0))
                                   (non_consts ([0: ?,
                                                 value<
                                                  (consts (0))
                                                   (non_consts ([0: ?, *]))>]))>)
                 block1_arg0 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst offset
                (makeblock 0 (?,value<
                                 (consts (0))
                                  (non_consts ([0: ?,
                                                value<
                                                 (consts (0))
                                                  (non_consts ([0: ?, *]))>]))>)
                  block0_arg0 block))
              (apply effects_dps block 1 f (field_imm 1 param) tailcall)))
          (setfield_ptr(heap-init)_computed dst offset 0))))
  (apply (field_imm 1 (global Toploop!)) "effects" effects))
val effects :
  ('a @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o.future & 'p.future & 'mm2 & global many > 'q | 'mm2 | aliased] ->
  (('a * 'a) list @ [< 'mm1 & 'n > 'mm1] ->
   'b list @ [< 'mm0 & global > 'mm0 | 'm]) @ [< global > 'o.future | 'p.future | monadic_to_comonadic_min('q) | nonportable] =
  <fun>
|}]

(* Check the case where several constructors
   are nested across a duplicating context: the [f None ::]
   part should not be duplicated in each branch. *)
let[@tail_mod_cons] rec map_stutter f xs =
  f None :: (
    match xs with
    | [] -> []
    | x :: xs -> f (Some x) :: map_stutter f xs
  )
;;
[%%expect{|
(letrec
  (map_stutter
     (function {nlocal = 1} f[L]
       xs[L][value<
              (consts (0))
               (non_consts ([0: ?,
                             value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
       tail_mod_cons
       : (consts (0))
          (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
       (makeblock 0 (?,value<
                        (consts (0))
                         (non_consts ([0: ?,
                                       value<
                                        (consts (0)) (non_consts ([0: ?, *]))>]))>)
         (apply f 0)
         (if xs
           (let
             (block =
                (makemutable 0 (?,value<
                                   (consts (0))
                                    (non_consts ([0: ?,
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: ?, *]))>]))>)
                  (apply f (makeblock 0 (field_imm 0 xs))) 24029))
             (seq (apply map_stutter_dps block 1 f (field_imm 1 xs)) block))
           0)))
    map_stutter_dps
      (function {nlocal = 1} dst offset[value<int>] f[L]
        xs[L][value<
               (consts (0))
                (non_consts ([0: ?,
                              value<(consts (0)) (non_consts ([0: ?, *]))>]))>]
        tail_mod_cons
        : (consts (0))
           (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
        (let
          (block0_arg0 =? (apply f 0)
           block =
             (makemutable 0 (?,value<
                                (consts (0))
                                 (non_consts ([0: ?,
                                               value<
                                                (consts (0))
                                                 (non_consts ([0: ?, *]))>]))>)
               block0_arg0 24029))
          (seq (setfield_ptr(heap-init)_computed dst offset block)
            (if xs
              (let
                (block0_arg0 =? (apply f (makeblock 0 (field_imm 0 xs)))
                 block =
                   (makemutable 0 (?,value<
                                      (consts (0))
                                       (non_consts ([0: ?,
                                                     value<
                                                      (consts (0))
                                                       (non_consts ([0: ?, *]))>]))>)
                     block0_arg0 24029))
                (seq (setfield_ptr(heap-init)_computed block 1 block)
                  (apply map_stutter_dps block 1 f (field_imm 1 xs) tailcall)))
              (setfield_ptr(heap-init)_computed block 1 0))))))
  (apply (field_imm 1 (global Toploop!)) "map_stutter" map_stutter))
val map_stutter :
  ('a option @ [> 'n] -> 'b @ [< 'm & global]) @ [< 'o.future & 'p.future & 'mm2 & global many > 'q | 'mm2 | aliased] ->
  ('a list @ [< 'mm1 & 'n > 'mm1] -> 'b list @ [< 'mm0 & global > 'mm0 | 'm]) @ [< global > 'o.future | 'p.future | monadic_to_comonadic_min('q) | nonportable] =
  <fun>
|}]

(* Check the case where several constructors
   are nested across a non-duplicating context;
   the [f None :: .] part can be delayed below the let..in,
   buts it expression argument must be let-bound
   before the let..in is evaluated. *)
type 'a stream = { hd : 'a; tl : unit -> 'a stream }
let[@tail_mod_cons] rec smap_stutter f xs n =
  if n = 0 then []
  else f None :: (
    let v = f (Some xs.hd) in
    v :: smap_stutter f (xs.tl ()) (n - 1)
  )
;;
[%%expect{|
0
type 'a stream = { hd : 'a; tl : unit -> 'a stream; }
(letrec
  (smap_stutter
     (function {nlocal = 1} f[L]
       xs[L][value<(consts ()) (non_consts ([0: *, *]))>] n[L][value<int>]
       tail_mod_cons
       : (consts (0))
          (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
       (if (%eq n 0) 0
         (makeblock 0 (?,value<
                          (consts (0))
                           (non_consts ([0: ?,
                                         value<
                                          (consts (0))
                                           (non_consts ([0: ?, *]))>]))>)
           (apply f 0)
           (let
             (v =? (apply f (makeblock 0 (*) (field_imm 0 xs)))
              block =
                (makemutable 0 (?,value<
                                   (consts (0))
                                    (non_consts ([0: ?,
                                                  value<
                                                   (consts (0))
                                                    (non_consts ([0: ?, *]))>]))>)
                  v 24029))
             (seq
               (apply smap_stutter_dps block 1 f (apply (field_imm 1 xs) 0)
                 (%int_sub n 1))
               block)))))
    smap_stutter_dps
      (function {nlocal = 1} dst offset[value<int>] f[L]
        xs[L][value<(consts ()) (non_consts ([0: *, *]))>] n[L][value<int>]
        tail_mod_cons
        : (consts (0))
           (non_consts ([0: ?, value<(consts (0)) (non_consts ([0: ?, *]))>]))
        (if (%eq n 0) (setfield_ptr(heap-init)_computed dst offset 0)
          (let
            (block0_arg0 =? (apply f 0)
             v =? (apply f (makeblock 0 (*) (field_imm 0 xs)))
             block =
               (makemutable 0 (?,value<
                                  (consts (0))
                                   (non_consts ([0: ?,
                                                 value<
                                                  (consts (0))
                                                   (non_consts ([0: ?, *]))>]))>)
                 v 24029))
            (seq
              (setfield_ptr(heap-init)_computed dst offset
                (makeblock 0 (?,value<
                                 (consts (0))
                                  (non_consts ([0: ?,
                                                value<
                                                 (consts (0))
                                                  (non_consts ([0: ?, *]))>]))>)
                  block0_arg0 block))
              (apply smap_stutter_dps block 1 f (apply (field_imm 1 xs) 0)
                (%int_sub n 1) tailcall))))))
  (apply (field_imm 1 (global Toploop!)) "smap_stutter" smap_stutter))
val smap_stutter :
  ('a option @ [> 'n | aliased nonportable] -> 'b @ [< 'm & global]) @ [< 'p.future & 'q.future & 'mm4 & global many > 'mm0 | 'mm4 | aliased] ->
  ('a stream @ [< 'mm1.future & 'mm2.future & 'n & global > aliased nonportable] ->
   (int @ [< many uncontended] -> 'b list @ [< 'mm3 & global > 'mm3 | 'm]) @ [< global > 'mm1.future | 'mm2.future | 'o.future | nonportable]) @ [< 'o.future & global > 'p.future | 'q.future | monadic_to_comonadic_min('mm0) | nonportable] =
  <fun>
|}]
