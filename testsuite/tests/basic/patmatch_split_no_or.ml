(* TEST
 flags = "-nostdlib -nopervasives -dlambda -dcanonical-ids";
 expect;
*)

(******************************************************************************)

(* Check that the extra split indeed happens when the last row is made of
   "variables" only *)

let last_is_anys = function
  | true, false -> 1
  | _, false -> 2
  | _, _ -> 3
;;
[%%expect{|
(let
<<<<<<< oxcaml
  (last_is_anys/14 =
     (function {nlocal = 0}
       param/16[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
||||||| upstream-base
  (last_is_anys/11 =
     (function param/13 : int
=======
  (last_is_anys/0 =
     (function param/0 : int
>>>>>>> upstream-incoming
       (catch
<<<<<<< oxcaml
         (if (field_imm 0 param/16) (if (field_imm 1 param/16) (exit 1) 1)
           (if (field_imm 1 param/16) (exit 1) 2))
        with (1) 3)))
  (apply (field_imm 1 (global Toploop!)) "last_is_anys" last_is_anys/14))
||||||| upstream-base
         (if (field_imm 0 param/13) (if (field_imm 1 param/13) (exit 1) 1)
           (if (field_imm 1 param/13) (exit 1) 2))
        with (1) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys/11))
=======
         (if (field_imm 0 param/0) (if (field_imm 1 param/0) (exit 2) 1)
           (if (field_imm 1 param/0) (exit 2) 2))
        with (2) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys/0))
>>>>>>> upstream-incoming
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
<<<<<<< oxcaml
  (last_is_vars/21 =
     (function {nlocal = 0}
       param/25[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
||||||| upstream-base
  (last_is_vars/18 =
     (function param/22 : int
=======
  (last_is_vars/0 =
     (function param/1 : int
>>>>>>> upstream-incoming
       (catch
<<<<<<< oxcaml
         (if (field_imm 0 param/25) (if (field_imm 1 param/25) (exit 3) 1)
           (if (field_imm 1 param/25) (exit 3) 2))
        with (3) 3)))
  (apply (field_imm 1 (global Toploop!)) "last_is_vars" last_is_vars/21))
||||||| upstream-base
         (if (field_imm 0 param/22) (if (field_imm 1 param/22) (exit 3) 1)
           (if (field_imm 1 param/22) (exit 3) 2))
        with (3) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars/18))
=======
         (if (field_imm 0 param/1) (if (field_imm 1 param/1) (exit 5) 1)
           (if (field_imm 1 param/1) (exit 5) 2))
        with (5) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars/0))
>>>>>>> upstream-incoming
val last_is_vars : bool * bool -> int = <fun>
|}]

(******************************************************************************)

(* Check that the [| _, false, true -> 12] gets raised. *)

type t = ..
type t += A | B of unit | C of bool * int;;
[%%expect{|
0
type t = ..
(let
<<<<<<< oxcaml
  (A/29 = (makeblock_unique 248 "A" (caml_fresh_oo_id 0))
   B/30 = (makeblock_unique 248 "B" (caml_fresh_oo_id 0))
   C/31 = (makeblock_unique 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_imm 1 (global Toploop!)) "A/29" A/29)
    (apply (field_imm 1 (global Toploop!)) "B/30" B/30)
    (apply (field_imm 1 (global Toploop!)) "C/31" C/31)))
||||||| upstream-base
  (A/26 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/27 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/28 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_mut 1 (global Toploop!)) "A/26" A/26)
    (apply (field_mut 1 (global Toploop!)) "B/27" B/27)
    (apply (field_mut 1 (global Toploop!)) "C/28" C/28)))
=======
  (A/0 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/0 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/0 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_mut 1 (global Toploop!)) "A/26" A/0)
    (apply (field_mut 1 (global Toploop!)) "B/27" B/0)
    (apply (field_mut 1 (global Toploop!)) "C/28" C/0)))
>>>>>>> upstream-incoming
type t += A | B of unit | C of bool * int
|}]

let f = function
  | A, true, _ -> 1
  | _, false, false -> 11
  | B _, true, _ -> 2
  | C _, true, _ -> 3
  | _, false, true -> 12
  | _ -> 4
;;
[%%expect{|
(let
<<<<<<< oxcaml
  (C/31 =? (apply (field_imm 0 (global Toploop!)) "C/31")
   B/30 =? (apply (field_imm 0 (global Toploop!)) "B/30")
   A/29 =? (apply (field_imm 0 (global Toploop!)) "A/29")
   f/32 =
     (function {nlocal = 0}
       param/34[value<
                 (consts ()) (non_consts ([0: *, value<int>, value<int>]))>]
       : int
       (let (*match*/35 =a? (field_imm 0 param/34))
||||||| upstream-base
  (C/28 = (apply (field_mut 0 (global Toploop!)) "C/28")
   B/27 = (apply (field_mut 0 (global Toploop!)) "B/27")
   A/26 = (apply (field_mut 0 (global Toploop!)) "A/26")
   f/29 =
     (function param/31 : int
       (let (*match*/32 =a (field_imm 0 param/31))
=======
  (C/0 = (apply (field_mut 0 (global Toploop!)) "C/28")
   B/0 = (apply (field_mut 0 (global Toploop!)) "B/27")
   A/0 = (apply (field_mut 0 (global Toploop!)) "A/26")
   f/0 =
     (function param/2 : int
       (let (*match*/0 =a (field_imm 0 param/2))
>>>>>>> upstream-incoming
         (catch
<<<<<<< oxcaml
           (if (%eq *match*/35 A/29) (if (field_imm 1 param/34) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field_imm 1 param/34)
             (if (%eq (field_imm 0 *match*/35) B/30) 2
               (if (%eq (field_imm 0 *match*/35) C/31) 3 4))
             (if (field_imm 2 param/34) 12 11))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/32))
||||||| upstream-base
           (if (== *match*/32 A/26) (if (field_imm 1 param/31) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field_imm 1 param/31)
             (if (== (field_imm 0 *match*/32) B/27) 2
               (if (== (field_imm 0 *match*/32) C/28) 3 4))
             (if (field_imm 2 param/31) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/29))
=======
           (if (== *match*/0 A/0) (if (field_imm 1 param/2) 1 (exit 11))
             (exit 11))
          with (11)
           (if (field_imm 1 param/2)
             (if (== (field_imm 0 *match*/0) B/0) 2
               (if (== (field_imm 0 *match*/0) C/0) 3 4))
             (if (field_imm 2 param/2) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/0))
>>>>>>> upstream-incoming
val f : t * bool * bool -> int = <fun>
|}]
