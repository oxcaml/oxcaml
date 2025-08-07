(* TEST
 flags = "-nostdlib -nopervasives -dlambda";
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
  (last_is_anys/19 =
     (function {nlocal = 0}
       param/21[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
       (catch
         (if (field_imm 0 param/21) (if (field_imm 1 param/21) (exit 1) 1)
           (if (field_imm 1 param/21) (exit 1) 2))
        with (1) 3)))
  (apply (field_imm 1 (global Toploop!)) "last_is_anys" last_is_anys/19))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/26 =
     (function {nlocal = 0}
       param/30[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
       (catch
         (if (field_imm 0 param/30) (if (field_imm 1 param/30) (exit 3) 1)
           (if (field_imm 1 param/30) (exit 3) 2))
        with (3) 3)))
  (apply (field_imm 1 (global Toploop!)) "last_is_vars" last_is_vars/26))
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
  (A/34 = (makeblock_unique 248 "A" (caml_fresh_oo_id 0))
   B/35 = (makeblock_unique 248 "B" (caml_fresh_oo_id 0))
   C/36 = (makeblock_unique 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_imm 1 (global Toploop!)) "A/34" A/34)
    (apply (field_imm 1 (global Toploop!)) "B/35" B/35)
    (apply (field_imm 1 (global Toploop!)) "C/36" C/36)))
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
  (C/36 =? (apply (field_imm 0 (global Toploop!)) "C/36")
   B/35 =? (apply (field_imm 0 (global Toploop!)) "B/35")
   A/34 =? (apply (field_imm 0 (global Toploop!)) "A/34")
   f/37 =
     (function {nlocal = 0}
       param/39[value<
                 (consts ()) (non_consts ([0: *, value<int>, value<int>]))>]
       : int
       (let (*match*/40 =a? (field_imm 0 param/39))
         (catch
           (if (== *match*/40 A/34) (if (field_imm 1 param/39) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field_imm 1 param/39)
             (if (== (field_imm 0 *match*/40) B/35) 2
               (if (== (field_imm 0 *match*/40) C/36) 3 4))
             (if (field_imm 2 param/39) 12 11))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/37))
val f : t * bool * bool -> int = <fun>
|}]
