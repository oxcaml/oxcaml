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
  (last_is_anys/25 =
     (function {nlocal = 0}
       param/27[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
       (catch
         (if (field_imm 0 param/27) (if (field_imm 1 param/27) (exit 1) 1)
           (if (field_imm 1 param/27) (exit 1) 2))
        with (1) 3)))
  (apply (field_imm 1 (global Toploop!)) "last_is_anys" last_is_anys/25))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/32 =
     (function {nlocal = 0}
       param/36[value<(consts ()) (non_consts ([0: value<int>, value<int>]))>]
       : int
       (catch
         (if (field_imm 0 param/36) (if (field_imm 1 param/36) (exit 3) 1)
           (if (field_imm 1 param/36) (exit 3) 2))
        with (3) 3)))
  (apply (field_imm 1 (global Toploop!)) "last_is_vars" last_is_vars/32))
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
  (A/40 = (makeblock_unique 248 "A" (caml_fresh_oo_id 0))
   B/41 = (makeblock_unique 248 "B" (caml_fresh_oo_id 0))
   C/42 = (makeblock_unique 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_imm 1 (global Toploop!)) "A/40" A/40)
    (apply (field_imm 1 (global Toploop!)) "B/41" B/41)
    (apply (field_imm 1 (global Toploop!)) "C/42" C/42)))
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
  (C/42 =? (apply (field_imm 0 (global Toploop!)) "C/42")
   B/41 =? (apply (field_imm 0 (global Toploop!)) "B/41")
   A/40 =? (apply (field_imm 0 (global Toploop!)) "A/40")
   f/43 =
     (function {nlocal = 0}
       param/45[value<
                 (consts ()) (non_consts ([0: *, value<int>, value<int>]))>]
       : int
       (let (*match*/46 =a? (field_imm 0 param/45))
         (catch
           (if (%eq *match*/46 A/40) (if (field_imm 1 param/45) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field_imm 1 param/45)
             (if (%eq (field_imm 0 *match*/46) B/41) 2
               (if (%eq (field_imm 0 *match*/46) C/42) 3 4))
             (if (field_imm 2 param/45) 12 11))))))
  (apply (field_imm 1 (global Toploop!)) "f" f/43))
val f : t * bool * bool -> int = <fun>
|}]
