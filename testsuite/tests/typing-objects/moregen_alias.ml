(* TEST
 flags = "-I ${ocamlsrcdir}/typing -I ${ocamlsrcdir}/parsing -I ${ocamlsrcdir}/utils";
 include ocamlcommon;
 expect;
*)

let env =
  let source = {|
    module M = struct
      type t = < m : u -> u >
      and u = < m : t -> t >
      type a
      type b
      type 'a p
    end
    module Alias = M
    module N = struct
      type a
      type t = < m : u -> u >
      and u = < m : t -> t >
    end
  |} in
  let _, _, _, _, _, env =
    Typemod.type_structure (Lazy.force Env.initial)
      (Parse.implementation (Lexing.from_string source))
  in
  env
;;
[%%expect{|
val env : Env.t = <abstr>
|}];;

let constr module_name name args =
  let lid =
    Longident.Ldot
      (Location.mknoloc (Longident.Lident module_name), Location.mknoloc name)
  in
  let path, _ =
    Env.lookup_type ~loc:Location.none lid env
  in
  Btype.newgenty (Types.Tconstr (path, args, ref Types.Mnil))
;;

let moregeneral left right =
  match Ctype.moregeneral ~self_check:false env false [] [] left right with
  | _ -> true
  | exception Ctype.Moregen _ -> false
;;
[%%expect{|
val constr : string -> string -> Types.type_expr list -> Types.type_expr =
  <fun>
val moregeneral : Types.type_expr -> Types.type_expr -> bool = <fun>
|}];;

(* Comparing the two spellings must not traverse the recursive object's
   methods. Such a traversal would print ikind queries for their arrow modes. *)
let result =
  let left = constr "M" "t" [] in
  let right = constr "Alias" "t" [] in
  Clflags.ikinds_debug := true;
  Fun.protect
    ~finally:(fun () -> Clflags.ikinds_debug := false)
    (fun () -> moregeneral left right, moregeneral right left)
;;
[%%expect{|
val result : bool * bool = (true, true)
|}];;

moregeneral (constr "M" "a" []) (constr "Alias" "a" []);;
[%%expect{|
- : bool = true
|}];;

moregeneral (constr "M" "a" []) (constr "Alias" "b" []);;
[%%expect{|
- : bool = false
|}];;

moregeneral (constr "M" "a" []) (constr "N" "a" []);;
[%%expect{|
- : bool = false
|}];;

(* Different paths still fall back to structural comparison. *)
moregeneral (constr "M" "t" []) (constr "N" "t" []);;
[%%expect{|
- : bool = true
|}];;

(* Parameterized types still have to compare their arguments. *)
moregeneral
  (constr "M" "p" [constr "M" "a" []])
  (constr "Alias" "p" [constr "Alias" "a" []]);;
[%%expect{|
- : bool = true
|}];;

moregeneral
  (constr "M" "p" [constr "M" "a" []])
  (constr "Alias" "p" [constr "Alias" "b" []]);;
[%%expect{|
- : bool = false
|}];;
