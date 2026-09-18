(* TEST
 flags = "-syntax-quotations";
 readonly_files = "use_directive_on.ml";
 toplevel;
*)

(* With -syntax-quotations, [$] is the splice token, not an operator. *)
let ( $ ) f x = f x;;

#syntax quotations off
let ( $ ) f x = f x;;

(* The directive persists across phrases... *)
succ $ 1;;

(* ... including into #use'd files, whose own directives stay local to
   them. *)
#use "use_directive_on.ml";;
succ $ 2;;
