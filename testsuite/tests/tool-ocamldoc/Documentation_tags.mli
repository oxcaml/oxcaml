(* TEST
   ocamldoc with html;
*)

(** Test the html rendering of ocamldoc documentation tags *)

(**
 @author yes
 @param no No description
 @param neither see no description
 @deprecated since the start of time
 @return ()
 @see "Documentation_tags.mli" Self reference
 @since Now
 @before Time not implemented
*)
val heterological : unit

(**
 @raise Not_found Never
 @raise Invalid_argument Never
*)
val noop : unit
