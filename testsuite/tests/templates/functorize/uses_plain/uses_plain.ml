(* Parameters: P *)

(* Parameterised module that references both:

   - a non-parameterised compunit [Plain] (in [greet]'s return type), and
   - a fully-instantiated compunit [Basic-P_int] (built via [-instantiate]
     from [Basic] parameterised by P).

   Neither shows up in the cmi's [bound_globals], so neither is bundled
   by [-functorize] — both stay as global references in the bundle's
   signature. *)
let greet (_p : P.t) : Plain.t = Plain.greeting

(* Direct module alias to a non-parameterised module *)
module Plain_alias = Plain

(* Alias the fully-instantiated [Basic(P:=P_int)] and use it. *)
module Basic_int = Basic(P)(P_int) [@jane.non_erasable.instances]
let from_basic_int : Basic_int.t = Basic_int.create 0
