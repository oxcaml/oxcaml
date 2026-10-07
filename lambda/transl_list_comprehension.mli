open Lambda
open Typedtree

(** Translate list comprehensions; see the .ml file for more details *)

(** Translate a list comprehension ([Typedtree.comprehension], when it's the
    body of a [Typedtree.Texp_list_comprehension]) into Lambda.

    The only variables and types this term directly refers to are those from
    [CamlinternalComprehension] and those that come from the list comprehension
    itself.

    This function needs to translate expressions from Typedtree into Lambda, and
    so is parameterized by [Translcore.transl_exp], its [transl_ctx] argument, and
    the [loc]ation. *)
val comprehension :
  transl_exp:(transl_ctx:transl_ctx -> Lambda.layout -> expression -> lambda) ->
  transl_ctx:transl_ctx ->
  loc:scoped_location ->
  comprehension ->
  lambda
