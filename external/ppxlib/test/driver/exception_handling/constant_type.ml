open Ppxlib

let kind = Context_free.Rule.Constant_kind.Integer

let rewriter loc s =
  Location.raise_errorf ~loc
    "A raised located error in the constant rewriting transformation." s

let rule = Context_free.Rule.constant kind 'g' rewriter;;

Driver.register_transformation ~rules:[ rule ] "constant";;

let () = Driver.standalone ()
