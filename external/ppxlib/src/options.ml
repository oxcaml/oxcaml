module Forcable_bool = struct
  type t = True | False | Force

  let arg value =
    Arg.Symbol
      ( [ "true"; "false"; "force" ],
        fun flag ->
          value :=
            match flag with
            | "true" -> True
            | "false" -> False
            | "force" -> Force
            | _ -> assert false )
end

let default_allow_unused_code_warnings : Forcable_bool.t = False
let default_allow_unused_type_warnings : Forcable_bool.t = False
let perform_checks = false

(* The checks on extensions are only to get better error messages
   since the compiler will choke on unknown extensions. We disable
   them externally to make it easier to use non ppxlib based
   rewriters with ppxlib *)
let perform_checks_on_extensions = false
let perform_locations_check = true
let fail_on_duplicate_derivers = false
let diff_command = None

(* When embedding errors, the driver carries on after a rewriter raises: the
   error takes the place of the code the rewriter was to generate, and the
   other rewriters and the checks still run. Otherwise the first exception
   stops it, and is reported with the code as it was before any rewriting.
   Drivers linking [ppxlib.runner] stop (see [Driver.stop_after_exception]). *)
let continue_after_exception = true
