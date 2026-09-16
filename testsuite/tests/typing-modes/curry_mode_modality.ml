(* TEST
  expect;
*)

let use_portable (_ @ portable) = ()
[%%expect{|
val use_portable : 'a @ portable -> unit = <fun>
|}]

module type No_modality = sig
  val f : 'a @ portable contended -> unit -> unit
end

module No_modality : No_modality = struct
  let f _ () = ()
end
[%%expect{|
module type No_modality =
  sig val f : 'a @ portable contended -> unit -> unit end
module No_modality : No_modality
|}]

let () = use_portable (No_modality.f 42)
[%%expect{|
Line 1, characters 22-40:
1 | let () = use_portable (No_modality.f 42)
                          ^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module type Modality = sig
  val f : 'a @ portable contended -> unit -> unit @@ portable
end

module Modality : Modality = struct
  let f _ () = ()
end
[%%expect{|
module type Modality =
  sig val f : 'a @ portable contended -> unit -> unit @@ portable end
module Modality : Modality
|}]

let () = use_portable (Modality.f 42)
[%%expect{|
|}]

module type Modality_uncontended_arg = sig
  val f : 'a @ portable -> unit -> unit @@ portable
end

module Modality_uncontended_arg : Modality_uncontended_arg = struct
  let f _ () = ()
end
[%%expect{|
module type Modality_uncontended_arg =
  sig val f : 'a @ portable -> unit -> unit @@ portable end
module Modality_uncontended_arg : Modality_uncontended_arg
|}]

let () = use_portable (Modality_uncontended_arg.f 42)
[%%expect{|
Line 1, characters 22-53:
1 | let () = use_portable (Modality_uncontended_arg.f 42)
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module type Default = sig @@ portable
  val f : 'a @ portable contended -> unit -> unit
  val g : 'a @ portable contended -> unit -> unit @@ nonportable
end

module Default : Default = struct
  let f _ () = ()
  let g _ () = ()
end
[%%expect{|
module type Default =
  sig
    val f : 'a @ portable contended -> unit -> unit @@ portable
    val g : 'a @ portable contended -> unit -> unit
  end
module Default : Default
|}]

let () = use_portable (Default.f 42)
[%%expect{|
|}]

let () = use_portable (Default.g 42)
[%%expect{|
Line 1, characters 22-36:
1 | let () = use_portable (Default.g 42)
                          ^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* The curry mode of [f] is fixed when [No_modality] is translated.
   A modality applied later can't affect the already translated
   curry mode. The printer makes this visible with parentheses:
   [(unit -> unit) @@ portable]. *)
module type Module_modality = sig
  module M : No_modality @@ portable
end

module Module_modality : Module_modality = struct
  module M = No_modality
end
[%%expect{|
module type Module_modality =
  sig
    module M :
      sig val f : 'a @ portable contended -> (unit -> unit) @@ portable end
  end
module Module_modality : Module_modality
|}]

let () = use_portable Module_modality.M.f
[%%expect{|
|}]

let () = use_portable (Module_modality.M.f 42)
[%%expect{|
Line 1, characters 22-46:
1 | let () = use_portable (Module_modality.M.f 42)
                          ^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

(* Same as [Module_modality]: the curry mode of [f] is not recomputed and
   shows up as nonportable. *)
module type Module_modality_prefix = sig
  module (M @@ portable) : No_modality
end

module Module_modality_prefix : Module_modality_prefix = struct
  module M = No_modality
end
[%%expect{|
module type Module_modality_prefix =
  sig
    module M :
      sig val f : 'a @ portable contended -> (unit -> unit) @@ portable end
  end
module Module_modality_prefix : Module_modality_prefix
|}]

let () = use_portable (Module_modality_prefix.M.f 42)
[%%expect{|
Line 1, characters 22-53:
1 | let () = use_portable (Module_modality_prefix.M.f 42)
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]

module type Default_module = sig @@ portable
  module M : sig
    val f : 'a @ portable contended -> unit -> unit
  end
end

(* No_modality is not more general than Default_module. Default_module has
  a portable curry mode *)
module Default_module : Default_module = struct
  module M = No_modality
end
[%%expect{|
module type Default_module =
  sig
    module M :
      sig val f : 'a @ portable contended -> unit -> unit @@ portable end
  end
Lines 9-11, characters 41-3:
 9 | .........................................struct
10 |   module M = No_modality
11 | end
Error: Signature mismatch:
       Modules do not match:
         sig module M = No_modality end
       is not included in
         Default_module
       In module "M":
       Modules do not match:
         sig val f : 'a @ portable contended -> unit -> unit end
       is not included in
         sig val f : 'a @ portable contended -> unit -> unit @@ portable end
       In module "M":
       Values do not match:
         val f : 'a @ portable contended -> unit -> unit
       is not included in
         val f : 'a @ portable contended -> unit -> unit @@ portable
       The type "'a @ portable contended -> unit -> unit"
       is not compatible with the type
         "'a @ portable contended -> (unit -> unit) @ portable"
|}]


(* Same as [Module_modality]: [include] of a named module type reuses its
   already-translated curry-mode. *)
module type Include_modality = sig
  include No_modality @@ portable
end

module Include_modality : Include_modality = No_modality
[%%expect{|
module type Include_modality =
  sig val f : 'a @ portable contended -> (unit -> unit) @@ portable end
module Include_modality : Include_modality
|}]

let () = use_portable (Include_modality.f 42)
[%%expect{|
Line 1, characters 22-45:
1 | let () = use_portable (Include_modality.f 42)
                          ^^^^^^^^^^^^^^^^^^^^^^^
Error: This value is "nonportable" but is expected to be "portable".
|}]
