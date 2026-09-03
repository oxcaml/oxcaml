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
