(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* Test that we report unused warnings in the appropriate locations when typing
   an mli file. *)

module A : sig
  val x : int
  type a
  type t = A | B | C of { e : int }
  type r = { f : int; g : int }
  type ur = #{ i : int; j : int }
  type p = private C
  type pr = private { h : int }
  type pur = private #{ k : int }
  type pv = private [> `A ]
  exception E
  module M : sig val z : int type u = D end
  module N = M
  module type S = sig val w : int end
  module type Abs
  module I : S
  module F (X : sig type t end) : sig type t = X.t end
  module G () : sig val y : int end
  type o = ..
  type o += O
  type o += private Q
  class c : object method m : int end
  class type ct = object method n : int end
  kind_ k = value
  kind_ ka
end

(* Struct definitions via a `module type of` are used. *)
module B : module type of struct
  let x = 1
  type a
  type t = A | B | C of { e : int }
  type r = { f : int; g : int }
  type ur = #{ i : int; j : int }
  type p = private C
  type pr = private { h : int }
  type pur = private #{ k : int }
  type pv = private [> `A ]
  exception E
  module M = struct let z = 3 type u = D end
  module N = M
  module type S = sig val w : int end
  module type Abs
  module I : S = struct let w = 1 end
  module P : sig val z : int end with M = M
  module F (X : sig val q : int end) = struct let v = X.q end
  module G () = struct let y = 1 end
  type o = ..
  type o += O
  type o += private Q
  class c = object method m = 1 end
  class type ct = object method n : int end
  kind_ k = value
  kind_ ka
end

(* Used items from a struct still cause warnings. *)
module C : sig
  include module type of struct
    let shadowed_in_struct = 1 (* warning 32 *)
    let shadowed_in_struct = 2
    let shadowed_by_val = 1 (* warning 32 *)
    let used_internally = 1
    let y = used_internally
    module M = struct
      let hidden = 1 (* warning 32 *)
      let hidden = 2
    end
    open struct
      let opened = 1 (* warning 32 *)
      type opened_t (* warning 34 *)
      kind_ opened_k (* warning 191 *)
    end
  end
  val shadowed_by_val : string
end

module type T = module type of struct
  let a = 1 (* warning 32 *)
  let a = 2
  type t = K
end

module D : module type of struct
  let b = 1 (* warning 32 *)
  let b = 2
end

(* Aliases that aren't exposed and are unused. *)

module M := A (* warning 60 *)
type t := int (* warning 34 *)

(* Shadowed values are unused. *)
val x : int (* warning 32 *)
val x : string

include sig val z : int end (* warning 32 *)
val z : bool

(* Anonymous modules are not part of the signature, so items in them are
   unused. *)
module _ : sig
  val v : int (* warning 32 *)
  type t = A (* warning 34 *)
  type r = { f : int } (* warning 34 *)
  module M : sig val w : int end (* warnings 32, 60 *)
  kind_ k (* warning 191 *)
  class type ct = object end (* warning 34 *)
  include module type of struct
    let s = 1 (* warning 32 *)
  end
end

module _ : module type of struct
  let u = 1 (* warning 32 *)
end
