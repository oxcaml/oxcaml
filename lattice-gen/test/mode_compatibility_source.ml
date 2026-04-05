[@@@warning "-32-60"]

module type Axis_const_like = sig
  type t

  val min : t
  val max : t
  val le : t -> t -> bool
  val equal : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val print : Format.formatter -> t -> unit
  val legacy : t
end

module type Axis_like = sig
  module Const : Axis_const_like

  type const = Const.t
  type 'd t constraint 'd = 'l * 'r
  type l = (Allowance.allowed * Allowance.disallowed) t
  type r = (Allowance.disallowed * Allowance.allowed) t
  type lr = (Allowance.allowed * Allowance.allowed) t
  type simple_error = { left : const; right : const }
  type error = simple_error
  type equate_step =
    | Left_le_right
    | Right_le_left
  type equate_error = equate_step * error

  val min : lr
  val max : lr
  val legacy : lr
  val of_const : const -> ('l * 'r) t
  val to_const_exn : lr -> const
  val newvar : unit -> ('l * 'r) t
  val newvar_above : l -> ('l * 'r) t * bool
  val newvar_below : r -> ('l * 'r) t * bool
  val to_simple_error : error -> simple_error
  val print_error : Format.formatter -> error -> unit
  val print_equate_error : Format.formatter -> equate_error -> unit
  val submode : l -> r -> (unit, error) result
  val submode_exn : l -> r -> unit
  val equate : lr -> lr -> (unit, equate_error) result
  val equate_exn : lr -> lr -> unit
  val join : l list -> l
  val meet : r list -> r
  val print : ?verbose:bool -> Format_doc.formatter -> ('l * 'r) t -> unit
  val show : ?verbose:bool -> ('l * 'r) t -> string
  val zap_to_floor : l -> const
  val zap_to_ceil : r -> const
end

module type Product_axis_like = sig
  type 'a t
  type packed = P : 'a t -> packed

  val all : packed list
  val print : Format.formatter -> 'a t -> unit
end

module type Product_const_like = sig
  type 'a axis
  type t

  val min : t
  val max : t
  val le : t -> t -> bool
  val equal : t -> t -> bool
  val join : t -> t -> t
  val meet : t -> t -> t
  val print : Format.formatter -> t -> unit
  val legacy : t
  val proj : 'a axis -> t -> 'a
  val min_with : 'a axis -> 'a -> t
  val max_with : 'a axis -> 'a -> t
end

module type Product_like = sig
  module Axis : Product_axis_like
  module Const : Product_const_like with type 'a axis := 'a Axis.t

  type const = Const.t
  type 'd t constraint 'd = 'l * 'r
  type l = (Allowance.allowed * Allowance.disallowed) t
  type r = (Allowance.disallowed * Allowance.allowed) t
  type lr = (Allowance.allowed * Allowance.allowed) t
  type ('a, 'd) axis_mode constraint 'd = 'l * 'r
  type simple_error = { left : const; right : const }
  type error = simple_error
  type equate_step =
    | Left_le_right
    | Right_le_left
  type equate_error = equate_step * error

  val min : lr
  val max : lr
  val legacy : lr
  val of_const : const -> ('l * 'r) t
  val to_const_exn : lr -> const
  val newvar : unit -> ('l * 'r) t
  val newvar_above : l -> ('l * 'r) t * bool
  val newvar_below : r -> ('l * 'r) t * bool
  val to_simple_error : error -> simple_error
  val print_error : Format.formatter -> error -> unit
  val print_equate_error : Format.formatter -> equate_error -> unit
  val submode : l -> r -> (unit, error) result
  val submode_exn : l -> r -> unit
  val equate : lr -> lr -> (unit, equate_error) result
  val equate_exn : lr -> lr -> unit
  val join : l list -> l
  val meet : r list -> r
  val print : ?verbose:bool -> Format_doc.formatter -> ('l * 'r) t -> unit
  val show : ?verbose:bool -> ('l * 'r) t -> string
  val zap_to_floor : l -> const
  val zap_to_ceil : r -> const
  val proj : 'a Axis.t -> ('l * 'r) t -> ('a, 'l * 'r) axis_mode
  val min_with :
    'a Axis.t -> ('a, 'l * 'r) axis_mode -> ('l * Allowance.disallowed) t
  val max_with :
    'a Axis.t -> ('a, 'l * 'r) axis_mode -> (Allowance.disallowed * 'r) t
end

module Locality_check : Axis_like = Generated.Locality
module Locality_op_check : Axis_like = Generated.Locality_op
module Packed_axes_kind_view_check : Product_like = Generated.Packed_axes_kind_view
module Value_check : Product_like = Generated.Value
module Alloc_check : Product_like = Generated.Alloc

let _known_unchecked_mode_gaps =
  [ "hint-aware APIs and apply_hint";
    "Guts / conservative const inspection";
    "Mode decomposition into Monadic and Comonadic";
    "higher-order modality and crossing modules"
  ]
