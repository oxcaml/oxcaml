(* part of missing_code.ml *)
module F(X : sig val x : int end) = struct
  module G(Y : sig val y : int end) = struct
    module H(Z : sig val z : int end) = struct
      let w = X.x + Y.y + Z.z
    end
  end
end
[@@inline never]
