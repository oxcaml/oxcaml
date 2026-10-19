module F (X : sig type t end) = struct
  type t = X.t list
end
