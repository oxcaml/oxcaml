module F (X : sig module type T end) : sig module type S = X.T end
