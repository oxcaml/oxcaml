(* See [test_hash_type_aliases.ml]. *)

type f64 = float#

type i64 : bits64 = int64_u

module R = struct
  type r = { x : unit }
end

module U = struct
  type u = R.r#
end
