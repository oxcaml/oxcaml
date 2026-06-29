(* Parameters: P *)

module No_direct_access_to_r_impl = struct
  module R_impl = No_such_module
end

module R_impl = R_impl
