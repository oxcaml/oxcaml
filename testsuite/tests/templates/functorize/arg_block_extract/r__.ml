module No_direct_access_to_r = struct
  module R = No_such_module
end

module R = R
