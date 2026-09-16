module X = struct
  exception Test
end

let () = raise X.Test;;
