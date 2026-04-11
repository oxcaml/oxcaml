module Result = struct
  let ( let* ) = Result.bind
  let ( let+ ) v f = Result.map f v
end
