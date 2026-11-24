val r : int option ref
val g : int @ local -> unit

module type S = module type of struct
  let f (x @ nonportable) = ((x : _ @ portable), ignore (x = Some 1))
end
