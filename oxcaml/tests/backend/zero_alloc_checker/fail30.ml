let[@zero_alloc strict] allocate_forever (sink : int ref ref) =
  while true do
    sink := ref 42
  done
