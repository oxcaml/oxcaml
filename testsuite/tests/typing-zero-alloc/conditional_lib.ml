let[@inline never] [@zero_alloc conditional] apply f x = f x
let[@inline never] [@zero_alloc conditional] apply2 f x y = f x y

let[@inline always] [@zero_alloc conditional] apply_inline f x = f x
let[@inline always] [@zero_alloc conditional] apply2_inline f x y = f x y

let[@inline never] [@zero_alloc strict] identity x = x
let[@inline never] [@zero_alloc strict] add_ints x y = x + y

let[@inline never] allocate_ref x = ref x
let[@inline never] allocate_pair x y = x, y
