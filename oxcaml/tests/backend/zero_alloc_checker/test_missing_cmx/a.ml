let[@inline never] [@local never] [@specialize never] inner_foo x = x, x + 1

let[@inline always] foo x = inner_foo x

let[@inline never] [@local never] [@specialize never] inner_bar x = x + 1

let[@inline always] bar x = inner_bar x
