let poly_ id x = x

let counter = ref 0

class cell =
  object
    val mutable contents = 0

    method get = contents

    method set x = contents <- x
  end
