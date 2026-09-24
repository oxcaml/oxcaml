let counter =
  object%js (self)
    val mutable count = 0

    method increment = self##.count := self##.count + 1

    method get = self##.count
  end

let annotated_counter =
  object%js (self : _)
    val mutable count = 0

    method increment = self##.count := self##.count + 1

    method get = self##.count
  end
