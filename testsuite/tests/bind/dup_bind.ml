(* [-bind Foo Lib__bar -bind Foo Lib__baz]: the later one shadows the
   earlier one, so [Foo] resolves to [Lib__baz]. [Foo.only_baz] exists only in
   [Lib__baz]. *)
let y = Foo.only_baz
