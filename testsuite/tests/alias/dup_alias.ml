(* [-alias Foo Bar -alias Foo Baz]: the later alias shadows the earlier one, so
   [Foo] resolves to [Baz]. [Foo.only_baz] exists only in [Baz]. *)
let y = Foo.only_baz
