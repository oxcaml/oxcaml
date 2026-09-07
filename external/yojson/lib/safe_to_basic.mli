val to_basic : t -> Basic.t
(**
     Long integers are converted to JSON strings.

     Examples:
{v
`Intlit "12345678901234567890"  ->    `String "12345678901234567890"
v}
  *)
