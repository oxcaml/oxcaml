type version = int * int

type version_range =
  | Only of version
  | Up_to of version
  | From of version
  | Between of version * version

(*[[%%ignore]], [[%%expect{|...|}] or [%%expect_in 5.3 {|...|}]*)
type expect_block =
  | Ignore
  | Regular
  | Versioned of (version_range * string) list

type chunk = {
  phrases : string;
  phrases_start : Lexing.position;
  expect : expect_block;
}

val split_file : file_contents:string -> Lexing.lexbuf -> chunk list
