let json_value =
  `Assoc
    [
      ("null", `Null);
      ("bool", `Bool true);
      ("int", `Int 0);
      ("intlit", `Intlit "10000000000000000000");
      ("float", `Float 0.);
      ("string", `String "string");
      ("list", `List [ `Int 0; `Int 1; `Int 2 ]);
      ("assoc", `Assoc [ ("value", `Int 42) ]);
    ]

let crlf = "\r\n"

let snippets =
  [
    "{";
    {|"null":null,|};
    {|"bool":true,|};
    {|"int":0,|};
    {|"intlit":10000000000000000000,|};
    {|"float":0.0,|};
    {|"string":"string",|};
    {|"list":[0,1,2],|};
    {|"assoc":{"value":42}|};
    "}";
  ]

let json_string = String.concat "" snippets
let json_string_crlf = String.concat crlf snippets
let unquoted_json = {|{foo: null}|}
let unquoted_value = `Assoc [ ("foo", `Null) ]
let json_string_newline = json_string ^ "\n"
