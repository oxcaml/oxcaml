export const unsupportedSamples = [
  {
    id: "backlog-comprehensions-list-squares",
    topic: "Backlog",
    label: "Comprehensions",
    filename: "comprehensions_list_squares.ml",
    mode: "run",
    source: [
      "let xs = [x * x for x = 1 to 5];;",
      "List.iter (fun x -> print_endline (string_of_int x)) xs;;",
    ].join("\n"),
    expected_kind: "diagnostic",
    expected_text: [
      "The extension \"comprehensions\" is disabled",
    ],
    browser_status: "unsupported",
    local_status: "verified",
    origin: "testsuite/tests/comprehensions/list_comprehensions_pure.ml",
    failure_reason:
      "The browser wrapper does not enable the `comprehensions` extension, so the sample is rejected before it can demonstrate the feature.",
    local_result_summary:
      "Verified with the local compiler using `-extension comprehensions`: the program prints 1, 4, 9, 16, 25.",
    next_support_guess: "needs extension enable in the browser wrapper",
  },
  {
    id: "backlog-zero-alloc-checker",
    topic: "Backlog",
    label: "Zero_alloc checker",
    filename: "zero_alloc_checker.ml",
    mode: "check",
    source: [
      "let[@zero_alloc] add b x y = if b then x + y else x",
    ].join("\n"),
    expected_kind: "success",
    expected_text: "",
    browser_status: "unsupported",
    local_status: "verified",
    origin: "jane/doc/extensions/_11-miscellaneous-extensions/zero_alloc_check.md",
    failure_reason:
      "The playground accepts `[@zero_alloc]` syntax, but it does not yet surface checker behavior well enough for the sample to teach the feature.",
    local_result_summary:
      "Verified locally: the syntax is accepted, but the current wrapper does not produce a useful zero_alloc pass/fail demonstration for the playground.",
    next_support_guess: "needs stronger zero_alloc checker exposure in the wrapper and browser diagnostics",
  },
];
