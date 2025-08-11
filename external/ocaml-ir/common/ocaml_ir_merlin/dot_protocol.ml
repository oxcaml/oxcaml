module Directive = struct
  type include_path =
    [ `B of string
    | `S of string
    | `BH of string
    | `SH of string
    | `CMI of string
    | `CMT of string
    ]

  type no_processing_required =
    [ `EXT of string list
    | `FLG of string list
    | `STDLIB of string
    | `SUFFIX of string
    | `READER of string list
    | `EXCLUDE_QUERY_DIR
    | `UNIT_NAME_FOR of string
    ]

  module Processed = struct
    type acceptable_in_input =
      [ include_path
      | no_processing_required
      ]

    type t =
      [ acceptable_in_input
      | `ERROR_MSG of string
      ]
  end

  module Raw = struct
    type t =
      [ Processed.acceptable_in_input
      | `PKG of string list
      ]
  end
end

type directive = Directive.Processed.t
