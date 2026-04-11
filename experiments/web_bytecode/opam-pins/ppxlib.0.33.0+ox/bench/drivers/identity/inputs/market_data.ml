(* Taken from ocaml-gemini (https://github.com/struktured/ocaml-gemini) which is
   under the MIT license:
   https://github.com/struktured/ocaml-gemini/blob/8dc095edcdc02c3090f7fd9da8cc940ecded32d6/lib/market_data.ml
*)

open Common

module Side = struct
  module Bid_ask = struct
    module T = struct
      type t = [ `Bid | `Ask ] [@@deriving sexp, enumerate]

      let to_string : [< t ] -> string = function
        | `Bid -> "bid"
        | `Ask -> "ask"
    end

    include T
    include (Json.Make (T) : Json.S with type t := t)
  end

  module Auction = struct
    module T = struct
      type t = [ `Auction ] [@@deriving sexp, enumerate]

      let to_string = function `Auction -> "auction"
    end

    include T
    include (Json.Make (T) : Json.S with type t := t)
  end

  module T = struct
    type t = [ Bid_ask.t | Auction.t ] [@@deriving sexp, enumerate]

    let to_string : [< t ] -> string = function
      | #Bid_ask.t as bid_ask -> Bid_ask.to_string bid_ask
      | #Auction.t as auction -> Auction.to_string auction
  end

  include T
  include (Json.Make (T) : Json.S with type t := t)
end

module T = struct
  let name = "marketdata"
  let version = "v1"
  let path = v1 :: [ "marketdata" ]

  type uri_args = Symbol.t [@@deriving sexp, yojson, enumerate]

  let authentication = `Public
  let default_uri_args = Some `Ethusd
  let encode_uri_args = Symbol.to_string

  type query = unit [@@deriving sexp]

  let encode_query _ = failwith "queries not supported"

  module Message_type = struct
    module T = struct
      type t = [ `Update | `Heartbeat ] [@@deriving sexp, enumerate]

      let to_string = function `Update -> "update" | `Heartbeat -> "heartbeat"
    end

    include T
    include (Json.Make (T) : Json.S with type t := t)
  end

  module Event_type = struct
    module T = struct
      type t = [ `Trade | `Change | `Auction | `Auction_open | `Block_trade ]
      [@@deriving sexp, enumerate, compare]

      let to_string = function
        | `Trade -> "trade"
        | `Change -> "change"
        | `Auction -> "auction"
        | `Auction_open -> "auction_open"
        | `Block_trade -> "block_trade"
    end

    include T
    include Comparable.Make (T)
    include (Json.Make (T) : Json.S with type t := t)
  end

  type heartbeat = unit [@@deriving sexp, of_yojson]

  (*
  let _with_common_headers (type t) ~event_id ~timestamp
      (module T : Csvfields.Csv.Csvable with type t = t) =
    let module TT = struct
      include T
      let csv_header = ["event_id";"timestamp"]@csv_header
      let row_of_t t =
        [
          (Int64.to_string event_id);
          (Timestamp.to_string timestamp)
        ] @ (row_of_t t)

      let csv_header_spec =
        [(Leaf "event_id" : Csvfields.Csv.Spec.t);Leaf "timestamp"] @ csv_header_spec

    end in
    (module TT : Csvfields.Csv.Csvable with type t = t)
*)

  module Reason = struct
    module T = struct
      type t = [ `Place | `Trade | `Cancel | `Initial ]
      [@@deriving sexp, enumerate]

      let to_string = function
        | `Place -> "place"
        | `Trade -> "trade"
        | `Cancel -> "cancel"
        | `Initial -> "initial"
    end

    include T
    include (Json.Make (T) : Json.S with type t := t)
  end

  module Change_event = struct
    module T = struct
      type t = {
        price : Decimal_string.t;
        side : Side.Bid_ask.t;
        reason : Reason.t;
        remaining : Decimal_string.t;
        delta : Decimal_string.t;
      }
      [@@deriving sexp, of_yojson, fields, csv]
    end

    module Decorated = struct
      type t = {
        event_id : Int_number.t;
        timestamp : Timestamp.t;
        price : Decimal_string.t;
        side : Side.Bid_ask.t;
        reason : Reason.t;
        remaining : Decimal_string.t;
        delta : Decimal_string.t;
      }
      [@@deriving sexp, of_yojson, fields, csv]

      let create ~event_id ~timestamp
          ({ reason; side; price; remaining; delta } : T.t) =
        { event_id; timestamp; side; reason; remaining; price; delta }
    end

    let to_decorated = Decorated.create

    include T
  end

  module Trade_event = struct
    module T = struct
      type t = {
        tid : Int_number.t;
        price : Decimal_string.t;
        amount : Decimal_string.t;
        maker_side : Side.t; [@key "makerSide"]
      }
      [@@deriving of_yojson, sexp, fields, csv]
    end

    module Decorated = struct
      type t = {
        timestamp : Timestamp.t;
        tid : Int_number.t;
        price : Decimal_string.t;
        amount : Decimal_string.t;
        maker_side : Side.t; [@key "makerSide"]
      }
      [@@deriving of_yojson, sexp, fields, csv]

      let create ~event_id ~timestamp ({ tid; price; amount; maker_side } : T.t)
          =
        match Int64.equal event_id tid with
        | true -> { timestamp; tid; price; amount; maker_side }
        | false ->
            failwith
              "logical error: event_id and trade id (tid) should be equal"
    end

    let to_decorated = Decorated.create

    include T
  end

  module Block_trade_event = struct
    module T = struct
      type t = { price : Decimal_string.t; amount : Decimal_string.t }
      [@@deriving of_yojson, sexp, fields, csv]
    end

    module Decorated = struct
      type t = {
        event_id : Int_number.t;
        timestamp : Timestamp.t;
        price : Decimal_string.t;
        amount : Decimal_string.t;
      }
      [@@deriving of_yojson, sexp, fields, csv]

      let create ~event_id ~timestamp ({ price; amount } : T.t) =
        { event_id; timestamp; price; amount }
    end

    let to_decorated = Decorated.create

    include T
  end

  module Auction_open_event = struct
    type t = {
      auction_open_ms : Timestamp.Ms.t;
      auction_time_ms : Timestamp.Ms.t;
      first_indicative_ms : Timestamp.Ms.t;
      last_cancel_time_ms : Timestamp.Ms.t;
    }
    [@@deriving sexp, of_yojson, fields, csv]
  end

  module Auction_result = struct
    module T = struct
      type t = [ `Success | `Failure ] [@@deriving sexp, enumerate]

      let to_string = function `Success -> "success" | `Failure -> "failure"
    end

    include T
    include (Json.Make (T) : Json.S with type t := t)
  end

  module Auction_indicative_price_event = struct
    type t = {
      eid : Int_number.t;
      result : Auction_result.t;
      time_ms : Timestamp.Ms.t;
      highest_bid_price : Decimal_string.t;
      lowest_ask_price : Decimal_string.t;
      collar_price : Decimal_string.t;
      indicative_price : Decimal_string.t;
      indicative_quantity : Decimal_string.t;
    }
    [@@deriving sexp, of_yojson, fields, csv]
  end

  module Auction_outcome_event = struct
    type t = {
      eid : Int_number.t;
      result : Auction_result.t;
      time_ms : Timestamp.Ms.t;
      highest_bid_price : Decimal_string.t;
      lowest_ask_price : Decimal_string.t;
      collar_price : Decimal_string.t;
      auction_price : Decimal_string.t;
      auction_quantity : Decimal_string.t;
    }
    [@@deriving sexp, of_yojson, fields, csv]
  end

  module Auction_event_type = struct
    module T = struct
      type t = [ `Auction_open | `Auction_indicative_price | `Auction_outcome ]
      [@@deriving sexp, enumerate]

      let to_string = function
        | `Auction_open -> "auction_open"
        | `Auction_indicative_price -> "auction_indicative_price"
        | `Auction_outcome -> "auction_outcome"
    end

    include T
    include (Json.Make (T) : Json.S with type t := t)
  end

  module Auction_event = struct
    type t =
      [ `Auction_open of Auction_open_event.t
      | `Auction_indicative_price of Auction_indicative_price_event.t
      | `Auction_outcome of Auction_outcome_event.t ]
    [@@deriving sexp]

    let of_yojson : Yojson.Safe.t -> (t, string) Result.t = function
      | `Assoc assoc as json -> (
          List.Assoc.find assoc ~equal:String.equal "type" |> function
          | None ->
              Result.failf "no auction event type in json payload: %s"
                (Yojson.Safe.to_string json)
          | Some event_type -> (
              Auction_event_type.of_yojson event_type |> function
              | Result.Error _ as e -> e
              | Result.Ok event_type -> (
                  let json' =
                    `Assoc (List.Assoc.remove ~equal:String.equal assoc "type")
                  in
                  match event_type with
                  | `Auction_open ->
                      Auction_open_event.of_yojson json'
                      |> Result.map ~f:(fun event -> `Auction_open event)
                  | `Auction_indicative_price ->
                      Auction_indicative_price_event.of_yojson json'
                      |> Result.map ~f:(fun event ->
                             `Auction_indicative_price event)
                  | `Auction_outcome ->
                      Auction_outcome_event.of_yojson json'
                      |> Result.map ~f:(fun event -> `Auction_outcome event))))
      | #Yojson.Safe.t as json ->
          Result.failf "expected association type in json payload: %s"
            (Yojson.Safe.to_string json)
  end

  type event =
    [ `Change of Change_event.t
    | `Trade of Trade_event.t
    | `Auction of Auction_event.t
    | `Auction_open of Auction_open_event.t
    | `Block_trade of Block_trade_event.t ]
  [@@deriving sexp]

  let event_of_yojson : Yojson.Safe.t -> (event, string) Result.t = function
    | `Assoc assoc as json -> (
        List.Assoc.find assoc ~equal:String.equal "type" |> function
        | None ->
            Result.failf "no event type in json payload: %s"
              (Yojson.Safe.to_string json)
        | Some event_type -> (
            Event_type.of_yojson event_type |> function
            | Result.Error _ as e -> e
            | Result.Ok event_type -> (
                let json' =
                  `Assoc (List.Assoc.remove ~equal:String.equal assoc "type")
                in
                match event_type with
                | `Change ->
                    Change_event.of_yojson json'
                    |> Result.map ~f:(fun event -> `Change event)
                | `Trade ->
                    Trade_event.of_yojson json'
                    |> Result.map ~f:(fun event -> `Trade event)
                | `Auction ->
                    Auction_event.of_yojson json'
                    |> Result.map ~f:(fun event -> `Auction event)
                | `Auction_open ->
                    Auction_open_event.of_yojson json'
                    |> Result.map ~f:(fun event -> `Auction_open event)
                | `Block_trade ->
                    Block_trade_event.of_yojson json'
                    |> Result.map ~f:(fun event -> `Block_trade event))))
    | #Yojson.Safe.t as json ->
        Result.failf "expected association type in json payload: %s"
          (Yojson.Safe.to_string json)

  module Update = struct
    type t = {
      event_id : Int_number.t; [@key "eventId"]
      events : event array; [@default [||]]
      timestamp : Timestamp.Sec.t option; [@default None]
      timestampms : Timestamp.Ms.t option; [@default None]
    }
    [@@deriving sexp, of_yojson]
  end

  type message = [ `Heartbeat of heartbeat | `Update of Update.t ]
  [@@deriving sexp]

  type response = { socket_sequence : Int_number.t; message : message }
  [@@deriving sexp]

  let response_of_yojson : Yojson.Safe.t -> (response, string) Result.t =
    function
    | `Assoc assoc as json -> (
        ( List.Assoc.find ~equal:String.equal assoc "socket_sequence",
          List.Assoc.find ~equal:String.equal assoc "type" )
        |> function
        | None, _ ->
            Result.failf "no sequence number in json payload: %s"
              (Yojson.Safe.to_string json)
        | _, None ->
            Result.failf "no message type in json payload: %s"
              (Yojson.Safe.to_string json)
        | Some socket_sequence, Some message_type -> (
            Result.both
              (Int_number.of_yojson socket_sequence)
              (Message_type.of_yojson message_type)
            |> function
            | Result.Error _ as e -> e
            | Result.Ok (socket_sequence, message_type) ->
                let json' =
                  `Assoc
                    ( List.Assoc.remove ~equal:String.equal assoc "type"
                    |> fun assoc ->
                      List.Assoc.remove ~equal:String.equal assoc
                        "socket_sequence" )
                in
                (match message_type with
                | `Heartbeat ->
                    heartbeat_of_yojson json'
                    |> Result.map ~f:(fun event -> `Heartbeat event)
                | `Update ->
                    Update.of_yojson json'
                    |> Result.map ~f:(fun event -> `Update event))
                |> Result.map ~f:(fun message -> { socket_sequence; message })))
    | #Yojson.Safe.t as json ->
        Result.failf
          "response_of_yojson:expected association type in json payload: %s"
          (Yojson.Safe.to_string json)

  module Csv_of_event = Ws.Csv_of_event (Event_type)

  let events_of_response (response : response) =
    let csv_of_events = Csv_of_event.empty in
    match response.message with
    | `Heartbeat _ -> csv_of_events
    | `Update (update : Update.t) ->
        let event_id = update.event_id in
        let timestamp =
          Option.(
            first_some update.timestamp update.timestamp
            |> value ~default:(Time.now ()))
        in
        Array.fold ~init:csv_of_events update.events
          ~f:(fun csv_of_events (event : event) ->
            match event with
            | `Change change ->
                Csv_of_event.add' csv_of_events `Change
                  (module Change_event.Decorated)
                  [ Change_event.to_decorated ~event_id ~timestamp change ]
            | `Trade trade ->
                Csv_of_event.add' csv_of_events `Trade
                  (module Trade_event.Decorated)
                  [ Trade_event.to_decorated ~event_id ~timestamp trade ]
            | `Auction _auction -> csv_of_events
            | `Auction_open _auction -> csv_of_events
            | `Block_trade block_trade ->
                Csv_of_event.add' csv_of_events `Block_trade
                  (module Block_trade_event.Decorated)
                  [
                    Block_trade_event.to_decorated ~event_id ~timestamp
                      block_trade;
                  ])
end

include T
include Ws.Make_no_request (T)
