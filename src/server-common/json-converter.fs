module Aornota.Sweepstake2026.Server.Common.JsonConverter

open Aornota.Sweepstake2026.Common.Json

open Thoth.Json.Net

let private extraCoders = // note: needed to handle unit (for some reason)
    Extra.empty
    |> Extra.withDecimal
    |> Extra.withCustom (fun _ -> Encode.nil) (fun _ _ -> Ok ())

// Note: toJson/fromJson differ from Aornota.Sweepstake2019.Ui.Common.JsonConverter; see notes for Ui versions for details.

let toJson<'a> value = Json (Encode.Auto.toString<'a> (SPACE_COUNT, value, extra = extraCoders))

let fromJson<'a> (Json json) =
    match Decode.Auto.fromString<'a> (json, extra = extraCoders) with
    | Ok value -> value
    | Error error -> failwithf "Unable to deserialize %s -> %s" json error
