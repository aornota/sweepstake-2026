module Aornota.Sweepstake2026.Ui.Common.JsonConverter

open Aornota.Sweepstake2026.Common.Json

open Thoth.Json

let extraCoders = // note: needed to handle unit (for some reason)
    Extra.empty
    |> Extra.withDecimal
    |> Extra.withCustom (fun _ -> Encode.nil) (fun _ _ -> Ok ())

(* Note: toJson/fromJson differ from Aornota.Sweepstake2026.Server.Common.JsonConverter because need to be inline (to keep Fable happy) - and work with string (rather than Json "wrapper")
   in order to minimize changes compared with previous use of Fable.Core.JsInterop toJson/ofJson. *)

let inline toJson<'a> value = Encode.Auto.toString<'a>(SPACE_COUNT, value, extra = extraCoders)

let inline fromJson<'a> json =
    match Decode.Auto.fromString<'a>(json, extra = extraCoders) with
    | Ok value -> value
    | Error error -> failwithf "Unable to deserialize %s -> %s" json error
