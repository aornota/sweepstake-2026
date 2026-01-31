module Aornota.Sweepstake2026.Server.Common.Helpers

open System

let thingAsync thing = async { return thing }

let discardOk result = result |> Result.map ignore

let tupleError thing result = match result with | Ok ok -> ok |> Ok | Error error -> (thing, error) |> Error

let dateTimeOffsetUtc (year, month, day, hour, minute) = DateTime (year, month, day, hour, minute, 00, DateTimeKind.Utc) |> DateTimeOffset
