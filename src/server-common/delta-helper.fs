module Aornota.Sweepstake2026.Server.Common.DeltaHelper

open Aornota.Sweepstake2026.Common.Delta

open System.Collections.Generic

let isEmpty delta = delta.Added.Length + delta.Changed.Length + delta.Removed.Length = 0

let delta (before:Dictionary<'a, 'b>) (after:Dictionary<'a, 'b>) =
    let added = after |> List.ofSeq |> List.choose (fun (KeyValue (key, value)) -> if key |> before.ContainsKey |> not then (key, value) |> Some else None)
    let changed = after |> List.ofSeq |> List.choose (fun (KeyValue (key, value)) ->
        if key |> before.ContainsKey then
            let existingValue = before.[key]
            if value <> existingValue then (key, value) |> Some else None
        else None)
    let removed = before |> List.ofSeq |> List.choose (fun (KeyValue (key, _)) -> if key |> after.ContainsKey |> not then key |> Some else None)
    { Added = added ; Changed = changed ; Removed = removed }
