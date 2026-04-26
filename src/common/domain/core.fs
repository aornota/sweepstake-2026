module Aornota.Sweepstake2026.Common.Domain.Core

open System

type SessionId = | SessionId of guid : Guid with static member Create () = Guid.NewGuid () |> SessionId

type Group = | GroupA | GroupB | GroupC | GroupD | GroupE | GroupF | GroupG | GroupH | GroupI | GroupJ | GroupK | GroupL

type DraftOrdinal = | DraftOrdinal of draftOrdinal : int

let groups = [ GroupA ; GroupB ; GroupC ; GroupD ; GroupE ; GroupF ; GroupG ; GroupH ; GroupI ; GroupJ ; GroupK ; GroupL ]

let groupText group =
    let groupText = match group with | GroupA -> "A" | GroupB -> "B" | GroupC -> "C" | GroupD -> "D" | GroupE -> "E" | GroupF -> "F" | GroupG -> "G" | GroupH -> "H" | GroupI -> "I" | GroupJ -> "J" | GroupK -> "K" | GroupL -> "L"
    sprintf "Group %s" groupText
