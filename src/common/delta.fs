module Aornota.Sweepstake2026.Common.Delta

open Aornota.Sweepstake2026.Common.Revision

type Delta<'a, 'b> = {
    Added : ('a * 'b) list
    Changed : ('a * 'b) list
    Removed : 'a list }

type DeltaError<'a, 'b> =
    | MissedDelta of currentRvn : Rvn * deltaRvn : Rvn
    | AddedAlreadyExist of items : ('a * 'b) list
    | ChangedDoNotExist of items : ('a * 'b) list
    | RemovedDoNotExist of keys : 'a list
