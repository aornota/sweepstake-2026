module Aornota.Sweepstake2026.Common.Revision

type Rvn = | Rvn of rvn : int

let initialRvn = Rvn 1

let incrementRvn (Rvn rvn) = Rvn (rvn + 1)

let validateNextRvn (currentRvn:Rvn option) (Rvn nextRvn) =
    match currentRvn, nextRvn with
    | None, nextRvn when nextRvn = 1 -> true
    | Some (Rvn currentRvn), nextRvn when currentRvn + 1 = nextRvn -> true
    | _ -> false
