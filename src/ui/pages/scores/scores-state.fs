module Aornota.Sweepstake2026.Ui.Pages.Scores.State

open Aornota.Sweepstake2026.Ui.Pages.Scores.Common

open Elmish

let initialize userId : State * Cmd<Input> = { CurrentScoresFilter = userId |> Sweepstaker ; LastSweepstaker = None ; LastBest = None; LastBestUnpicked = None }, Cmd.none

let private updateLast state =
    match state.CurrentScoresFilter with
    | Sweepstaker userId -> { state with LastSweepstaker = userId }
    | Best best -> { state with LastBest = best }
    | BestUnpicked best -> { state with LastBestUnpicked = best }

let transition input state =
    let state, cmd, isUserNonApiActivity =
        match input with
        | ShowSweepstaker userId -> // note: no need to check for unknown userId (should never happen)
            let state = state |> updateLast
            let state =
                match state.CurrentScoresFilter, userId with
                | Sweepstaker (Some _), None -> state
                | _, None -> { state with CurrentScoresFilter = state.LastSweepstaker |> Sweepstaker }
                | _ -> { state with CurrentScoresFilter = userId |> Sweepstaker }
            state, Cmd.none, true
        | ShowBest best ->
            let state = state |> updateLast
            let state =
                match state.CurrentScoresFilter, best with
                | Best (Some _), None -> state
                | _, None -> { state with CurrentScoresFilter = state.LastBest |> Best }
                | _ -> { state with CurrentScoresFilter = best |> Best }
            state, Cmd.none, true
        | ShowBestUnpicked best ->
            let state = state |> updateLast
            let state =
                match state.CurrentScoresFilter, best with
                | BestUnpicked (Some _), None -> state
                | _, None -> { state with CurrentScoresFilter = state.LastBestUnpicked |> BestUnpicked }
                | _ -> { state with CurrentScoresFilter = best |> BestUnpicked }
            state, Cmd.none, true
    state, cmd, isUserNonApiActivity
