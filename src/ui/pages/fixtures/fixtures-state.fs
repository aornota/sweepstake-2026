module Aornota.Sweepstake2026.Ui.Pages.Fixtures.State

open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.JsonConverter
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Common.Toasts
open Aornota.Sweepstake2026.Ui.Pages.Fixtures.Common
open Aornota.Sweepstake2026.Ui.Shared

open Elmish

open System

let initialize () : State * Cmd<Input> =
    { CurrentFixturesFilter = AllFixtures ; LastGroup = None ; ConfirmParticipantState = None ; AddMatchEventState = None ; RemoveMatchEventState = None }, Cmd.none

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private handleConfirmParticipantCmdResult (result:Result<Unconfirmed, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.ConfirmParticipantState with
    | Some confirmParticipantState ->
        match confirmParticipantState.ConfirmParticipantStatus with
        | Some ConfirmParticipantPending ->
            match result with
            | Ok unconfirmed ->
                let unconfirmedText = unconfirmed |> unconfirmedText
                { state with ConfirmParticipantState = None }, sprintf "<strong>%s</strong> has been confirmed" unconfirmedText |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ConfirmParticipantCmdResult error -> %A" error) (error |> cmdErrorText)
                let confirmParticipantState = { confirmParticipantState with ConfirmParticipantStatus = errorText |> ConfirmParticipantFailed |> Some }
                { state with ConfirmParticipantState = confirmParticipantState |> Some }, "Unable to confirm participant" |> errorToastCmd
        | Some (ConfirmParticipantFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ConfirmParticipantCmdResult when ConfirmParticipantStatus is not ConfirmParticipantPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ConfirmParticipantCmdResult when ConfirmParticipantState is None -> %A" result)

let private handleAddMatchEventCmdResult (result:Result<MatchEvent, AuthCmdError<string>>) (squadDic:SquadDic) state : State * Cmd<Input> =
    match state.AddMatchEventState with
    | Some addMatchEventState ->
        match addMatchEventState.AddMatchEventStatus with
        | Some AddMatchEventPending ->
            match result with
            | Ok matchEvent ->
                let matchEventText = matchEvent |> matchEventText squadDic
                { state with AddMatchEventState = None }, sprintf "<strong>%s</strong> has been added" matchEventText |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "AddMatchEventCmdResult error -> %A" error) (error |> cmdErrorText)
                let addMatchEventState = { addMatchEventState with AddMatchEventStatus = errorText |> AddMatchEventFailed |> Some }
                { state with AddMatchEventState = addMatchEventState |> Some }, "Unable to add match event" |> errorToastCmd
        | Some (AddMatchEventFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AddMatchEventCmdResult when AddMatchEventStatus is not AddMatchEventPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddMatchEventCmdResult when AddMatchEventState is None -> %A" result)

let private handleRemoveMatchEventCmdResult (result:Result<MatchEvent, AuthCmdError<string>>) (squadDic:SquadDic) state : State * Cmd<Input> =
    match state.RemoveMatchEventState with
    | Some removeMatchEventState ->
        match removeMatchEventState.RemoveMatchEventStatus with
        | Some RemoveMatchEventPending ->
            match result with
            | Ok matchEvent ->
                let matchEventText = matchEvent |> matchEventText squadDic
                { state with RemoveMatchEventState = None }, sprintf "<strong>%s</strong> has been removed" matchEventText |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "RemoveMatchEventCmdResult error -> %A" error) (error |> cmdErrorText)
                let removeMatchEventState = { removeMatchEventState with RemoveMatchEventStatus = errorText |> RemoveMatchEventFailed |> Some }
                { state with RemoveMatchEventState = removeMatchEventState |> Some }, "Unable to remove match event" |> errorToastCmd
        | Some (RemoveMatchEventFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected RemoveMatchEventCmdResult when RemoveMatchEventStatus is not RemoveMatchEventPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemoveMatchEventCmdResult when RemoveMatchEventState is None -> %A" result)

let private handleServerFixturesMsg serverFixturesMsg (squadDic:SquadDic) state : State * Cmd<Input> =
    match serverFixturesMsg with
    | ConfirmParticipantCmdResult result ->
        state |> handleConfirmParticipantCmdResult result
    | AddMatchEventCmdResult result ->
        state |> handleAddMatchEventCmdResult result squadDic
    | RemoveMatchEventCmdResult result ->
        state |> handleRemoveMatchEventCmdResult result squadDic

let private updateLast state = match state.CurrentFixturesFilter with | GroupFixtures group -> { state with LastGroup = group } | _ -> state

let private handleConfirmParticipantInput confirmParticipantInput (fixtureDic:FixtureDic) state : State * Cmd<Input> * bool =
    match confirmParticipantInput, state.ConfirmParticipantState with
    | SquadSelected squadIdJson, Some confirmParticipantState ->
        let squadId =
            if squadIdJson |> String.IsNullOrWhiteSpace then None
            else
                try squadIdJson |> fromJson<SquadId> |> Some
                with _ -> None
        let confirmParticipantState = { confirmParticipantState with SquadId = squadId }
        { state with ConfirmParticipantState = confirmParticipantState |> Some }, Cmd.none, true
    | ConfirmConfirmParticipant, Some confirmParticipantState ->
        let confirmParticipantState = { confirmParticipantState with ConfirmParticipantStatus = ConfirmParticipantPending |> Some }
        let fixtureId, role, squadId = confirmParticipantState.FixtureId, confirmParticipantState.Role, confirmParticipantState.SquadId
        let currentRvn = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId].Rvn else initialRvn
        match squadId with
        | Some squadId ->
            let cmd = (fixtureId, currentRvn, role, squadId) |> ConfirmParticipantCmd |> UiAuthFixturesMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ConfirmParticipantState = confirmParticipantState |> Some }, cmd, true
        | None -> state, Cmd.none, false // note: should never happen
    | CancelConfirmParticipant, Some confirmParticipantState ->
        match confirmParticipantState.ConfirmParticipantStatus with
        | Some ConfirmParticipantPending ->
            state, shouldNeverHappenCmd "Unexpected CancelConfirmParticipant when ConfirmParticipantPending", false
        | Some (ConfirmParticipantFailed _) | None ->
            { state with ConfirmParticipantState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ConfirmParticipantInput when ConfirmParticipantState is None -> %A" confirmParticipantInput), false

let private handleAddMatchEventInput addMatchEventInput (fixtureDic:FixtureDic) state : State * Cmd<Input> * bool =
    match addMatchEventInput, state.AddMatchEventState with
    | PlayerSelected playerIdJson, Some addMatchEventState ->
        let playerId =
            if playerIdJson |> String.IsNullOrWhiteSpace then None
            else
                try playerIdJson |> fromJson<PlayerId> |> Some
                with _ -> None
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | GoalEvent (_, assistedBy) -> (playerId, assistedBy) |> GoalEvent |> Some
            | OwnGoalEvent _ -> playerId |> OwnGoalEvent |> Some
            | PenaltyEvent (opponentSquadId, opponentHasCleanSheet, _, penaltyType, savedBy) ->
                (opponentSquadId, opponentHasCleanSheet, playerId, penaltyType, savedBy) |> PenaltyEvent |> Some
            | CardEvent (_, card) -> (playerId, card) |> CardEvent |> Some
            | CleanSheetEvent _ -> playerId |> CleanSheetEvent |> Some
            | ManOfTheMatchEvent _ -> playerId |> ManOfTheMatchEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected PlayerSelected when AddMatchEvent is %A" addMatchEvent), false
    | OtherPlayerSelected playerIdJson, Some addMatchEventState ->
        let assistedBy =
            if playerIdJson |> String.IsNullOrWhiteSpace then None
            else
                try playerIdJson |> fromJson<PlayerId> |> Some
                with _ -> None
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | GoalEvent (playerId, _) -> (playerId, assistedBy) |> GoalEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected OtherPlayerSelected when AddMatchEvent is %A" addMatchEvent), false
    | PenaltyTypeChanged penaltyType, Some addMatchEventState ->
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | PenaltyEvent (opponentSquadId, opponentHasCleanSheet, playerId, _, savedBy) ->
                let savedBy = match penaltyType with | PenaltyScored | PenaltyMissed -> None | PenaltySaved -> savedBy
                (opponentSquadId, opponentHasCleanSheet, playerId, penaltyType |> Some, savedBy) |> PenaltyEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected PenaltyTypeChanged when AddMatchEvent is %A" addMatchEvent), false
    | OppositionPlayerSelected playerIdJson, Some addMatchEventState ->
        let savedBy =
            if playerIdJson |> String.IsNullOrWhiteSpace then None
            else
                try playerIdJson |> fromJson<PlayerId> |> Some
                with _ -> None
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | PenaltyEvent (opponentSquadId, opponentHasCleanSheet, playerId, Some PenaltySaved, _) ->
                (opponentSquadId, opponentHasCleanSheet, playerId, Some PenaltySaved, savedBy) |> PenaltyEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected OppositionPlayerSelected when AddMatchEvent is %A" addMatchEvent), false
    | CardSelected card, Some addMatchEventState ->
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | CardEvent (playerId, _) -> (playerId, card |> Some) |> CardEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected CardSelected when AddMatchEvent is %A" addMatchEvent), false
    | HomeScoreDecremented, Some addMatchEventState ->
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | PenaltyShootoutEvent (awaySquadId, homeScore, awayScore) when homeScore > 0u -> (awaySquadId, homeScore - 1u, awayScore) |> PenaltyShootoutEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected HomeScoreDecremented when AddMatchEvent is %A" addMatchEvent), false
    | HomeScoreIncremented, Some addMatchEventState ->
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | PenaltyShootoutEvent (awaySquadId, homeScore, awayScore) -> (awaySquadId, homeScore + 1u, awayScore) |> PenaltyShootoutEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected HomeScoreIncremented when AddMatchEvent is %A" addMatchEvent), false
    | AwayScoreDecremented, Some addMatchEventState ->
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | PenaltyShootoutEvent (awaySquadId, homeScore, awayScore) when awayScore > 0u -> (awaySquadId, homeScore, awayScore - 1u) |> PenaltyShootoutEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AwayScoreDecremented when AddMatchEvent is %A" addMatchEvent), false
    | AwayScoreIncremented, Some addMatchEventState ->
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let newAddMatchEvent =
            match addMatchEvent with
            | PenaltyShootoutEvent (awaySquadId, homeScore, awayScore) -> (awaySquadId, homeScore, awayScore + 1u) |> PenaltyShootoutEvent |> Some
            | _ -> None
        match newAddMatchEvent with
        | Some newAddMatchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEvent = newAddMatchEvent }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AwayScoreIncremented when AddMatchEvent is %A" addMatchEvent), false
    | AddMatchEvent, Some addMatchEventState ->
        let fixtureId, squadId = addMatchEventState.FixtureId, addMatchEventState.SquadId
        let currentRvn = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId].Rvn else initialRvn
        let addMatchEvent = addMatchEventState.AddMatchEvent
        let matchEvent =
            match addMatchEvent with
            | GoalEvent (Some playerId, assistedBy) -> (squadId, playerId, assistedBy) |> Goal |> Some
            | OwnGoalEvent (Some playerId) -> (squadId, playerId) |> OwnGoal |> Some
            | PenaltyEvent (_, false, Some playerId, Some PenaltyScored, None) -> (squadId, playerId, Scored) |> Penalty |> Some
            | PenaltyEvent (_, _, Some playerId, Some PenaltyMissed, None) -> (squadId, playerId, Missed) |> Penalty |> Some
            | PenaltyEvent (opponentSquadId, _, Some playerId, Some PenaltySaved, Some savedBy) -> (squadId, playerId, (opponentSquadId, savedBy) |> Saved) |> Penalty |> Some
            | CardEvent (Some playerId, Some card) -> match card with | Yellow | SecondYellow -> (squadId, playerId) |> YellowCard |> Some | Red -> (squadId, playerId) |> RedCard |> Some
            | CleanSheetEvent (Some playerId) -> (squadId, playerId) |> CleanSheet |> Some
            | PenaltyShootoutEvent (_, homeScore, awayScore) when homeScore <> awayScore -> (homeScore, awayScore) |> PenaltyShootout |> Some
            | ManOfTheMatchEvent (Some playerId) -> (squadId, playerId) |> ManOfTheMatch |> Some
            | _ -> None
        match matchEvent with
        | Some matchEvent ->
            let addMatchEventState = { addMatchEventState with AddMatchEventStatus = AddMatchEventPending |> Some }
            let cmd = (fixtureId, currentRvn, matchEvent) |> AddMatchEventCmd |> UiAuthFixturesMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with AddMatchEventState = addMatchEventState |> Some }, cmd, true
        | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ConfirmAddMatchEvent when AddMatchEvent is %A" addMatchEvent), false
    | CancelAddMatchEvent, Some addMatchEventState ->
        match addMatchEventState.AddMatchEventStatus with
        | Some AddMatchEventPending ->
            state, shouldNeverHappenCmd "Unexpected CancelAddMatchEvent when AddMatchEventPending", false
        | Some (AddMatchEventFailed _) | None ->
            { state with AddMatchEventState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddMatchEventInput when AddMatchEventState is None -> %A" addMatchEventInput), false

let private handleRemoveMatchEventInput removeMatchEventInput (fixtureDic:FixtureDic) state : State * Cmd<Input> * bool =
    match removeMatchEventInput, state.RemoveMatchEventState with
    | RemoveMatchEvent, Some removeMatchEventState ->
        let removeMatchEventState = { removeMatchEventState with RemoveMatchEventStatus = RemoveMatchEventPending |> Some }
        let fixtureId, matchEventId, matchEvent = removeMatchEventState.FixtureId, removeMatchEventState.MatchEventId, removeMatchEventState.MatchEvent
        let currentRvn = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId].Rvn else initialRvn
        let cmd = (fixtureId, currentRvn, matchEventId, matchEvent) |> RemoveMatchEventCmd |> UiAuthFixturesMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with RemoveMatchEventState = removeMatchEventState |> Some }, cmd, true
    | CancelRemoveMatchEvent, Some removeMatchEventState ->
        match removeMatchEventState.RemoveMatchEventStatus with
        | Some RemoveMatchEventPending ->
            state, shouldNeverHappenCmd "Unexpected CancelRemoveMatchEvent when RemoveMatchEventPending", false
        | Some (RemoveMatchEventFailed _) | None ->
            { state with RemoveMatchEventState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemoveMatchEventInput when RemoveMatchEventState is None -> %A" removeMatchEventInput), false

let transition input (fixturesProjection:Projection<_ * FixtureDic>) (squadsProjection:Projection<_ * SquadDic>) state =
    let state, cmd, isUserNonApiActivity =
        match input, fixturesProjection, squadsProjection with
        | AddNotificationMessage _, _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerFixturesMsg serverFixturesMsg, Ready _, Ready (_, squadDic) ->
            let state, cmd = state |> handleServerFixturesMsg serverFixturesMsg squadDic
            state, cmd, false
        | ShowAllFixtures, Ready _, Ready _ ->
            let state = state |> updateLast
            { state with CurrentFixturesFilter = AllFixtures }, Cmd.none, true
        | ShowGroupFixtures group, Ready _, Ready _ ->
            let state = state |> updateLast
            let state =
                match state.CurrentFixturesFilter, group with
                | GroupFixtures (Some _), None -> state
                | _, None -> { state with CurrentFixturesFilter = state.LastGroup |> GroupFixtures }
                | _ -> { state with CurrentFixturesFilter = group |> GroupFixtures }
            state, Cmd.none, true
        | ShowKnockoutFixtures, Ready _, Ready _ ->
            let state = state |> updateLast
            { state with CurrentFixturesFilter = KnockoutFixtures }, Cmd.none, true
        | ShowFixture fixtureId, Ready _, Ready _ -> // note: no need to check for unknown fixtureId (should never happen)
            let state = state |> updateLast
            { state with CurrentFixturesFilter = fixtureId |> Fixture }, Cmd.none, true
        | ShowConfirmParticipantModal (fixtureId, role, unconfirmed), Ready _, Ready _ -> // note: no need to check for unknown fixtureId (should never happen)
            let confirmParticipantState = { FixtureId = fixtureId ; Role = role ; Unconfirmed = unconfirmed ; SquadId = None ; ConfirmParticipantStatus = None }
            { state with ConfirmParticipantState = confirmParticipantState |> Some }, Cmd.none, true
        | ConfirmParticipantInput confirmParticipantInput, Ready (_, fixtureDic), Ready _ ->
            state |> handleConfirmParticipantInput confirmParticipantInput fixtureDic
        | ShowAddGoalModal (fixtureId, squadId), Ready _, Ready _ ->
            let addMatchEvent = (None, None) |> GoalEvent
            let addMatchEventState = { FixtureId = fixtureId ; SquadId = squadId ; AddMatchEvent = addMatchEvent ; AddMatchEventStatus = None }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | ShowAddOwnGoalModal (fixtureId, squadId), Ready _, Ready _ ->
            let addMatchEvent = None |> OwnGoalEvent
            let addMatchEventState = { FixtureId = fixtureId ; SquadId = squadId ; AddMatchEvent = addMatchEvent ; AddMatchEventStatus = None }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | ShowAddPenaltyModal (fixtureId, squadId, opponentSquadId, opponentHasCleanSheet), Ready _, Ready _ ->
            let addMatchEvent = (opponentSquadId, opponentHasCleanSheet, None, None, None) |> PenaltyEvent
            let addMatchEventState = { FixtureId = fixtureId ; SquadId = squadId ; AddMatchEvent = addMatchEvent ; AddMatchEventStatus = None }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | ShowAddCardModal (fixtureId, squadId), Ready _, Ready _ ->
            let addMatchEvent = (None, None) |> CardEvent
            let addMatchEventState = { FixtureId = fixtureId ; SquadId = squadId ; AddMatchEvent = addMatchEvent ; AddMatchEventStatus = None }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | ShowAddCleanSheetModal (fixtureId, squadId), Ready _, Ready _ ->
            let addMatchEvent = None |> CleanSheetEvent
            let addMatchEventState = { FixtureId = fixtureId ; SquadId = squadId ; AddMatchEvent = addMatchEvent ; AddMatchEventStatus = None }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | ShowAddPenaltyShootoutModal (fixtureId, homeSquadId, awaySquadId), Ready _, Ready _ ->
            let addMatchEvent = (awaySquadId, 0u, 0u) |> PenaltyShootoutEvent
            let addMatchEventState = { FixtureId = fixtureId ; SquadId = homeSquadId ; AddMatchEvent = addMatchEvent ; AddMatchEventStatus = None }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | ShowAddManOfTheMatchModal (fixtureId, squadId), Ready _, Ready _ ->
            let addMatchEvent = None |> ManOfTheMatchEvent
            let addMatchEventState = { FixtureId = fixtureId ; SquadId = squadId ; AddMatchEvent = addMatchEvent ; AddMatchEventStatus = None }
            { state with AddMatchEventState = addMatchEventState |> Some }, Cmd.none, true
        | AddMatchEventInput addMatchEventInput, Ready (_, fixtureDic), Ready _ ->
            state |> handleAddMatchEventInput addMatchEventInput fixtureDic
        | ShowRemoveMatchEventModal (fixtureId, matchEventId, matchEvent), Ready _, Ready _ -> // note: no need to check for unknown fixtureId &c. (should never happen)
            let removeMatchEventState = { FixtureId = fixtureId ; MatchEventId = matchEventId ; MatchEvent = matchEvent ; RemoveMatchEventStatus = None }
            { state with RemoveMatchEventState = removeMatchEventState |> Some }, Cmd.none, true
        | RemoveMatchEventInput removeMatchEventInput, Ready (_, fixtureDic), Ready _ ->
            state |> handleRemoveMatchEventInput removeMatchEventInput fixtureDic
        | _, _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" fixturesProjection input), false
    state, cmd, isUserNonApiActivity
