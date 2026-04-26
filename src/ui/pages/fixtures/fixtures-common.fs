module Aornota.Sweepstake2026.Ui.Pages.Fixtures.Common

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Shared

type FixturesFilter =
    | AllFixtures
    | GroupFixtures of group : Group option
    | KnockoutFixtures
    | Fixture of fixtureId : FixtureId

type ConfirmParticipantInput =
    | SquadSelected of squadIdJson : string
    | ConfirmConfirmParticipant
    | CancelConfirmParticipant

type PenaltyType = | PenaltyScored | PenaltyMissed | PenaltySaved

type AddMatchEventInput =
    | PlayerSelected of playerIdJson : string
    | OtherPlayerSelected of playerIdJson : string
    | PenaltyTypeChanged of penaltyType : PenaltyType
    | OppositionPlayerSelected of playerIdJson : string
    | CardSelected of card : Card
    | HomeScoreDecremented
    | HomeScoreIncremented
    | AwayScoreDecremented
    | AwayScoreIncremented
    | AddMatchEvent
    | CancelAddMatchEvent

type RemoveMatchEventInput =
    | RemoveMatchEvent
    | CancelRemoveMatchEvent

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerFixturesMsg of serverFixturesMsg : ServerFixturesMsg
    | ShowAllFixtures
    | ShowGroupFixtures of group : Group option
    | ShowKnockoutFixtures
    | ShowFixture of fixtureId : FixtureId
    | ShowConfirmParticipantModal of fixtureId : FixtureId * role : Role * unconfirmed : Unconfirmed
    | ConfirmParticipantInput of confirmParticipantInput : ConfirmParticipantInput
    | ShowAddGoalModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddOwnGoalModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddPenaltyModal of fixtureId : FixtureId * squadId : SquadId * opponentSquadId : SquadId * opponentHasCleanSheet : bool
    | ShowAddCardModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddCleanSheetModal of fixtureId : FixtureId * squadId : SquadId
    | ShowAddPenaltyShootoutModal of fixtureId : FixtureId * homeSquadId : SquadId * awaySquadId : SquadId
    | ShowAddManOfTheMatchModal of fixtureId : FixtureId * squadId : SquadId
    | AddMatchEventInput of addMatchEventInput : AddMatchEventInput
    | ShowRemoveMatchEventModal of fixtureId : FixtureId * matchEventId : MatchEventId * matchEvent : MatchEvent
    | RemoveMatchEventInput of removeMatchEventInput : RemoveMatchEventInput

type ConfirmParticipantStatus =
    | ConfirmParticipantPending
    | ConfirmParticipantFailed of errorText : string

type ConfirmParticipantState = {
    FixtureId : FixtureId
    Role : Role
    Unconfirmed : Unconfirmed
    SquadId : SquadId option
    ConfirmParticipantStatus : ConfirmParticipantStatus option }

type AddMatchEvent =
    | GoalEvent of playerId : PlayerId option * assistedBy : PlayerId option
    | OwnGoalEvent of playerId : PlayerId option
    | PenaltyEvent of opponentSquadId : SquadId * opponentHasCleanSheet : bool * playerId : PlayerId option * penaltyType : PenaltyType option * savedBy : PlayerId option
    | CardEvent of playerId : PlayerId option * card : Card option
    | CleanSheetEvent of playerId : PlayerId option
    | PenaltyShootoutEvent of awaySquadId : SquadId * homeScore : uint32 * awayScore : uint32
    | ManOfTheMatchEvent of playerId : PlayerId option

type AddMatchEventStatus =
    | AddMatchEventPending
    | AddMatchEventFailed of errorText : string

type AddMatchEventState = {
    FixtureId : FixtureId
    SquadId : SquadId
    AddMatchEvent : AddMatchEvent
    AddMatchEventStatus : AddMatchEventStatus option }

type RemoveMatchEventStatus =
    | RemoveMatchEventPending
    | RemoveMatchEventFailed of errorText : string

type RemoveMatchEventState = {
    FixtureId : FixtureId
    MatchEventId : MatchEventId
    MatchEvent : MatchEvent
    RemoveMatchEventStatus : RemoveMatchEventStatus option }

type State = {
    CurrentFixturesFilter : FixturesFilter
    LastGroup : Group option
    ConfirmParticipantState : ConfirmParticipantState option
    AddMatchEventState : AddMatchEventState option
    RemoveMatchEventState : RemoveMatchEventState option }

let unconfirmedText unconfirmed =
    match unconfirmed with
    | Winner (Group group) -> sprintf "%s winner" (group |> groupText)
    | Winner (RoundOf32 matchNumber) -> sprintf "Match %i winner" matchNumber
    | Winner (RoundOf16 matchNumber) -> sprintf "Match %i winner" matchNumber
    | Winner (QuarterFinal quarterFinalOrdinal) -> sprintf "Quarter-final %i winner" quarterFinalOrdinal
    | Winner (SemiFinal semiFinalOrdinal) -> sprintf "Semi-final %i winner" semiFinalOrdinal
    | Winner _ -> SHOULD_NEVER_HAPPEN
    | RunnerUp group -> sprintf "%s runner-up" (group |> groupText)
    | ThirdPlace groups -> sprintf "Third-place (%s)" (groups |> List.map groupText |> String.concat " | ")
    | Loser (SemiFinal semiFinalOrdinal) -> sprintf "Semi-final %i loser" semiFinalOrdinal
    | Loser _ -> SHOULD_NEVER_HAPPEN

let matchEventText (squadDic:SquadDic) matchEvent =
    match matchEvent with
    | Goal (squadId, playerId, Some assistedBy) ->
        let (PlayerName playerName), (PlayerName assistedByName) = (squadId, playerId) |> playerName squadDic, (squadId, assistedBy) |> playerName squadDic
        sprintf "Goal scored by %s (assisted by %s)" playerName assistedByName
    | Goal (squadId, playerId, None) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Goal scored by %s" playerName
    | OwnGoal (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Own goal scored by %s" playerName
    | Penalty (squadId, playerId, Scored) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Penalty scored by %s" playerName
    | Penalty (squadId, playerId, Missed) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Penalty missed by %s" playerName
    | Penalty (squadId, playerId, Saved (savedBySquadId, savedByPlayerId)) ->
        let (PlayerName playerName), (PlayerName savedByName) = (squadId, playerId) |> playerName squadDic, (savedBySquadId, savedByPlayerId) |> playerName squadDic
        sprintf "Penalty missed by %s (saved by %s)" playerName savedByName
    | YellowCard (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Yellow card for %s" playerName
    | RedCard (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Red card for %s" playerName
    | CleanSheet (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Clean sheet for %s" playerName
    | ManOfTheMatch (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        sprintf "Man-of-the-match for %s" playerName
    | PenaltyShootout _ -> "Penalty shootout"
