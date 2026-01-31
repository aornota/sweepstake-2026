module Aornota.Sweepstake2026.Ui.Pages.Fixtures.Render

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Ui.Common.JsonConverter
open Aornota.Sweepstake2026.Ui.Common.LazyViewOrHMR
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Common.TimestampHelper
open Aornota.Sweepstake2026.Ui.Pages.Fixtures.Common
open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common
open Aornota.Sweepstake2026.Ui.Shared
open Aornota.Sweepstake2026.Ui.Theme.Common
open Aornota.Sweepstake2026.Ui.Theme.Render.Bulma
open Aornota.Sweepstake2026.Ui.Theme.Shared
open Aornota.Sweepstake2026.Common.Domain.Squad // note: after Aornota.Sweepstake2026.Ui.Render.Bulma to avoid collision with Icon.Forward

open System

module RctH = Fable.React.Helpers

let private possibleParticipants unconfirmed (fixtureDic:FixtureDic) (squadDic:SquadDic) =
    match unconfirmed with
    | Winner (Group group) | RunnerUp group ->
        squadDic |> List.ofSeq |> List.choose (fun (KeyValue (squadId, squad)) -> if squad.Group = group then squadId |> Some else None)
    | ThirdPlace groups ->
        squadDic |> List.ofSeq |> List.choose (fun (KeyValue (squadId, squad)) -> if groups |> List.contains squad.Group then squadId |> Some else None)
    | Winner (RoundOf16 matchNumber) ->
        fixtureDic |> List.ofSeq |> List.map (fun (KeyValue (_, fixture)) ->
            match fixture.Stage with
            | RoundOf16 otherMatchNumber when otherMatchNumber = matchNumber ->
                match fixture.HomeParticipant, fixture.AwayParticipant with
                | Confirmed homeSquadId, Confirmed awaySquadId -> [ homeSquadId ; awaySquadId ]
                | _ -> []
            | _ -> [])
            |> List.collect id
    | Winner (QuarterFinal quarterFinalOrdinal) ->
        fixtureDic |> List.ofSeq |> List.map (fun (KeyValue (_, fixture)) ->
            match fixture.Stage with
            | QuarterFinal otherOrdinal when otherOrdinal = quarterFinalOrdinal ->
                match fixture.HomeParticipant, fixture.AwayParticipant with
                | Confirmed homeSquadId, Confirmed awaySquadId -> [ homeSquadId ; awaySquadId ]
                | _ -> []
            | _ -> [])
            |> List.collect id
    | Winner (SemiFinal semiFinalOrdinal) ->
        fixtureDic |> List.ofSeq |> List.map (fun (KeyValue (_, fixture)) ->
            match fixture.Stage with
            | SemiFinal otherOrdinal when otherOrdinal = semiFinalOrdinal ->
                match fixture.HomeParticipant, fixture.AwayParticipant with
                | Confirmed homeSquadId, Confirmed awaySquadId -> [ homeSquadId ; awaySquadId ]
                | _ -> []
            | _ -> [])
            |> List.collect id
    | _ -> []

let private renderConfirmParticipantModal (useDefaultTheme, fixtureDic:FixtureDic, squadDic:SquadDic, confirmParticipantState:ConfirmParticipantState) dispatch =
    let theme = getTheme useDefaultTheme
    let unconfirmed, squadId = confirmParticipantState.Unconfirmed, confirmParticipantState.SquadId
    let unconfirmedText = unconfirmed |> unconfirmedText
    let titleText = sprintf "Confirm %s" unconfirmedText
    let title = [ [ strong titleText ] |> para theme paraCentredSmall ]
    let isConfirmingParticipant, confirmInteraction, onDismiss =
        let confirm = (fun _ -> ConfirmConfirmParticipant |> dispatch)
        let cancel = (fun _ -> CancelConfirmParticipant |> dispatch)
        match confirmParticipantState.ConfirmParticipantStatus with
        | Some ConfirmParticipantPending -> true, Loading, None
        | Some (ConfirmParticipantFailed _) | None ->
            match squadId with
            | Some _ -> false, Clickable (confirm, None), cancel |> Some
            | None -> false, NotEnabled None, cancel |> Some
    let errorText = match confirmParticipantState.ConfirmParticipantStatus with | Some (ConfirmParticipantFailed errorText) -> errorText |> Some | Some ConfirmParticipantPending | None -> None
    let warning = [
        [ strong (sprintf "Please confirm the %s" unconfirmedText) ] |> para theme paraCentredSmaller
        br
        [ str "Please note that this action is irreversible." ] |> para theme paraCentredSmallest ]
    let possibleParticipants = possibleParticipants unconfirmed fixtureDic squadDic
    let values =
        possibleParticipants
        |> List.choose (fun squadId ->
            if squadId |> squadDic.ContainsKey then
                let squad = squadDic.[squadId]
                if squad.Eliminated |> not then
                    let (SquadName squadName) = squad.SquadName
                    (squadId |> toJson, squadName) |> Some
                else None
            else None) // note: should never happen
        |> List.sortBy snd
    let values = (String.Empty, String.Empty) :: values
    let defaultValue = match squadId with | Some squadId -> squadId |> toJson |> Some | None -> String.Empty |> Some
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield [ str "Please select the team" ] |> para theme paraCentredSmaller
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            select theme values defaultValue isConfirmingParticipant (SquadSelected >> dispatch) ]
        yield divVerticalSpace 5
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Confirm participant" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private possiblePlayers goalkeepersFirst (squadDic:SquadDic) forSquadId =
    if forSquadId |> squadDic.ContainsKey then
        let squad = squadDic.[forSquadId]
        squad.PlayerDic |> List.ofSeq |> List.choose (fun (KeyValue (playerId, player)) ->
            match player.PlayerStatus with
            | Active ->
                let (PlayerName playerName) = player.PlayerName
                (playerId, player, playerName) |> Some
            | Withdrawn _ -> None)
        |> List.sortBy (fun (_, player, playerName) ->
            let goalkeeperSort = match goalkeepersFirst, player.PlayerType with | true, Goalkeeper -> 0 | true, _ -> 1 | _ -> 0
            goalkeeperSort, playerName)
        |> List.map (fun (playerId, _, playerName) -> playerId |> toJson, playerName)
    else []

let private renderAddMatchEventModal (useDefaultTheme, fixtureDic:FixtureDic, squadDic:SquadDic, addMatchEventState:AddMatchEventState) dispatch =
    let radioInline theme text isChecked disabled onChange =
        let semantic = if isChecked then Success else Link
        let radioData = { radioDefaultSmall with RadioSemantic = semantic |> Some ; HasBackgroundColour = isChecked }
        radioInline theme radioData (Guid.NewGuid()) (text |> str) isChecked disabled onChange
    let theme = getTheme useDefaultTheme
    let fixtureId, squadId, addMatchEvent = addMatchEventState.FixtureId, addMatchEventState.SquadId, addMatchEventState.AddMatchEvent
    let matchEvents = if fixtureId |> fixtureDic.ContainsKey then match fixtureDic.[fixtureId].MatchResult with | Some matchResult -> matchResult.MatchEvents | None -> [] else []
    let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
    let addText, titleText, appendSquad =
        match addMatchEvent with
        | GoalEvent _ -> "Add goal", "Add goal", true
        | OwnGoalEvent _ -> "Add own goal", "Add own goal", true
        | PenaltyEvent (_, opponentHasCleanSheet, _, _, _) ->
            let titleText = if opponentHasCleanSheet then "Add missed/saved penalty" else "Add penalty"
            "Add penalty", titleText, true
        | CardEvent _ -> "Add card", "Add card", true
        | CleanSheetEvent _ -> "Add clean sheet", "Add clean sheet", true
        | PenaltyShootoutEvent (awaySquadId, _, _) ->
            let awaySquad = if awaySquadId |> squadDic.ContainsKey then squadDic.[awaySquadId] |> Some else None
            match squad, awaySquad with
            | Some squad, Some awaySquad ->
                let (SquadName squadName), (SquadName awaySquadName) = squad.SquadName, awaySquad.SquadName
                "Add penalty shootout", sprintf "Add penalty shootout for %s and %s" squadName awaySquadName, false
            | _ -> "Add penalty shootout", "Add penalty shootout", false // note: should never happen
        | ManOfTheMatchEvent _ -> "Add man-of-the-match", "Add man-of-the-match", true
    let titleText =
        match appendSquad, squad with
        | true, Some squad ->
            let (SquadName squadName) = squad.SquadName
            sprintf "%s for %s" titleText squadName
        | _ -> titleText
    let title = [ [ strong titleText ] |> para theme paraCentredSmall ]
    let isAdding, onDismiss =
        match addMatchEventState.AddMatchEventStatus with
        | Some AddMatchEventPending -> true, None
        | Some (AddMatchEventFailed _) | None -> false, (fun _ -> CancelAddMatchEvent |> dispatch) |> Some
    let errorText = match addMatchEventState.AddMatchEventStatus with | Some (AddMatchEventFailed errorText) -> errorText |> Some | Some AddMatchEventPending | None -> None
    let addMatchEventInteraction = Clickable ((fun _ -> AddMatchEvent |> dispatch), None)
    let addMatchEventInteraction, contents =
        match addMatchEvent with
        | GoalEvent (playerId, assistedBy) ->
            let interaction = match playerId with | Some playerId when playerId |> Some <> assistedBy -> addMatchEventInteraction | _ -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers false squadDic)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let defaultAssistedByValue = match assistedBy with | Some assistedBy -> assistedBy |> toJson |> Some | None -> String.Empty |> Some
            let contents =
                [
                    [ str "Please enter the player who scored the goal and (optionally) the player who assisted the goal" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    divVerticalSpace 5
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultAssistedByValue isAdding (OtherPlayerSelected >> dispatch) ]
                ]
            interaction, contents
        | OwnGoalEvent playerId ->
            let interaction = match playerId with | Some _ -> addMatchEventInteraction | None -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers false squadDic)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let contents =
                [
                    [ str "Please select the player who scored the own goal" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    divVerticalSpace 5
                ]
            interaction, contents
        | PenaltyEvent (opponentSquadId, opponentHasCleanSheet, playerId, penaltyType, savedBy) ->
            let scored, missed, saved = "Scored", "Missed", "Saved"
            let onScored = (fun _ -> PenaltyScored |> PenaltyTypeChanged |> dispatch)
            let onMissed = (fun _ -> PenaltyMissed |> PenaltyTypeChanged |> dispatch)
            let onSaved = (fun _ -> PenaltySaved |> PenaltyTypeChanged |> dispatch)
            match penaltyType with
            | None ->
                let scoredRadio, missedRadio, savedRadio =
                    radioInline theme scored false (isAdding || opponentHasCleanSheet) onScored, radioInline theme missed false isAdding onMissed, radioInline theme saved false isAdding onSaved
                let contents =
                    [
                        [ str "Please select the penalty type" ] |> para theme paraCentredSmaller
                        br
                        field theme { fieldDefault with Grouped = Centred |> Some } [ scoredRadio ; missedRadio ;  savedRadio ]
                        divVerticalSpace 5
                    ]
                NotEnabled None, contents
            | Some PenaltyScored when opponentHasCleanSheet |> not ->
                let interaction = match playerId with | Some _ -> addMatchEventInteraction | None -> NotEnabled None
                let scoredRadio, missedRadio, savedRadio =
                    radioInline theme scored true isAdding onScored, radioInline theme missed false isAdding onMissed, radioInline theme saved false isAdding onSaved
                let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers false squadDic)
                let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
                let contents =
                    [
                        [ str "Please select the player who scored the penalty" ] |> para theme paraCentredSmaller
                        br
                        field theme { fieldDefault with Grouped = Centred |> Some } [ scoredRadio ; missedRadio ; savedRadio ]
                        divVerticalSpace 5
                        field theme { fieldDefault with Grouped = Centred |> Some } [
                            select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                        divVerticalSpace 5
                    ]
                interaction, contents
            | Some PenaltyMissed ->
                let interaction = match playerId with | Some _ -> addMatchEventInteraction | None -> NotEnabled None
                let scoredRadio, missedRadio, savedRadio =
                    radioInline theme scored false isAdding onScored, radioInline theme missed true isAdding onMissed, radioInline theme saved false isAdding onSaved
                let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers false squadDic)
                let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
                let contents =
                    [
                        [ str "Please select the player who missed the penalty" ] |> para theme paraCentredSmaller
                        br
                        field theme { fieldDefault with Grouped = Centred |> Some } [ scoredRadio ; missedRadio ; savedRadio ]
                        divVerticalSpace 5
                        field theme { fieldDefault with Grouped = Centred |> Some } [
                            select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                        divVerticalSpace 5
                    ]
                interaction, contents
            | Some PenaltySaved ->
                let interaction = match playerId, savedBy with | Some _, Some _ -> addMatchEventInteraction | _ -> NotEnabled None
                let scoredRadio, missedRadio, savedRadio =
                    radioInline theme scored false isAdding onScored, radioInline theme missed false isAdding onMissed, radioInline theme saved true isAdding onSaved
                let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers false squadDic)
                let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
                let savedByValues = (String.Empty, String.Empty) :: (opponentSquadId |> possiblePlayers true squadDic)
                let defaultSavedByValue = match savedBy with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
                let contents =
                    [
                        [ str "Please select the player who took the penalty and the player who saved the penalty" ] |> para theme paraCentredSmaller
                        br
                        field theme { fieldDefault with Grouped = Centred |> Some } [ scoredRadio ; missedRadio ; savedRadio ]
                        divVerticalSpace 5
                        field theme { fieldDefault with Grouped = Centred |> Some } [
                            select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                        divVerticalSpace 5
                        field theme { fieldDefault with Grouped = Centred |> Some } [
                            select theme savedByValues defaultSavedByValue isAdding (OppositionPlayerSelected >> dispatch) ]
                        divVerticalSpace 5
                    ]
                interaction, contents
            | _ -> NotEnabled None, []
        | CardEvent (playerId, card) ->
            let interaction = match playerId, card with | Some _, Some _ -> addMatchEventInteraction | _ -> NotEnabled None
            let alreadyHasYellow =
                matchEvents |> List.exists (fun (_, matchEvent) -> match matchEvent with | YellowCard (_, otherPlayerId) when otherPlayerId |> Some = playerId -> true | _ -> false)
            let alreadyHasRed =
                matchEvents |> List.exists (fun (_, matchEvent) -> match matchEvent with | RedCard (_, otherPlayerId) when otherPlayerId |> Some = playerId -> true | _ -> false)
            let warning =
                match playerId with
                | Some playerId when alreadyHasYellow || alreadyHasRed ->
                    let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
                    let warnings =
                        [
                            if alreadyHasYellow then yield [ str (sprintf "%s already has a yellow card" playerName) ] |> para theme paraCentredSmaller
                            if alreadyHasRed then yield [ str (sprintf "%s already has a red card" playerName) ] |> para theme paraCentredSmaller
                        ]
                    [ notification theme notificationWarning warnings ; br ]
                | _ -> []
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers false squadDic)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let yellowChecked, redChecked = match card with | Some Yellow | Some SecondYellow -> true, false | Some Red -> false, true | None -> false, false
            let yellowRadio = radioInline theme "Yellow card" yellowChecked isAdding (fun _ -> Yellow |> CardSelected |> dispatch)
            let redRadio = radioInline theme "Red card" redChecked isAdding (fun _ -> Red |> CardSelected |> dispatch)
            let contents =
                [
                    yield! warning
                    yield [ str "Please select the player and the card type" ] |> para theme paraCentredSmaller
                    yield br
                    yield field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    yield divVerticalSpace 5
                    yield field theme { fieldDefault with Grouped = Centred |> Some } [ yellowRadio ; redRadio ]
                    yield divVerticalSpace 5
                ]
            interaction, contents
        | CleanSheetEvent playerId ->
            let interaction = match playerId with | Some _ -> addMatchEventInteraction | None -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers true squadDic)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let contents =
                [
                    [ str "Please select the player who kept a clean sheet" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    divVerticalSpace 5
                ]
            interaction, contents
        | PenaltyShootoutEvent (awaySquadId, homeScore, awayScore) ->
            let interaction = if homeScore <> awayScore then addMatchEventInteraction else NotEnabled None
            let awaySquad = if awaySquadId |> squadDic.ContainsKey then squadDic.[awaySquadId] |> Some else None
            let homeSquadName, awaySquadName =
                match squad, awaySquad with
                | Some squad, Some awaySquad ->
                    let (SquadName squadName), (SquadName awaySquadName) = squad.SquadName, awaySquad.SquadName
                    squadName, awaySquadName
                | _ -> "Home", "Away" // note: should never happen
            let warning = [
                [ strong "Please enter the penalty shootout details" ] |> para theme paraCentredSmaller
                br
                [ str "Please note that this action is irreversible." ] |> para theme paraCentredSmallest ]
            let left =
                let decrementInteraction = if homeScore > 0u then Clickable ((fun _ -> HomeScoreDecremented |> dispatch), None) else NotEnabled None
                let incrementInteraction = Clickable ((fun _ -> HomeScoreIncremented |> dispatch), None)
                let decrement = [ str "-" ] |> button theme { buttonLinkSmall with Interaction = decrementInteraction }
                let increment = [ str "+" ] |> button theme { buttonLinkSmall with Interaction = incrementInteraction }
                [
                    [ strong homeSquadName ] |> para theme paraCentredSmaller
                    br
                    div divCentred [
                        decrement
                        div { divDefault with PadH = 5 |> Some } [ [ str (sprintf "%i" homeScore) ] |> para theme paraCentredSmaller ]
                        increment ]
                ]
            let right =
                let decrementInteraction = if awayScore > 0u then Clickable ((fun _ -> AwayScoreDecremented |> dispatch), None) else NotEnabled None
                let incrementInteraction = Clickable ((fun _ -> AwayScoreIncremented |> dispatch), None)
                let decrement = [ str "-" ] |> button theme { buttonLinkSmall with Interaction = decrementInteraction }
                let increment = [ str "+" ] |> button theme { buttonLinkSmall with Interaction = incrementInteraction }
                [
                    [ strong awaySquadName ] |> para theme paraCentredSmaller
                    br
                    div divCentred [
                        decrement
                        div { divDefault with PadH = 5 |> Some } [ [ str (sprintf "%i" awayScore) ] |> para theme paraCentredSmaller ]
                        increment ]
                ]
            let contents =
                [
                    notification theme notificationWarning warning
                    br
                    columnsLeftAndRight left right
                ]
            interaction, contents
        | ManOfTheMatchEvent playerId ->
            let interaction = match playerId with | Some _ -> addMatchEventInteraction | None -> NotEnabled None
            let values = (String.Empty, String.Empty) :: (squadId |> possiblePlayers false squadDic)
            let defaultPlayerValue = match playerId with | Some playerId -> playerId |> toJson |> Some | None -> String.Empty |> Some
            let contents =
                [
                    [ str "Please select the man-of-the-match" ] |> para theme paraCentredSmaller
                    br
                    field theme { fieldDefault with Grouped = Centred |> Some } [
                        select theme values defaultPlayerValue isAdding (PlayerSelected >> dispatch) ]
                    divVerticalSpace 5
                ]
            interaction, contents
    let contents = match contents with | _ :: _ -> contents | [] -> [ [ str "Coming soon" ] |> para theme paraCentredSmaller ; br ] // note: should never happen
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield! contents
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str addText ] |> button theme { buttonLinkSmall with Interaction = addMatchEventInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderRemoveMatchEventModal (useDefaultTheme, squadDic:SquadDic, removeMatchEventState:RemoveMatchEventState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Remove match event" ] |> para theme paraCentredSmall ]
    let matchEvent = removeMatchEventState.MatchEvent
    let removeMatchEventInteraction, onDismiss =
        match removeMatchEventState.RemoveMatchEventStatus with
        | Some RemoveMatchEventPending -> Loading, None
        | Some (RemoveMatchEventFailed _) | None -> Clickable ((fun _ -> RemoveMatchEvent |> dispatch), None), (fun _ -> CancelRemoveMatchEvent |> dispatch) |> Some
    let errorText = match removeMatchEventState.RemoveMatchEventStatus with | Some (RemoveMatchEventFailed errorText) -> errorText |> Some | Some RemoveMatchEventPending | None -> None
    let warning = [
        [ str (matchEvent |> matchEventText squadDic) ] |> para theme paraCentredSmaller
        br
        [ strong "Are you sure you want to remove this match event?" ] |> para theme paraCentredSmaller ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Remove match event" ] |> button theme { buttonLinkSmall with Interaction = removeMatchEventInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private filterTabs currentFixturesFilter dispatch =
    let isActive filter =
        match filter with
        | AllFixtures -> currentFixturesFilter = AllFixtures
        | GroupFixtures _ -> match currentFixturesFilter with | GroupFixtures _ -> true | _ -> false
        | KnockoutFixtures -> currentFixturesFilter = KnockoutFixtures
        | Fixture _ -> false
    let filterText filter = match filter with | AllFixtures -> "All" | GroupFixtures _ -> "Group" | KnockoutFixtures -> "Knockout" | Fixture _ -> SHOULD_NEVER_HAPPEN
    let onClick filter =
        match filter with
        | AllFixtures -> (fun _ -> ShowAllFixtures |> dispatch )
        | GroupFixtures _ -> (fun _ -> None |> ShowGroupFixtures |> dispatch )
        | KnockoutFixtures -> (fun _ -> ShowKnockoutFixtures |> dispatch )
        | Fixture _ -> ignore
    let filters = [ AllFixtures ; None |> GroupFixtures ; KnockoutFixtures ]
    filters |> List.map (fun filter -> { IsActive = filter |> isActive ; TabText = filter |> filterText ; TabLinkType = Internal (filter |> onClick) } )

let private groupTabs currentFixturesFilter dispatch =
    let groupTab currentGroup dispatch group =
        { IsActive = group |> Some = currentGroup ; TabText = group |> groupText ; TabLinkType = Internal (fun _ -> group |> Some |> ShowGroupFixtures |> dispatch ) }
    match currentFixturesFilter with
    | GroupFixtures currentGroup -> groups |> List.map (groupTab currentGroup dispatch)
    | _ -> []

let private startsIn (_timestamp:DateTime) : Fable.React.ReactElement option * bool =
#if TICK
    let startsIn, imminent = _timestamp |> startsIn
    (if imminent then strong startsIn else str startsIn) |> Some, imminent
#else
    None, false
#endif

let private stageText stage =
    match stage with
    | Group group -> group |> groupText
    | RoundOf16 matchNumber -> sprintf "Round of 16 (match %i)" matchNumber
    | QuarterFinal quarterFinalOrdinal -> sprintf "Quarter-final %i" quarterFinalOrdinal
    | SemiFinal semiFinalOrdinal -> sprintf "Semi-final %i" semiFinalOrdinal
    | Final -> "Final"

let private confirmedFixtureDetails (squadDic:SquadDic) fixture =
    match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with
    | Confirmed homeSquadId, Confirmed awaySquadId, Some matchResult ->
        let matchOutcome, homeScoreEvents, awayScoreEvents, matchEvents = matchResult.MatchOutcome, matchResult.HomeScoreEvents, matchResult.AwayScoreEvents, matchResult.MatchEvents
        let winnerSquadId, penaltyShootoutText =
            match matchOutcome.PenaltyShootoutOutcome with
            | Some penaltyShootoutOutcome ->
                if penaltyShootoutOutcome.HomeScore > penaltyShootoutOutcome.AwayScore then
                    let (SquadName squadName) = homeSquadId |> squadName squadDic
                    let penaltyShootoutText = sprintf "%s win %i - %i on penalities" squadName penaltyShootoutOutcome.HomeScore penaltyShootoutOutcome.AwayScore
                    homeSquadId |> Some, penaltyShootoutText |> Some
                else if penaltyShootoutOutcome.AwayScore > penaltyShootoutOutcome.HomeScore then
                    let (SquadName squadName) = awaySquadId |> squadName squadDic
                    let penaltyShootoutText = sprintf "%s win %i - %i on penalities" squadName penaltyShootoutOutcome.AwayScore penaltyShootoutOutcome.HomeScore
                    awaySquadId |> Some, penaltyShootoutText |> Some
                else None, None // note: should never happen
            | None ->
                if matchOutcome.HomeGoals > matchOutcome.AwayGoals then homeSquadId |> Some, None
                else if matchOutcome.AwayGoals > matchOutcome.HomeGoals then awaySquadId |> Some, None
                else None, None
        let homeIsWinner, homeName, homeGoals =
            let (SquadName squadName) = homeSquadId |> squadName squadDic
            homeSquadId |> Some = winnerSquadId, squadName, matchOutcome.HomeGoals
        let awayIsWinner, awayName, awayGoals =
            let (SquadName squadName) = awaySquadId |> squadName squadDic
            awaySquadId |> Some = winnerSquadId, squadName, matchOutcome.AwayGoals
        let teams = (homeSquadId, homeName, awaySquadId, awayName) |> Some
        let result = (homeIsWinner, homeGoals, awayIsWinner, awayGoals, penaltyShootoutText, homeScoreEvents, awayScoreEvents, matchEvents) |> Some
        teams, result
    | Confirmed homeSquadId, Confirmed awaySquadId, None ->
        let (SquadName homeName), (SquadName awayName) = homeSquadId |> squadName squadDic, awaySquadId |> squadName squadDic
        (homeSquadId, homeName, awaySquadId, awayName) |> Some, None
    | _ -> None, None

let private teamEvents theme fixtureId role forSquadId hasShootout matchEvents canAdministerResults (squadDic:SquadDic) dispatch =
    let isHome = match role with | Home -> true | Away -> false
    let paraEvent = if isHome then { paraDefaultSmallest with ParaAlignment = RightAligned } else paraDefaultSmallest
    matchEvents
    |> List.mapi (fun i (matchEventId, matchEvent) -> i, matchEventId, matchEvent)
    |> List.sortBy (fun (i, _, matchEvent) ->
        (match matchEvent with | Goal _ | OwnGoal _ | Penalty _ -> 0 | YellowCard _ | RedCard _ -> 1 | CleanSheet _ -> 2 | ManOfTheMatch _ -> 3 | _ -> 4), i)
    |> List.choose (fun (_, matchEventId, matchEvent) ->
        let textAndCanRemove =
            match matchEvent with
            | Goal (squadId, _, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, hasShootout |> not) |> Some
            | OwnGoal (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, hasShootout |> not) |> Some
            | Penalty (squadId, _, Scored) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, hasShootout |> not) |> Some
            | Penalty (squadId, _, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | YellowCard (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | RedCard (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | CleanSheet (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | ManOfTheMatch (squadId, _) when squadId = forSquadId ->
                (matchEvent |> matchEventText squadDic, true) |> Some
            | _ -> None
        match textAndCanRemove, canAdministerResults with
        | Some (text, canRemove), true when canRemove ->
            let removeMatchEvent = [ str "Remove" ] |> link theme (Internal (fun _ -> (fixtureId, matchEventId, matchEvent) |> ShowRemoveMatchEventModal |> dispatch))
            if isHome then [ str (sprintf "%s " text) ; removeMatchEvent ] |> para theme paraEvent |> Some
            else [ removeMatchEvent ; str (sprintf " %s" text) ] |> para theme paraEvent |> Some
        | Some (text, _), _ ->
            [ str text ] |> para theme paraEvent |> Some
        | _ -> None)

let private addLinks theme fixtureId role forSquadId opponentSquadId opponentGoals hasShootout matchEvents dispatch =
    let isHome = match role with | Home -> true | Away -> false
    let hasCleanSheet = matchEvents |> List.exists (fun (_, matchEvent) -> match matchEvent with | CleanSheet (squadId, _) when squadId = forSquadId -> true | _ -> false)
    let opponentHasCleanSheet = matchEvents |> List.exists (fun (_, matchEvent) -> match matchEvent with | CleanSheet (squadId, _) when squadId <> forSquadId -> true | _ -> false)
    let needsManOfTheMatch = matchEvents |> List.exists (fun (_, matchEvent) -> match matchEvent with | ManOfTheMatch _ -> true | _ -> false) |> not
    let paraEvent = if isHome then { paraDefaultSmallest with ParaAlignment = RightAligned } else paraDefaultSmallest
    let addGoal =
        if hasShootout |> not && opponentHasCleanSheet |> not then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddGoalModal |> dispatch)
            [ [ str "Add goal" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addOwnGoal =
        if hasShootout |> not && hasCleanSheet |> not then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddOwnGoalModal |> dispatch)
            [ [ str "Add own goal" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addPenalty =
        let onClick = (fun _ -> (fixtureId, forSquadId, opponentSquadId, opponentHasCleanSheet) |> ShowAddPenaltyModal |> dispatch)
        let text = if hasShootout || opponentHasCleanSheet then "Add missed/saved penalty" else "Add penalty"
        [ [ str text ] |> link theme (Internal onClick) ] |> para theme paraEvent
    let addCard =
        let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddCardModal |> dispatch)
        [ [ str "Add card" ] |> link theme (Internal onClick) ] |> para theme paraEvent
    let addCleanSheet =
        if hasCleanSheet |> not && opponentGoals = 0u then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddCleanSheetModal |> dispatch)
            [ [ str "Add clean sheet" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    let addManOfTheMatch =
        if needsManOfTheMatch then
            let onClick = (fun _ -> (fixtureId, forSquadId) |> ShowAddManOfTheMatchModal |> dispatch)
            [ [ str "Add man-of-the-match" ] |> link theme (Internal onClick) ] |> para theme paraEvent |> Some
        else None
    [
        yield RctH.ofOption addGoal
        yield RctH.ofOption addOwnGoal
        yield addPenalty
        yield addCard
        yield RctH.ofOption addCleanSheet
        yield RctH.ofOption addManOfTheMatch
    ]

let private renderFixture useDefaultTheme fixtureId (fixtureDic:FixtureDic) (squadDic:SquadDic) (_userDic:UserDic) authUser dispatch =
    let theme = getTheme useDefaultTheme
    let canAdministerResults = match authUser with | Some authUser -> authUser.Permissions.ResultsAdminPermission | None -> false
    let fixture, (teams, result) =
        if fixtureId |> fixtureDic.ContainsKey then
            let fixture = fixtureDic.[fixtureId]
            fixture |> Some, fixture |> confirmedFixtureDetails squadDic
        else None, (None, None)
    match fixture, teams with
    | Some fixture, Some (homeSquadId, homeName, awaySquadId, awayName) ->
        let homeIsWinner, homeGoals, awayIsWinner, awayGoals, penaltyShootoutText, _homeScoreEvents, _awayScoreEvents, matchEvents =
            match result with
            | Some (homeIsWinner, homeGoals, awayIsWinner, awayGoals, penaltyShootoutText, homeScoreEvents, awayScoreEvents, matchEvents) ->
                homeIsWinner, homeGoals, awayIsWinner, awayGoals, penaltyShootoutText, homeScoreEvents, awayScoreEvents, matchEvents
            | None ->
                let emptyScoreEvents = { TeamScoreEvents = [] ; PlayerScoreEvents = [] }
                false, 0u, false, 0u, None, emptyScoreEvents, emptyScoreEvents, []
        let hasShootout = match penaltyShootoutText with Some _ -> true | None -> false
        let date, time = fixture.KickOff.LocalDateTime |> dateText, fixture.KickOff.LocalDateTime.ToString ("HH:mm")
        let dateAndTime = sprintf "%s at %s" date time
        let homeOutcome =
            let homeOutcome = sprintf "%s %i" homeName homeGoals
            let homeOutcome = if homeIsWinner then strong homeOutcome else str homeOutcome
            [ homeOutcome ] |> para theme { paraDefaultSmall with ParaAlignment = RightAligned }
        let awayOutcome =
            let awayOutcome = sprintf "%i %s" awayGoals awayName
            let awayOutcome = if awayIsWinner then strong awayOutcome else str awayOutcome
            [ awayOutcome ] |> para theme paraDefaultSmall
        let homeEvents = teamEvents theme fixtureId Home homeSquadId hasShootout matchEvents canAdministerResults squadDic dispatch
        let awayEvents = teamEvents theme fixtureId Away awaySquadId hasShootout matchEvents canAdministerResults squadDic dispatch
        let homeAddLinks = if canAdministerResults then addLinks theme fixtureId Home homeSquadId awaySquadId awayGoals hasShootout matchEvents dispatch else []
        let awayAddLinks = if canAdministerResults then addLinks theme fixtureId Away awaySquadId homeSquadId homeGoals hasShootout matchEvents dispatch else []
        [
            yield [ str (fixture.Stage |> stageText) ] |> para theme paraCentredSmaller
            yield [ str dateAndTime ] |> para theme paraCentredSmallest
            yield divVerticalSpace 10
            yield columnsLeftAndRight [ homeOutcome ] [ awayOutcome ]
            match penaltyShootoutText with
            | Some penaltyShootoutText ->
                yield [ em penaltyShootoutText ] |> para theme paraCentredSmaller
                yield divVerticalSpace 20
            | None ->
                let isKnockout = match fixture.Stage with | Group _ -> false | _ -> true
                if homeGoals = awayGoals && isKnockout then
                    let onClick = (fun _ -> (fixtureId, homeSquadId, awaySquadId) |> ShowAddPenaltyShootoutModal |> dispatch)
                    let addPenaltyShootout = [ str "Add penalty shootout" ] |> link theme (Internal onClick)
                    yield [ addPenaltyShootout ] |> para theme paraCentredSmallest
                    yield divVerticalSpace 20
                else ()
            if homeEvents.Length + awayEvents.Length > 0 then
                yield columnsLeftAndRight homeEvents awayEvents
            if homeAddLinks.Length + awayAddLinks.Length > 0 then
                yield columnsLeftAndRight homeAddLinks awayAddLinks

            // TODO-SOON: Points-4-sweepstakers? "Special" News post?...

        ]
    | _ -> [] // note: should never happen

let private renderFixtures (useDefaultTheme, currentFixtureFilter, fixtureDic:FixtureDic, squadDic:SquadDic, authUser, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    let matchesFilter fixture =
        match currentFixtureFilter with
        | AllFixtures -> true
        | GroupFixtures currentGroup ->
            match fixture.Stage with | Group group -> group |> Some = currentGroup | RoundOf16 _ | QuarterFinal _ | SemiFinal _ | Final -> false
        | KnockoutFixtures ->
            match fixture.Stage with | RoundOf16 _ | QuarterFinal _ | SemiFinal _ | Final -> true | Group _ -> false
        | Fixture _ -> false
    let canConfirmParticipant, canAdministerResults =
        match authUser with
        | Some authUser ->
            let canConfirmParticipant = match authUser.Permissions.FixturePermissions with | Some fixturePermissions -> fixturePermissions.ConfirmFixturePermission | None -> false
            canConfirmParticipant, authUser.Permissions.ResultsAdminPermission
        | None -> false, false
    let confirmParticipant role participant fixtureId =
        match participant with
        | Confirmed _ -> None
        | Unconfirmed unconfirmed ->
            if canConfirmParticipant then
                let confirmable =
                    match unconfirmed with
                    | Winner (Group group) | RunnerUp group ->
                        let dependsOnPending =
                            fixtureDic |> List.ofSeq |> List.filter (fun (KeyValue (_, fixture)) ->
                                match fixture.Stage with
                                | Group otherGroup when otherGroup = group -> match fixture.MatchResult with | Some _ -> false | None -> true
                                | _ -> false)
                        dependsOnPending.Length = 0
                    | ThirdPlace groups ->
                        let dependsOnPending =
                            fixtureDic |> List.ofSeq |> List.filter (fun (KeyValue (_, fixture)) ->
                                match fixture.Stage with
                                | Group group when groups |> List.contains group -> match fixture.MatchResult with | Some _ -> false | None -> true
                                | _ -> false)
                        dependsOnPending.Length = 0
                    | Winner (RoundOf16 matchNumber) ->
                        let dependsOnPending =
                            fixtureDic |> List.ofSeq |> List.filter (fun (KeyValue (_, fixture)) ->
                                match fixture.Stage with
                                | RoundOf16 otherMatchNumber when otherMatchNumber = matchNumber -> match fixture.MatchResult with | Some _ -> false | None -> true
                                | _ -> false)
                        dependsOnPending.Length = 0
                    | Winner (QuarterFinal quarterFinalOrdinal) ->
                        let dependsOnPending =
                            fixtureDic |> List.ofSeq |> List.filter (fun (KeyValue (_, fixture)) ->
                                match fixture.Stage with
                                | QuarterFinal otherOrdinal when otherOrdinal = quarterFinalOrdinal -> match fixture.MatchResult with | Some _ -> false | None -> true
                                | _ -> false)
                        dependsOnPending.Length = 0
                    | Winner (SemiFinal semiFinalOrdinal) ->
                        let dependsOnPending =
                            fixtureDic |> List.ofSeq |> List.filter (fun (KeyValue (_, fixture)) ->
                                match fixture.Stage with
                                | SemiFinal otherOrdinal when otherOrdinal = semiFinalOrdinal -> match fixture.MatchResult with | Some _ -> false | None -> true
                                | _ -> false)
                        dependsOnPending.Length = 0
                    | _ -> false
                if confirmable then
                    let paraConfirm = match role with | Home -> { paraDefaultSmallest with ParaAlignment = RightAligned } | Away -> paraDefaultSmallest
                    let onClick = (fun _ -> (fixtureId, role, unconfirmed) |> ShowConfirmParticipantModal |> dispatch)
                    let confirmParticipant = [ [ str "Confirm participant" ] |> para theme paraConfirm ] |> link theme (Internal onClick)
                    confirmParticipant |> Some
                else None
            else None
    let stageElement stage =
        let stageText =
            match stage with
            | Group _ -> match currentFixtureFilter with | GroupFixtures _ | Fixture _ -> None | AllFixtures | KnockoutFixtures -> stage |> stageText |> Some
            | _ -> stage |> stageText |> Some
        match stageText with | Some stageText -> [ str stageText ] |> para theme paraDefaultSmallest |> Some | None -> None
    let details fixture =
        match fixture |> confirmedFixtureDetails squadDic with
        | Some (_, homeName, _, awayName), Some (homeIsWinner, homeGoals, awayIsWinner, awayGoals, penaltyShootoutText, _, _, _) ->
            let home = if homeIsWinner then strong homeName else str homeName
            let homeGoals = sprintf "%i" homeGoals
            let homeGoals = if homeIsWinner then strong homeGoals else str homeGoals
            let homeGoals = [ homeGoals ] |> para theme paraDefaultSmallest |> Some
            let away = if awayIsWinner then strong awayName else str awayName
            let awayGoals = sprintf "%i" awayGoals
            let awayGoals = if awayIsWinner then strong awayGoals else str awayGoals
            let awayGoals = [ awayGoals ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } |> Some
            let penaltyShootout = match penaltyShootoutText with | Some penaltyShootoutText -> [ em penaltyShootoutText ] |> para theme paraDefaultSmallest |> Some | None -> None
            home, homeGoals, str "-", away, awayGoals, penaltyShootout
        | _ ->
            let homeParticipant, awayParticipant = fixture.HomeParticipant, fixture.AwayParticipant
            let home =
                match homeParticipant with
                | Confirmed squadId ->
                    let (SquadName squadName) = squadId |> squadName squadDic
                    squadName
                | Unconfirmed unconfirmed -> unconfirmed |> unconfirmedText
            let away =
                match awayParticipant with
                | Confirmed squadId ->
                    let (SquadName squadName) = squadId |> squadName squadDic
                    squadName
                | Unconfirmed unconfirmed -> unconfirmed |> unconfirmedText
            str home, None, str "vs.", str away, None, None
    let extra (fixtureId, fixture) =
        let local = fixture.KickOff.LocalDateTime
        let hasResult = match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with | Confirmed _ , Confirmed _, Some _ -> true | _ -> false
        let onClick = (fun _ -> fixtureId |> ShowFixture |> dispatch)
        if hasResult then
            let showFixtureText = if canAdministerResults then "Edit details" else "View details"
            [ [ str showFixtureText ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal onClick) |> Some
        else
            if canAdministerResults && local < DateTime.Now then
                [ [ str "Add details" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal onClick) |> Some
            else
                let paraExtra = { paraDefaultSmallest with ParaAlignment = RightAligned ; ParaColour = GreyscalePara Grey }
                let extra, imminent = if local < DateTime.Now then em "Result pending" |> Some, true else local |> startsIn
                let paraExtra = if imminent then { paraExtra with ParaColour = GreyscalePara GreyDarker } else paraExtra
                match extra with | Some extra -> [ extra ] |> para theme paraExtra |> Some | None -> None
    let fixtureRow (fixtureId, fixture) =
        let date, time = fixture.KickOff.LocalDateTime |> dateText, fixture.KickOff.LocalDateTime.ToString ("HH:mm")
        let home, homeGoals, vs, away, awayGoals, penaltyShootout = fixture |> details
        tr false [
            td [ [ str date ] |> para theme paraDefaultSmallest ]
            td [ [ str time ] |> para theme paraDefaultSmallest ]
            td [ RctH.ofOption (fixture.Stage |> stageElement) ]
            td [ RctH.ofOption (confirmParticipant Home fixture.HomeParticipant fixtureId) ]
            td [ [ home ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
            td [ RctH.ofOption homeGoals ]
            td [ [ vs ] |> para theme paraCentredSmallest ]
            td [ RctH.ofOption awayGoals ]
            td [ [ away ] |> para theme paraDefaultSmallest ]
            td [ RctH.ofOption (confirmParticipant Away fixture.AwayParticipant fixtureId) ]
            td [ RctH.ofOption penaltyShootout ]
            td [ RctH.ofOption ((fixtureId, fixture) |> extra) ] ]
    let fixtures =
        fixtureDic
        |> List.ofSeq
        |> List.map (fun (KeyValue (fixtureId, fixture)) -> (fixtureId, fixture))
        |> List.filter (fun (_, fixture) -> fixture |> matchesFilter)
        |> List.sortBy (fun (_, fixture) -> fixture.KickOff)
    let fixtureRows = fixtures |> List.map (fun (fixtureId, fixture) -> (fixtureId, fixture) |> fixtureRow)
    div divCentred [
        yield table theme false { tableDefault with IsNarrow = true } [
            thead [
                tr false [
                    th [ [ strong "Date" ] |> para theme paraDefaultSmallest ]
                    th [ [ strong "Time" ] |> para theme paraDefaultSmallest ]
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th []
                    th [] ] ]
            tbody [ yield! fixtureRows ] ] ]

let render (useDefaultTheme, state, authUser:AuthUser option, fixturesProjection:Projection<_ * FixtureDic>, squadsProjection:Projection<_ * SquadDic>, usersProjection:Projection<_ * UserDic>, hasModal, ticks:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ strong "Fixtures / Results" ] |> para theme paraCentredSmall
        yield hr theme false
        match fixturesProjection, squadsProjection, usersProjection with
        | Pending, _, _ | _, Pending, _ | _, _, Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed, _, _ | _, Failed, _ | _, _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, fixtureDic), Ready (_, squadDic), Ready (_, userDic) ->
            let currentFixturesFilter = match state.CurrentFixturesFilter with | GroupFixtures None -> GroupA |> Some |> GroupFixtures | _ -> state.CurrentFixturesFilter
            let filterTabs = filterTabs currentFixturesFilter dispatch
            match hasModal, state.ConfirmParticipantState with
            | false, Some confirmParticipantState ->
                yield div divDefault [ lazyViewOrHMR2 renderConfirmParticipantModal (useDefaultTheme, fixtureDic, squadDic, confirmParticipantState) (ConfirmParticipantInput >> dispatch) ]
            | _ -> ()
            match hasModal, state.AddMatchEventState with
            | false, Some addMatchEventState ->
                yield div divDefault [ lazyViewOrHMR2 renderAddMatchEventModal (useDefaultTheme, fixtureDic, squadDic, addMatchEventState) (AddMatchEventInput >> dispatch) ]
            | _ -> ()
            match hasModal, state.RemoveMatchEventState with
            | false, Some removeMatchEventState ->
                yield div divDefault [ lazyViewOrHMR2 renderRemoveMatchEventModal (useDefaultTheme, squadDic, removeMatchEventState) (RemoveMatchEventInput >> dispatch) ]
            | _ -> ()
            yield div divCentred [ tabs theme { tabsDefault with TabsSize = Normal ; Tabs = filterTabs } ]
            match currentFixturesFilter with
            | Fixture fixtureId ->
                yield br
                yield! renderFixture useDefaultTheme fixtureId fixtureDic squadDic userDic authUser dispatch
            | _ ->
                let groupTabs = groupTabs currentFixturesFilter dispatch
                match groupTabs with
                | _ :: _ ->
                    yield div divCentred [ tabs theme { tabsDefault with Tabs = groupTabs } ]
                | [] -> ()
                yield br
                yield lazyViewOrHMR2 renderFixtures (useDefaultTheme, currentFixturesFilter, fixtureDic, squadDic, authUser, ticks) dispatch ]
