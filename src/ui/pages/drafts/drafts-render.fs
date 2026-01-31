module Aornota.Sweepstake2026.Ui.Pages.Drafts.Render

open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Ui.Common.LazyViewOrHMR
open Aornota.Sweepstake2026.Ui.Pages.Drafts.Common
open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common
open Aornota.Sweepstake2026.Ui.Shared
open Aornota.Sweepstake2026.Ui.Theme.Common
open Aornota.Sweepstake2026.Ui.Theme.Render.Bulma
open Aornota.Sweepstake2026.Ui.Theme.Shared

module RctH = Fable.React.Helpers

let private draftTabs drafts currentDraftId dispatch =
    drafts |> List.map (fun (draftId, draft) ->
        { IsActive = draftId = currentDraftId ; TabText = draft.DraftOrdinal |> draftText ; TabLinkType = Internal (fun _ -> draftId |> ShowDraft |> dispatch ) })

let private squadDescription (squadDic:SquadDic) squadId =
    if squadId |> squadDic.ContainsKey then
        let squad = squadDic.[squadId]
        let (SquadName squadName), (CoachName coachName) = squad.SquadName, squad.CoachName
        sprintf "%s (%s)" squadName coachName
    else UNKNOWN

let private playerDescriptionAndExtraAndWithdrawn (squadDic:SquadDic) (squadId, playerId) =
    if squadId |> squadDic.ContainsKey then
        let squad = squadDic.[squadId]
        let (SquadName squadName) = squad.SquadName
        let playerDic = squad.PlayerDic
        if playerId |> playerDic.ContainsKey then
            let player = playerDic.[playerId]
            let (PlayerName playerName) = player.PlayerName
            let withdrawn = match player.PlayerStatus with | Active -> false | Withdrawn _ -> true
            sprintf "%s (%s)" playerName squadName, player.PlayerType |> playerTypeText, withdrawn
        else UNKNOWN, UNKNOWN, false
    else UNKNOWN, UNKNOWN, false

let private playerDescription (squadDic:SquadDic) (squadId, playerId) =
    let playerDescription, _, _ = (squadId, playerId) |> playerDescriptionAndExtraAndWithdrawn squadDic
    playerDescription

let private userNameText (userDic:UserDic) userId =
    let (UserName userName) = userId |> userName userDic
    userName

let private renderUserDraftPicks theme userDraftPicks (userDic:UserDic) (squadDic:SquadDic) =
    let userDraftPickElement userDraftPickDto =
        let text =
            match userDraftPickDto.UserDraftPick with
            | TeamPick squadId -> squadId |> squadDescription squadDic
            | PlayerPick (squadId, playerId) -> (squadId, playerId) |> playerDescription squadDic
        [ str (sprintf "%i. %s" userDraftPickDto.Rank text) ] |> para theme paraDefaultSmallest
    userDraftPicks
    |> List.map (fun (userId, userDraftPickDtos) -> userId |> userNameText userDic, userDraftPickDtos)
    |> List.sortBy fst
    |> List.map (fun (userName, userDraftPickDtos) ->
        [
            yield [ strong userName ; str " wanted" ] |> para theme paraDefaultSmall
            yield! userDraftPickDtos |> List.map userDraftPickElement
            yield br
        ])
    |> List.collect id

let private draftPickText (squadDic:SquadDic) draftPick =
    match draftPick with
    | TeamPicked squadId -> squadId |> squadDescription squadDic
    | PlayerPicked (squadId, playerId) -> (squadId, playerId) |> playerDescription squadDic

let private renderProcessingEvents theme processingEvents (userDic:UserDic) (squadDic:SquadDic) =
    let ignoredElements reason (ignored:(UserId * DraftPick list) list) =
        let ignoredElement userName draftPick =
            [ str (sprintf "Removed %s for " (draftPick |> draftPickText squadDic)) ; strong userName ; str (sprintf ": %s" reason) ] |> para theme paraDefaultSmallest
        ignored
        |> List.map (fun (userId, draftPicks) -> userId |> userNameText userDic, draftPicks)
        |> List.sortBy fst
        |> List.map (fun (userName, draftPicks) ->
            [
                yield br
                yield! draftPicks |> List.map (ignoredElement userName)
            ])
        |> List.collect id
    let eventElements event =
        match event with
        | ProcessingStarted seed -> [ [ strong "Using random seed" ; str (sprintf " %i" seed) ] |> para theme paraDefaultSmall ]
        | WithdrawnPlayersIgnored ignored ->
            let ignored = ignored |> List.map (fun (userId, squadAndPlayerIds) -> userId, squadAndPlayerIds |> List.map PlayerPicked)
            ignored |> ignoredElements "player has been withdrawn"
        | RoundStarted round -> [ br ; [ strong (sprintf "Round %i" round) ] |> para theme paraDefaultSmaller ]
        | AlreadyPickedIgnored ignored -> ignored |> ignoredElements "picked in an earlier draft / round"
        | NoLongerRequiredIgnored ignored -> ignored |> ignoredElements "no longer required"
        | UncontestedPick (draftPick, userId) ->
            [
                br
                [ strong (userId |> userNameText userDic) ; str " has a unique pick for this round: " ; strong (draftPick |> draftPickText squadDic) ] |> para theme paraDefaultSmallest
            ]
        | ContestedPick (draftPick, userDetails, winner) ->
            let pickPriorities =
                userDetails
                |> List.mapi (fun i (userId, pickPriority, _) ->
                    [
                        if i = 0 then yield br
                        yield [ strong (userId |> userNameText userDic) ; str (sprintf " has pick priority %i" pickPriority) ] |> para theme paraDefaultSmallest
                    ])
                |> List.collect id
            let randomNumbers =
                userDetails
                |> List.choose (fun (userId, _, randomNumber) -> match randomNumber with | Some randomNumber -> (userId, randomNumber) |> Some | None -> None)
                |> List.map (fun (userId, randomNumber) ->
                    [
                        br
                        [ strong (userId |> userNameText userDic) ; str " has highest pick priority" ] |> para theme paraDefaultSmallest
                        [ strong (userId |> userNameText userDic) ; str (sprintf " assigned random number %.8f" randomNumber) ] |> para theme paraDefaultSmallest
                    ])
                |> List.collect id
            [
                yield br
                yield [ strong (draftPick |> draftPickText squadDic) ; str " is a contested pick for this round" ] |> para theme paraDefaultSmallest
                yield! pickPriorities
                yield! randomNumbers
                yield br
                yield [ strong (winner |> userNameText userDic) ; str " has the highest random number" ] |> para theme paraDefaultSmallest
            ]
        | PickPriorityChanged (userId, pickPriority) ->
            [ br ; [ str (sprintf "Pick priority changed to %i for " pickPriority) ; strong (userId |> userNameText userDic) ] |> para theme paraDefaultSmallest ]
        | Picked (_, draftPick, userId, _) ->
            [ br ; [ strong (userId |> userNameText userDic) ; str " successfully picked " ; str (draftPick |> draftPickText squadDic) ] |> para theme paraDefaultSmallest ]
    processingEvents |> List.collect eventElements

let private renderPicked theme picked (userDic:UserDic) (squadDic:SquadDic) =
    let draftPickElement draftPick = [ str (draftPick |> draftPickText squadDic) ] |> para theme paraDefaultSmallest
    picked
    |> List.groupBy fst
    |> List.map (fun (userId, items) ->
        let (UserName userName) = userId |> userName userDic
        userName, items |> List.map snd)
    |> List.sortBy fst
    |> List.map (fun (userName, draftPicks) ->
        [
            yield [ strong userName ; str " ended up with" ] |> para theme paraDefaultSmall
            yield! draftPicks |> List.map draftPickElement
            yield br
        ])
    |> List.collect id

let private renderProcessingDetails (useDefaultTheme, processingDetails, userDic:UserDic, squadDic:SquadDic) =
    let theme = getTheme useDefaultTheme
    let picked = processingDetails.ProcessingEvents |> List.choose (fun event -> match event with | Picked (_, draftPick, userId, _) -> (userId, draftPick) |> Some | _ -> None)
    div divDefault [
        yield! renderUserDraftPicks theme processingDetails.UserDraftPicks userDic squadDic
        yield! renderProcessingEvents theme processingDetails.ProcessingEvents userDic squadDic
        yield br
        yield! renderPicked theme picked userDic squadDic ]

let private renderActiveDraft (useDefaultTheme, state, draftId, draft:Draft, isOpen, needsMorePicks, userDraftPickDic:UserDraftPickDic, squadDic:SquadDic) dispatch =
    let theme = getTheme useDefaultTheme
    let draftTextLower = draft.DraftOrdinal |> draftTextLower
    let canInteract = isOpen && needsMorePicks
    let userDraftPickRow (userDraftPick, rank) =
        let description, extra, withdrawn =
            match userDraftPick with
            | TeamPick squadId ->
                let description = squadId |> squadDescription squadDic
                [ str description ] |> para theme paraDefaultSmallest, None, None
            | PlayerPick (squadId, playerId) ->
                let (description, extra, withdrawn) = (squadId, playerId) |> playerDescriptionAndExtraAndWithdrawn squadDic
                let withdrawn =
                    if withdrawn then
                        [ [ str "Withdrawn" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } |> Some
                    else None
                [ str description ] |> para theme paraDefaultSmallest, [ str extra ] |> para theme paraCentredSmallest |> Some, withdrawn
        let isWithdrawn = match withdrawn with | Some _ -> true | _ -> false
        let count = userDraftPickDic.Count
        let increasePriorityButton =
            let interaction, highlight =
                if canInteract |> not || isWithdrawn || rank < 2 then None, false
                else
                    match state.ChangePriorityPending, state.RemovalPending with
                    | None, None ->
                        let highlight = match state.LastPriorityChanged with | Some (lastPick, Increase) when lastPick = userDraftPick -> true | Some _ | None -> false
                        Clickable ((fun _ -> (draftId, userDraftPick, Increase) |> ChangePriority |> dispatch), None) |> Some, highlight
                    | Some (pendingPick, Increase, _), _ when pendingPick = userDraftPick -> Loading |> Some, true
                    | _ -> NotEnabled None |> Some, false
            let buttonData = if highlight then buttonPrimarySmall else buttonLinkSmall
            match interaction with
            | Some interaction ->
                [ button theme { buttonData with Interaction = interaction ; IconLeft = iconAscendingSmall |> Some } [] ]
                |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } |> Some
            | None -> None
        let decreasePriorityButton =
            let interaction, highlight =
                if canInteract |> not || isWithdrawn || rank > count - 1 then None, false
                else
                    match state.ChangePriorityPending, state.RemovalPending with
                    | None, None ->
                        let highlight = match state.LastPriorityChanged with | Some (lastPick, Decrease) when lastPick = userDraftPick -> true | Some _ | None -> false
                        Clickable ((fun _ -> (draftId, userDraftPick, Decrease) |> ChangePriority |> dispatch), None) |> Some, highlight
                    | Some (pendingPick, Decrease, _), _ when pendingPick = userDraftPick -> Loading |> Some, true
                    | _ -> NotEnabled None |> Some, false
            let buttonData = if highlight then buttonPrimarySmall else buttonLinkSmall
            match interaction with
            | Some interaction ->
                [ button theme { buttonData with Interaction = interaction ; IconLeft = iconDescendingSmall |> Some } [] ]
                |> para theme paraDefaultSmallest |> Some
            | None -> None
        let removeButton =
            if canInteract then
                let interaction =
                    match state.RemovalPending, state.ChangePriorityPending with
                    | None, None -> Clickable ((fun _ -> (draftId, userDraftPick) |> RemoveFromDraft |> dispatch), None)
                    | Some (pendingPick, _), _ when pendingPick = userDraftPick -> Loading
                    | _ -> NotEnabled None
                [ [ str (sprintf "Remove from %s" draftTextLower) ] |> button theme { buttonDangerSmall with Interaction = interaction } ] |> para theme paraDefaultSmallest |> Some
            else None
        tr false [
            td [ RctH.ofOption increasePriorityButton ]
            td [ [ str (sprintf "#%i" rank) ] |> para theme paraCentredSmallest ]
            td [ RctH.ofOption decreasePriorityButton ]
            td [ description ]
            td [ RctH.ofOption extra ]
            td [ RctH.ofOption removeButton ]
            td [ RctH.ofOption withdrawn ] ]
    let userDraftPickRows =
        userDraftPickDic |> List.ofSeq |> List.sortBy (fun (KeyValue (_, rank)) -> rank) |> List.map (fun (KeyValue (userDraftPick, rank)) -> (userDraftPick, rank) |> userDraftPickRow)
    div divCentred [
        if userDraftPickDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true } [
                thead [
                    tr false [
                        th []
                        th [ [ strong "Rank" ] |> para theme paraCentredSmallest ]
                        th []
                        th []
                        th []
                        th []
                        th [] ] ]
                tbody [ yield! userDraftPickRows ] ]
        else if needsMorePicks then
            yield [ strong (sprintf "You have not made any selections for the %s" draftTextLower) ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger }
        else
            yield [ strong (sprintf "You do not need to make any selections for the %s" draftTextLower) ] |> para theme paraCentredSmallest ]

let render (useDefaultTheme, state, authUser:AuthUser, draftsProjection:Projection<_ * DraftDic * CurrentUserDraftDto option>, usersProjection:Projection<_ * UserDic>, squadsProjection:Projection<_ * SquadDic>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ strong "Drafts" ] |> para theme paraCentredSmall
        yield hr theme false
        match draftsProjection, squadsProjection with
        | Pending, _ | _, Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed, _ | _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, draftDic, currentUserDraftDto), Ready (_, squadDic) ->
            let relevantDrafts =
                draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) -> draftId, draft)
                |> List.filter (fun (_, draft) -> match draft.DraftStatus with | Opened _ | PendingProcessing _ | Processed -> true | _ -> false)
                |> List.sortBy (fun (_, draft) -> draft.DraftOrdinal)
            match relevantDrafts |> List.rev with
            | (latestDraftId, latestDraft) :: _ ->
                let currentDraft = match state.CurrentDraftId with | Some currentDraftId -> relevantDrafts |> List.tryFind (fun (draftId, _) -> draftId = currentDraftId) | None -> None
                let currentDraftId, draft = match currentDraft with | Some (draftId, draft) -> draftId, draft | None -> latestDraftId, latestDraft
                let draftTabs = draftTabs relevantDrafts currentDraftId dispatch
                yield div divCentred [ tabs theme { tabsDefault with Tabs = draftTabs } ]
                yield br
                let isOpen = match draft.DraftStatus with | Opened _ -> true | _ -> false
                match draft.DraftStatus with
                | Opened _ | PendingProcessing _ ->
                    let userDraftPickDic = currentUserDraftDto |> userDraftPickDic
                    let squad, players = authUser.UserId |> pickedByUser squadDic
                    let pickedCounts = (squad, players) |> pickedCounts
                    let needsMorePicks = match pickedCounts |> stillRequired with | Some _ -> true | None -> false
                    yield lazyViewOrHMR2 renderActiveDraft (useDefaultTheme, state, currentDraftId, draft, isOpen, needsMorePicks, userDraftPickDic, squadDic) dispatch
                | Processed ->
                    match usersProjection, draft.ProcessingDetails with
                    | Pending, _ ->
                        yield div divCentred [ icon iconSpinnerPulseLarge ]
                    | Failed, _ -> // note: should never happen
                        yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
                    | Ready (_, userDic), Some processingDetails ->
                        yield lazyViewOrHMR renderProcessingDetails (useDefaultTheme, processingDetails, userDic, squadDic)
                    | Ready _, None ->  // note: should never happen
                        yield [ str "There are no processing details for this draft" ] |> para theme paraCentredSmaller
                | _ -> () // note: should never happen
            | [] -> yield [ str "Coming soon" ] |> para theme paraCentredSmaller ] // note: should never happen
