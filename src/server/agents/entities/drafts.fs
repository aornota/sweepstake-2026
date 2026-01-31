module Aornota.Sweepstake2026.Server.Agents.Entities.Drafts

(* Broadcasts: SendMsg
               DraftsRead
               UserDraftsRead
   Subscribes: Tick
               SquadsRead
               SquadEventWritten (PlayerAdded | PlayerTypeChanged | PlayerWithdrawn)
               DraftsEventsRead
               UserDraftsEventsRead *)

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Agents.Persistence
open Aornota.Sweepstake2026.Server.Agents.Ticker
open Aornota.Sweepstake2026.Server.Authorization
open Aornota.Sweepstake2026.Server.Common.Helpers
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Events.DraftEvents
open Aornota.Sweepstake2026.Server.Events.SquadEvents
open Aornota.Sweepstake2026.Server.Events.UserDraftEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private RemoveFromDraftCmdResultToServerMsg = Result<UserDraftPick, UserDraftPick * AuthCmdError<string>> -> ServerMsg

type private DraftsInput =
    | IsAwaitingStart of reply : AsyncReplyChannel<bool>
    | Start of reply : AsyncReplyChannel<unit>
    | Reset of reply : AsyncReplyChannel<unit>
    | Housekeeping
    | OnSquadsRead of squadsRead : SquadRead list
    | OnPlayerAdded of squadId : SquadId * playerId : PlayerId * playerType : PlayerType
    | OnPlayerTypeChanged of squadId : SquadId * playerId : PlayerId * playerType : PlayerType
    | OnPlayerWithdrawn of squadId : SquadId * playerId : PlayerId
    | OnDraftsEventsRead of draftsEvents : (DraftId * (Rvn * DraftEvent) list) list
    | OnUserDraftsEventsRead of userDraftsEvents : (UserDraftId * (Rvn * UserDraftEvent) list) list
    | HandleCreateDraftCmd of token : ProcessDraftToken * auditUserId : UserId * draftId : DraftId * draftOrdinal : DraftOrdinal * draftType : DraftType
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleProcessDraftCmd of token : ProcessDraftToken * auditUserId : UserId * draftId : DraftId * currentRvn : Rvn * connectionId : ConnectionId
    | HandleChangePriorityCmd of token : DraftToken * auditUserId : UserId * draftId : DraftId * currentRvn : Rvn * userDraftPick : UserDraftPick
        * priorityChange : PriorityChange * connectionId : ConnectionId
    | HandleAddToDraftCmd of token : DraftToken * auditUserId : UserId * draftId : DraftId * currentRvn : Rvn * userDraftPick : UserDraftPick
        * connectionId : ConnectionId
    | HandleRemoveFromDraftCmd of token : DraftToken * auditUserId : UserId * draftId : DraftId * currentRvn : Rvn * userDraftPick : UserDraftPick
        * toServerMsg : RemoveFromDraftCmdResultToServerMsg * connectionId : ConnectionId
    | HandleFreePickCmd of token : DraftToken * auditUserId : UserId * draftId : DraftId * currentRvn : Rvn * draftPick : DraftPick * connectionId : ConnectionId

type private PickPriorityDic = Dictionary<UserId, uint32>

type private Draft =
    { Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus ; DraftPicks : (DraftPick * PickedBy) list ; ProcessingEvents : ProcessingEvent list
      PickPriorityDic : PickPriorityDic }
type private DraftDic = Dictionary<DraftId, Draft>

type private UserDraftPickDic = Dictionary<UserDraftPick, int>
type private UserDraft = { Rvn : Rvn ; UserDraftKey : UserDraftKey ; UserDraftPickDic : UserDraftPickDic }
type private UserDraftDic = Dictionary<UserDraftId, UserDraft>
type private UserDraftLookupDic = Dictionary<UserDraftKey, UserDraftId>

type private Player = { PlayerType : PlayerType ; Withdrawn : bool }
type private PlayerDic = Dictionary<PlayerId, Player>
type private SquadDic = Dictionary<SquadId, PlayerDic>

let [<Literal>] private HOUSEKEEPING_INTERVAL = 1.<minute>

let private log category = (Entity Entity.Drafts, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private agentId = Guid "ffffffff-ffff-ffff-ffff-000000000001" |> UserId

let private applyDraftEvent source (idAndDraftResult:Result<DraftId * Draft option, OtherError<string>>) (nextRvn, draftEvent:DraftEvent) =
    let otherError errorText = otherError (sprintf "%s#applyDraftEvent" source) errorText
    match idAndDraftResult, draftEvent with
    | Ok (draftId, _), _ when draftId <> draftEvent.DraftId -> // note: should never happen
        ifDebug (sprintf "DraftId mismatch for %A -> %A" draftId draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" draftId nextRvn draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), _ when validateNextRvn (Some draft.Rvn) nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" draftId draft.Rvn nextRvn draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, None), DraftCreated (_, draftOrdinal, draftType) ->
        let draft =
            { Rvn = initialRvn ; DraftOrdinal = draftOrdinal ; DraftStatus = draftType |> defaultDraftStatus ; DraftPicks = [] ; ProcessingEvents = []
              PickPriorityDic = PickPriorityDic () }
        (draftId, draft |> Some) |> Ok
    | Ok (draftId, None), _ -> // note: should never happen
        ifDebug (sprintf "Invalid initial DraftEvent for %A -> %A" draftId draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftCreated _ -> // note: should never happen
        ifDebug (sprintf "Invalid non-initial DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftOpened _ ->
        match draft.DraftStatus with
        | PendingOpen (_, ends) ->
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = ends |> Opened } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftPendingProcessing _ ->
        match draft.DraftStatus with
        | Opened _ ->
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = PendingProcessing false } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftProcessed _ ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = Processed } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftFreeSelection _ ->
        match draft.DraftStatus with
        | PendingFreeSelection ->
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = FreeSelection } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.ProcessingStarted (_, seed) ->
        match draft.DraftStatus with
        | PendingProcessing false ->
            let processingEvents = (seed |> ProcessingEvent.ProcessingStarted) :: draft.ProcessingEvents
            (draftId, { draft with Rvn = nextRvn ; DraftStatus = PendingProcessing true ; ProcessingEvents = processingEvents } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.WithdrawnPlayersIgnored (_, ignored) ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            let processingEvents = (ignored |> ProcessingEvent.WithdrawnPlayersIgnored) :: draft.ProcessingEvents
            (draftId, { draft with Rvn = nextRvn ; ProcessingEvents = processingEvents } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.RoundStarted (_, round) ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            let processingEvents = (round |> ProcessingEvent.RoundStarted) :: draft.ProcessingEvents
            (draftId, { draft with Rvn = nextRvn ; ProcessingEvents = processingEvents } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.AlreadyPickedIgnored (_, ignored) ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            let processingEvents = (ignored |> ProcessingEvent.AlreadyPickedIgnored) :: draft.ProcessingEvents
            (draftId, { draft with Rvn = nextRvn ; ProcessingEvents = processingEvents } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.NoLongerRequiredIgnored (_, ignored) ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            let processingEvents = (ignored |> ProcessingEvent.NoLongerRequiredIgnored) :: draft.ProcessingEvents
            (draftId, { draft with Rvn = nextRvn ; ProcessingEvents = processingEvents } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.UncontestedPick (_, draftPick, userId) ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            let processingEvents = ((draftPick, userId) |> ProcessingEvent.UncontestedPick) :: draft.ProcessingEvents
            (draftId, { draft with Rvn = nextRvn ; ProcessingEvents = processingEvents } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.ContestedPick (_, draftPick, userDetails, winner) ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            let processingEvents = ((draftPick, userDetails, winner) |> ProcessingEvent.ContestedPick) :: draft.ProcessingEvents
            (draftId, { draft with Rvn = nextRvn ; ProcessingEvents = processingEvents } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.PickPriorityChanged (_, userId, pickPriority) ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            let processingEvents = ((userId, pickPriority) |> ProcessingEvent.PickPriorityChanged) :: draft.ProcessingEvents
            let pickPriorityDic = draft.PickPriorityDic
            if userId |> pickPriorityDic.ContainsKey then pickPriorityDic.[userId] <- pickPriority else (userId, pickPriority) |> pickPriorityDic.Add
            (draftId, { draft with Rvn = nextRvn ; ProcessingEvents = processingEvents } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.Picked (_, draftOrdinal, draftPick, userId, timestamp) ->
        match draft.DraftStatus with
        | PendingProcessing true ->
            let draftPicks = (draftPick, (userId, draftOrdinal |> Some, timestamp)) :: draft.DraftPicks
            let processingEvents = ((draftOrdinal, draftPick, userId, timestamp) |> ProcessingEvent.Picked) :: draft.ProcessingEvents
            (draftId, { draft with Rvn = nextRvn ; DraftPicks = draftPicks ; ProcessingEvents = processingEvents } |> Some)
            |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (draftId, Some draft), DraftEvent.FreePick (_, draftPick, userId, timestamp) ->
        match draft.DraftStatus with
        | FreeSelection ->
            let draftPicks = (draftPick, (userId, None, timestamp)) :: draft.DraftPicks
            (draftId, { draft with Rvn = nextRvn ; DraftPicks = draftPicks } |> Some) |> Ok
        | _ -> ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError
    (*| Ok (draftId, Some draft), _ -> // note: should never happen
        ifDebug (sprintf "Invalid DraftEvent for %A (%A) -> %A" draftId draft draftEvent) UNEXPECTED_ERROR |> otherError*)
    | Error error, _ -> error |> Error

let private initializeDrafts source (draftsEvents:(DraftId * (Rvn * DraftEvent) list) list) =
    let source = sprintf "%s#initializeDrafts" source
    let draftDic = DraftDic ()
    let results =
        draftsEvents
        |> List.map (fun (draftId, events) ->
            match events with
            | _ :: _ -> events |> List.fold (fun idAndDraftResult (rvn, draftEvent) -> applyDraftEvent source idAndDraftResult (rvn, draftEvent)) (Ok (draftId, None))
            | [] -> ifDebug (sprintf "No DraftEvents for %A" draftId) UNEXPECTED_ERROR |> otherError source) // note: should never happen
    results
    |> List.choose (fun idAndDraftResult -> match idAndDraftResult with | Ok (draftId, Some draft) -> (draftId, draft) |> Some | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (draftId, draft) -> draftDic.Add (draftId, draft))
    let errors =
        results
        |> List.choose (fun idAndDraftResult ->
            match idAndDraftResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> ifDebug (sprintf "%s: applyDraftEvent returned Ok (_, None)" source) UNEXPECTED_ERROR |> OtherError |> Some // note: should never happen
            | Error error -> error |> Some)
    draftDic, errors

let private updateDraft draftId draft (draftDic:DraftDic) = if draftId |> draftDic.ContainsKey then draftDic.[draftId] <- draft

let private tryFindDraft draftId onError (draftDic:DraftDic) =
    if draftId |> draftDic.ContainsKey then (draftId, draftDic.[draftId]) |> Ok else ifDebug (sprintf "%A does not exist" draftId) UNEXPECTED_ERROR |> onError

let private tryApplyDraftEvent source draftId draft nextRvn thing draftEvent =
    match applyDraftEvent source (Ok (draftId, draft)) (nextRvn, draftEvent) with
    | Ok (_, Some post) -> (post, nextRvn, draftEvent, thing) |> Ok
    | Ok (_, None) -> ifDebug "applyDraftEvent returned Ok (_, None)" UNEXPECTED_ERROR |> otherCmdError source // note: should never happen
    | Error otherError -> otherError |> OtherAuthCmdError |> Error

let private tryWriteDraftEventAsync auditUserId rvn draftEvent (draft:Draft) thing = async {
    let! result = (auditUserId, rvn, draftEvent) |> persistence.WriteDraftEventAsync
    return match result with | Ok _ -> (draftEvent.DraftId, draft, thing) |> Ok | Error persistenceError -> persistenceError |> AuthCmdPersistenceError |> Error }

let rec private tryApplyAndWriteDraftEventsAsync source auditUserId currentRvn draftId draft draftEvents = async {
    match draftEvents with
    | draftEvent :: draftEvents ->
        match tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn currentRvn) () draftEvent with
        | Ok (draft, rvn, draftEvent, _) ->
            let! result = tryWriteDraftEventAsync auditUserId rvn draftEvent draft ()
            match result with
            | Ok (draftId, draft, _) -> return! draftEvents |> tryApplyAndWriteDraftEventsAsync source auditUserId rvn draftId draft
            | Error error -> return error |> Error
        | Error error -> return error |> Error
    | [] -> return (draftId, draft, currentRvn) |> Ok }

type private DraftPickSet = HashSet<DraftPick>

type private UserStatus = { UserId : UserId ; AlreadyPicked : DraftPickSet ; PendingPicks : UserDraftPick list ; PickPriority : uint32 }

let private ignoreWithdrawnPlayers draftId (squadDic:SquadDic) (userStatuses:UserStatus list) =
    let isWithdrawnOrUnknown (squadId, playerId) =
        if squadId |> squadDic.ContainsKey then
            let playerDic = squadDic.[squadId]
            if playerId |> playerDic.ContainsKey then playerDic.[playerId].Withdrawn else false
        else true
    let ignored = Dictionary<UserId, HashSet<SquadId * PlayerId>> ()
    let userStatuses =
        userStatuses |> List.choose (fun userStatus ->
            let ignoredForUser = HashSet<SquadId * PlayerId> ()
            let pendingPicks =
                userStatus.PendingPicks |> List.choose (fun userDraftPick ->
                    match userDraftPick with
                    | TeamPick _ -> userDraftPick |> Some
                    | PlayerPick (squadId, playerId) ->
                        if (squadId, playerId) |> isWithdrawnOrUnknown then
                            (squadId, playerId) |> ignoredForUser.Add |> ignore
                            None
                        else userDraftPick |> Some)
            if ignoredForUser.Count > 0 then (userStatus.UserId, ignoredForUser) |> ignored.Add |> ignore
            if pendingPicks.Length > 0 then { userStatus with PendingPicks = pendingPicks } |> Some else None)
    let events =
        if ignored.Count > 0 then
            let ignored = ignored |> List.ofSeq |> List.map (fun (KeyValue (userId, playerIdSet)) -> userId, playerIdSet |> List.ofSeq)
            [ (draftId, ignored) |> DraftEvent.WithdrawnPlayersIgnored ]
        else []
    userStatuses, events

let private pendingCount (userStatuses:UserStatus list) = userStatuses |> List.sumBy (fun userStatus -> userStatus.PendingPicks.Length)

let private ignoreAlreadyPicked draftId (allPicked:DraftPickSet) (userStatuses:UserStatus list) =
    let ignored = Dictionary<UserId, HashSet<DraftPick>> ()
    let userStatuses =
        userStatuses |> List.choose (fun userStatus ->
            let ignoredForUser = HashSet<DraftPick> ()
            let pendingPicks =
                userStatus.PendingPicks |> List.choose (fun userDraftPick ->
                    match userDraftPick with
                    | TeamPick squadId ->
                        if (squadId |> TeamPicked) |> allPicked.Contains then
                            (squadId |> TeamPicked) |> ignoredForUser.Add |> ignore
                            None
                        else userDraftPick |> Some
                    | PlayerPick (squadId, playerId) ->
                        if ((squadId, playerId) |> PlayerPicked) |> allPicked.Contains then
                            ((squadId, playerId) |> PlayerPicked) |> ignoredForUser.Add |> ignore
                            None
                        else userDraftPick |> Some)
            if ignoredForUser.Count > 0 then (userStatus.UserId, ignoredForUser) |> ignored.Add |> ignore
            if pendingPicks.Length > 0 then { userStatus with PendingPicks = pendingPicks } |> Some else None)
    let events =
        if ignored.Count > 0 then
            let ignored = ignored |> List.ofSeq |> List.map (fun (KeyValue (userId, draftPickSet)) -> userId, draftPickSet |> List.ofSeq)
            [ (draftId, ignored) |> DraftEvent.AlreadyPickedIgnored ]
        else []
    userStatuses, events

let private playerTypeAndWithdrawn (squadDic:SquadDic) (squadId, playerId) =
    if squadId |> squadDic.ContainsKey then
        let playerDic = squadDic.[squadId]
        if playerId |> playerDic.ContainsKey then
            let player = playerDic.[playerId]
            (player.PlayerType, player.Withdrawn) |> Some
        else None
    else None

let private teamPickCount (alreadyPicked:DraftPickSet) =
    alreadyPicked |> List.ofSeq |> List.filter (fun draftPick -> match draftPick with | TeamPicked _ -> true | PlayerPicked _ -> false) |> List.length

let private goalkeeperPickCount (squadDic:SquadDic) (alreadyPicked:DraftPickSet) =
    alreadyPicked |> List.ofSeq |> List.filter (fun draftPick ->
        match draftPick with
        | TeamPicked _ -> false
        | PlayerPicked (squadId, playerId) -> match (squadId, playerId) |> playerTypeAndWithdrawn squadDic with | Some (Goalkeeper, false) -> true | _ -> false) |> List.length

let private outfieldPlayerPickCount (squadDic:SquadDic) (alreadyPicked:DraftPickSet) =
    alreadyPicked |> List.ofSeq |> List.filter (fun draftPick ->
        match draftPick with
        | TeamPicked _ -> false
        | PlayerPicked (squadId, playerId) ->
            match (squadId, playerId) |> playerTypeAndWithdrawn squadDic with | Some (Goalkeeper, _) -> false | Some (_, false) -> true | _ -> false) |> List.length

let private ignoreNoLongerRequired draftId (squadDic:SquadDic) (userStatuses:UserStatus list) =
    let isTeamRequired (alreadyPicked:DraftPickSet) = alreadyPicked |> teamPickCount < MAX_TEAM_PICKS
    let isPlayerRequired (alreadyPicked:DraftPickSet) (squadId, playerId) =
        match (squadId, playerId) |> playerTypeAndWithdrawn squadDic with
        | Some (Goalkeeper, _) -> alreadyPicked |> goalkeeperPickCount squadDic < MAX_GOALKEEPER_PICKS
        | Some (_) -> alreadyPicked |> outfieldPlayerPickCount squadDic < MAX_OUTFIELD_PLAYER_PICKS
        | None -> false // note: should never happen
    let ignored = Dictionary<UserId, HashSet<DraftPick>> ()
    let userStatuses =
        userStatuses |> List.choose (fun userStatus ->
            let ignoredForUser = HashSet<DraftPick> ()
            let pendingPicks =
                userStatus.PendingPicks |> List.choose (fun userDraftPick ->
                    match userDraftPick with
                    | TeamPick squadId ->
                        if isTeamRequired userStatus.AlreadyPicked |> not then
                            (squadId |> TeamPicked) |> ignoredForUser.Add |> ignore
                            None
                        else userDraftPick |> Some
                    | PlayerPick (squadId, playerId) ->
                        if (squadId, playerId) |> isPlayerRequired userStatus.AlreadyPicked |> not then
                            ((squadId, playerId) |> PlayerPicked) |> ignoredForUser.Add |> ignore
                            None
                        else userDraftPick |> Some)
            if ignoredForUser.Count > 0 then (userStatus.UserId, ignoredForUser) |> ignored.Add |> ignore
            if pendingPicks.Length > 0 then { userStatus with PendingPicks = pendingPicks } |> Some else None)
    let events =
        if ignored.Count > 0 then
            let ignored = ignored |> List.ofSeq |> List.map (fun (KeyValue (userId, draftPickSet)) -> userId, draftPickSet |> List.ofSeq)
            [ (draftId, ignored) |> DraftEvent.NoLongerRequiredIgnored ]
        else []
    userStatuses, events

let private processTopPicks (random:Random) draftId draftOrdinal (allPicked:DraftPickSet) (userStatuses:UserStatus list) =
    let draftPick userDraftPick = match userDraftPick with | TeamPick squadId -> squadId |> TeamPicked | PlayerPick (squadId, playerId) -> (squadId, playerId) |> PlayerPicked
    let events = HashSet<DraftEvent> ()
    let topPicks =
        userStatuses |> List.choose (fun userStatus -> match userStatus.PendingPicks with | userDraftPick :: _ -> (userDraftPick |> draftPick, userStatus.UserId) |> Some | [] -> None)
    "Processing uncontested picks" |> Verbose |> log
    let uncontestedPicks = topPicks |> List.groupBy fst |> List.choose (fun (_, pairs) -> match pairs with | [ draftPick, userId ] -> (draftPick, userId) |> Some | _ -> None)
    let userStatuses =
        userStatuses |> List.map (fun userStatus ->
            match uncontestedPicks |> List.filter (fun (_, userId) -> userId = userStatus.UserId) with
            | (draftPick, _) :: _ ->
                (draftId, draftPick, userStatus.UserId) |> DraftEvent.UncontestedPick |> events.Add |> ignore
                (draftId, draftOrdinal, draftPick, userStatus.UserId, DateTimeOffset.UtcNow) |> DraftEvent.Picked |> events.Add |> ignore
                draftPick |> allPicked.Add |> ignore
                draftPick |> userStatus.AlreadyPicked.Add |> ignore
                userStatus
            | [] -> userStatus)
    "Processing contested picks" |> Verbose |> log
    let contestedPicks = topPicks |> List.groupBy fst |> List.choose (fun (draftPick, pairs) -> match pairs with | [ _ ] | [] -> None | _ -> (draftPick, pairs |> List.map snd) |> Some)
    let contestedPicks =
        contestedPicks |> List.map (fun (draftPick, userIds) ->
            let userDetails = userIds |> List.choose (fun userId ->
                match userStatuses |> List.choose (fun userStatus -> if userStatus.UserId = userId then (userId, userStatus.PickPriority) |> Some else None) with
                | [ (userId, pickPriority) ] -> (userId, pickPriority) |> Some
                | _ -> None)
            let highestPriority = userDetails |> List.maxBy snd |> snd
            let userDetails = userDetails |> List.map (fun (userId, pickPriority) ->
                let coinToss = if pickPriority = highestPriority then random.NextDouble () |> Some else None
                userId, pickPriority, coinToss)
            let winner, _, _ = userDetails |> List.maxBy (fun (_, _, coinToss) -> match coinToss with | Some coinToss -> coinToss | None -> -1.)
            draftPick, userDetails, winner)
    contestedPicks |> List.iter (fun (draftPick, userDetails, winner) -> (draftId, draftPick, userDetails, winner) |> DraftEvent.ContestedPick |> events.Add |> ignore)
    let userStatuses =
        userStatuses |> List.map (fun userStatus ->
            match contestedPicks |> List.filter (fun (_, userDetails, _) -> userDetails |> List.exists (fun (userId, _, _) -> userId = userStatus.UserId)) with
            | (draftPick, _, winner) :: _ ->
                let userId = userStatus.UserId
                let isWinner = winner = userId
                let pickPriority =
                    if isWinner then
                        (draftId, draftOrdinal, draftPick, userId, DateTimeOffset.UtcNow) |> DraftEvent.Picked |> events.Add |> ignore
                        draftPick |> allPicked.Add |> ignore
                        draftPick |> userStatus.AlreadyPicked.Add |> ignore
                        userStatus.PickPriority
                    else
                        let pickPriority = userStatus.PickPriority + 1u
                        (draftId, userId, pickPriority) |> DraftEvent.PickPriorityChanged |> events.Add |> ignore
                        pickPriority
                { userStatus with PickPriority = pickPriority }
            | [] -> userStatus)
    // Note: Finally, remember to remove "top picks"!
    let userStatuses =
        userStatuses |> List.choose (fun userStatus ->
            let pendingPicks = match userStatus.PendingPicks with | _ :: pendingPicks -> pendingPicks | [] -> []
            if pendingPicks.Length > 0 then { userStatus with PendingPicks = pendingPicks } |> Some else None)
    userStatuses, events |> List.ofSeq

let rec private processRounds (random:Random) draftId draftOrdinal (userStatuses:UserStatus list) (allPicked:DraftPickSet) squadDic round (allEvents:DraftEvent list) =
    let initialPendingCount = userStatuses |> pendingCount
    sprintf "Processing round #%i -> (%i user status/es) (%i pending pick/s) (%i already picked)" round userStatuses.Length initialPendingCount allPicked.Count |> Verbose |> log
    let roundEvents = [ (draftId, round) |> DraftEvent.RoundStarted ]
    "Ignoring already picked teams/players" |> Verbose |> log
    let userStatuses, events = userStatuses |> ignoreAlreadyPicked draftId allPicked
    let roundEvents = roundEvents @ events
    "Ignoring no-longer-required teams/players" |> Verbose |> log
    let userStatuses, events = userStatuses |> ignoreNoLongerRequired draftId squadDic
    let roundEvents = roundEvents @ events
    if userStatuses.Length > 0 then
        sprintf "Processing top picks for round #%i (%i user status/es)" round userStatuses.Length |> Verbose |> log
        let userStatuses, events = userStatuses |> processTopPicks random draftId draftOrdinal allPicked
        let roundEvents = roundEvents @ events
        if userStatuses |> pendingCount = initialPendingCount then sprintf "Pending pick/s count (%i) did not change during round #%i" initialPendingCount round |> OtherError |> Error
        else processRounds random draftId draftOrdinal userStatuses allPicked squadDic (round + 1u) (allEvents @ roundEvents)
    else
        "All user draft picks processed" |> Verbose |> log
        (allEvents @ roundEvents) |> Ok

let private processDraft source draftId draftOrdinal (pickPriorityDic:PickPriorityDic) (userDrafts:UserDraft list) (existingPicks:(DraftPick * UserId) list) squadDic : Result<DraftEvent list, OtherError<string>> =
    sprintf "%s for %A (%i user draft/s) (%i already picked)" source draftId userDrafts.Length existingPicks.Length |> Verbose |> log
    let userStatuses =
        userDrafts |> List.map (fun userDraft ->
            let (userId, _) = userDraft.UserDraftKey
            let alreadyPicked = DraftPickSet ()
            existingPicks |> List.iter (fun (draftPick, forUserId) -> if forUserId = userId then draftPick |> alreadyPicked.Add |> ignore)
            let pendingPicks = userDraft.UserDraftPickDic |> List.ofSeq |> List.sortBy (fun (KeyValue (_, rank)) -> rank) |> List.map (fun (KeyValue (userDraftPick, _)) -> userDraftPick)
            let pickPriority = if userId |> pickPriorityDic.ContainsKey then pickPriorityDic.[userId] else 0u
            { UserId = userId ; AlreadyPicked = alreadyPicked ; PendingPicks = pendingPicks ; PickPriority = pickPriority } )
    let allPicked = DraftPickSet ()
    existingPicks |> List.iter (fun (draftPick, _) -> draftPick |> allPicked.Add |> ignore)
    "Ignoring withdrawn players" |> Verbose |> log
    let userStatuses, events = userStatuses |> ignoreWithdrawnPlayers draftId squadDic
    let seed = 12345678
    sprintf "Using random seed %i" seed |> Verbose |> log
    let random = Random seed
    processRounds random draftId draftOrdinal userStatuses allPicked squadDic 1u ([ (draftId, seed) |> DraftEvent.ProcessingStarted ] @ events)

let private applyUserDraftEvent source idAndUserDraftResult (nextRvn, userDraftEvent:UserDraftEvent) =
    let otherError errorText = otherError (sprintf "%s#applyUserDraftEvent" source) errorText
    match idAndUserDraftResult, userDraftEvent with
    | Ok (userDraftId, _), _ when userDraftId <> userDraftEvent.UserDraftId -> // note: should never happen
        ifDebug (sprintf "UserDraftId mismatch for %A -> %A" userDraftId userDraftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userDraftId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" userDraftId nextRvn userDraftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userDraftId, Some userDraft), _ when validateNextRvn (Some userDraft.Rvn) nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" userDraftId userDraft.Rvn nextRvn userDraftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userDraftId, None), UserDraftCreated (_, userId, draftId) ->
        (userDraftId, { Rvn = initialRvn ; UserDraftKey = userId, draftId ; UserDraftPickDic = UserDraftPickDic () } |> Some) |> Ok
    | Ok (userDraftId, None), _ -> // note: should never happen
        ifDebug (sprintf "Invalid initial UserDraftEvent for %A -> %A" userDraftId userDraftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userDraftId, Some userDraft), UserDraftCreated _ -> // note: should never happen
        ifDebug (sprintf "Invalid non-initial UserDraftEvent for %A (%A) -> %A" userDraftId userDraft userDraftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userDraftId, Some userDraft), Drafted (_, userDraftPick) ->
        let userDraftPickDic = userDraft.UserDraftPickDic
        if userDraftPick |> userDraftPickDic.ContainsKey |> not then
            (userDraftPick, userDraftPickDic.Count + 1) |> userDraftPickDic.Add
            (userDraftId, { userDraft with Rvn = nextRvn } |> Some) |> Ok
        else ifDebug (sprintf "%A already drafted for %A (%A) -> %A" userDraftPick userDraftId userDraft userDraftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userDraftId, Some userDraft), Undrafted (_, userDraftPick) ->
        let userDraftPickDic = userDraft.UserDraftPickDic
        if userDraftPick |> userDraftPickDic.ContainsKey then
            let updatedUserDraftPickDic = UserDraftPickDic ()
            userDraftPick |> userDraftPickDic.Remove |> ignore
            userDraftPickDic |> List.ofSeq |> List.map (fun (KeyValue (userDraftPick, rank)) -> userDraftPick, rank) |> List.sortBy snd |> List.iteri (fun i (userDraftPick, _) ->
                (userDraftPick, i + 1) |> updatedUserDraftPickDic.Add)
            (userDraftId, { userDraft with Rvn = nextRvn ; UserDraftPickDic = updatedUserDraftPickDic } |> Some) |> Ok
        else ifDebug (sprintf "%A not already drafted for %A (%A) -> %A" userDraftPick userDraftId userDraft userDraftEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userDraftId, Some userDraft), PriorityChanged (_, userDraftPick, priorityChange) ->
        let userDraftPickDic = userDraft.UserDraftPickDic
        if userDraftPick |> userDraftPickDic.ContainsKey then
            let adjustment = match priorityChange with | Increase -> -1.5 | Decrease -> 1.5
            let updatedUserDraftPickDic = UserDraftPickDic ()
            userDraftPickDic
            |> List.ofSeq
            |> List.map (fun (KeyValue (existingUserDraftPick, rank)) ->
                let adjustedRank = if existingUserDraftPick = userDraftPick then (float rank) + adjustment else float rank
                existingUserDraftPick, adjustedRank)
            |> List.sortBy snd
            |> List.iteri (fun i (userDraftPick, _) -> (userDraftPick, i + 1) |> updatedUserDraftPickDic.Add)
            (userDraftId, { userDraft with Rvn = nextRvn ; UserDraftPickDic = updatedUserDraftPickDic } |> Some) |> Ok
        else ifDebug (sprintf "%A not already drafted for %A (%A) -> %A" userDraftPick userDraftId userDraft userDraftEvent) UNEXPECTED_ERROR |> otherError
    | Error error, _ -> error |> Error

let private initializeUserDrafts source (userDraftsEvents:(UserDraftId * (Rvn * UserDraftEvent) list) list) =
    let source = sprintf "%s#initializeUserDrafts" source
    let userDraftDic = UserDraftDic ()
    let results =
        userDraftsEvents
        |> List.map (fun (userDraftId, events) ->
            match events with
            | _ :: _ -> events |> List.fold (fun idAndUserDraftResult (rvn, userDraftEvent) -> applyUserDraftEvent source idAndUserDraftResult (rvn, userDraftEvent)) (Ok (userDraftId, None))
            | [] -> ifDebug (sprintf "No UserDraftEvents for %A" userDraftId) UNEXPECTED_ERROR |> otherError source) // note: should never happen
    results
    |> List.choose (fun idAndUserDraftResult -> match idAndUserDraftResult with | Ok (userDraftId, Some userDraft) -> (userDraftId, userDraft) |> Some | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (userDraftId, userDraft) -> userDraftDic.Add (userDraftId, userDraft))
    let errors =
        results
        |> List.choose (fun idAndUserDraftResult ->
            match idAndUserDraftResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> ifDebug (sprintf "%s: applyUserDraftEvent returned Ok (_, None)" source) UNEXPECTED_ERROR |> OtherError |> Some // note: should never happen
            | Error error -> error |> Some)
    userDraftDic, errors

let private updateUserDraft userDraftId userDraft (userDraftDic:UserDraftDic) = if userDraftId |> userDraftDic.ContainsKey then userDraftDic.[userDraftId] <- userDraft

let private tryFindUserDraft userId draftId onError (userDraftDic:UserDraftDic) (userDraftLookupDic:UserDraftLookupDic) =
    if (userId, draftId) |> userDraftLookupDic.ContainsKey then
        let userDraftId = userDraftLookupDic.[(userId, draftId)]
        if userDraftId |> userDraftDic.ContainsKey then
            ((userId, draftId), userDraftId, userDraftDic.[userDraftId]) |> Ok
        else ifDebug (sprintf "%A does not exist" userDraftId) UNEXPECTED_ERROR |> onError
    else ifDebug (sprintf "(%A %A) does not exist" userId draftId) UNEXPECTED_ERROR |> onError

let private tryApplyUserDraftEvent source userDraftId userDraft nextRvn thing userDraftEvent =
    match applyUserDraftEvent source (Ok (userDraftId, userDraft)) (nextRvn, userDraftEvent) with
    | Ok (_, Some userDraft) -> (userDraft, nextRvn, userDraftEvent, thing) |> Ok
    | Ok (_, None) -> ifDebug "applyUserDraftEvent returned Ok (_, None)" UNEXPECTED_ERROR |> otherCmdError source // note: should never happen
    | Error otherError -> otherError |> OtherAuthCmdError |> Error

let private tryWriteUserDraftEventAsync auditUserId rvn userDraftEvent (userDraft:UserDraft) thing = async {
    let! result = (auditUserId, rvn, userDraftEvent) |> persistence.WriteUserDraftEventAsync
    return match result with | Ok _ -> (userDraftEvent.UserDraftId, userDraft, thing) |> Ok | Error persistenceError -> persistenceError |> AuthCmdPersistenceError |> Error }

let private ifAllRead (draftDic:DraftDic option, userDraftDic:UserDraftDic option, squadsRead:(SquadRead list) option) =
    match draftDic, userDraftDic, squadsRead with
    | Some draftDic, Some userDraftDic, Some squadsRead ->
        let userDraftLookupDic = UserDraftLookupDic ()
        userDraftDic |> List.ofSeq |> List.iter (fun (KeyValue (userDraftId, userDraft)) -> (userDraft.UserDraftKey, userDraftId) |> userDraftLookupDic.Add)
        let squadDic = SquadDic ()
        squadsRead
        |> List.iter (fun squadRead ->
            let playerDic = PlayerDic ()
            squadRead.PlayersRead |> List.iter (fun playerRead ->
                let withdrawn = match playerRead.PlayerStatus with | Active -> false | Withdrawn _ -> true
                (playerRead.PlayerId, { PlayerType = playerRead.PlayerType ; Withdrawn = withdrawn }) |> playerDic.Add)
            (squadRead.SquadId, playerDic) |> squadDic.Add)
        (draftDic, userDraftDic, userDraftLookupDic, squadDic) |> Some
    | _ -> None

let private housekeeping source (draftDic:DraftDic) =
    // Note: Only limited "validation" (since other scenarios should never arise).
    let now = DateTimeOffset.UtcNow
    let pendingProcessing = draftDic |> List.ofSeq |> List.choose (fun (KeyValue (_, draft)) ->
        match draft.DraftStatus with | PendingProcessing _ -> draft |> Some | _ -> None)
    if pendingProcessing.Length = 0 then
        let noLongerPendingOpen =
            draftDic |> List.ofSeq |> List.choose (fun (KeyValue (draftId, draft)) ->
                match draft.DraftStatus with | PendingOpen (starts, _) when starts < now -> (draftId, draft, starts) |> Some | _ -> None)
            |> List.sortBy (fun (_, _, starts) -> starts)
        match noLongerPendingOpen with
        | (draftId, draft, _) :: _ ->
            let result =
                draftId |> DraftOpened |> tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn draft.Rvn) ()
                |> Result.bind (fun (draft, rvn, draftEvent, _) -> async { return! tryWriteDraftEventAsync agentId rvn draftEvent draft () } |> Async.RunSynchronously)
            result |> logResult source (fun (draftId, draft, _) -> sprintf "Audit%A %A %A" agentId draftId draft |> Some)
            match result with | Ok (draftId, draft, _) -> draftDic |> updateDraft draftId draft | Error _ -> ()
        | [] -> ()
    let noLongerOpened = draftDic |> List.ofSeq |> List.choose (fun (KeyValue (draftId, draft)) ->
        match draft.DraftStatus with | Opened ends when ends < now -> (draftId, draft) |> Some | _ -> None)
    noLongerOpened |> List.iter (fun (draftId, draft) ->
        let result =
            draftId |> DraftPendingProcessing |> tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn draft.Rvn) ()
            |> Result.bind (fun (draft, rvn, draftEvent, _) -> async { return! tryWriteDraftEventAsync agentId rvn draftEvent draft () } |> Async.RunSynchronously)
        result |> logResult source (fun (draftId, draft, _) -> sprintf "Audit%A %A %A" agentId draftId draft |> Some)
        match result with | Ok (draftId, draft, _) -> draftDic |> updateDraft draftId draft | Error _ -> ())
    let unprocessed = draftDic |> List.ofSeq |> List.choose (fun (KeyValue (_, draft)) ->
        match draft.DraftStatus with | PendingOpen _ | Opened _ | PendingProcessing _ -> draft |> Some | _ -> None)
    if unprocessed.Length = 0 then
        let noLongerPendingFreeSelection = draftDic |> List.ofSeq |> List.choose (fun (KeyValue (draftId, draft)) ->
            match draft.DraftStatus with | PendingFreeSelection -> (draftId, draft) |> Some | _ -> None)
        noLongerPendingFreeSelection |> List.iter (fun (draftId, draft) ->
            let result =
                draftId |> DraftFreeSelection |> tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn draft.Rvn) ()
                |> Result.bind (fun (draft, rvn, draftEvent, _) -> async { return! tryWriteDraftEventAsync agentId rvn draftEvent draft () } |> Async.RunSynchronously)
            result |> logResult source (fun (draftId, draft, _) -> sprintf "Audit%A %A %A" agentId draftId draft |> Some)
            match result with | Ok (draftId, draft, _) -> draftDic |> updateDraft draftId draft | Error _ -> ())

type Drafts () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> true |> reply.Reply ; return! awaitingStart ()
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead" |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None None
            | Reset _ -> "Reset when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | Housekeeping -> "Housekeeping when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSquadsRead _ -> "OnSquadsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerAdded _ -> "OnPlayerAdded when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerTypeChanged _ -> "OnPlayerTypeChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerWithdrawn _ -> "OnPlayerWithdrawn when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDraftsEventsRead _ -> "OnDraftsEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserDraftsEventsRead _ -> "OnUserDraftsEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleCreateDraftCmd _ -> "HandleCreateDraftCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleProcessDraftCmd _ -> "HandleProcessDraftCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleChangePriorityCmd _ -> "HandleChangePriorityCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleAddToDraftCmd _ -> "HandleAddToDraftCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleRemoveFromDraftCmd _ -> "HandleRemoveFromDraftCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleFreePickCmd _ -> "HandleFreePickCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingAllRead draftDic userDraftDic squadsRead = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! pendingAllRead draftDic userDraftDic squadsRead
            | Start _ -> "Start when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | Reset _ -> "Reset when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | Housekeeping -> "Housekeeping when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | OnSquadsRead squadsRead ->
                let source = "OnSquadsRead"
                sprintf "%s (%i squad/s) when pendingAllRead" source squadsRead.Length |> Info |> log
                let squads = squadsRead |> Some
                match (draftDic, userDraftDic, squads) |> ifAllRead with
                | Some (draftDic, userDraftDic, userDraftLookupDic, squadDic) ->
                    return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
                | None -> return! pendingAllRead draftDic userDraftDic squads
            | OnPlayerAdded _ -> "OnPlayerAdded when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | OnPlayerTypeChanged _ -> "OnPlayerTypeChanged when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | OnPlayerWithdrawn _ -> "OnPlayerWithdrawn when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | OnDraftsEventsRead draftsEvents ->
                let source = "OnDraftsEventsRead"
                let draftDic, errors = initializeDrafts source draftsEvents
                errors |> List.iter (fun (OtherError errorText) -> errorText |> Danger |> log)
                sprintf "%s (%i draft/s) when pendingAllRead" source draftsEvents.Length |> Info |> log
                let draftsRead = draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) ->
                    { DraftId = draftId ; Rvn = draft.Rvn ; DraftOrdinal = draft.DraftOrdinal ; DraftStatus = draft.DraftStatus ; DraftPicks = draft.DraftPicks
                      ProcessingEvents = draft.ProcessingEvents })
                draftsRead |> DraftsRead |> broadcaster.Broadcast
                let draftDic = draftDic |> Some
                match (draftDic, userDraftDic, squadsRead) |> ifAllRead with
                | Some (draftDic, userDraftDic, userDraftLookupDic, squadDic) ->
                    return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
                | None -> return! pendingAllRead draftDic userDraftDic squadsRead
            | OnUserDraftsEventsRead userDraftsEvents ->
                let source = "OnUserDraftsEventsRead"
                let userDraftDic, errors = initializeUserDrafts source userDraftsEvents
                errors |> List.iter (fun (OtherError errorText) -> errorText |> Danger |> log)
                sprintf "%s (%i user draft/s) when pendingAllRead" source userDraftsEvents.Length |> Info |> log
                let userDraftsRead = userDraftDic |> List.ofSeq |> List.map (fun (KeyValue (userDraftId, userDraft)) ->
                    let userDraftPicksRead = userDraft.UserDraftPickDic |> List.ofSeq |> List.map (fun (KeyValue (userDraftPick, rank)) ->
                        { UserDraftPick = userDraftPick ; Rank = rank })
                    { UserDraftId = userDraftId ; Rvn = userDraft.Rvn ; UserDraftKey = userDraft.UserDraftKey ; UserDraftPicksRead = userDraftPicksRead })
                userDraftsRead |> UserDraftsRead |> broadcaster.Broadcast
                let userDraftDic = userDraftDic |> Some
                match (draftDic, userDraftDic, squadsRead) |> ifAllRead with
                | Some (draftDic, userDraftDic, userDraftLookupDic, squadDic) ->
                    return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
                | None -> return! pendingAllRead draftDic userDraftDic squadsRead
            | HandleCreateDraftCmd _ -> "HandleCreateDraftCmd when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | HandleProcessDraftCmd _ -> "HandleProcessDraftCmd when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | HandleChangePriorityCmd _ -> "HandleChangePriorityCmd when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | HandleAddToDraftCmd _ -> "HandleAddToDraftCmd when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | HandleRemoveFromDraftCmd _ -> "HandleRemoveFromDraftCmd when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead
            | HandleFreePickCmd _ -> "HandleFreePickCmd when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftDic userDraftDic squadsRead }
        and managingDrafts draftDic userDraftDic userDraftLookupDic squadDic = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | Start _ -> sprintf "Start when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" draftDic.Count userDraftDic.Count squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | Reset reply ->
                sprintf "Reset when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s) -> pendingAllRead" draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None None
            | Housekeeping ->
                let source = "Housekeeping"
                sprintf "%s when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" source draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                draftDic |> housekeeping source
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | OnSquadsRead _ -> sprintf "OnSquadsRead when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" draftDic.Count userDraftDic.Count squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | OnPlayerAdded (squadId, playerId, playerType) ->
                sprintf "OnPlayerAdded when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                if squadId |> squadDic.ContainsKey then
                    let playerDic = squadDic.[squadId]
                    if playerId |> playerDic.ContainsKey |> not then
                        (playerId, { PlayerType = playerType ; Withdrawn = false }) |> playerDic.Add
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | OnPlayerTypeChanged (squadId, playerId, playerType) ->
                sprintf "OnPlayerTypeChanged when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                if squadId |> squadDic.ContainsKey then
                    let playerDic = squadDic.[squadId]
                    if playerId |> playerDic.ContainsKey then
                        let player = playerDic.[playerId]
                        playerDic.[playerId] <- { player with PlayerType = playerType }
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | OnPlayerWithdrawn (squadId, playerId) ->
                sprintf "OnPlayerWithdrawn when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" draftDic.Count userDraftDic.Count squadDic.Count |> Info |> log
                if squadId |> squadDic.ContainsKey then
                    let playerDic = squadDic.[squadId]
                    if playerId |> playerDic.ContainsKey then
                        let player = playerDic.[playerId]
                        playerDic.[playerId] <- { player with Withdrawn = true }
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | OnDraftsEventsRead _ -> sprintf "OnDraftsEventsRead when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" draftDic.Count userDraftDic.Count squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | OnUserDraftsEventsRead _ -> sprintf "OnUserDraftsEventsRead when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" draftDic.Count userDraftDic.Count squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | HandleCreateDraftCmd (_, auditUserId, draftId, draftOrdinal, draftType, reply) ->
                let source = "HandleCreateDraftCmd"
                sprintf "%s for %A (%A %A) when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" source draftId draftOrdinal draftType draftDic.Count userDraftDic.Count squadDic.Count |> Verbose |> log
                let result =
                    if draftId |> draftDic.ContainsKey |> not then () |> Ok else ifDebug (sprintf "%A already exists" draftId) UNEXPECTED_ERROR |> otherCmdError source
                    // Note: Only limited validation (since other scenarios should never arise).
                    |> Result.bind (fun _ ->
                        match draftDic |> List.ofSeq |> List.tryFind (fun (KeyValue (_, draft)) -> draft.DraftOrdinal = draftOrdinal) with
                        | Some _ -> ifDebug (sprintf "%A already exists" draftOrdinal) UNEXPECTED_ERROR |> otherCmdError source
                        | None -> () |> Ok)
                    |> Result.bind (fun _ ->
                        let highestOrdinal =
                            if draftDic.Count = 0 then 0
                            else
                                draftDic |> List.ofSeq |> List.map (fun (KeyValue (_, draft)) ->
                                    let (DraftOrdinal ordinal) = draft.DraftOrdinal
                                    ordinal) |> List.max
                        let (DraftOrdinal newOrdinal) = draftOrdinal
                        if newOrdinal <> highestOrdinal + 1 then ifDebug (sprintf "%A is not contiguous with highest existing ordinal %i" draftOrdinal highestOrdinal) UNEXPECTED_ERROR |> otherCmdError source
                        else (draftId, draftOrdinal, draftType) |> DraftCreated |> tryApplyDraftEvent source draftId None initialRvn ())
                let! result = match result with | Ok (draft, rvn, draftEvent, _) -> tryWriteDraftEventAsync auditUserId rvn draftEvent draft () | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (draftId, draft, _) -> sprintf "Audit%A %A %A" auditUserId draftId draft |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                match result with | Ok (draftId, draft, _) -> (draftId, draft) |> draftDic.Add | Error _ -> ()
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | HandleProcessDraftCmd (_, auditUserId, draftId, currentRvn, connectionId) ->
                let source = "HandleProcessDraftCmd"
                sprintf "%s for %A (%A) when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" source draftId currentRvn draftDic.Count userDraftDic.Count squadDic.Count |> Verbose |> log
                let result =
                    draftDic |> tryFindDraft draftId (otherCmdError source)
                    |> Result.bind (fun (draftId, draft) ->
                        match draft.DraftStatus with
                        | PendingProcessing false -> (draftId, draft) |> Ok
                        | PendingProcessing true -> ifDebug (sprintf "%A (%A) is already being processed" draftId draft) UNEXPECTED_ERROR |> otherCmdError source
                        | _ -> ifDebug (sprintf "%A (%A) is not PendingProcessing" draftId draft) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (draftId, draft) ->
                        let existingPicks =
                            draftDic |> List.ofSeq |> List.map (fun (KeyValue (_, draft)) -> draft.DraftPicks) |> List.collect id
                            |> List.map (fun (draftPick, (userId, _, _)) -> draftPick, userId)
                        let userDraftIds = userDraftLookupDic |> List.ofSeq |> List.choose (fun (KeyValue ((_, otherDraftId), userDraftId)) ->
                            if otherDraftId = draftId then userDraftId |> Some else None)
                        let userDrafts = userDraftIds |> List.choose (fun userDraftId -> if userDraftId |> userDraftDic.ContainsKey then userDraftDic.[userDraftId] |> Some else None)
                        let (DraftOrdinal draftOrdinal) = draft.DraftOrdinal
                        let previousDraftOrdinal = DraftOrdinal (draftOrdinal - 1)
                        let previousDraft = draftDic |> List.ofSeq |> List.tryFind (fun (KeyValue (_, otherDraft)) -> otherDraft.DraftOrdinal = previousDraftOrdinal)
                        let pickPriorityDic = match previousDraft with | Some (KeyValue (_, draft)) -> draft.PickPriorityDic | None -> PickPriorityDic ()
                        match processDraft source draftId draft.DraftOrdinal pickPriorityDic userDrafts existingPicks squadDic with
                        | Ok draftEvents -> (draftId, draft, draftEvents) |> Ok
                        | Error error -> error |> OtherAuthCmdError |> Error)
                let! result =
                    match result with
                    | Ok (draftId, draft, draftEvents) -> draftEvents |> tryApplyAndWriteDraftEventsAsync source auditUserId currentRvn draftId draft
                    | Error error -> error |> Error |> thingAsync
                let result = result |> Result.bind (fun (draftId, draft, rvn) -> draftId |> DraftProcessed |> tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn rvn) ())
                let! result =
                    match result with
                    | Ok (draft, rvn, draftEvent, _) -> tryWriteDraftEventAsync auditUserId rvn draftEvent draft ()
                    | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (draftId, draft, _) -> Some (sprintf "Audit%A %A %A" auditUserId draftId draft)) // note: log success/failure here (rather than assuming that calling code will do so)
                let serverMsg = result |> discardOk |> ProcessDraftCmdResult |> ServerDraftAdminMsg
                (serverMsg, [ connectionId ]) |> SendMsg |> broadcaster.Broadcast
                match result with
                | Ok (draftId, draft, _) ->
                    draftDic |> updateDraft draftId draft
                    draftDic |> housekeeping source
                | Error _ -> ()
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | HandleChangePriorityCmd (_, auditUserId, draftId, currentRvn, userDraftPick, priorityChange, connectionId) ->
                let source = "HandleChangePriorityCmd"
                sprintf "%s for %A %A (%A %A %A) when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" source auditUserId draftId currentRvn userDraftPick priorityChange draftDic.Count userDraftDic.Count squadDic.Count |> Verbose |> log
                let result =
                    tryFindDraft draftId (otherCmdError source) draftDic
                    |> Result.bind (fun (_, draft) -> match draft.DraftStatus with | Opened _ -> () |> Ok | _ -> ifDebug (sprintf "%A is not Opened" draftId) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun _ -> tryFindUserDraft auditUserId draftId (otherCmdError source) userDraftDic userDraftLookupDic)
                    |> Result.bind (fun (_, userDraftId, userDraft) ->
                        if userDraftPick |> userDraft.UserDraftPickDic.ContainsKey then (userDraftId, userDraft, userDraft.UserDraftPickDic.[userDraftPick]) |> Ok
                        else ifDebug (sprintf "%A not drafted" userDraftPick) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (userDraftId, userDraft, rank) ->
                        let count = userDraft.UserDraftPickDic.Count
                        match priorityChange with
                        | Increase when rank < 2 -> ifDebug (sprintf "%A already has the highest rank" userDraftPick) UNEXPECTED_ERROR |> otherCmdError source
                        | Decrease when rank > count - 1 -> ifDebug (sprintf "%A already has the lowest rank" userDraftPick) UNEXPECTED_ERROR |> otherCmdError source
                        | _ -> (userDraftId, userDraft) |> Ok)
                    |> Result.bind (fun (userDraftId, userDraft) ->
                        (userDraftId, userDraftPick, priorityChange) |> PriorityChanged |> tryApplyUserDraftEvent source userDraftId (userDraft |> Some) (incrementRvn currentRvn) userDraftPick)
                let! result =
                    match result with
                    | Ok (userDraft, rvn, userDraftEvent, userDraftPick) -> tryWriteUserDraftEventAsync auditUserId rvn userDraftEvent userDraft userDraftPick
                    | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (userDraftId, userDraft, _) -> sprintf "Audit%A %A %A" auditUserId userDraftId userDraft |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                let serverMsg = result |> Result.bind (fun (_, _, userDraftPick) -> userDraftPick |> Ok) |> tupleError userDraftPick |> ChangePriorityCmdResult |> ServerDraftsMsg
                (serverMsg, [ connectionId ]) |> SendMsg |> broadcaster.Broadcast
                match result with | Ok (userDraftId, userDraft, _) -> userDraftDic |> updateUserDraft userDraftId userDraft | Error _ -> ()
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | HandleAddToDraftCmd (_, auditUserId, draftId, currentRvn, userDraftPick, connectionId) ->
                let source = "HandleAddToDraftCmd"
                sprintf "%s for %A %A (%A %A) when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" source auditUserId draftId currentRvn userDraftPick draftDic.Count userDraftDic.Count squadDic.Count |> Verbose |> log
                let result =
                    tryFindDraft draftId (otherCmdError source) draftDic
                    |> Result.bind (fun (_, draft) -> match draft.DraftStatus with | Opened _ -> () |> Ok | _ -> ifDebug (sprintf "%A is not Opened" draftId) UNEXPECTED_ERROR |> otherCmdError source)
                let! result =
                    match result with
                    | Ok _ ->
                        if (auditUserId, draftId) |> userDraftLookupDic.ContainsKey then async {
                            let result = tryFindUserDraft auditUserId draftId (otherCmdError source) userDraftDic userDraftLookupDic
                            return result |> Result.bind (fun (_, userDraftId, userDraft) -> (userDraftId, userDraft) |> Ok) }
                        else async {
                            let userDraftId = UserDraftId.Create ()
                            let result = (userDraftId, auditUserId, draftId) |> UserDraftCreated |> tryApplyUserDraftEvent source userDraftId None initialRvn ()
                            let! result = match result with | Ok (userDraft, rvn, userDraftEvent, _) -> tryWriteUserDraftEventAsync auditUserId rvn userDraftEvent userDraft () | Error error -> error |> Error |> thingAsync
                            result |> logResult source (fun (userDraftId, userDraft, _) -> sprintf "Audit%A %A %A" auditUserId userDraftId userDraft |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                            match result with
                            | Ok (userDraftId, userDraft, _) ->
                                (userDraftId, userDraft) |> userDraftDic.Add
                                ((auditUserId, draftId), userDraftId) |> userDraftLookupDic.Add
                            | Error _ -> ()
                            return result |> Result.bind (fun (userDraftId, userDraft, _) -> (userDraftId, userDraft) |> Ok) }
                    | Error error -> error |> Error |> thingAsync
                let result =
                    result
                    |> Result.bind (fun (userDraftId, userDraft) ->
                        if userDraftPick |> userDraft.UserDraftPickDic.ContainsKey |> not then (userDraftId, userDraft) |> Ok
                        else ifDebug (sprintf "%A already drafted" userDraftPick) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (userDraftId, userDraft) ->
                        (userDraftId, userDraftPick) |> Drafted |> tryApplyUserDraftEvent source userDraftId (userDraft |> Some) (incrementRvn currentRvn) userDraftPick)
                let! result =
                    match result with
                    | Ok (userDraft, rvn, userDraftEvent, userDraftPick) -> tryWriteUserDraftEventAsync auditUserId rvn userDraftEvent userDraft userDraftPick
                    | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (userDraftId, userDraft, _) -> sprintf "Audit%A %A %A" auditUserId userDraftId userDraft |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                let serverMsg = result |> Result.bind (fun (_, _, userDraftPick) -> userDraftPick |> Ok) |> tupleError userDraftPick |> AddToDraftCmdResult |> ServerSquadsMsg
                (serverMsg, [ connectionId ]) |> SendMsg |> broadcaster.Broadcast
                match result with | Ok (userDraftId, userDraft, _) -> userDraftDic |> updateUserDraft userDraftId userDraft | Error _ -> ()
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | HandleRemoveFromDraftCmd (_, auditUserId, draftId, currentRvn, userDraftPick, toServerMsg, connectionId) ->
                let source = "HandleRemoveFromDraftCmd"
                sprintf "%s for %A %A (%A %A) when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" source auditUserId draftId currentRvn userDraftPick draftDic.Count userDraftDic.Count squadDic.Count |> Verbose |> log
                let result =
                    tryFindDraft draftId (otherCmdError source) draftDic
                    |> Result.bind (fun (_, draft) -> match draft.DraftStatus with | Opened _ -> () |> Ok | _ -> ifDebug (sprintf "%A is not Opened" draftId) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun _ -> tryFindUserDraft auditUserId draftId (otherCmdError source) userDraftDic userDraftLookupDic)
                    |> Result.bind (fun (_, userDraftId, userDraft) ->
                        if userDraftPick |> userDraft.UserDraftPickDic.ContainsKey then (userDraftId, userDraft) |> Ok
                        else ifDebug (sprintf "%A not drafted" userDraftPick) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (userDraftId, userDraft) ->
                        (userDraftId, userDraftPick) |> Undrafted |> tryApplyUserDraftEvent source userDraftId (userDraft |> Some) (incrementRvn currentRvn) userDraftPick)
                let! result =
                    match result with
                    | Ok (userDraft, rvn, userDraftEvent, userDraftPick) -> tryWriteUserDraftEventAsync auditUserId rvn userDraftEvent userDraft userDraftPick
                    | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (userDraftId, userDraft, _) -> sprintf "Audit%A %A %A" auditUserId userDraftId userDraft |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                let serverMsg =
                    result |> Result.bind (fun (_, _, userDraftPick) -> userDraftPick |> Ok) |> tupleError userDraftPick |> toServerMsg
                (serverMsg, [ connectionId ]) |> SendMsg |> broadcaster.Broadcast
                match result with | Ok (userDraftId, userDraft, _) -> userDraftDic |> updateUserDraft userDraftId userDraft | Error _ -> ()
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic
            | HandleFreePickCmd (_, auditUserId, draftId, currentRvn, draftPick, connectionId) ->
                let source = "HandleFreePickCmd"
                sprintf "%s for %A %A (%A %A) when managingDrafts (%i draft/s) (%i user draft/s) (%i squad/s)" source auditUserId draftId currentRvn draftPick draftDic.Count userDraftDic.Count squadDic.Count |> Verbose |> log
                let result =
                    tryFindDraft draftId (otherCmdError source) draftDic
                    |> Result.bind (fun (_, draft) ->
                        match draft.DraftStatus with
                        | FreeSelection -> (draftId, draft) |> Ok
                        | _ -> ifDebug (sprintf "%A is not FreeSelection" draftId) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (draftId, draft) ->
                        let existingPicks =
                            draftDic |> List.ofSeq |> List.map (fun (KeyValue (_, draft)) -> draft.DraftPicks) |> List.collect id
                            |> List.map (fun (draftPick, (userId, _, _)) -> draftPick, userId)
                        if existingPicks |> List.exists (fun (existingDraftPick, _) -> existingDraftPick = draftPick) |> not then (draftId, draft, existingPicks) |> Ok
                        else ifDebug (sprintf "%A has already been picked" draftPick) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (draftId, draft, existingPicks) ->
                        let userPicks = existingPicks |> List.choose (fun (draftPick, userId) -> if userId = auditUserId then draftPick |> Some else None)
                        let teamCount = userPicks |> List.filter (fun draftPick -> match draftPick with | TeamPicked _-> true | PlayerPicked _ -> false ) |> List.length
                        let goalkeeperCount =
                            userPicks
                            |> List.filter (fun draftPick ->
                                match draftPick with
                                | TeamPicked _ -> false
                                | PlayerPicked (squadId, playerId) -> match (squadId, playerId) |> playerTypeAndWithdrawn squadDic with | Some (Goalkeeper, false) -> true | _ -> false)
                            |> List.length
                        let outfieldPlayerCount =
                            userPicks
                            |> List.filter (fun draftPick ->
                                match draftPick with
                                | TeamPicked _ -> false
                                | PlayerPicked (squadId, playerId) ->
                                    match (squadId, playerId) |> playerTypeAndWithdrawn squadDic with | Some (Goalkeeper, _) -> false | Some (_, false) -> true | _ -> false)
                            |> List.length
                        let ok = (draftId, draft) |> Ok
                        match draftPick with
                        | TeamPicked _ ->
                            if teamCount < MAX_TEAM_PICKS then ok else ifDebug "Team/coach is not required" UNEXPECTED_ERROR |> otherCmdError source
                        | PlayerPicked (squadId, playerId) ->
                            match (squadId, playerId) |> playerTypeAndWithdrawn squadDic with
                            | Some (Goalkeeper, _) ->
                                if goalkeeperCount < MAX_GOALKEEPER_PICKS then ok else ifDebug "Goalkeeper is not required" UNEXPECTED_ERROR |> otherCmdError source
                            | Some _ ->
                                if outfieldPlayerCount < MAX_OUTFIELD_PLAYER_PICKS then ok else ifDebug "Outfield player is not required" UNEXPECTED_ERROR |> otherCmdError source
                            | None -> UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (draftId, draft) ->
                        (draftId, draftPick, auditUserId, DateTimeOffset.UtcNow) |> FreePick |> tryApplyDraftEvent source draftId (draft |> Some) (incrementRvn currentRvn) draftPick)
                let! result =
                    match result with
                    | Ok (draft, rvn, draftEvent, draftPick) -> tryWriteDraftEventAsync auditUserId rvn draftEvent draft draftPick
                    | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (userDraftId, userDraft, _) -> sprintf "Audit%A %A %A" auditUserId userDraftId userDraft |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                let serverMsg =
                    result |> Result.bind (fun (_, _, draftPick) -> draftPick |> Ok) |> FreePickCmdResult |> ServerSquadsMsg
                (serverMsg, [ connectionId ]) |> SendMsg |> broadcaster.Broadcast
                match result with | Ok (draftId, draft, _) -> draftDic |> updateDraft draftId draft | Error _ -> ()
                return! managingDrafts draftDic userDraftDic userDraftLookupDic squadDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Entity Entity.Drafts |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member self.Start () =
        if IsAwaitingStart |> agent.PostAndReply then
            // Note: Not interested in DraftEventWritten | UserDraftEventWritten events (since Drafts agent causes these in the first place - and will already have maintained its internal state accordingly).
            let onEvent = (fun event ->
                match event with
                | Tick (ticks, secondsPerTick) -> if (ticks, secondsPerTick) |> isEveryNSeconds (int (HOUSEKEEPING_INTERVAL |> minutesToSeconds) * 1<second>) then Housekeeping |> agent.Post
                | SquadsRead squadsRead -> squadsRead |> OnSquadsRead |> agent.Post
                | SquadEventWritten (_, userEvent) ->
                    match userEvent with
                    | PlayerAdded (squadId, playerId, _, playerType) -> (squadId, playerId, playerType) |> OnPlayerAdded |> agent.Post
                    | PlayerTypeChanged (squadId, playerId, playerType) -> (squadId, playerId, playerType) |> OnPlayerTypeChanged |> agent.Post
                    | PlayerWithdrawn (squadId, playerId, _) -> (squadId, playerId) |> OnPlayerWithdrawn |> agent.Post
                    | _ -> () // note: only interested in PlayerAdded | PlayerTypeChanged | PlayerWithdrawn
                | DraftsEventsRead draftsEvents -> draftsEvents |> self.OnDraftsEventsRead
                | UserDraftsEventsRead userDraftsEvents -> userDraftsEvents |> self.OnUserDraftsEventsRead
                | _ -> ())
            let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
            sprintf "agent subscribed to Tick | SquadsRead | SquadEventWritten (subset) | DraftsEventsRead | UserDraftsEventsRead broadcasts -> %A" subscriptionId |> Info |> log
            Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
        else
            "agent has already been started" |> Info |> log
    member __.Reset () = Reset |> agent.PostAndReply // note: not async (since need to reset agents deterministically)
    member __.Housekeeping () = Housekeeping |> agent.Post
    member __.OnSquadsRead squadsRead = squadsRead |> OnSquadsRead |> agent.Post
    member __.OnDraftsEventsRead draftsEvents = draftsEvents |> OnDraftsEventsRead |> agent.Post
    member __.OnUserDraftsEventsRead userDraftsEvents = userDraftsEvents |> OnUserDraftsEventsRead |> agent.Post
    member __.HandleCreateDraftCmdAsync (token, auditUserId, draftId, draftOrdinal, draftType) =
        (fun reply -> (token, auditUserId, draftId, draftOrdinal, draftType, reply) |> HandleCreateDraftCmd) |> agent.PostAndAsyncReply
    member __.HandleProcessDraftCmd (token, auditUserId, draftId, currentRvn, connectionId) = (token, auditUserId, draftId, currentRvn, connectionId) |> HandleProcessDraftCmd |> agent.Post
    member __.HandleChangePriorityCmd (token, auditUserId, draftId, currentRvn, userDraftPick, priorityChange, connectionId) =
        (token, auditUserId, draftId, currentRvn, userDraftPick, priorityChange, connectionId) |> HandleChangePriorityCmd |> agent.Post
    member __.HandleAddToDraftCmd (token, auditUserId, draftId, currentRvn, userDraftPick, connectionId) =
        (token, auditUserId, draftId, currentRvn, userDraftPick, connectionId) |> HandleAddToDraftCmd |> agent.Post
    member __.HandleRemoveFromDraftCmd (token, auditUserId, draftId, currentRvn, userDraftPick, toServerMsg, connectionId) =
        (token, auditUserId, draftId, currentRvn, userDraftPick, toServerMsg, connectionId) |> HandleRemoveFromDraftCmd |> agent.Post
    member __.HandleFreePickCmd (token, auditUserId, draftId, currentRvn, draftPick, connectionId) =
        (token, auditUserId, draftId, currentRvn, draftPick, connectionId) |> HandleFreePickCmd |> agent.Post

let drafts = Drafts ()
