module Aornota.Sweepstake2026.Server.Agents.Projections.Drafts

(* Broadcasts: SendMsg
   Subscribes: DraftsRead
               DraftEventWritten (DraftCreated | DraftOpened | DraftPendingProcessing | DraftProcessed | DraftFreeSelection | ProcessingStarted | ...)
               UserDraftsRead
               UserDraftEventWritten  (UserDraftCreated | Drafted | Undrafted | PriorityChanged)
               ConnectionsSignedOut | Disconnected *)

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Authorization
open Aornota.Sweepstake2026.Server.Common.DeltaHelper
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Events.DraftEvents
open Aornota.Sweepstake2026.Server.Events.UserDraftEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private DraftsInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnDraftsRead of draftsRead : DraftRead list
    | OnDraftEventWritten of rvn : Rvn * draftEvent : DraftEvent
    | OnUserDraftsRead of userDraftsRead : UserDraftRead list
    | OnUserDraftEventWritten of rvn : Rvn * userDraftEvent : UserDraftEvent
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeDraftsProjectionQry of token : DraftToken * connectionId : ConnectionId * userId : UserId
        * reply : AsyncReplyChannel<Result<DraftDto list * CurrentUserDraftDto option, AuthQryError<string>>>

type private UserDraftPickDic = Dictionary<UserDraftPick, int>

type private Draft =
    { Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus ; ProcessingEvents : ProcessingEvent list ; ProcessedUserDraftPicks : (UserId * UserDraftPickDic) list }
type private DraftDic = Dictionary<DraftId, Draft>

type private UserDraft = { Rvn : Rvn ; UserDraftPickDic : UserDraftPickDic }
type private UserDraftDic = Dictionary<UserDraftKey, UserDraft>
type private UserDraftLookupDic = Dictionary<UserDraftId, UserDraftKey>

type private Projectee = { LastRvn : Rvn ; UserId : UserId }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { DraftDic : DraftDic ; UserDraftDic : UserDraftDic }

type private StateChangeType =
    | Initialization of draftDic : DraftDic * userDraftDic : UserDraftDic
    | DraftChange of draftDic : DraftDic * state : State
    | UserDraftChange of userDraftDic : UserDraftDic * state : State

type private DraftDtoDic = Dictionary<DraftId, DraftDto>
type private CurrentUserDraftDtoChangesDic = Dictionary<UserId, CurrentUserDraftDto option>

let private log category = (Projection Drafts, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private userDraftPickDtos (userDraftPickDic:UserDraftPickDic) : UserDraftPickDto list =
    userDraftPickDic |> List.ofSeq |> List.map (fun (KeyValue (userDraftPick, rank)) -> { UserDraftPick = userDraftPick ; Rank = rank })

let private draftDto (draftId, draft:Draft) : DraftDto =
    let processingDetails =
        match draft.DraftStatus with
        | Processed ->
            let userDraftPicks = draft.ProcessedUserDraftPicks |> List.map (fun (userId, userDraftPickDic) -> userId, userDraftPickDic |> userDraftPickDtos)
            { UserDraftPicks = userDraftPicks ; ProcessingEvents = draft.ProcessingEvents |> List.rev } |> Some
        | _ -> None
    { DraftId = draftId ; Rvn = draft.Rvn ; DraftOrdinal = draft.DraftOrdinal ; DraftStatus = draft.DraftStatus ; ProcessingDetails = processingDetails }

let private draftDtoDic (draftDic:DraftDic) =
    let draftDtoDic = DraftDtoDic ()
    draftDic |> List.ofSeq |> List.iter (fun (KeyValue (draftId, draft)) ->
        let draftDto = (draftId, draft) |> draftDto
        (draftDto.DraftId, draftDto) |> draftDtoDic.Add)
    draftDtoDic

let private draftDtos state = state.DraftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) -> (draftId, draft) |> draftDto)

let private currentUserDraftDto userId state =
    match state.UserDraftDic |> List.ofSeq |> List.filter (fun (KeyValue (userDraftKey, _)) -> fst userDraftKey = userId) with // note: should be single match (or none)
    | KeyValue (userDraftKey, userDraft) :: _ -> { UserDraftKey = userDraftKey ; Rvn = userDraft.Rvn ; UserDraftPickDtos = userDraft.UserDraftPickDic |> userDraftPickDtos } |> Some
    | [] -> None

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendDraftDtoDelta (projecteeDic:ProjecteeDic) draftDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendDraftDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, draftDtoDelta) |> DraftsDeltaMsg |> DraftsProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private sendCurrentUserDraftDtoChanges (projecteeDic:ProjecteeDic) (currentUserDraftDtoChangesDic:CurrentUserDraftDtoChangesDic) =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        if projectee.UserId |> currentUserDraftDtoChangesDic.ContainsKey then
            let currentUserDraftDto = currentUserDraftDtoChangesDic.[projectee.UserId]
            let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
            sprintf "sendCurrentUserDraftDtoChanges -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
            (projectee.LastRvn, currentUserDraftDto) |> CurrentUserDraftDtoChangedMsg |> DraftsProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ]
            (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization (draftDic, userDraftDic) ->
            sprintf "%s -> initialized" source |> Info |> log
            { DraftDic = DraftDic draftDic ; UserDraftDic = UserDraftDic userDraftDic }
        | DraftChange (draftDic, state) ->
            let previousDraftDtoDic = state.DraftDic |> draftDtoDic
            let draftDtoDic = draftDic |> draftDtoDic
            let draftDtoDelta = draftDtoDic |> delta previousDraftDtoDic
            if draftDtoDelta |> isEmpty |> not then
                sprintf "%s -> DraftDto delta %A -> %i projectee/s" source draftDtoDelta projecteeDic.Count |> Info |> log
                draftDtoDelta |> sendDraftDtoDelta projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with DraftDic = DraftDic draftDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
        | UserDraftChange (userDraftDic, state) ->
            let currentUserDraftDto (userDraftKey, userDraft) = { UserDraftKey = userDraftKey ; Rvn = userDraft.Rvn ; UserDraftPickDtos = userDraft.UserDraftPickDic |> userDraftPickDtos }
            let currentUserDraftDtoChangesDic = CurrentUserDraftDtoChangesDic () // Dictionary<UserId, CurrentUserDraftDto option>
            userDraftDic |> List.ofSeq |> List.iter (fun (KeyValue (userDraftKey, userDraft)) ->
                let previousCurrentUserDraftDto =
                    if userDraftKey |> state.UserDraftDic.ContainsKey then
                        let previousUserDraft = state.UserDraftDic.[userDraftKey]
                        (userDraftKey, previousUserDraft) |> currentUserDraftDto |> Some
                    else None
                let currentCurrentUserDraftDto = (userDraftKey, userDraft) |> currentUserDraftDto |> Some
                if currentCurrentUserDraftDto <> previousCurrentUserDraftDto then (fst userDraftKey, currentCurrentUserDraftDto) |> currentUserDraftDtoChangesDic.Add)
            state.UserDraftDic |> List.ofSeq |> List.iter (fun (KeyValue (userDraftKey, _)) ->
                if userDraftKey |> userDraftDic.ContainsKey |> not then (fst userDraftKey, None) |> currentUserDraftDtoChangesDic.Add)
            if currentUserDraftDtoChangesDic.Count > 0 then
                sprintf "%s -> %i CurrentUserDraftDto change/s -> %i (potential) projectee/s" source currentUserDraftDtoChangesDic.Count projecteeDic.Count |> Info |> log
                currentUserDraftDtoChangesDic |> sendCurrentUserDraftDtoChanges projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with UserDraftDic = UserDraftDic userDraftDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

let private isOpen draftId (draftDic:DraftDic) =
    if draftId |> draftDic.ContainsKey then
        let draft = draftDic.[draftId]
        match draft.DraftStatus with | Opened _ -> true | _ -> false
    else false

let private ifAllRead source (draftsRead:(DraftRead list) option, userDraftsRead:(UserDraftRead list) option) =
    match draftsRead, userDraftsRead with
    | Some draftsRead, Some userDraftsRead ->
        let draftDic = DraftDic ()
        draftsRead |> List.iter (fun draftRead ->
            let draft =
                { Rvn = draftRead.Rvn ; DraftOrdinal = draftRead.DraftOrdinal ; DraftStatus = draftRead.DraftStatus ; ProcessingEvents = draftRead.ProcessingEvents
                  ProcessedUserDraftPicks = [] }
            (draftRead.DraftId, draft) |> draftDic.Add)
        let userDraftDic = UserDraftDic ()
        let userDraftLookupDic = UserDraftLookupDic ()
        userDraftsRead |> List.iter (fun userDraftRead ->
            let (userId, draftId) = userDraftRead.UserDraftKey
            if draftId |> draftDic.ContainsKey then
                let draft = draftDic.[draftId]
                // Note: We only care about UserDraftRead if Draft is "active" (Opened or PendingProcessing) - or Processed.
                if draft.DraftStatus |> isActive then
                    let userDraftPickDic = UserDraftPickDic ()
                    userDraftRead.UserDraftPicksRead |> List.iter (fun userDraftPickRead -> (userDraftPickRead.UserDraftPick, userDraftPickRead.Rank) |> userDraftPickDic.Add)
                    (userDraftRead.UserDraftKey, { Rvn = userDraftRead.Rvn ; UserDraftPickDic = userDraftPickDic }) |> userDraftDic.Add
                    (userDraftRead.UserDraftId, userDraftRead.UserDraftKey) |> userDraftLookupDic.Add
                else
                    match draft.DraftStatus with
                    | Processed ->
                        let userDraftPickDic = UserDraftPickDic ()
                        userDraftRead.UserDraftPicksRead |> List.iter (fun userDraftPickRead -> (userDraftPickRead.UserDraftPick, userDraftPickRead.Rank) |> userDraftPickDic.Add)
                        let processedUserDraftPicks = (userId, userDraftPickDic) :: draft.ProcessedUserDraftPicks
                        draftDic.[draftId] <- { draft with ProcessedUserDraftPicks = processedUserDraftPicks }
                    | _ -> ())
        let projecteeDic = ProjecteeDic ()
        let state = (draftDic, userDraftDic) |> Initialization |> updateState source projecteeDic
        (state, draftDic, userDraftDic, userDraftLookupDic, projecteeDic) |> Some
    | _ -> None

type Drafts () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead (0 drafts) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None
            | OnDraftsRead _ -> "OnDraftsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDraftEventWritten _ -> "OnDraftEventWritten when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserDraftsRead _ -> "OnUserDraftsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserDraftEventWritten _ -> "OnUserDraftEventWritten when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeDraftsProjectionQry _ -> "HandleInitializeDraftsProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingAllRead draftsRead userDraftsRead = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead userDraftsRead
            | OnDraftsRead draftsRead ->
                let source = "OnDraftsRead"
                sprintf "%s (%i draft/s) when pendingAllRead" source draftsRead.Length |> Info |> log
                let draftsRead = draftsRead |> Some
                match (draftsRead, userDraftsRead) |> ifAllRead source with
                | Some (state, draftDic, userDraftDic, userDraftLookupDic, projecteeDic) ->
                    return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic
                | None -> return! pendingAllRead draftsRead userDraftsRead
            | OnDraftEventWritten _ -> "OnDraftEventWritten when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead userDraftsRead
            | OnUserDraftsRead userDraftsRead ->
                let source = "OnUserDraftsRead"
                sprintf "%s (%i user draft/s) when pendingAllRead" source userDraftsRead.Length |> Info |> log
                let userDraftsRead = userDraftsRead |> Some
                match (draftsRead, userDraftsRead) |> ifAllRead source with
                | Some (state, draftDic, userDraftDic, userDraftLookupDic, projecteeDic) ->
                    return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic
                | None -> return! pendingAllRead draftsRead userDraftsRead
            | OnUserDraftEventWritten _ -> "OnUserDraftEventWritten when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead userDraftsRead
            | RemoveConnections _ -> "RemoveConnections when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead userDraftsRead
            | HandleInitializeDraftsProjectionQry _ -> "HandleInitializeDraftsProjectionQry when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead draftsRead userDraftsRead }
        and projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingDrafts" |> IgnoredInput |> Agent |> log ; return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic
            | OnDraftsRead _ -> "OnDraftsRead when projectingDrafts" |> IgnoredInput |> Agent |> log ; return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic
            | OnDraftEventWritten (rvn, draftEvent) ->
                let source = "OnDraftEventWritten"
                sprintf "%s (%A) when projectingDrafts (%i draft/s) (%i current user draft/s) (%i projectee/s)" source draftEvent draftDic.Count userDraftDic.Count projecteeDic.Count |> Info |> log
                let state =
                    match draftEvent with
                    | DraftCreated (draftId, draftOrdinal, draftType) ->
                        if draftId |> draftDic.ContainsKey |> not then // note: silently ignore already-known draftId (should never happen)
                            let draft =
                                { Rvn = rvn ; DraftOrdinal = draftOrdinal ; DraftStatus = draftType |> defaultDraftStatus ; ProcessingEvents = []
                                  ProcessedUserDraftPicks = [] }
                            (draftId, draft) |> draftDic.Add
                            (draftDic, state) |> DraftChange |> updateState source projecteeDic
                        else state
                    | _ ->
                        let draftId = draftEvent.DraftId
                        if draftId |> draftDic.ContainsKey then // note: silently ignore unknown draftId (should never happen)
                            let draft = draftDic.[draftId]
                            let draft, userDraftChanged =
                                match draftEvent with
                                | DraftCreated _ -> draft, false // note: should never happen
                                | DraftOpened _ ->
                                    (match draft.DraftStatus with | PendingOpen (_, ends) -> { draft with Rvn = rvn ; DraftStatus = ends |> Opened } | _ -> draft), false
                                | DraftPendingProcessing _ ->
                                    (match draft.DraftStatus with | Opened _ -> { draft with Rvn = rvn ; DraftStatus = PendingProcessing false } | _ -> draft), false
                                | DraftProcessed _ ->
                                    match draft.DraftStatus with
                                    | PendingProcessing true ->
                                        // Note: When Draft changes to Processed, populate ProcessedUserDraftPickDics - then clear UserDraftDic and UserDraftLookupDic.
                                        let processedUserDraftPicks = userDraftDic |> List.ofSeq |> List.map (fun (KeyValue ((userId, _), userDraft)) ->
                                            userId, UserDraftPickDic userDraft.UserDraftPickDic)
                                        userDraftDic.Clear ()
                                        userDraftLookupDic.Clear ()
                                        { draft with Rvn = rvn ; DraftStatus = Processed ; ProcessedUserDraftPicks = processedUserDraftPicks }, true
                                    | _ -> draft, false
                                | DraftFreeSelection _ ->
                                    (match draft.DraftStatus with | PendingFreeSelection -> { draft with Rvn = rvn ; DraftStatus = FreeSelection } | _ -> draft), false
                                | DraftEvent.ProcessingStarted (_, seed) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing false ->
                                            let processingEvents = (seed |> ProcessingEvent.ProcessingStarted) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; DraftStatus = PendingProcessing true ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.WithdrawnPlayersIgnored (_, ignored) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing true ->
                                            let processingEvents = (ignored |> ProcessingEvent.WithdrawnPlayersIgnored) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.RoundStarted (_, round) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing true ->
                                            let processingEvents = (round |> ProcessingEvent.RoundStarted) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.AlreadyPickedIgnored (_, ignored) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing true ->
                                            let processingEvents = (ignored |> ProcessingEvent.AlreadyPickedIgnored) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.NoLongerRequiredIgnored (_, ignored) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing true ->
                                            let processingEvents = (ignored |> ProcessingEvent.NoLongerRequiredIgnored) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.UncontestedPick (_, draftPick, userId) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing true ->
                                            let processingEvents = ((draftPick, userId) |> ProcessingEvent.UncontestedPick) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.ContestedPick (_, draftPick, userDetails, winner) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing true ->
                                            let processingEvents = ((draftPick, userDetails, winner) |> ProcessingEvent.ContestedPick) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.PickPriorityChanged (_, userId, pickPriority) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing true ->
                                            let processingEvents = ((userId, pickPriority) |> ProcessingEvent.PickPriorityChanged) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.Picked (_, draftOrdinal, draftPick, userId, timestamp) ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | PendingProcessing true ->
                                            let processingEvents = ((draftOrdinal, draftPick, userId, timestamp) |> ProcessingEvent.Picked) :: draft.ProcessingEvents
                                            { draft with Rvn = rvn ; ProcessingEvents = processingEvents }
                                        | _ -> draft
                                    draft, false
                                | DraftEvent.FreePick _ ->
                                    let draft =
                                        match draft.DraftStatus with
                                        | FreeSelection -> {draft with Rvn = rvn }
                                        | _ -> draft
                                    draft, false
                            draftDic.[draftId] <- draft
                            let state = (draftDic, state) |> DraftChange |> updateState source projecteeDic
                            if userDraftChanged then (userDraftDic, state) |> UserDraftChange |> updateState source projecteeDic else state
                        else state
                return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic
            | OnUserDraftsRead _ -> "OnUserDraftsRead when projectingDrafts" |> IgnoredInput |> Agent |> log ; return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic
            | OnUserDraftEventWritten (rvn, userDraftEvent) ->
                let source = "OnUserDraftEventWritten"
                sprintf "%s (%A) when projectingDrafts (%i draft/s) (%i current user draft/s) (%i projectee/s)" source userDraftEvent draftDic.Count userDraftDic.Count projecteeDic.Count |> Info |> log
                let state =
                    match userDraftEvent with
                    | UserDraftCreated (userDraftId, userId, draftId) -> // note: silently ignore already-known userDraftId (should never happen)
                        if userDraftId |> userDraftLookupDic.ContainsKey |> not then
                            if draftDic |> isOpen draftId then
                                let userDraftKey = (userId, draftId)
                                (userDraftKey, { Rvn = rvn ; UserDraftPickDic = UserDraftPickDic () }) |> userDraftDic.Add
                                (userDraftId, userDraftKey) |> userDraftLookupDic.Add
                                (userDraftDic, state) |> UserDraftChange |> updateState source projecteeDic
                            else state
                        else state
                    | _ ->
                        let userDraftKey =
                            if userDraftEvent.UserDraftId |> userDraftLookupDic.ContainsKey then userDraftLookupDic.[userDraftEvent.UserDraftId] |> Some
                            else None // note: should never happen
                        match userDraftKey with
                        | Some userDraftKey ->
                            if userDraftKey |> userDraftDic.ContainsKey then // note: silently ignore unknown userDraftKey (should never happen)
                                if draftDic |> isOpen (snd userDraftKey) then
                                    let userDraft = userDraftDic.[userDraftKey]
                                    let userDraft =
                                        match userDraftEvent with
                                        | Drafted (_, userDraftPick) ->
                                            let userDraftPickDic = userDraft.UserDraftPickDic
                                            if userDraftPick |> userDraftPickDic.ContainsKey |> not then
                                                (userDraftPick, userDraftPickDic.Count + 1) |> userDraftPickDic.Add
                                                { userDraft with Rvn = rvn }
                                            else userDraft // note: should never happen
                                        | Undrafted (_, userDraftPick) ->
                                            let userDraftPickDic = userDraft.UserDraftPickDic
                                            if userDraftPick |> userDraftPickDic.ContainsKey then
                                                let updatedUserDraftPickDic = UserDraftPickDic ()
                                                userDraftPick |> userDraftPickDic.Remove |> ignore
                                                userDraftPickDic
                                                |> List.ofSeq |> List.map (fun (KeyValue (userDraftPick, rank)) -> userDraftPick, rank) |> List.sortBy snd
                                                |> List.iteri (fun i (userDraftPick, _) ->
                                                    (userDraftPick, i + 1) |> updatedUserDraftPickDic.Add)
                                                { userDraft with Rvn = rvn ; UserDraftPickDic = updatedUserDraftPickDic }
                                            else userDraft // note: should never happen
                                        | PriorityChanged (_, userDraftPick, priorityChange) ->
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
                                                { userDraft with Rvn = rvn ; UserDraftPickDic = updatedUserDraftPickDic }
                                            else userDraft // note: should never happen
                                        | _ -> userDraft // note: should never happen
                                    userDraftDic.[userDraftKey] <- userDraft
                                    (userDraftDic, state) |> UserDraftChange |> updateState source projecteeDic
                                else state
                            else state
                        | None -> state
                return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnection"
                sprintf "%s (%A) when projectingDrafts (%i draft/s) (%i current user draft/s) (%i projectee/s)" source connectionIds draftDic.Count userDraftDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds
                sprintf "%s when projectingDrafts -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic
            | HandleInitializeDraftsProjectionQry (_, connectionId, userId, reply) ->
                let source = "HandleInitializeDraftsProjectionQry"
                sprintf "%s for %A (%A) when projectingDrafts (%i draft/s) (%i current user draft/s) (%i projectee/s)" source connectionId userId draftDic.Count userDraftDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn ; UserId = userId }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingDrafts -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let draftDtos = state |> draftDtos
                let currentUserDraftDto = state |> currentUserDraftDto userId
                let result = (draftDtos, currentUserDraftDto) |> Ok
                result |> logResult source (fun (draftDtos, currentUserDraftDto) -> sprintf "%i draft/s (%A)" draftDtos.Length currentUserDraftDto |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingDrafts state draftDic userDraftDic userDraftLookupDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Drafts |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | DraftsRead draftsRead -> draftsRead |> OnDraftsRead |> agent.Post
            | DraftEventWritten (rvn, draftEvent) -> (rvn, draftEvent) |> OnDraftEventWritten |> agent.Post
            | UserDraftsRead userDraftsRead -> userDraftsRead |> OnUserDraftsRead |> agent.Post
            | UserDraftEventWritten (rvn, userDraftEvent) -> (rvn, userDraftEvent) |> OnUserDraftEventWritten |> agent.Post
            | ConnectionsSignedOut connectionIds -> connectionIds |> RemoveConnections |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to DraftsRead | DraftEventWritten | UserDraftsRead | UserDraftEventWritten | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeDraftsProjectionQryAsync (token, connectionId, userId) =
        (fun reply -> (token, connectionId, userId, reply) |> HandleInitializeDraftsProjectionQry) |> agent.PostAndAsyncReply

let drafts = Drafts ()
