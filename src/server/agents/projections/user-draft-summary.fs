module Aornota.Sweepstake2026.Server.Agents.Projections.UserDraftSummary

(* Broadcasts: SendMsg
   Subscribes: UserDraftsRead
               UserDraftEventWritten (UserDraftCreated | Drafted | Undrafted)
               ConnectionsSignedOut | Disconnected *)

open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Authorization
open Aornota.Sweepstake2026.Server.Common.DeltaHelper
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Events.UserDraftEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private UserDraftSummaryInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnUserDraftsRead of userDraftsRead : UserDraftRead list
    | OnUserDraftEventWritten of userDraftEvent : UserDraftEvent
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeUserDraftSummaryProjectionQry of token : DraftAdminToken * connectionId : ConnectionId * userId : UserId
        * reply : AsyncReplyChannel<Result<UserDraftSummaryDto list, AuthQryError<string>>>

type private UserDraftSummaryDic = Dictionary<UserDraftId, UserDraftSummaryDto>

type private Projectee = { LastRvn : Rvn }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { UserDraftSummaryDic : UserDraftSummaryDic }

type private StateChangeType =
    | Initialization of userDraftSummaryDic : UserDraftSummaryDic
    | UserDraftChange of userDraftSummaryDic : UserDraftSummaryDic * state : State

type private UserDraftSummaryDtoDic = Dictionary<UserDraftKey, UserDraftSummaryDto>

let private log category = (Projection UserDraftSummary, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private userDraftSummaryDto userDraftKey pickCount = { UserDraftKey = userDraftKey ; PickCount = pickCount }

let private userDraftSummaryDtoDic (userDraftSummaryDic:UserDraftSummaryDic) =
    let userDraftSummaryDtoDic = UserDraftSummaryDtoDic ()
    userDraftSummaryDic |> List.ofSeq |> List.iter (fun (KeyValue (_, userDraftSummaryDto)) ->
        (userDraftSummaryDto.UserDraftKey, userDraftSummaryDto) |> userDraftSummaryDtoDic.Add)
    userDraftSummaryDtoDic

let private userDraftSummaryDtos state = state.UserDraftSummaryDic |> List.ofSeq |> List.map (fun (KeyValue (_, userDraftSummaryDto)) -> userDraftSummaryDto)

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendDraftDtoDelta (projecteeDic:ProjecteeDic) userDraftSummaryDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendUserDraftSummaryDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, userDraftSummaryDtoDelta) |> UserDraftSummariesDeltaMsg |> UserDraftSummaryProjectionMsg |> ServerDraftAdminMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization userDraftSummaryDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { UserDraftSummaryDic = UserDraftSummaryDic userDraftSummaryDic }
        | UserDraftChange (userDraftSummaryDic, state) ->
            let previousUserDraftSummaryDtoDic = state.UserDraftSummaryDic |> userDraftSummaryDtoDic
            let userDraftSummaryDtoDic = userDraftSummaryDic |> userDraftSummaryDtoDic
            let userDraftSummaryDtoDelta = userDraftSummaryDtoDic |> delta previousUserDraftSummaryDtoDic
            if userDraftSummaryDtoDelta |> isEmpty |> not then
                sprintf "%s -> UserDraftSummaryDto delta %A -> %i projectee/s" source userDraftSummaryDtoDelta projecteeDic.Count |> Info |> log
                userDraftSummaryDtoDelta |> sendDraftDtoDelta projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with UserDraftSummaryDic = UserDraftSummaryDic userDraftSummaryDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

type UserDraftSummary () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead (0 drafts) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingUserDraftsRead ()
            | OnUserDraftsRead _ -> "OnUserDraftsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserDraftEventWritten _ -> "OnUserDraftEventWritten when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeUserDraftSummaryProjectionQry _ -> "HandleInitializeUserDraftSummaryProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingUserDraftsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingUserDraftsRead" |> IgnoredInput |> Agent |> log ; return! pendingUserDraftsRead ()
            | OnUserDraftsRead userDraftsRead ->
                let source = "OnUserDraftsRead"
                sprintf "%s (%i user draft/s) when pendingUserDraftsRead" source userDraftsRead.Length |> Info |> log
                let userDraftSummaryDic = UserDraftSummaryDic ()
                userDraftsRead |> List.iter (fun userDraftRead ->
                    let userDraftSummaryDto = userDraftSummaryDto userDraftRead.UserDraftKey userDraftRead.UserDraftPicksRead.Length
                    (userDraftRead.UserDraftId, userDraftSummaryDto) |> userDraftSummaryDic.Add)
                let projecteeDic = ProjecteeDic ()
                let state = userDraftSummaryDic |> Initialization |> updateState source projecteeDic
                return! projectingUserDraftSummary state userDraftSummaryDic projecteeDic
            | OnUserDraftEventWritten _ -> "OnUserDraftEventWritten when pendingUserDraftsRead" |> IgnoredInput |> Agent |> log ; return! pendingUserDraftsRead ()
            | RemoveConnections _ -> "RemoveConnections when pendingUserDraftsRead" |> IgnoredInput |> Agent |> log ; return! pendingUserDraftsRead ()
            | HandleInitializeUserDraftSummaryProjectionQry _ -> "HandleInitializeUserDraftSummaryProjectionQry when pendingUserDraftsRead" |> IgnoredInput |> Agent |> log ; return! pendingUserDraftsRead () }
        and projectingUserDraftSummary state userDraftSummaryDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingUserDraftSummary" |> IgnoredInput |> Agent |> log ; return! projectingUserDraftSummary state userDraftSummaryDic projecteeDic
            | OnUserDraftsRead _ -> "OnUserDraftsRead when projectingUserDraftSummary" |> IgnoredInput |> Agent |> log ; return! projectingUserDraftSummary state userDraftSummaryDic projecteeDic
            | OnUserDraftEventWritten userDraftEvent ->
                let source = "OnUserDraftEventWritten"
                sprintf "%s (%A) when projectingUserDraftSummary (%i user draft/s) (%i projectee/s)" source userDraftEvent userDraftSummaryDic.Count projecteeDic.Count |> Info |> log
                let state =
                    match userDraftEvent with
                    | UserDraftCreated (userDraftId, userId, draftId) ->
                        if userDraftId |> userDraftSummaryDic.ContainsKey |> not then // note: silently ignore already-known userDraftId (should never happen)
                            (userDraftId, userDraftSummaryDto (userId, draftId) 0) |> userDraftSummaryDic.Add
                            (userDraftSummaryDic, state) |> UserDraftChange |> updateState source projecteeDic
                        else state
                    | _ ->
                        let userDraftId = userDraftEvent.UserDraftId
                        if userDraftId |> userDraftSummaryDic.ContainsKey then // note: silently ignore unknown userDraftId (should never happen)
                            let userDraftSummary = userDraftSummaryDic.[userDraftId]
                            let userDraftSummary =
                                match userDraftEvent with
                                | Drafted _ -> { userDraftSummary with PickCount = userDraftSummary.PickCount + 1 }
                                | Undrafted _ -> { userDraftSummary with PickCount = userDraftSummary.PickCount - 1 }
                                | _ -> userDraftSummary // note: should never happen
                            userDraftSummaryDic.[userDraftId] <- userDraftSummary
                            (userDraftSummaryDic, state) |> UserDraftChange |> updateState source projecteeDic
                        else state
                return! projectingUserDraftSummary state userDraftSummaryDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnection"
                sprintf "%s (%A) when projectingUserDraftSummary (%i user draft/s) (%i projectee/s)" source connectionIds userDraftSummaryDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds
                sprintf "%s when projectingUserDraftSummary -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingUserDraftSummary state userDraftSummaryDic projecteeDic
            | HandleInitializeUserDraftSummaryProjectionQry (_, connectionId, userId, reply) ->
                let source = "HandleInitializeUserDraftSummaryProjectionQry"
                sprintf "%s for %A (%A) when projectingUserDraftSummary (%i user draft/s) (%i projectee/s)" source connectionId userId userDraftSummaryDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingUserDraftSummary -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> userDraftSummaryDtos |> Ok
                result |> logResult source (fun userDraftSummaryDtos -> sprintf "%i user draft/s" userDraftSummaryDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingUserDraftSummary state userDraftSummaryDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.UserDraftSummary |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | UserDraftsRead userDraftsRead -> userDraftsRead |> OnUserDraftsRead |> agent.Post
            | UserDraftEventWritten (_, userDraftEvent) ->
                match userDraftEvent with
                | UserDraftCreated _ | Drafted _ | Undrafted _ -> userDraftEvent |> OnUserDraftEventWritten |> agent.Post
                | PriorityChanged _ -> ()
            | ConnectionsSignedOut connectionIds -> connectionIds |> RemoveConnections |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to UserDraftsRead | UserDraftEventWritten (subset) | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeUserDraftSummaryProjectionQryAsync (token, connectionId, userId) =
        (fun reply -> (token, connectionId, userId, reply) |> HandleInitializeUserDraftSummaryProjectionQry) |> agent.PostAndAsyncReply

let userDraftSummary = UserDraftSummary ()
