module Aornota.Sweepstake2026.Server.Agents.Projections.Chat

(* Broadcasts: SendMsg
   Subscribes: Tick
               ConnectionsSignedOut | Disconnected *)

open Aornota.Sweepstake2026.Common.Delta
open Aornota.Sweepstake2026.Common.Domain.Chat
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Agents.Ticker
open Aornota.Sweepstake2026.Server.Authorization
open Aornota.Sweepstake2026.Server.Common.DeltaHelper
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private ChatInput =
    | Start of reply : AsyncReplyChannel<unit>
    | Housekeeping
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeChatProjectionQry of token : ChatToken * connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<ChatMessageDto list * bool, AuthQryError<string>>>
    | HandleMoreChatMessagesQry of token : ChatToken * connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<Rvn * ChatMessageDto list * bool, AuthQryError<string>>>
    | HandleSendChatMessageCmd of token : ChatToken * userId : UserId * chatMessageId : ChatMessageId * messageText : Markdown
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>

type private ChatMessage = { Ordinal : int ; UserId : UserId ; MessageText : Markdown ; Timestamp : DateTimeOffset }
type private ChatMessageDic = Dictionary<ChatMessageId, ChatMessage>

type private Projectee = { LastRvn : Rvn ; MinChatMessageOrdinal : int option ; LastHasMoreChatMessages : bool }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { ChatMessageDic : ChatMessageDic }

type private StateChangeType =
    | Initialization of chatMessageDic : ChatMessageDic
    | ChatMessageChange of chatMessageDic : ChatMessageDic * state : State

let [<Literal>] private HOUSEKEEPING_INTERVAL = 1.<minute>
let [<Literal>] private CHAT_MESSAGE_EXPIRES_AFTER = 24.<hour>
let [<Literal>] private CHAT_MESSAGE_BATCH_SIZE = 10

let private log category = (Projection Chat, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private cutoff (after:int<second>) = float (after * -1) |> DateTimeOffset.UtcNow.AddSeconds

let private chatMessageDto (chatMessageId, chatMessage:ChatMessage) =
    { ChatMessageId = chatMessageId ; UserId = chatMessage.UserId ; MessageText = chatMessage.MessageText ; Timestamp = chatMessage.Timestamp }

let private chatMessageDtos state = state.ChatMessageDic |> List.ofSeq |> List.map (fun (KeyValue (chatMessageId, chatMessage)) -> (chatMessageId, chatMessage) |> chatMessageDto)

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendChatMessageDelta removedOrdinals minChatMessageOrdinal (projecteeDic:ProjecteeDic) (chatMessageDelta:Delta<ChatMessageId, ChatMessage>) =
    let isRelevant projecteeMinChatMessageOrdinal ordinal =
        match projecteeMinChatMessageOrdinal with
        | Some projecteeMinChatMessageOrdinal when projecteeMinChatMessageOrdinal > ordinal -> false
        | Some _ | None -> true
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let hasMoreChatMessages =
            match projectee.MinChatMessageOrdinal, minChatMessageOrdinal with
            | Some projecteeMinChatMessageOrdinal, Some minChatMessageOrdinal when projecteeMinChatMessageOrdinal > minChatMessageOrdinal -> true
            | Some _, Some _ | Some _, None | None, Some _ | None, None -> false
        let addedChatMessageDtos =
            chatMessageDelta.Added // note: no need to filter based on projectee.MinChatMessageOrdinal
            |> List.map (fun (chatMessageId, chatMessage) -> chatMessageId, (chatMessageId, chatMessage) |> chatMessageDto)
        let changedChatMessageDtos =
            chatMessageDelta.Changed
            |> List.filter (fun (_, chatMessage) -> chatMessage.Ordinal |> isRelevant projectee.MinChatMessageOrdinal)
            |> List.map (fun (chatMessageId, chatMessage) -> chatMessageId, (chatMessageId, chatMessage) |> chatMessageDto)
        let removedChatMessageIds =
            chatMessageDelta.Removed
            |> List.choose (fun chatMessageId ->
                match removedOrdinals |> List.tryFind (fun (removedChatMessageId, _) -> removedChatMessageId = chatMessageId) with
                | Some (_, ordinal) -> (chatMessageId, ordinal) |> Some
                | None -> None) // note: should never happen
            |> List.filter (fun (_, ordinal) -> ordinal |> isRelevant projectee.MinChatMessageOrdinal)
            |> List.map fst
        let chatMessageDtoDelta = { Added = addedChatMessageDtos ; Changed = changedChatMessageDtos ; Removed = removedChatMessageIds }
        if chatMessageDtoDelta |> isEmpty |> not || hasMoreChatMessages <> projectee.LastHasMoreChatMessages then
            let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn ; LastHasMoreChatMessages = hasMoreChatMessages }
            sprintf "sendChatMessageDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
            (projectee.LastRvn, chatMessageDtoDelta, hasMoreChatMessages) |> ChatMessagesDeltaMsg |> ChatProjectionMsg |> ServerChatMsg |> sendMsg [ connectionId ]
            (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization chatMessageDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { ChatMessageDic = ChatMessageDic chatMessageDic }
        | ChatMessageChange (chatMessageDic, state) ->
            let chatMessageDelta = chatMessageDic |> delta state.ChatMessageDic
            if chatMessageDelta |> isEmpty |> not then
                let removedOrdinals = chatMessageDelta.Removed |> List.choose (fun chatMessageId ->
                    if chatMessageId |> state.ChatMessageDic.ContainsKey |> not then None // note: ignore unknown chatMessageId (should never happen)
                    else
                        let chatMessage = state.ChatMessageDic.[chatMessageId]
                        (chatMessageId, chatMessage.Ordinal) |> Some)
                let minChatMessageOrdinal =
                    if chatMessageDic.Count > 0 then chatMessageDic |> List.ofSeq |> List.map (fun (KeyValue (_, chatMessage)) -> chatMessage.Ordinal) |> List.min |> Some
                    else None
                sprintf "%s -> ChatMessage delta %A -> %i projectee/s" source chatMessageDelta projecteeDic.Count |> Info |> log
                chatMessageDelta |> sendChatMessageDelta removedOrdinals minChatMessageOrdinal projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with ChatMessageDic = ChatMessageDic chatMessageDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

type Chat () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                let source = "Start"
                "Start when awaitingStart -> pendingOnUsersRead (0 chat messages) (0 projectees)" |> Info |> log
                let chatMessageDic = ChatMessageDic ()
                let projecteeDic = ProjecteeDic ()
                let state = chatMessageDic |> Initialization |> updateState source projecteeDic
                () |> reply.Reply
                return! projectingChat state chatMessageDic projecteeDic
            | Housekeeping -> "Housekeeping when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeChatProjectionQry _ -> "HandleInitializeChatProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleMoreChatMessagesQry _ -> "HandleMoreChatMessagesQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleSendChatMessageCmd _ -> "HandleSendChatMessageCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and projectingChat state chatMessageDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingChat" |> IgnoredInput |> Agent |> log ; return! projectingChat state chatMessageDic projecteeDic
            | Housekeeping ->
                let source = "Housekeeping"
                sprintf "%s when projectingChat (%i chat message/s) (%i projectee/s)" source chatMessageDic.Count projecteeDic.Count |> Info |> log
                let expirationCutoff = cutoff (int (CHAT_MESSAGE_EXPIRES_AFTER |> hoursToSeconds) * 1<second>)
                let updatedChatMessageDic = ChatMessageDic ()
                chatMessageDic |> List.ofSeq |> List.iter (fun (KeyValue (chatMessageId, chatMessage)) ->
                    if chatMessage.Timestamp > expirationCutoff then (chatMessageId, chatMessage) |> updatedChatMessageDic.Add)
                let state, chatMessageDic =
                    if updatedChatMessageDic.Count = chatMessageDic.Count then state, chatMessageDic
                    else
                        let state = (updatedChatMessageDic, state) |> ChatMessageChange |> updateState source projecteeDic
                        state, updatedChatMessageDic
                return! projectingChat state chatMessageDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingChat (%i chat message/s) (%i projectee/s)" source connectionIds chatMessageDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds
                sprintf "%s when projectingChat -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingChat state chatMessageDic projecteeDic
            | HandleInitializeChatProjectionQry (_, connectionId, reply) ->
                let source = "HandleInitializeChatProjectionQry"
                sprintf "%s for %A when projectingChat (%i chat message/s) (%i projectee/s)" source connectionId chatMessageDic.Count projecteeDic.Count |> Info |> log
                let initializedState, minChatMessageOrdinal, hasMoreChatMessages =
                    if chatMessageDic.Count <= CHAT_MESSAGE_BATCH_SIZE then
                        let minChatMessageOrdinal =
                            if chatMessageDic.Count > 0 then chatMessageDic |> List.ofSeq |> List.map (fun (KeyValue (_, chatMessage)) -> chatMessage.Ordinal) |> List.min |> Some
                            else None
                        state, minChatMessageOrdinal, false
                    else
                        let initialChatMessages =
                            chatMessageDic
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (chatMessageId, chatMessage)) -> chatMessageId, chatMessage)
                            |> List.sortBy (fun (_, chatMessage) -> chatMessage.Ordinal) |> List.rev |> List.take CHAT_MESSAGE_BATCH_SIZE
                        let minChatMessageOrdinal = initialChatMessages |> List.map (fun (_, chatMessage) -> chatMessage.Ordinal) |> List.min |> Some
                        let initialChatMessageDic = ChatMessageDic ()
                        initialChatMessages |> List.iter (fun (chatMessageId, chatMessage) -> (chatMessageId, chatMessage) |> initialChatMessageDic.Add)
                        { state with ChatMessageDic = initialChatMessageDic }, minChatMessageOrdinal, true
                let projectee = { LastRvn = initialRvn ; MinChatMessageOrdinal = minChatMessageOrdinal ; LastHasMoreChatMessages = hasMoreChatMessages }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingChat -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = (initializedState |> chatMessageDtos, hasMoreChatMessages) |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingChat state chatMessageDic projecteeDic
            | HandleMoreChatMessagesQry (_, connectionId, reply) ->
                let source = "HandleMoreChatMessagesQry"
                sprintf "%s for %A when projectingChat (%i chat message/s) (%i projectee/s)" source connectionId chatMessageDic.Count projecteeDic.Count |> Info |> log
                let result =
                    if connectionId |> projecteeDic.ContainsKey |> not then ifDebug (sprintf "%A does not exist" connectionId) UNEXPECTED_ERROR |> OtherError |> OtherAuthQryError |> Error
                    else
                        let projectee = projecteeDic.[connectionId]
                        let moreChatMessages =
                            chatMessageDic
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (chatMessageId, chatMessage)) -> chatMessageId, chatMessage)
                            |> List.filter (fun (_, chatMessage) ->
                                match projectee.MinChatMessageOrdinal with
                                | Some minChatMessageOrdinal -> minChatMessageOrdinal > chatMessage.Ordinal
                                | None -> true) // note: should never happen
                            |> List.sortBy (fun (_, chatMessage) -> chatMessage.Ordinal) |> List.rev
                        let moreChatMessages, hasMoreChatMessages =
                            if moreChatMessages.Length <= CHAT_MESSAGE_BATCH_SIZE then moreChatMessages, false
                            else moreChatMessages |> List.take CHAT_MESSAGE_BATCH_SIZE, true
                        let minChatMessageOrdinal =
                            if moreChatMessages.Length > 0 then moreChatMessages |> List.map (fun (_, chatMessage) -> chatMessage.Ordinal) |> List.min |> Some
                            else projectee.MinChatMessageOrdinal // note: should never happen
                        let chatMessageDtos = moreChatMessages |> List.map chatMessageDto
                        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn ; MinChatMessageOrdinal = minChatMessageOrdinal ; LastHasMoreChatMessages = hasMoreChatMessages }
                        projecteeDic.[connectionId] <- projectee
                        (projectee.LastRvn, chatMessageDtos, hasMoreChatMessages) |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingChat state chatMessageDic projecteeDic
            | HandleSendChatMessageCmd (_, userId, chatMessageId, messageText, reply) ->
                let source = "HandleSendChatMessageCmd"
                sprintf "%s (%A %A) when projectingChat (%i chat message/s) (%i projectee/s)" source userId chatMessageId chatMessageDic.Count projecteeDic.Count |> Info |> log
                let result =
                    if chatMessageId |> chatMessageDic.ContainsKey then ifDebug (sprintf "%A already exists" chatMessageId) UNEXPECTED_ERROR |> OtherError |> OtherAuthCmdError |> Error
                    else
                        match messageText |> validateChatMessageText with
                        | Some errorText -> errorText |> OtherError |> OtherAuthCmdError |> Error
                        | None ->
                            let nextOrdinal =
                                if chatMessageDic.Count = 0 then 1
                                else (chatMessageDic |> List.ofSeq |> List.map (fun (KeyValue (_, chatMessage)) -> chatMessage.Ordinal) |> List.max) + 1
                            (chatMessageId, { Ordinal = nextOrdinal ; UserId = userId ; MessageText = messageText ; Timestamp = DateTimeOffset.UtcNow }) |> chatMessageDic.Add |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                let state = (chatMessageDic, state) |> ChatMessageChange |> updateState source projecteeDic
                return! projectingChat state chatMessageDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Chat |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | Tick (ticks, secondsPerTick) -> if (ticks, secondsPerTick) |> isEveryNSeconds (int (HOUSEKEEPING_INTERVAL |> minutesToSeconds) * 1<second>) then Housekeeping |> agent.Post
            | ConnectionsSignedOut connectionIds -> connectionIds |> RemoveConnections |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to Tick | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeChatProjectionQryAsync (token, connectionId) =
        (fun reply -> (token, connectionId, reply) |> HandleInitializeChatProjectionQry) |> agent.PostAndAsyncReply
    member __.HandleMoreChatMessagesQryAsync (token, connectionId) =
        (fun reply -> (token, connectionId, reply) |> HandleMoreChatMessagesQry) |> agent.PostAndAsyncReply
    member __.HandleSendChatMessageCmdAsync (token, userId, chatMessageId, messageText) =
        (fun reply -> (token, userId, chatMessageId, messageText, reply) |> HandleSendChatMessageCmd) |> agent.PostAndAsyncReply

let chat = Chat ()
