module Aornota.Sweepstake2026.Ui.Pages.Chat.State

open Aornota.Sweepstake2026.Common.Delta
open Aornota.Sweepstake2026.Common.Domain.Chat
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Json
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.JsonConverter
open Aornota.Sweepstake2026.Ui.Common.LocalStorage
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Common.Toasts
open Aornota.Sweepstake2026.Ui.Pages.Chat.Common
open Aornota.Sweepstake2026.Ui.Shared

open System

open Elmish

let [<Literal>] private CHAT_PREFERENCES_KEY = "sweepstake-2026-ui-chat-preferences"

let private readPreferencesCmd =
    let readPreferences () = async {
        return Key CHAT_PREFERENCES_KEY |> readJson |> Option.map (fun (Json json) -> json |> fromJson<DateTimeOffset>) }
    Cmd.OfAsync.either readPreferences () (Ok >> ReadPreferencesResult) (Error >> ReadPreferencesResult)

let private writePreferencesCmd state =
    let writePreferences (lastChatSeen:DateTimeOffset) = async {
        do lastChatSeen |> toJson |> Json |> writeJson (Key CHAT_PREFERENCES_KEY) }
    match state.LastChatSeen with
    | Some lastChatSeen -> Cmd.OfAsync.either writePreferences lastChatSeen (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)
    | None -> Cmd.none

let initialize (authUser:AuthUser) isCurrentPage readPreferences lastChatSeen : State * Cmd<Input> =
    let chatProjection, chatProjectionCmd =
        if authUser.Permissions.ChatPermission then Pending, InitializeChatProjectionQry |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg
        else Failed, Cmd.none
    let state =
        { AuthUser = authUser; ChatProjection = chatProjection ; PreferencesRead = readPreferences |> not ; LastChatSeen = lastChatSeen ; IsCurrentPage = isCurrentPage ; UnseenCount = 0 }
    let readPreferencesCmd = if readPreferences then readPreferencesCmd else Cmd.none
    state, Cmd.batch [ readPreferencesCmd ; chatProjectionCmd ]

let private defaultNewChatMessageState () = { NewChatMessageId = ChatMessageId.Create () ; NewMessageText = String.Empty ; NewMessageErrorText = None ; SendChatMessageStatus = None }

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private updateLastChatSeen state =
    let lastChatSeen = if state.IsCurrentPage && state.PreferencesRead then (DateTimeOffset.UtcNow.AddSeconds 10.) |> Some else state.LastChatSeen
    let unseenCount =
        match state.PreferencesRead, state.ChatProjection with
        | true, Ready (_, chatMessageDic, _) ->
            match lastChatSeen with
            | Some lastChatSeen -> chatMessageDic |> List.ofSeq |> List.filter (fun (KeyValue (_, chatMessage)) -> chatMessage.Timestamp > lastChatSeen) |> List.length
            | None -> chatMessageDic.Count
        | _ -> state.UnseenCount
    let state = { state with LastChatSeen = lastChatSeen ; UnseenCount = unseenCount }
    state, state |> writePreferencesCmd

let private chatMessage (chatMessageDto:ChatMessageDto) =
    { UserId = chatMessageDto.UserId ; MessageText = chatMessageDto.MessageText ; Timestamp = chatMessageDto.Timestamp ; Expired = false }

let private chatMessageDic (chatMessageDtos:ChatMessageDto list) =
    let chatMessageDic = ChatMessageDic ()
    chatMessageDtos |> List.iter (fun chatMessageDto ->
        let chatMessageId = chatMessageDto.ChatMessageId
        if chatMessageId |> chatMessageDic.ContainsKey |> not then // note: silently ignore duplicate chatMessageIds (should never happer)
            (chatMessageId, chatMessageDto |> chatMessage) |> chatMessageDic.Add)
    chatMessageDic

let private applyChatMessagesDelta currentRvn deltaRvn (delta:Delta<ChatMessageId, ChatMessageDto>) (chatMessageDic:ChatMessageDic) =
    let chatMessageDic = ChatMessageDic chatMessageDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (chatMessageId, chatMessageDto) -> if chatMessageId |> chatMessageDic.ContainsKey then (chatMessageId, chatMessageDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (chatMessageId, chatMessageDto) -> (chatMessageId, chatMessageDto |> chatMessage) |> chatMessageDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (chatMessageId, chatMessageDto) -> if chatMessageId |> chatMessageDic.ContainsKey |> not then (chatMessageId, chatMessageDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (chatMessageId, chatMessageDto) -> chatMessageDic.[chatMessageId] <- (chatMessageDto |> chatMessage)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun chatMessageId -> if chatMessageId |> chatMessageDic.ContainsKey |> not then chatMessageId |> Some else None)
        // Note: delta.Removed correspond to "expired" (i.e. no longer cached on server) - but marked as such on client, rather than removed.
        if doNotExist.Length = 0 then delta.Removed |> List.iter (fun chatMessageId ->
            let chatMessage = chatMessageDic.[chatMessageId]
            chatMessageDic.[chatMessageId] <- { chatMessage with Expired = true }) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> chatMessageDic |> Ok)

let private handleServerChatMsg serverChatMsg state : State * Cmd<Input> =
    match serverChatMsg, state.ChatProjection with
    | InitializeChatProjectionQryResult (Ok (chatMessageDtos, hasMoreChatMessages)), Pending ->
        let readyState = { HasMoreChatMessages = hasMoreChatMessages ; MoreChatMessagesPending = false ; NewChatMessageState = defaultNewChatMessageState () }
        let state = { state with ChatProjection = (initialRvn, chatMessageDtos |> chatMessageDic, readyState) |> Ready }
        state |> updateLastChatSeen
    | InitializeChatProjectionQryResult (Error error), Pending ->
        { state with ChatProjection = Failed }, error |> qryErrorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | MoreChatMessagesQryResult (Ok (newRvn, chatMessageDtos, hasMoreChatMessages)), Ready (rvn, chatMessageDic, readyState) ->
        if readyState.MoreChatMessagesPending |> not then // note: silently ignore unexpected result
            state, Cmd.none
        else if chatMessageDtos.Length = 0 then
            let readyState = { readyState with HasMoreChatMessages = hasMoreChatMessages ; MoreChatMessagesPending = false }
            { state with ChatProjection = (newRvn, chatMessageDic, readyState) |> Ready }, "Unable to retrieve more chat messages<br><br>They have probably expired" |> warningToastCmd
        else
            let addedChatMessageDtos = chatMessageDtos |> List.map (fun chatMessageDto -> chatMessageDto.ChatMessageId, chatMessageDto)
            let chatMessageDtoDelta = { Added = addedChatMessageDtos ; Changed = [] ; Removed = [] }
            match chatMessageDic |> applyChatMessagesDelta rvn newRvn chatMessageDtoDelta with
            | Ok chatMessageDic ->
                let readyState = { readyState with HasMoreChatMessages = hasMoreChatMessages ; MoreChatMessagesPending = false }
                let state = { state with ChatProjection = (newRvn, chatMessageDic, readyState) |> Ready }
                state |> updateLastChatSeen
            | Error error ->
                let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" chatMessageDtoDelta chatMessageDic error)
                let state, cmd = initialize state.AuthUser state.IsCurrentPage false state.LastChatSeen
                state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | MoreChatMessagesQryResult (Error error), Ready (rvn, chatMessageDic, readyState) ->
        if readyState.MoreChatMessagesPending |> not then // note: silently ignore unexpected result
            state, Cmd.none
        else
            let readyState = { readyState with MoreChatMessagesPending = false }
            { state with ChatProjection = (rvn, chatMessageDic, readyState) |> Ready }, shouldNeverHappenCmd (sprintf "MoreChatMessagesQryResult Error %A" error)
    | SendChatMessageCmdResult result, Ready (rvn, chatMessageDic, readyState) ->
        let newChatMessageState = readyState.NewChatMessageState
        match newChatMessageState.SendChatMessageStatus with
        | Some SendChatMessagePending ->
            match result with
            | Ok _ ->
                let readyState = { readyState with NewChatMessageState = defaultNewChatMessageState () }
                { state with ChatProjection = (rvn, chatMessageDic, readyState) |> Ready }, "Chat message has been sent" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "SendChatMessageCmdResult error -> %A" error) (error |> cmdErrorText)
                let newChatMessageState = { newChatMessageState with SendChatMessageStatus = errorText |> SendChatMessageFailed |> Some }
                let readyState = { readyState with NewChatMessageState = newChatMessageState }
                { state with ChatProjection = (rvn, chatMessageDic, readyState) |> Ready }, "Unable to send chat message" |> errorToastCmd
        | Some (SendChatMessageFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected SendChatMessageCmdResult when SendChatMessageStatus is not SendChatMessagePending -> %A" result)
    | ChatProjectionMsg (ChatMessagesDeltaMsg (deltaRvn, chatMessageDtoDelta, hasMoreChatMessages)), Ready (rvn, chatMessageDic, readyState) ->
        match chatMessageDic |> applyChatMessagesDelta rvn deltaRvn chatMessageDtoDelta with
        | Ok chatMessageDic ->
            let readyState = { readyState with HasMoreChatMessages = hasMoreChatMessages }
            let state = { state with ChatProjection = (deltaRvn, chatMessageDic, readyState) |> Ready }
            state |> updateLastChatSeen
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" chatMessageDtoDelta chatMessageDic error)
            let state, cmd = initialize state.AuthUser state.IsCurrentPage false state.LastChatSeen
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | ChatProjectionMsg _, _ -> // note: silently ignore ChatProjectionMsg if not Ready
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerChatMsg when %A -> %A" state.ChatProjection serverChatMsg)

let transition input state =
    let state, cmd, isUserNonApiActivity =
        match input, state.ChatProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ShowMarkdownSyntaxModal, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReadPreferencesResult (Ok lastChatSeen), _ ->
            let state = { state with PreferencesRead = true ; LastChatSeen = lastChatSeen }
            let state, cmd = state |> updateLastChatSeen
            state, cmd, false
        | ReadPreferencesResult (Error _), _ -> // note: silently ignore error
            state, None |> Ok |> ReadPreferencesResult |> Cmd.ofMsg, false
        | WritePreferencesResult _, _ -> // note: nothing to do here
            state, Cmd.none, false
        | ReceiveServerChatMsg serverChatMsg, _ ->
            let state, cmd = state |> handleServerChatMsg serverChatMsg
            state, cmd, false
        | ToggleChatIsCurrentPage isCurrentPage, _ ->
            let state = { state with IsCurrentPage = isCurrentPage }
            let state, cmd = state |> updateLastChatSeen
            state, cmd, false
        | DismissChatMessage chatMessageId, Ready (_, chatMessageDic, _) -> // note: silently ignore unknown chatMessageId (should never happen)
            if chatMessageId |> chatMessageDic.ContainsKey then chatMessageId |> chatMessageDic.Remove |> ignore
            state, Cmd.none, true
        | NewMessageTextChanged newMessageText, Ready (rvn, chatMessageDic, readyState) ->
            let newChatMessageState = { readyState.NewChatMessageState with NewMessageText = newMessageText ; NewMessageErrorText = Markdown newMessageText |> validateChatMessageText }
            let readyState = { readyState with NewChatMessageState = newChatMessageState }
            { state with ChatProjection = (rvn, chatMessageDic, readyState) |> Ready }, Cmd.none, true
        | MoreChatMessages, Ready (rvn, chatMessageDic, readyState) -> // note: assume no need to validate HasMoreChatMessages (i.e. because Chat.Render.render will ensure that MoreChatMessages can only be dispatched when true)
            let readyState = { readyState with MoreChatMessagesPending = true }
            let cmd = MoreChatMessagesQry |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ChatProjection = (rvn, chatMessageDic, readyState) |> Ready }, cmd, false
        | SendChatMessage, Ready (rvn, chatMessageDic, readyState) -> // note: assume no need to validate NewMessageText (i.e. because Chat.Render.render will ensure that SendChatMessage can only be dispatched when valid)
            let newChatMessageState = readyState.NewChatMessageState
            let newChatMessageState = { newChatMessageState with SendChatMessageStatus = SendChatMessagePending |> Some }
            let readyState = { readyState with NewChatMessageState = newChatMessageState }
            let newChatMessageId, newMessageText = newChatMessageState.NewChatMessageId, Markdown (newChatMessageState.NewMessageText.Trim ())
            let cmd = (newChatMessageId, newMessageText) |> SendChatMessageCmd |> UiAuthChatMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ChatProjection = (rvn, chatMessageDic, readyState) |> Ready }, cmd, false
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.ChatProjection input), false
    state, cmd, isUserNonApiActivity
