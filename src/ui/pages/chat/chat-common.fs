module Aornota.Sweepstake2026.Ui.Pages.Chat.Common

open Aornota.Sweepstake2026.Common.Domain.Chat
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Shared

open System
open System.Collections.Generic

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | ShowMarkdownSyntaxModal
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReadPreferencesResult of result : Result<DateTimeOffset option, exn>
    | WritePreferencesResult of result : Result<unit, exn>
    | ReceiveServerChatMsg of serverChatMsg : ServerChatMsg
    | ToggleChatIsCurrentPage of isCurrentPage : bool
    | DismissChatMessage of chatMessageId : ChatMessageId
    | NewMessageTextChanged of newMessageText : string
    | MoreChatMessages
    | SendChatMessage

type SendChatMessageStatus =
    | SendChatMessagePending
    | SendChatMessageFailed of errorText : string

type NewChatMessageState = {
    NewChatMessageId : ChatMessageId
    NewMessageText : string
    NewMessageErrorText : string option
    SendChatMessageStatus : SendChatMessageStatus option }

type ChatMessage = { UserId : UserId ; MessageText : Markdown ; Timestamp : DateTimeOffset ; Expired : bool }
type ChatMessageDic = Dictionary<ChatMessageId, ChatMessage>

type ReadyState = {
    HasMoreChatMessages : bool
    MoreChatMessagesPending : bool
    NewChatMessageState : NewChatMessageState }

type State = {
    AuthUser : AuthUser
    ChatProjection : Projection<Rvn * ChatMessageDic * ReadyState>
    PreferencesRead : bool
    LastChatSeen : DateTimeOffset option
    IsCurrentPage : bool
    UnseenCount : int }
