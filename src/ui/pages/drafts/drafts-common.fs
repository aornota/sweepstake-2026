module Aornota.Sweepstake2026.Ui.Pages.Drafts.Common

open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerDraftsMsg of serverDraftsMsg : ServerDraftsMsg
    | ShowDraft of draftId : DraftId
    | ChangePriority of draftId : DraftId * userDraftPick : UserDraftPick * priorityChange : PriorityChange
    | RemoveFromDraft of draftId : DraftId * userDraftPick : UserDraftPick

type State = {
    CurrentDraftId : DraftId option
    RemovalPending : (UserDraftPick * Rvn) option
    ChangePriorityPending : (UserDraftPick * PriorityChange * Rvn) option
    LastPriorityChanged : (UserDraftPick * PriorityChange) option }
