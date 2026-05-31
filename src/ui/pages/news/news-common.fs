module Aornota.Sweepstake2026.Ui.Pages.News.Common

open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Shared

open System
open System.Collections.Generic

type AddPostInput =
    | NewMessageChanged of newMessage : string
    | AddPost
    | CancelAddPost

type EditPostInput =
    | MessageChanged of message : string
    | EditPost
    | CancelEditPost

type RemovePostInput =
    | ConfirmRemovePost
    | CancelRemovePost

type AddCustomMessageInput =
    | NewCustomMessageChanged of newCustomMessage : string
    | AddCustomMessage
    | CancelAddCustomMessage

type EditCustomMessageInput =
    | CustomMessageChanged of customMessage : string
    | EditCustomMessage
    | CancelEditCustomMessage

type RemoveCustomMessageInput =
    | ConfirmRemoveCustomMessage
    | CancelRemoveCustomMessage

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | ShowMarkdownSyntaxModal
    | SendUiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReadPreferencesResult of result : Result<DateTimeOffset option, exn>
    | WritePreferencesResult of result : Result<unit, exn>
    | ReceiveServerNewsMsg of serverNewsMsg : ServerNewsMsg
    | ToggleNewsIsCurrentPage of isCurrentPage : bool
    | DismissPost of postId : PostId
    | MorePosts
    | ShowAddPostModal
    | AddPostInput of addPostInput : AddPostInput
    | ShowEditPostModal of postId : PostId
    | EditPostInput of editPostInput : EditPostInput
    | ShowRemovePostModal of postId : PostId
    | RemovePostInput of removePostInput : RemovePostInput
    | ShowAddCustomMessageModal of fixtureId : FixtureId
    | AddCustomMessageInput of addCustomMessageInput : AddCustomMessageInput
    | ShowEditCustomMessageModal of fixtureId : FixtureId
    | EditCustomMessageInput of editCustomMessageInput : EditCustomMessageInput
    | ShowRemoveCustomMessageModal of fixtureId : FixtureId
    | RemoveCustomMessageInput of removeCustomMessageInput : RemoveCustomMessageInput

type Post = { Rvn : Rvn ; UserId : UserId ; Message : Markdown ; Timestamp : DateTimeOffset ; Removed : bool }
type PostDic = Dictionary<PostId, Post>

type AddPostStatus =
    | AddPostPending
    | AddPostFailed of errorText : string

type AddPostState = {
    NewPostId : PostId
    NewMessage : string
    NewMessageErrorText : string option
    AddPostStatus : AddPostStatus option }

type EditPostStatus =
    | EditPostPending
    | EditPostFailed of errorText : string

type EditPostState = {
    PostId : PostId
    Message : string
    MessageErrorText : string option
    EditPostStatus : EditPostStatus option }

type RemovePostStatus =
    | RemovePostPending
    | RemovePostFailed of errorText : string

type RemovePostState = {
    PostId : PostId
    RemovePostStatus : RemovePostStatus option }

type AddCustomMessageStatus =
    | AddCustomMessagePending
    | AddCustomMessageFailed of errorText : string

type AddCustomMessageState = {
    FixtureId : FixtureId
    NewCustomMessage : string
    NewCustomMessageErrorText : string option
    AddCustomMessageStatus : AddCustomMessageStatus option }

type EditCustomMessageStatus =
    | EditCustomMessagePending
    | EditCustomMessageFailed of errorText : string

type EditCustomMessageState = {
    FixtureId : FixtureId
    CustomMessage : string
    CustomMessageErrorText : string option
    EditCustomMessageStatus : EditCustomMessageStatus option }

type RemoveCustomMessageStatus =
    | RemoveCustomMessagePending
    | RemoveCustomMessageFailed of errorText : string

type RemoveCustomMessageState = {
    FixtureId : FixtureId
    RemoveCustomMessageStatus : RemoveCustomMessageStatus option }

type ReadyState = {
    HasMorePosts : bool
    MorePostsPending : bool
    AddPostState : AddPostState option
    EditPostState : EditPostState option
    RemovePostState : RemovePostState option
    AddCustomMessageState : AddCustomMessageState option
    EditCustomMessageState : EditCustomMessageState option
    RemoveCustomMessageState : RemoveCustomMessageState option }

type State = {
    NewsProjection : Projection<Rvn * PostDic * ReadyState>
    PreferencesRead : bool
    LastNewsSeen : DateTimeOffset option
    IsCurrentPage : bool
    UnseenCount : int }
