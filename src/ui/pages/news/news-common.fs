module Aornota.Sweepstake2026.Ui.Pages.News.Common

open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Shared

open System
open System.Collections.Generic

type AddPostInput =
    | NewMessageTextChanged of newMessageText : string
    | AddPost
    | CancelAddPost

type EditPostInput =
    | MessageTextChanged of newMessageText : string
    | EditPost
    | CancelEditPost

type RemovePostInput =
    | ConfirmRemovePost
    | CancelRemovePost

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

type Post = { Rvn : Rvn ; UserId : UserId ; PostTypeDto : PostTypeDto ; Timestamp : DateTimeOffset ; Removed : bool }
type PostDic = Dictionary<PostId, Post>

type AddPostStatus =
    | AddPostPending
    | AddPostFailed of errorText : string

type AddPostState = {
    NewPostId : PostId
    NewMessageText : string
    NewMessageErrorText : string option
    AddPostStatus : AddPostStatus option }

type EditPostStatus =
    | EditPostPending
    | EditPostFailed of errorText : string

type EditPostState = {
    PostId : PostId
    MessageText : string
    MessageErrorText : string option
    EditPostStatus : EditPostStatus option }

type RemovePostStatus =
    | RemovePostPending
    | RemovePostFailed of errorText : string

type RemovePostState = {
    PostId : PostId
    RemovePostStatus : RemovePostStatus option }

type ReadyState = {
    HasMorePosts : bool
    MorePostsPending : bool
    AddPostState : AddPostState option
    EditPostState : EditPostState option
    RemovePostState : RemovePostState option }

type State = {
    NewsProjection : Projection<Rvn * PostDic * ReadyState>
    PreferencesRead : bool
    LastNewsSeen : DateTimeOffset option
    IsCurrentPage : bool
    UnseenCount : int }
