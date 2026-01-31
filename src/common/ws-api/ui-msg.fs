module Aornota.Sweepstake2026.Common.WsApi.UiMsg

open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision

open Aornota.Sweepstake2026.Common.Domain.Chat
open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User

type UiUnauthAppMsg =
    | SignInCmd of sessionId : SessionId * userName : UserName * password : Password
    | AutoSignInCmd of sessionId : SessionId * jwt : Jwt
    | InitializeUsersProjectionUnauthQry
    | InitializeSquadsProjectionQry
    | InitializeFixturesProjectionQry

type UiUnauthNewsMsg =
    | InitializeNewsProjectionQry
    | MorePostsQry

type UiUnauthMsg =
    | UiUnauthAppMsg of uiUnauthAppMsg : UiUnauthAppMsg
    | UiUnauthNewsMsg of uiUnauthNewsMsg : UiUnauthNewsMsg

type UiAuthAppMsg =
    | SignOutCmd
    | ChangePasswordCmd of currentRvn : Rvn * password : Password
    | InitializeUsersProjectionAuthQry
    | InitializeDraftsProjectionQry

type UiAuthUserAdminMsg =
    | CreateUserCmd of userId : UserId * userName : UserName * password : Password * userType : UserType
    | ResetPasswordCmd of userId : UserId * currentRvn : Rvn * password : Password
    | ChangeUserTypeCmd of userId : UserId * currentRvn : Rvn * userType : UserType

type UiAuthDraftAdminMsg =
    | InitializeUserDraftSummaryProjectionQry
    | ProcessDraftCmd of draftId : DraftId * currentRvn : Rvn

type UiAuthNewsMsg =
    | CreatePostCmd of postId : PostId * postType : PostType * messageText : Markdown
    | ChangePostCmd of postId : PostId * currentRvn : Rvn * messageText : Markdown
    | RemovePostCmd of postId : PostId * currentRvn : Rvn

type UiAuthSquadsMsg =
    | AddPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    | ChangePlayerNameCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName
    | ChangePlayerTypeCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerType : PlayerType
    | WithdrawPlayerCmd of squadId : SquadId * currentRvn : Rvn * playerId : PlayerId
    | EliminateSquadCmd of squadId : SquadId * currentRvn : Rvn
    | AddToDraftCmd of draftId : DraftId * currentRvn : Rvn * userDraftPick : UserDraftPick
    | RemoveFromDraftCmd of draftId : DraftId * currentRvn : Rvn * userDraftPick : UserDraftPick
    | FreePickCmd of draftId : DraftId * currentRvn : Rvn * draftPick : DraftPick

type UiAuthFixturesMsg =
    | ConfirmParticipantCmd of fixtureId : FixtureId * currentRvn : Rvn * role : Role * squadId : SquadId
    | AddMatchEventCmd of fixtureId : FixtureId * currentRvn : Rvn * matchEvent : MatchEvent
    | RemoveMatchEventCmd of fixtureId : FixtureId * currentRvn : Rvn * matchEventId : MatchEventId * matchEvent : MatchEvent

type UiAuthDraftsMsg =
    | ChangePriorityCmd of draftId : DraftId * currentRvn : Rvn * userDraftPick : UserDraftPick * priorityChange : PriorityChange
    | RemoveFromDraftCmd of draftId : DraftId * currentRvn : Rvn * userDraftPick : UserDraftPick

type UiAuthChatMsg =
    | InitializeChatProjectionQry
    | MoreChatMessagesQry
    | SendChatMessageCmd of chatMessageId : ChatMessageId * messageText : Markdown

type UiAuthMsg =
    | UserNonApiActivity
    | UiAuthAppMsg of uiAuthAppMsg : UiAuthAppMsg
    | UiAuthUserAdminMsg of uiAuthUserAdminMsg : UiAuthUserAdminMsg
    | UiAuthDraftAdminMsg of uiAuthDraftAdminMsg : UiAuthDraftAdminMsg
    | UiAuthNewsMsg of uiAuthNewsMsg : UiAuthNewsMsg
    | UiAuthSquadsMsg of uiAuthSquadsMsg : UiAuthSquadsMsg
    | UiAuthFixturesMsg of uiAuthFixturesMsg : UiAuthFixturesMsg
    | UiAuthDraftsMsg of uiAuthDraftsMsg : UiAuthDraftsMsg
    | UiAuthChatMsg of uiAuthChatMsg : UiAuthChatMsg

type UiMsg =
    | Wiff
    | UiUnauthMsg of uiUnauthMsg : UiUnauthMsg
    | UiAuthMsg of jwt : Jwt * uiAuthMsg : UiAuthMsg
