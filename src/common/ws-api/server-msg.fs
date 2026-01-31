module Aornota.Sweepstake2026.Common.WsApi.ServerMsg

open Aornota.Sweepstake2026.Common.Delta
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Revision

open Aornota.Sweepstake2026.Common.Domain.Chat
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User

open System

type ServerUiMsgError =
    | ReceiveUiMsgError of errorText : string
    | DeserializeUiMsgError of errorText : string

type JwtError = | JwtError of errorText : string

type AuthznError = | NotAuthorized

type PersistenceError = | PersistenceError of errorText : string

type OtherError<'a> = | OtherError of otherError : 'a

type SignInCmdError<'a> =
    | InvalidCredentials of errorText : string option
    | SignInCmdJwtError of jwtError : JwtError
    | OtherSignInCmdError of otherError : OtherError<'a>

type AutoSignInCmdError<'a> =
    | AutoSignInCmdJwtError of jwtError : JwtError
    | OtherAutoSignInCmdError of otherError : OtherError<'a>

type AuthCmdError<'a> =
    | AuthCmdJwtError of jwtError : JwtError
    | AuthCmdAuthznError of authznError : AuthznError
    | AuthCmdPersistenceError of persistenceError : PersistenceError
    | OtherAuthCmdError of otherError : OtherError<'a>

type AuthQryError<'a> =
    | AuthQryJwtError of jwtError : JwtError
    | AuthQryAuthznError of authznError : AuthznError
    | OtherAuthQryError of otherError : OtherError<'a>

type AutoSignOutReason =
    // TODO-NMB-LOW?... | SessionExpired
    | PasswordReset
    | PermissionsChanged of isPersonaNonGrata : bool

type UsersProjectionMsg =
    | UsersDeltaUnauthMsg of deltaRvn : Rvn * delta : Delta<UserId, UserUnauthDto>
    | UsersDeltaAuthMsg of deltaRvn : Rvn * delta : Delta<UserId, UserDto>
    | UserSignedInAuthMsg of userName : UserName
    | UserSignedOutAuthMsg of userName : UserName

type SquadsProjectionMsg =
    | SquadsDeltaMsg of deltaRvn : Rvn * delta : Delta<SquadId, SquadOnlyDto>
    | PlayersDeltaMsg of deltaRvn : Rvn * squadId : SquadId * squadRvn : Rvn * Delta<PlayerId, PlayerDto>

type FixturesProjectionMsg =
    | FixturesDeltaMsg of deltaRvn : Rvn * delta : Delta<FixtureId, FixtureDto>

type DraftsProjectionMsg =
    | DraftsDeltaMsg of deltaRvn : Rvn * delta : Delta<DraftId, DraftDto>
    | CurrentUserDraftDtoChangedMsg of changeRvn : Rvn * currentUserDraftDto : CurrentUserDraftDto option

type ServerAppMsg =
    | ServerUiMsgErrorMsg of serverUiMsgError : ServerUiMsgError
    | ConnectedMsg of startedOffset : DateTimeOffset * otherConnectionCount : int * signedInUserCount : int
    | SignInCmdResult of result : Result<AuthUser, SignInCmdError<string>>
    | AutoSignInCmdResult of result : Result<AuthUser, AutoSignInCmdError<string>>
    | ChangePasswordCmdResult of result : Result<Rvn, AuthCmdError<string>>
    | SignOutCmdResult of result : Result<unit, AuthCmdError<string>>
    | AutoSignOutMsg of reason : AutoSignOutReason option
    | InitializeUsersProjectionUnauthQryResult of result : Result<UserUnauthDto list, OtherError<string>>
    | InitializeUsersProjectionAuthQryResult of result : Result<UserDto list, AuthQryError<string>>
    | UsersProjectionMsg of usersProjectionMsg : UsersProjectionMsg
    | InitializeSquadsProjectionQryResult of result : Result<SquadDto list, OtherError<string>>
    | SquadsProjectionMsg of squadsProjectionMsg : SquadsProjectionMsg
    | InitializeFixturesProjectionQryResult of result : Result<FixtureDto list, OtherError<string>>
    | FixturesProjectionMsg of fixturesProjectionMsg : FixturesProjectionMsg
    | InitializeDraftsProjectionQryResult of result : Result<DraftDto list * CurrentUserDraftDto option, AuthQryError<string>>
    | DraftsProjectionMsg of draftsProjectionMsg : DraftsProjectionMsg

type ServerUserAdminMsg =
    | CreateUserCmdResult of result : Result<UserName, AuthCmdError<string>>
    | ResetPasswordCmdResult of result : Result<UserName, AuthCmdError<string>>
    | ChangeUserTypeCmdResult of result : Result<UserName, AuthCmdError<string>>

type UserDraftSummaryProjectionMsg =
    | UserDraftSummariesDeltaMsg of deltaRvn : Rvn * delta : Delta<UserDraftKey, UserDraftSummaryDto>

type ServerDraftAdminMsg =
    | InitializeUserDraftSummaryProjectionQryResult of result : Result<UserDraftSummaryDto list, AuthQryError<string>>
    | ProcessDraftCmdResult of result : Result<unit, AuthCmdError<string>>
    | UserDraftSummaryProjectionMsg of userDraftSummaryProjectionMsg : UserDraftSummaryProjectionMsg

type NewsProjectionMsg =
    | PostsDeltaMsg of deltaRvn : Rvn * delta : Delta<PostId, PostDto> * hasMorePosts : bool

type ServerNewsMsg =
    | InitializeNewsProjectionQryResult of result : Result<PostDto list * bool, OtherError<string>>
    | MorePostsQryResult of result : Result<Rvn * PostDto list * bool, OtherError<string>>
    | CreatePostCmdResult of result : Result<unit, AuthCmdError<string>>
    | ChangePostCmdResult of result : Result<unit, AuthCmdError<string>>
    | RemovePostCmdResult of result : Result<unit, AuthCmdError<string>>
    | NewsProjectionMsg of newsProjectionMsg : NewsProjectionMsg

type ServerSquadsMsg =
    | AddPlayerCmdResult of result : Result<Rvn * PlayerName, AuthCmdError<string>>
    | ChangePlayerNameCmdResult of result : Result<PlayerName * PlayerName, AuthCmdError<string>>
    | ChangePlayerTypeCmdResult of result : Result<PlayerName, AuthCmdError<string>>
    | WithdrawPlayerCmdResult of result : Result<PlayerName, AuthCmdError<string>>
    | EliminateSquadCmdResult of result : Result<SquadName, AuthCmdError<string>>
    | AddToDraftCmdResult of result : Result<UserDraftPick, UserDraftPick * AuthCmdError<string>>
    | RemoveFromDraftCmdResult of result : Result<UserDraftPick, UserDraftPick * AuthCmdError<string>>
    | FreePickCmdResult of result : Result<DraftPick, AuthCmdError<string>>

type ServerFixturesMsg =
    | ConfirmParticipantCmdResult of result : Result<Unconfirmed, AuthCmdError<string>>
    | AddMatchEventCmdResult of result : Result<MatchEvent, AuthCmdError<string>>
    | RemoveMatchEventCmdResult of result : Result<MatchEvent, AuthCmdError<string>>

type ServerDraftsMsg =
    | ChangePriorityCmdResult of result : Result<UserDraftPick, UserDraftPick * AuthCmdError<string>>
    | RemoveFromDraftCmdResult of result : Result<UserDraftPick, UserDraftPick * AuthCmdError<string>>

type ChatProjectionMsg =
    | ChatMessagesDeltaMsg of deltaRvn : Rvn * delta : Delta<ChatMessageId, ChatMessageDto> * hasMoreChatMessages : bool

type ServerChatMsg =
    | InitializeChatProjectionQryResult of result : Result<ChatMessageDto list * bool, AuthQryError<string>>
    | MoreChatMessagesQryResult of result : Result<Rvn * ChatMessageDto list * bool, AuthQryError<string>>
    | SendChatMessageCmdResult of result : Result<unit, AuthCmdError<string>>
    | ChatProjectionMsg of chatProjectionMsg : ChatProjectionMsg

type ServerMsg =
    | Waff
    | ServerAppMsg of serverAppMsg : ServerAppMsg
    | ServerUserAdminMsg of serverUserAdminMsg : ServerUserAdminMsg
    | ServerDraftAdminMsg of serverDraftAdminMsg : ServerDraftAdminMsg
    | ServerNewsMsg of serverNewsMsg : ServerNewsMsg
    | ServerSquadsMsg of serverSquadsMsg : ServerSquadsMsg
    | ServerFixturesMsg of serverFixturesMsg : ServerFixturesMsg
    | ServerDraftsMsg of serverDraftsMsg : ServerDraftsMsg
    | ServerChatMsg of serverChatMsg : ServerChatMsg

let otherError source errorText = ifDebugSource source errorText |> OtherError |> Error
let otherCmdError source errorText = ifDebugSource source errorText |> OtherError |> OtherAuthCmdError |> Error
let otherQryError source errorText = ifDebugSource source errorText |> OtherError |> OtherAuthQryError |> Error
