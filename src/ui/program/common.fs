module Aornota.Sweepstake2026.Ui.Program.Common

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Pages
open Aornota.Sweepstake2026.Ui.Shared

open System

open Browser.Types

type UnauthPage =
    | NewsPage
    | ScoresPage
    | SquadsPage
    | FixturesPage

type AuthPage =
    | UserAdminPage
    | DraftAdminPage
    | DraftsPage
    | ChatPage

type Page =
    | UnauthPage of unauthPage : UnauthPage
    | AuthPage of authPage: AuthPage

type Preferences = {
    UseDefaultTheme : bool
    SessionId : SessionId
    LastPage : Page option
    User : (UserName * Jwt) option }

type StaticModal =
    | ScoringSystem
    | DraftAlgorithm
    | Payouts
    | MarkdownSyntax

type WsError =
    | WsOnError of wsApiUrl : string
    | SendMsgWsNotOpenError of uiMsg : UiMsg
    | SendMsgOtherError of uiMsg : UiMsg * errorText : string
    | DeserializeServerMsgError of errorText : string

type UnauthPageInput =
    | NewsInput of newsInput : News.Common.Input
    | ScoresInput of scoresInput: Scores.Common.Input
    | SquadsInput of squadsInput : Squads.Common.Input
    | FixturesInput of fixturesInput : Fixtures.Common.Input

type SignInInput =
    | UserNameTextChanged of userNameText : string
    | PasswordTextChanged of passwordText : string
    | SignIn
    | CancelSignIn

type UnauthInput =
    | ShowUnauthPage of unauthPage : UnauthPage
    | UnauthPageInput of unauthPageInput : UnauthPageInput
    | ShowSignInModal
    | SignInInput of signInInput : SignInInput

type AuthPageInput =
    | UserAdminInput of userAdminInput : UserAdmin.Common.Input
    | DraftAdminInput of draftAdminInput : DraftAdmin.Common.Input
    | DraftsInput of draftsInput : Drafts.Common.Input
    | ChatInput of chatInput : Chat.Common.Input

type PageInput =
    | UPageInput of unauthPageInput : UnauthPageInput
    | APageInput of authPageInput : AuthPageInput

type ChangePasswordInput =
    | NewPasswordTextChanged of newPasswordText : string
    | ConfirmPasswordTextChanged of confirmPasswordText : string
    | ChangePassword
    | CancelChangePassword

type AuthInput =
    | ShowPage of page : Page
    | PageInput of pageInput : PageInput
    | ShowChangePasswordModal
    | ChangePasswordInput of changePasswordInput : ChangePasswordInput
    | SignOut

type AppInput =
    | ReadingPreferencesInput of result : Result<Preferences option, exn>
    | ConnectingInput of ws : WebSocket
    | UnauthInput of unauthInput : UnauthInput
    | AuthInput of authInput : AuthInput

type Input =
    | Tick
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | DismissNotificationMessage of notificationId : NotificationId
    | ToggleTheme
    | ToggleNavbarBurger
    | ShowStaticModal of staticModal : StaticModal
    | HideStaticModal
    | WritePreferencesResult of result : Result<unit, exn>
    | WsError of wsError : WsError
    | HandleServerMsg of serverMsg : ServerMsg
    | AppInput of appInput : AppInput

type ConnectedState = {
    Ws : WebSocket // TODO-NMB-MEDIUM: Switch to using Fable.Websockets.Elmish?...
    ServerStarted : DateTimeOffset }

type ConnectionState =
    | NotConnected
    | InitializingConnection of ws : WebSocket
    | Connected of connectedState : ConnectedState

type SignInStatus =
    | SignInPending
    | SignInFailed of errorText : string

type SignInState = {
    UserNameKey : Guid
    UserNameText : string
    UserNameErrorText : string option
    PasswordKey : Guid
    PasswordText : string
    PasswordErrorText : string option
    FocusPassword : bool
    SignInStatus : SignInStatus option }

type UnauthPageStates = {
    NewsState : News.Common.State
    ScoresState : Scores.Common.State
    SquadsState : Squads.Common.State
    FixturesState : Fixtures.Common.State }

type UnauthProjections = {
    UsersProjection : Projection<Rvn * UserDic>
    SquadsProjection : Projection<Rvn * SquadDic>
    FixturesProjection : Projection<Rvn * FixtureDic> }

type UnauthState = {
    CurrentUnauthPage : UnauthPage
    UnauthPageStates : UnauthPageStates
    UnauthProjections : UnauthProjections
    SignInState : SignInState option }

type ChangePasswordStatus =
    | ChangePasswordPending
    | ChangePasswordFailed of errorText : string

type ChangePasswordState = {
    MustChangePasswordReason : MustChangePasswordReason option
    NewPasswordKey : Guid
    NewPasswordText : string
    NewPasswordErrorText : string option
    ConfirmPasswordKey : Guid
    ConfirmPasswordText : string
    ConfirmPasswordErrorText : string option
    ChangePasswordStatus : ChangePasswordStatus option }

type AuthPageStates = {
    UserAdminState : UserAdmin.Common.State option
    DraftAdminState : DraftAdmin.Common.State option
    DraftsState : Drafts.Common.State
    ChatState : Chat.Common.State }

type AuthProjections = { DraftsProjection : Projection<Rvn * DraftDic * CurrentUserDraftDto option> }

type AuthState = {
    AuthUser : AuthUser
    LastUserActivity : DateTimeOffset
    CurrentPage : Page
    UnauthPageStates : UnauthPageStates
    UnauthProjections : UnauthProjections
    AuthPageStates : AuthPageStates
    AuthProjections : AuthProjections
    ChangePasswordState : ChangePasswordState option
    SigningOut : bool }

type AppState =
    | ReadingPreferences
    | Connecting of user : (UserName * Jwt) option * lastPage : Page option
    | ServiceUnavailable
    | AutomaticallySigningIn of user : (UserName * Jwt) * lastPage : Page option
    | Unauth of unauthState : UnauthState
    | Auth of authState : AuthState

type State = {
    Ticks : int<tick> // note: will only be updated when TICK is defined (see webpack.config.js)
    LastWiff : DateTimeOffset // note: will only be updated when TICK is defined
    NotificationMessages : NotificationMessage list
    UseDefaultTheme : bool
    SessionId : SessionId
    NavbarBurgerIsActive : bool
    StaticModal : StaticModal option
    ConnectionState : ConnectionState
    AppState : AppState }

// (pre-α) | α | β | γ | δ | ε | ζ | η | θ | ι | κ | λ | μ | ν | ξ | ο | π | ρ | σ | τ | υ | φ | χ | ψ | ω
let [<Literal>] SWEEPSTAKE_2026 = "sweepstake 2026 (α)"
