module Aornota.Sweepstake2026.Ui.Program.State

open Aornota.Sweepstake2026.Common.Delta
open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Json
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.JsonConverter
open Aornota.Sweepstake2026.Ui.Common.LocalStorage
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Common.TimestampHelper
open Aornota.Sweepstake2026.Ui.Common.Toasts
open Aornota.Sweepstake2026.Ui.Pages
open Aornota.Sweepstake2026.Ui.Pages.Chat.Common
open Aornota.Sweepstake2026.Ui.Pages.DraftAdmin.Common
open Aornota.Sweepstake2026.Ui.Pages.Drafts.Common
open Aornota.Sweepstake2026.Ui.Pages.Fixtures.Common
open Aornota.Sweepstake2026.Ui.Pages.News.Common
open Aornota.Sweepstake2026.Ui.Pages.Scores.Common
open Aornota.Sweepstake2026.Ui.Pages.Squads.Common
open Aornota.Sweepstake2026.Ui.Pages.UserAdmin.Common
open Aornota.Sweepstake2026.Ui.Program.Common
open Aornota.Sweepstake2026.Ui.Shared
open Aornota.Sweepstake2026.Ui.Theme.Common
open Aornota.Sweepstake2026.Ui.Theme.Shared

open System

open Browser
open Browser.Types

open Elmish

let [<Literal>] private APP_PREFERENCES_KEY = "sweepstake-2019-ui-app-preferences"

let [<Literal>] private WIFF_INTERVAL = 30.<second>

let [<Literal>] private LAST_ACTIVITY_THROTTLE = 10.<second>

let private setBodyClass useDefaultTheme = document.body.className <- getThemeClass (getTheme useDefaultTheme).ThemeClass

let private readPreferencesCmd =
    let readPreferences () = async {
        (* TEMP-NMB...
        do! ifDebugSleepAsync 20 100 *)
        return Key APP_PREFERENCES_KEY |> readJson |> Option.map (fun (Json json) -> json |> fromJson<Preferences>) }
    Cmd.OfAsync.either readPreferences () (Ok >> ReadingPreferencesInput >> AppInput) (Error >> ReadingPreferencesInput >> AppInput)

let private writePreferencesCmd state =
    let writePreferences uiState = async {
        let lastPage =
            match uiState.AppState with
            | Unauth unauthState -> UnauthPage unauthState.CurrentUnauthPage |> Some
            | Auth authState -> authState.CurrentPage |> Some
            | ReadingPreferences | Connecting _ | ServiceUnavailable | AutomaticallySigningIn _ -> None
        let user =
            match uiState.AppState with
            | Auth authState -> (authState.AuthUser.UserName, authState.AuthUser.Jwt) |> Some
            | ReadingPreferences | Connecting _ | ServiceUnavailable | AutomaticallySigningIn _ | Unauth _ -> None
        let preferences = { UseDefaultTheme = uiState.UseDefaultTheme ; SessionId = uiState.SessionId ; LastPage = lastPage ; User = user }
        do preferences |> toJson |> Json |> writeJson (Key APP_PREFERENCES_KEY) }
    Cmd.OfAsync.either writePreferences state (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)

let private sendMsg (ws:WebSocket) (uiMsg:UiMsg) =
    if ws.readyState <> WebSocketState.OPEN then uiMsg |> SendMsgWsNotOpenError |> WsError |> Cmd.ofMsg
    else
        try
            ifDebugFakeErrorFailWith "Fake sendMsg error"
            uiMsg |> toJson |> ws.send
            Cmd.none
        with exn -> (uiMsg, exn.Message) |> SendMsgOtherError |> WsError |> Cmd.ofMsg

let private sendUnauthMsgCmd connectionState uiUnauthMsg =
    match connectionState with
    | Connected connectedState ->
        uiUnauthMsg |> UiUnauthMsg |> sendMsg connectedState.Ws
    | NotConnected | InitializingConnection _ ->
        shouldNeverHappenText "sendUnauthMsgCmd called when ConnectionState is not Connected" |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private sendAuthMsgCmd connectionState authState uiAuthMsg =
    match connectionState with
    | Connected connectedState ->
        let authState = { authState with LastUserActivity = DateTimeOffset.UtcNow }
        authState, (authState.AuthUser.Jwt, uiAuthMsg) |> UiAuthMsg |> sendMsg connectedState.Ws
    | NotConnected | InitializingConnection _ ->
        authState, shouldNeverHappenText "sendAuthMsgCmd called when ConnectionState is not Connected" |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private addNotificationMessage notificationMessage state = { state with NotificationMessages = notificationMessage :: state.NotificationMessages }
let private removeNotificationMessage notificationId state = // note: silently ignore unknown notificationId
    { state with NotificationMessages = state.NotificationMessages |> removeNotificationMessage notificationId }

let private addDebugMessage debugText state = state |> addNotificationMessage (debugText |> debugDismissableMessage)
let private addInfoMessage infoText state = state |> addNotificationMessage (infoText |> infoDismissableMessage)
let private addWarningMessage warningText state = state |> addNotificationMessage (warningText |> warningDismissableMessage)
let private addDangerMessage dangerText state = state |> addNotificationMessage (dangerText |> dangerDismissableMessage)

let private shouldNeverHappen debugText state : State * Cmd<Input> = state |> addDebugMessage (shouldNeverHappenText debugText), Cmd.none

let private addDebugError debugText toastText state : State * Cmd<Input> =
    state |> addDebugMessage (sprintf "ERROR -> %s" debugText), match toastText with | Some toastText -> toastText |> errorToastCmd | None -> Cmd.none

let private addError errorText state = state |> addDangerMessage errorText

let defaultSignInState userName signInStatus = {
    UserNameKey = Guid.NewGuid ()
    UserNameText = match userName with | Some userName -> userName | None -> String.Empty
    UserNameErrorText = None
    PasswordKey = Guid.NewGuid ()
    PasswordText = String.Empty
    PasswordErrorText = None
    FocusPassword = match userName with | Some _ -> true | None -> false
    SignInStatus = signInStatus }

let private defaultUnauthState currentUnauthPage (unauthPageStates:UnauthPageStates option) (unauthProjections:UnauthProjections option) signInState state =
    let currentPage = match currentUnauthPage with | Some currentPage -> currentPage | None -> NewsPage
    let newsState, newsCmd =
        match unauthPageStates with
        | Some unauthPageStates -> unauthPageStates.NewsState, Cmd.none
        | None -> News.State.initialize (currentPage = NewsPage) true None
    let scoresState, scoresCmd =
        match unauthPageStates with
        | Some unauthPageStates -> unauthPageStates.ScoresState, Cmd.none
        | None -> Scores.State.initialize None
    let squadsState, squadsCmd =
        match unauthPageStates with
        | Some unauthPageStates ->
            let squadsState = unauthPageStates.SquadsState
            let pendingPicksState = { PendingPicks = [] ; PendingRvn = None }
            { squadsState with PendingPicksState = pendingPicksState }, Cmd.none
        | None -> Squads.State.initialize ()
    let fixturesState, fixturesCmd = match unauthPageStates with | Some unauthPageStates -> unauthPageStates.FixturesState, Cmd.none | None -> Fixtures.State.initialize ()
    let usersProjection =
        match unauthProjections with
        | Some unauthProjections ->
            match unauthProjections.UsersProjection with
            | Ready (rvn, userDic) ->
                let copiedUserDic = UserDic ()
                userDic |> List.ofSeq |> List.iter (fun (KeyValue (userId, (userName, _))) -> (userId, (userName, None)) |> copiedUserDic.Add)
                (rvn, copiedUserDic) |> Ready
            | Pending | Failed -> Pending
        | None -> Pending
    let squadsProjection, squadsProjectionCmd =
        match unauthProjections with
        | Some unauthProjections -> unauthProjections.SquadsProjection, Cmd.none
        | None -> Pending, InitializeSquadsProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
    let fixturesProjection, fixturesProjectionCmd =
        match unauthProjections with
        | Some unauthProjections -> unauthProjections.FixturesProjection, Cmd.none
        | None -> Pending, InitializeFixturesProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
    let unauthState = {
        CurrentUnauthPage = currentPage
        UnauthPageStates = { NewsState = newsState ; ScoresState = scoresState ; SquadsState = squadsState ; FixturesState = fixturesState }
        UnauthProjections = { UsersProjection = usersProjection ; SquadsProjection = squadsProjection ; FixturesProjection = fixturesProjection }
        SignInState = signInState }
    let usersProjectionUnauthCmd = InitializeUsersProjectionUnauthQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
    let newsCmd = newsCmd |> Cmd.map (NewsInput >> UnauthPageInput >> UnauthInput >> AppInput)
    let scoresCmd = scoresCmd |> Cmd.map (ScoresInput >> UnauthPageInput >> UnauthInput >> AppInput)
    let squadsCmd = squadsCmd |> Cmd.map (SquadsInput >> UnauthPageInput >> UnauthInput >> AppInput)
    let fixturesCmd = fixturesCmd |> Cmd.map (FixturesInput >> UnauthPageInput >> UnauthInput >> AppInput)
    let cmd = Cmd.batch [ usersProjectionUnauthCmd ; scoresCmd ; squadsProjectionCmd ; fixturesProjectionCmd ; newsCmd ; squadsCmd ; fixturesCmd ]
    { state with AppState = Unauth unauthState }, cmd
let private defaultChangePasswordState mustChangePasswordReason changePasswordStatus = {
    MustChangePasswordReason = mustChangePasswordReason
    NewPasswordKey = Guid.NewGuid ()
    NewPasswordText = String.Empty
    NewPasswordErrorText = None
    ConfirmPasswordKey = Guid.NewGuid ()
    ConfirmPasswordText = String.Empty
    ConfirmPasswordErrorText = None
    ChangePasswordStatus = changePasswordStatus }

let private defaultAuthState (authUser:AuthUser) currentPage (unauthPageStates:UnauthPageStates option) (unauthProjections:UnauthProjections option) state =
    let currentPage = match currentPage with | Some currentPage -> currentPage | None -> AuthPage ChatPage
    let newsState, newsCmd =
        match unauthPageStates with
        | Some unauthPageStates -> unauthPageStates.NewsState, Cmd.none
        | None -> News.State.initialize (currentPage = UnauthPage NewsPage) true None
    let scoresState, scoresCmd =
        match unauthPageStates with
        | Some unauthPageStates ->
            let scoresCmd = if currentPage = UnauthPage ScoresPage then Cmd.none else authUser.UserId |> Some |> ShowSweepstaker |> Cmd.ofMsg
            unauthPageStates.ScoresState, scoresCmd
        | None -> Scores.State.initialize (authUser.UserId |> Some)
    let squadsState, squadsCmd = match unauthPageStates with | Some unauthPageStates -> unauthPageStates.SquadsState, Cmd.none | None -> Squads.State.initialize ()
    let fixturesState, fixturesCmd = match unauthPageStates with | Some unauthPageStates -> unauthPageStates.FixturesState, Cmd.none | None -> Fixtures.State.initialize ()
    let initializeUserAdminState = currentPage = AuthPage UserAdminPage
    let userAdminState, userAdminCmd =
        if initializeUserAdminState then
            let userAdminState, userAdminCmd = UserAdmin.State.initialize ()
            userAdminState |> Some, userAdminCmd
        else None, Cmd.none
    let initializeDraftAdminState = currentPage = AuthPage DraftAdminPage
    let draftAdminState, draftAdminCmd =
        if initializeDraftAdminState then
            let draftAdminState, draftAdminCmd = DraftAdmin.State.initialize authUser
            draftAdminState |> Some, draftAdminCmd
        else None, Cmd.none
    let draftsState, draftsCmd = Drafts.State.initialize ()
    let chatState, chatCmd = Chat.State.initialize authUser (currentPage = AuthPage ChatPage) true None
    let usersProjection = match unauthProjections with | Some unauthProjections -> unauthProjections.UsersProjection | None -> Pending
    let squadsProjection, squadsProjectionCmd =
        match unauthProjections with
        | Some unauthProjections -> unauthProjections.SquadsProjection, Cmd.none
        | None -> Pending, InitializeSquadsProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
    let fixturesProjection, fixturesProjectionCmd =
        match unauthProjections with
        | Some unauthProjections -> unauthProjections.FixturesProjection, Cmd.none
        | None -> Pending, InitializeFixturesProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
    let authState = {
        AuthUser = authUser
        LastUserActivity = DateTimeOffset.UtcNow
        CurrentPage = currentPage
        UnauthPageStates = { NewsState = newsState ; ScoresState = scoresState ; SquadsState = squadsState ; FixturesState = fixturesState }
        UnauthProjections = { UsersProjection = usersProjection ; SquadsProjection = squadsProjection ; FixturesProjection = fixturesProjection }
        AuthPageStates = { UserAdminState = userAdminState ; DraftAdminState = draftAdminState ; DraftsState = draftsState ; ChatState = chatState }
        AuthProjections = { DraftsProjection = Pending }
        ChangePasswordState =
            match authUser.MustChangePasswordReason with
            | Some mustChangePasswordReason -> defaultChangePasswordState (mustChangePasswordReason |> Some) None |> Some
            | None -> None
        SigningOut = false }
    let authState, usersProjectionAuthCmd = InitializeUsersProjectionAuthQry |> UiAuthAppMsg |> sendAuthMsgCmd state.ConnectionState authState
    let authState, draftsProjectionCmd =
        match authUser.Permissions.DraftPermission with
        | Some _ -> InitializeDraftsProjectionQry |> UiAuthAppMsg |> sendAuthMsgCmd state.ConnectionState authState
        | None -> authState, Cmd.none
    let newsCmd = newsCmd |> Cmd.map (NewsInput >> UnauthPageInput >> UnauthInput >> AppInput)
    let scoresCmd = scoresCmd |> Cmd.map (ScoresInput >> UnauthPageInput >> UnauthInput >> AppInput)
    let squadsCmd = squadsCmd |> Cmd.map (SquadsInput >> UnauthPageInput >> UnauthInput >> AppInput)
    let fixturesCmd = fixturesCmd |> Cmd.map (FixturesInput >> UnauthPageInput >> UnauthInput >> AppInput)
    let userAdminCmd = userAdminCmd |> Cmd.map (UserAdminInput >> APageInput >> PageInput >> AuthInput >> AppInput)
    let draftAdminCmd = draftAdminCmd |> Cmd.map (DraftAdminInput >> APageInput >> PageInput >> AuthInput >> AppInput)
    let draftsCmd = draftsCmd |> Cmd.map (DraftsInput >> APageInput >> PageInput >> AuthInput >> AppInput)
    let chatCmd = chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
    let cmd =
        Cmd.batch [
            usersProjectionAuthCmd ; squadsProjectionCmd ; fixturesProjectionCmd ; draftsProjectionCmd ; newsCmd ; scoresCmd ; squadsCmd ; fixturesCmd ; userAdminCmd ; draftAdminCmd
            draftsCmd ; chatCmd ]
    { state with AppState = Auth authState }, cmd

let initialize () =
    let state = {
        Ticks = 0<tick>
        LastWiff = DateTimeOffset.UtcNow
        NotificationMessages = []
        UseDefaultTheme = true
        SessionId = SessionId.Create ()
        NavbarBurgerIsActive = false
        StaticModal = None
        ConnectionState = NotConnected
        AppState = ReadingPreferences }
    setBodyClass state.UseDefaultTheme
    state, readPreferencesCmd

let private handleWsError wsError state : State * Cmd<Input> =
    match wsError, state.AppState with
    | WsOnError wsApiUrl, Connecting _ ->
        let state = { state with ConnectionState = NotConnected ; AppState = ServiceUnavailable }
        state |> addDebugError (sprintf "WsOnError when Connecting -> %s" wsApiUrl) ("Unable to create a connection to the web server<br><br>Please try again later" |> Some)
    | WsOnError wsApiUrl, _ ->
        state |> addDebugError (sprintf "WsOnError when not Connecting -> %s" wsApiUrl) (UNEXPECTED_ERROR |> Some)
    | SendMsgWsNotOpenError uiMsg, _ ->
        state |> addDebugError (sprintf "SendMsgWsNotOpenError -> %A" uiMsg) ("The connection to the web server has been closed<br><br>Please try refreshing the page" |> Some)
    | SendMsgOtherError (uiMsg, errorText), _ ->
        state |> addDebugError (sprintf "SendMsgOtherError -> %s -> %A" errorText uiMsg) (unexpectedErrorWhen "sending a message" |> Some)
    | DeserializeServerMsgError errorText, _ ->
        state |> addDebugError (sprintf "DeserializeServerMsgError -> %s" errorText) (unexpectedErrorWhen "processing a received message" |> Some)

let private handleServerUiMsgError serverUiMsgError state =
    match serverUiMsgError with
    | ReceiveUiMsgError errorText ->
        state |> addDebugError (sprintf "Server ReceiveUiMsgError -> %s" errorText) ("The web server was unable to receive a message<br><br>Please try refreshing the page" |> Some)
    | DeserializeUiMsgError errorText ->
        state |> addDebugError (sprintf "Server DeserializeUiMsgError -> %s" errorText) ("The web server was unable to process a message<br><br>Please try refreshing the page" |> Some)

let private handleConnected ws (serverStarted:DateTimeOffset) otherConnectionCount signedInUserCount user lastPage state =
    let toastCmd =
#if DEBUG
        let serverStarted = ago serverStarted.LocalDateTime
        let otherConnections = if otherConnectionCount > 0 then sprintf "<strong>%i</strong>" otherConnectionCount else sprintf "%i" otherConnectionCount
        let signedInUsers = if signedInUserCount > 0 then sprintf "<strong>%i</strong>" signedInUserCount else sprintf "%i" signedInUserCount
        sprintf "Server started: %s<br>Other web socket connections: %s<br>Signed in users: %s" serverStarted otherConnections signedInUsers |> infoToastCmd
#else
        Cmd.none
#endif
    let state = { state with ConnectionState = Connected { Ws = ws ; ServerStarted = serverStarted } }
    let state, cmd =
        match user with
        | Some (userName, jwt) ->
            let state = { state with AppState = ((userName, jwt), lastPage) |> AutomaticallySigningIn }
            let cmd = (state.SessionId, jwt) |> AutoSignInCmd |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
            state, cmd
        | None ->
            let lastPage = match lastPage with | Some (UnauthPage unauthPage) -> unauthPage |> Some | Some (AuthPage _) | None -> None
            let showSignInCmd = ifDebug Cmd.none (ShowSignInModal |> UnauthInput |> AppInput |> Cmd.ofMsg)
            let state, cmd = state |> defaultUnauthState lastPage None None None
            state, Cmd.batch [ cmd ; showSignInCmd ]
    state, Cmd.batch [ cmd ; toastCmd ]

let private handleSignInResult (result:Result<AuthUser,SignInCmdError<string>>) unauthState state =
    match unauthState.SignInState, result with
    | Some _, Ok authUser ->
        let currentPage = UnauthPage unauthState.CurrentUnauthPage |> Some
        let state, cmd = state |> defaultAuthState authUser currentPage (unauthState.UnauthPageStates |> Some) (unauthState.UnauthProjections |> Some)
        let writePreferencesCmd = match authUser.MustChangePasswordReason with | Some _ -> Cmd.none | None -> state |> writePreferencesCmd
        let (UserName userName) = authUser.UserName
        state, Cmd.batch [ cmd ; writePreferencesCmd ; sprintf "You have signed in as <strong>%s</strong>" userName |> successToastCmd ]
    | Some signInState, Error error ->
        let errorText =
            match error with
            | InvalidCredentials (Some errorText) -> errorText
            | InvalidCredentials None -> sprintf "Unable to sign in as %s" signInState.UserNameText
            | SignInCmdJwtError _ | OtherSignInCmdError _ -> unexpectedErrorWhen "signing in"
        let errorText = ifDebug (sprintf "SignInCmdResult error -> %A" error) errorText
        let signInState = { signInState with SignInStatus = errorText |> SignInFailed |> Some }
        { state with AppState = Unauth { unauthState with SignInState = signInState |> Some } }, Cmd.none
    | None, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected SignInCmdResult when SignInState is None -> %A" result)

let private handleAutoSignInResult (result:Result<AuthUser,AutoSignInCmdError<string>>) userName lastPage state =
    match result with
    | Ok authUser ->
        let state, cmd = state |> defaultAuthState authUser lastPage None None
        let (UserName userName) = authUser.UserName
        state, Cmd.batch [ cmd ; sprintf "You have been automatically signed in as <strong>%s</strong>" userName |> successToastCmd ]
    | Error error ->
        let (UserName userName) = userName
        let toastCmd = sprintf "Unable to automatically sign in as <strong>%s</strong>" userName |> errorToastCmd
        let errorText = ifDebug (sprintf "AutoSignInCmdResult error -> %A" error) (unexpectedErrorWhen "automatically signing in")
        let lastPage = match lastPage with | Some (UnauthPage unauthPage) -> unauthPage |> Some | Some (AuthPage _) | None -> None
        let signInState = defaultSignInState (userName |> Some) (errorText |> SignInFailed |> Some)
        let state, cmd = state |> defaultUnauthState lastPage None None (signInState |> Some)
        state, Cmd.batch [ cmd ; state |> writePreferencesCmd ; toastCmd ]

let private handleChangePasswordResult (result:Result<Rvn, AuthCmdError<string>>) authState state =
    match authState.ChangePasswordState with
    | Some changePasswordState ->
        match changePasswordState.ChangePasswordStatus, result with
        | Some ChangePasswordPending, Ok rvn ->
            let authUser = { authState.AuthUser with Rvn = rvn }
            let authState = { authState with AuthUser = authUser ; ChangePasswordState = None }
            { state with AppState = Auth authState }, "Your password has been changed" |> successToastCmd
        | Some ChangePasswordPending, Error error ->
            let errorText =
                match error with
                | OtherAuthCmdError (OtherError errorText) -> errorText
                | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> unexpectedErrorWhen "changing password"
            let errorText = ifDebug (sprintf "ChangePasswordCmdResult error -> %A" error) errorText
            let changePasswordState = { changePasswordState with ChangePasswordStatus = errorText |> ChangePasswordFailed |> Some }
            let authState = { authState with ChangePasswordState = changePasswordState |> Some }
            { state with AppState = Auth authState }, "Unable to change password" |> errorToastCmd
        | Some _, _ | None, _ ->
            state |> shouldNeverHappen (sprintf "Unexpected ChangePasswordCmdResult when ChangePasswordState is Some but not ChangePasswordPending -> %A" result)
    | None ->
        state |> shouldNeverHappen (sprintf "Unexpected ChangePasswordCmdResult when ChangePasswordState is None -> %A" result)

let private handleSignOutResult (result:Result<unit, AuthCmdError<string>>) authState state =
    let toastCmd = "You have signed out" |> successToastCmd
    match authState.SigningOut, result with
    | true, Ok _ ->
        let currentUnauthPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
        let state, cmd = state |> defaultUnauthState currentUnauthPage (authState.UnauthPageStates |> Some) (authState.UnauthProjections |> Some) None
        state, Cmd.batch [ cmd ; state |> writePreferencesCmd ; toastCmd ]
    | true, Error error ->
        let state, _ = ifDebug (state |> addDebugError (sprintf "SignOutCmdResult error -> %A" error) None) (state |> addError (unexpectedErrorWhen "signing out"), Cmd.none)
        let currentUnauthPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
        let state, cmd = state |> defaultUnauthState currentUnauthPage (authState.UnauthPageStates |> Some) (authState.UnauthProjections |> Some) None
        state, Cmd.batch [ cmd ; state |> writePreferencesCmd ; toastCmd ]
    | false, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected SignOutCmdResult when not SigningOut -> %A" result)

let private handleAutoSignOut autoSignOutReason authState state =
    let because reasonText = sprintf "You have been automatically signed out because %s" reasonText
    let state, toastCmd =
        match autoSignOutReason with
        | Some PasswordReset -> state |> addWarningMessage (because "your password has been reset by a system administrator"), warningToastCmd
        | Some (PermissionsChanged false) -> state |> addWarningMessage (because "your permissions have been changed by a system administrator"), warningToastCmd
        | Some (PermissionsChanged true) -> state |> addDangerMessage (because "you are no longer permitted to access this system"), errorToastCmd
        | None -> state, infoToastCmd
    let currentUnauthPage = match authState.CurrentPage with | UnauthPage unauthPage -> unauthPage |> Some | AuthPage _ -> None
    let state, cmd = state |> defaultUnauthState currentUnauthPage (authState.UnauthPageStates |> Some) (authState.UnauthProjections |> Some) None
    state, Cmd.batch [ cmd ; state |> writePreferencesCmd ; "You have been automatically signed out" |> toastCmd ]

let private user ((userUnauthDto, userAuthDto):UserDto) : User = userUnauthDto.UserName, userAuthDto

let private userDicUnauth (userUnauthDtos:UserUnauthDto list) =
    let userDic = UserDic ()
    userUnauthDtos |> List.iter (fun userUnauthDto ->
        let userId = userUnauthDto.UserId
        if userId |> userDic.ContainsKey |> not then // note: silently ignore duplicate userIds (should never happen)
            (userId, (userUnauthDto, None) |> user) |> userDic.Add)
    userDic

let private userDicAuth (userDtos:UserDto list) =
    let userDic = UserDic ()
    userDtos |> List.iter (fun (userUnauthDto, userAuthDto) ->
        let userId = userUnauthDto.UserId
        if userId |> userDic.ContainsKey |> not then // note: silently ignore duplicate userIds (should never happen)
            (userId, (userUnauthDto, userAuthDto) |> user) |> userDic.Add)
    userDic

let private applyUsersDeltaUnauth currentRvn deltaRvn (delta:Delta<UserId, UserUnauthDto>) (userDic:UserDic) =
    let userDic = UserDic userDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (userId, userUnauthDto) -> if userId |> userDic.ContainsKey then (userId, userUnauthDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (userId, userUnauthDto) -> (userId, (userUnauthDto, None) |> user) |> userDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (userId, userUnauthDto) -> if userId |> userDic.ContainsKey |> not then (userId, userUnauthDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (userId, userUnauthDto) -> userDic.[userId] <- ((userUnauthDto, None) |> user)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun userId -> if userId |> userDic.ContainsKey |> not then userId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (userDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> userDic |> Ok)

let private applyUsersDeltaAuth currentRvn deltaRvn (delta:Delta<UserId, UserDto>) (userDic:UserDic) =
    let userDic = UserDic userDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (userId, userDto) -> if userId |> userDic.ContainsKey then (userId, userDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (userId, userDto) -> (userId, userDto |> user) |> userDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (userId, userDto) -> if userId |> userDic.ContainsKey |> not then (userId, userDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (userId, userDto) -> userDic.[userId] <- (userDto |> user)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun userId -> if userId |> userDic.ContainsKey |> not then userId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (userDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> userDic |> Ok)

let private player (playerDto:PlayerDto) = { PlayerName = playerDto.PlayerName ; PlayerType = playerDto.PlayerType ; PlayerStatus = playerDto.PlayerStatus ; PickedBy = playerDto.PickedBy }

let private squad (squadDto:SquadDto) =
    let playerDic = PlayerDic ()
    squadDto.PlayerDtos |> List.iter (fun playerDto ->
        let playerId = playerDto.PlayerId
        if playerId |> playerDic.ContainsKey |> not then // note: silently ignore duplicate playerIds (should never happen)
            (playerId, playerDto |> player) |> playerDic.Add)
    let squadOnlyDto = squadDto.SquadOnlyDto
    { Rvn = squadOnlyDto.Rvn ; SquadName = squadOnlyDto.SquadName ; Group = squadOnlyDto.Group ; Seeding = squadOnlyDto.Seeding ; CoachName = squadOnlyDto.CoachName
      Eliminated = squadOnlyDto.Eliminated ; PlayerDic = playerDic ; PickedBy = squadOnlyDto.PickedBy }

let private updateSquad (squadOnlyDto:SquadOnlyDto) (squad:Squad) =
    { squad with Rvn = squadOnlyDto.Rvn ; SquadName = squadOnlyDto.SquadName ; Group = squadOnlyDto.Group ; Seeding = squadOnlyDto.Seeding ; CoachName = squadOnlyDto.CoachName
                 Eliminated = squadOnlyDto.Eliminated ; PlayerDic = squad.PlayerDic ; PickedBy = squadOnlyDto.PickedBy }

let private squadDic (squadDtos:SquadDto list) =
    let squadDic = SquadDic ()
    squadDtos |> List.iter (fun squadDto ->
        let squadId = squadDto.SquadOnlyDto.SquadId
        if squadId |> squadDic.ContainsKey |> not then // note: silently ignore duplicate squadIds (should never happen)
            (squadId, squadDto |> squad) |> squadDic.Add)
    squadDic

let private applySquadsDelta currentRvn deltaRvn (delta:Delta<SquadId, SquadOnlyDto>) (squadDic:SquadDic) =
    let squadDic = SquadDic squadDic // note: copy to ensure that passed-in dictionary *not* modified if error [but no need to copy PlayerDic/s since not changing those]
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (squadId, squadOnlyDto) -> if squadId |> squadDic.ContainsKey then (squadId, squadOnlyDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (squadId, squadOnlyDto) ->
            let squadDto = { SquadOnlyDto = squadOnlyDto ; PlayerDtos = [] }
            (squadId, squadDto |> squad) |> squadDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (squadId, squadOnlyDto) -> if squadId |> squadDic.ContainsKey |> not then (squadId, squadOnlyDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (squadId, squadOnlyDto) ->
            let squad = squadDic.[squadId]
            squadDic.[squadId] <- (squad |> updateSquad squadOnlyDto)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun squadId -> if squadId |> squadDic.ContainsKey |> not then squadId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (squadDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> squadDic |> Ok)

let private applyPlayersDelta currentRvn deltaRvn (delta:Delta<PlayerId, PlayerDto>) (playerDic:PlayerDic) =
    let playerDic = PlayerDic playerDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (playerId, playerDto) -> if playerId |> playerDic.ContainsKey then (playerId, playerDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (playerId, playerDto) -> (playerId, playerDto |> player) |> playerDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (playerId, playerDto) -> if playerId |> playerDic.ContainsKey |> not then (playerId, playerDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (playerId, playerDto) -> playerDic.[playerId] <- (playerDto |> player)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun playerId -> if playerId |> playerDic.ContainsKey |> not then playerId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (playerDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> playerDic |> Ok)

let private fixture (fixtureDto:FixtureDto) =
    { Rvn = fixtureDto.Rvn ; Stage = fixtureDto.Stage ; HomeParticipant = fixtureDto.HomeParticipant ; AwayParticipant = fixtureDto.AwayParticipant ; KickOff = fixtureDto.KickOff
      MatchResult = fixtureDto.MatchResult }

let private fixtureDic (fixtureDtos:FixtureDto list) =
    let fixtureDic = FixtureDic ()
    fixtureDtos |> List.iter (fun fixtureDto ->
        let fixtureId = fixtureDto.FixtureId
        if fixtureId |> fixtureDic.ContainsKey |> not then // note: silently ignore duplicate fixtureIds (should never happen)
            (fixtureId, fixtureDto |> fixture) |> fixtureDic.Add)
    fixtureDic

let private applyFixturesDelta currentRvn deltaRvn (delta:Delta<FixtureId, FixtureDto>) (fixtureDic:FixtureDic) =
    let fixtureDic = FixtureDic fixtureDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (fixtureId, fixtureDto) -> if fixtureId |> fixtureDic.ContainsKey then (fixtureId, fixtureDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (fixtureId, fixtureDto) -> (fixtureId, fixtureDto |> fixture) |> fixtureDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (fixtureId, fixtureDto) -> if fixtureId |> fixtureDic.ContainsKey |> not then (fixtureId, fixtureDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (fixtureId, fixtureDto) -> fixtureDic.[fixtureId] <- (fixtureDto |> fixture)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun fixtureId -> if fixtureId |> fixtureDic.ContainsKey |> not then fixtureId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (fixtureDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> fixtureDic |> Ok)

let private draft (draftDto:DraftDto) = { Rvn = draftDto.Rvn ; DraftOrdinal = draftDto.DraftOrdinal ; DraftStatus = draftDto.DraftStatus ; ProcessingDetails = draftDto.ProcessingDetails }

let private draftDic (draftDtos:DraftDto list) =
    let draftDic = DraftDic ()
    draftDtos |> List.iter (fun draftDto ->
        let draftId = draftDto.DraftId
        if draftId |> draftDic.ContainsKey |> not then // note: silently ignore duplicate draftIds (should never happen)
            (draftId, draftDto |> draft) |> draftDic.Add)
    draftDic

let private applyDraftsDelta currentRvn deltaRvn (delta:Delta<DraftId, DraftDto>) (draftDic:DraftDic) =
    let draftDic = DraftDic draftDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (draftId, draftDto) -> if draftId |> draftDic.ContainsKey then (draftId, draftDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (draftId, draftDto) -> (draftId, draftDto |> draft) |> draftDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (draftId, draftDto) -> if draftId |> draftDic.ContainsKey |> not then (draftId, draftDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (draftId, draftDto) -> draftDic.[draftId] <- (draftDto |> draft)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun draftId -> if draftId |> draftDic.ContainsKey |> not then draftId |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (draftDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> draftDic |> Ok)

let private handleServerAppMsg serverAppMsg state =
    match serverAppMsg, state.AppState, state.ConnectionState with
    | ServerUiMsgErrorMsg serverUiMsgError, _, _ ->
        state |> handleServerUiMsgError serverUiMsgError
    | ConnectedMsg (serverStarted, otherConnections, signedIn), Connecting (user, lastPage), InitializingConnection ws ->
        state |> handleConnected ws serverStarted otherConnections signedIn user lastPage
    | SignInCmdResult result, Unauth unauthState, Connected _ ->
        state |> handleSignInResult result unauthState
    | AutoSignInCmdResult result, AutomaticallySigningIn ((userName, _), lastPage), Connected _ ->
        state |> handleAutoSignInResult result userName lastPage
    | ChangePasswordCmdResult result, Auth authState, Connected _ ->
        state |> handleChangePasswordResult result authState
    | SignOutCmdResult result, Auth authState, Connected _ ->
        state |> handleSignOutResult result authState
    | AutoSignOutMsg reason, Auth authState, Connected _ ->
        state |> handleAutoSignOut reason authState
    | InitializeUsersProjectionUnauthQryResult result, Unauth unauthState, Connected _ ->
        let unauthProjections = unauthState.UnauthProjections
        match unauthProjections.UsersProjection with
        | Pending | Ready _ -> // note: allow Ready since could be re-initializing
            let state, usersProjection =
                match result with
                | Ok userUnauthDtos ->
                    state, (initialRvn, userUnauthDtos |> userDicUnauth) |> Ready
                | Error (OtherError errorText) ->
                    state |> addNotificationMessage (errorText |> dangerDismissableMessage), Failed
            let unauthProjections = { unauthProjections with UsersProjection = usersProjection }
            let unauthState = { unauthState with UnauthProjections = unauthProjections }
            { state with AppState = Unauth unauthState }, Cmd.none
        | Failed ->
            state |> shouldNeverHappen (sprintf "Unexpected InitializeUsersProjectionUnauthQryResult when not Pending or Ready -> %A" result)
    | InitializeUsersProjectionAuthQryResult result, Auth authState, Connected _ ->
        let unauthProjections = authState.UnauthProjections
        match unauthProjections.UsersProjection with
        | Pending | Ready _ -> // note: allow Ready since could be re-initializing
            let state, usersProjection =
                match result with
                | Ok userDtos ->
                    state, (initialRvn, userDtos |> userDicAuth) |> Ready
                | Error error ->
                    state |> addNotificationMessage (error |> qryErrorText |> dangerDismissableMessage), Failed
            let unauthProjections = { unauthProjections with UsersProjection = usersProjection }
            let authState = { authState with UnauthProjections = unauthProjections }
            { state with AppState = Auth authState }, Cmd.none
        | Failed ->
            state |> shouldNeverHappen (sprintf "Unexpected InitializeUsersProjectionAuthQryResult when not Pending or Ready -> %A" result)
    | UsersProjectionMsg (UsersDeltaUnauthMsg (deltaRvn, userUnauthDtoDelta)), Unauth unauthState, Connected _ ->
        let unauthProjections = unauthState.UnauthProjections
        match unauthProjections.UsersProjection with
        | Ready (rvn, userDic) ->
            match userDic |> applyUsersDeltaUnauth rvn deltaRvn userUnauthDtoDelta with
            | Ok userDic ->
                let unauthProjections = { unauthProjections with UsersProjection = (deltaRvn, userDic) |> Ready }
                let unauthState = { unauthState with UnauthProjections = unauthProjections }
                { state with AppState = Unauth unauthState }, Cmd.none
            | Error error ->
                let unauthProjections = { unauthProjections with UsersProjection = Pending }
                let unauthState = { unauthState with UnauthProjections = unauthProjections }
                let usersProjectionUnauthCmd = InitializeUsersProjectionUnauthQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
                let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" userUnauthDtoDelta userDic error)
                { state with AppState = Unauth unauthState }, Cmd.batch [ usersProjectionUnauthCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | Pending | Failed -> // note: silently ignore UsersDeltaUnauthMsg if not Ready
            state, Cmd.none
    | UsersProjectionMsg (UsersDeltaUnauthMsg _), Auth _, Connected _ -> // note: silently ignore UsersDeltaUnauthMsg if Auth
        state, Cmd.none
    | UsersProjectionMsg (UsersDeltaAuthMsg _), Unauth _, Connected _ -> // note: silently ignore UsersDeltaAuthMsg if Unauth
        state, Cmd.none
    | UsersProjectionMsg (UsersDeltaAuthMsg (deltaRvn, userDtoDelta)), Auth authState, Connected _ ->
        let unauthProjections = authState.UnauthProjections
        match unauthProjections.UsersProjection with
        | Ready (rvn, userDic) ->
            match userDic |> applyUsersDeltaAuth rvn deltaRvn userDtoDelta with
            | Ok userDic ->
                let authPageStates = authState.AuthPageStates
                let userAdminState =
                    match authPageStates.UserAdminState with
                    | Some userAdminState ->
                        let createUsersState =
                            match userAdminState.CreateUsersState with
                            | Some createUsersState ->
                                let newUserNameText = createUsersState.NewUserNameText
                                if String.IsNullOrWhiteSpace newUserNameText |> not then
                                    let newUserNameErrorText = validateUserName (userDic |> userNames) (UserName newUserNameText)
                                    { createUsersState with NewUserNameErrorText = newUserNameErrorText } |> Some
                                else createUsersState |> Some
                            | None -> None
                        { userAdminState with CreateUsersState = createUsersState } |> Some
                    | None -> None
                let authPageStates = { authPageStates with UserAdminState = userAdminState }
                let unauthProjections = { unauthProjections with UsersProjection = (deltaRvn, userDic) |> Ready }
                let authState = { authState with AuthPageStates = authPageStates ; UnauthProjections = unauthProjections }
                { state with AppState = Auth authState }, Cmd.none
            | Error error ->
                let unauthProjections = { unauthProjections with UsersProjection = Pending }
                let authState = { authState with UnauthProjections = unauthProjections }
                let authState, usersProjectionAuthCmd = InitializeUsersProjectionAuthQry |> UiAuthAppMsg |> sendAuthMsgCmd state.ConnectionState authState
                let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" userDtoDelta userDic error)
                { state with AppState = Auth authState }, Cmd.batch [ usersProjectionAuthCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | Pending | Failed -> // note: silently ignore UsersDeltaUnauthMsg if not Ready
            state, Cmd.none
    | UsersProjectionMsg (UserSignedInAuthMsg _), Unauth _, Connected _ -> // note: silently ignore UserSignedInAuthMsg if Unauth
        state, Cmd.none
    | UsersProjectionMsg (UserSignedInAuthMsg (UserName userName)), Auth authState, Connected _ ->
        state, if UserName userName = authState.AuthUser.UserName then Cmd.none else sprintf "<strong>%s</strong> has signed in" userName |> infoToastCmd
    | UsersProjectionMsg (UserSignedOutAuthMsg _), Unauth _, Connected _ -> // note: silently ignore UserSignedInAuthMsg if Unauth
        state, Cmd.none
    | UsersProjectionMsg (UserSignedOutAuthMsg (UserName userName)), Auth authState, Connected _ ->
        state, if UserName userName = authState.AuthUser.UserName then Cmd.none else sprintf "<strong>%s</strong> has signed out" userName |> infoToastCmd
    | InitializeSquadsProjectionQryResult result, Unauth unauthState, Connected _ ->
        let unauthProjections = unauthState.UnauthProjections
        match unauthProjections.SquadsProjection with
        | Pending ->
            let state, squadsProjection =
                match result with
                | Ok squadDtos ->
                    let squadDic = squadDtos |> squadDic
                    state, (initialRvn, squadDic) |> Ready
                | Error (OtherError errorText) ->
                    state |> addNotificationMessage (errorText |> dangerDismissableMessage), Failed
            let unauthProjections = { unauthProjections with SquadsProjection = squadsProjection }
            let unauthState = { unauthState with UnauthProjections = unauthProjections }
            { state with AppState = Unauth unauthState }, Cmd.none
        | Failed | Ready _ ->
            state |> shouldNeverHappen (sprintf "Unexpected InitializeSquadsProjectionQryResult when not Pending -> %A" result)
    | InitializeSquadsProjectionQryResult result, Auth authState, Connected _ ->
        let unauthProjections = authState.UnauthProjections
        match unauthProjections.SquadsProjection with
        | Pending ->
            let state, squadsProjection =
                match result with
                | Ok squadDtos ->
                    let squadDic = squadDtos |> squadDic
                    state, (initialRvn, squadDic) |> Ready
                | Error (OtherError errorText) ->
                    state |> addNotificationMessage (errorText |> dangerDismissableMessage), Failed
            let unauthProjections = { unauthProjections with SquadsProjection = squadsProjection }
            let authState = { authState with UnauthProjections = unauthProjections }
            { state with AppState = Auth authState }, Cmd.none
        | Failed | Ready _ ->
            state |> shouldNeverHappen (sprintf "Unexpected InitializeSquadsProjectionQryResult when not Pending -> %A" result)
    | SquadsProjectionMsg (SquadsDeltaMsg (deltaRvn, squadDtoDelta)), Unauth unauthState, Connected _ ->
        let unauthProjections = unauthState.UnauthProjections
        match unauthProjections.SquadsProjection with
        | Ready (rvn, squadDic) ->
            match squadDic |> applySquadsDelta rvn deltaRvn squadDtoDelta with
            | Ok squadDic ->
                let unauthProjections = { unauthProjections with SquadsProjection = (deltaRvn, squadDic) |> Ready }
                let unauthState = { unauthState with UnauthProjections = unauthProjections }
                { state with AppState = Unauth unauthState }, Cmd.none
            | Error error ->
                let unauthProjections = { unauthProjections with SquadsProjection = Pending }
                let unauthState = { unauthState with UnauthProjections = unauthProjections }
                let squadsProjectionCmd = InitializeSquadsProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
                let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" squadDtoDelta squadDic error)
                { state with AppState = Unauth unauthState }, Cmd.batch [ squadsProjectionCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | Pending | Failed -> // note: silently ignore SquadsDeltaMsg if not Ready
            state, Cmd.none
    | SquadsProjectionMsg (SquadsDeltaMsg (deltaRvn, squadDtoDelta)), Auth authState, Connected _ ->
        let unauthProjections = authState.UnauthProjections
        match unauthProjections.SquadsProjection with
        | Ready (rvn, squadDic) ->
            match squadDic |> applySquadsDelta rvn deltaRvn squadDtoDelta with
            | Ok squadDic ->
                let unauthProjections = { unauthProjections with SquadsProjection = (deltaRvn, squadDic) |> Ready }
                let authState = { authState with UnauthProjections = unauthProjections }
                { state with AppState = Auth authState }, Cmd.none
            | Error error ->
                let unauthProjections = { unauthProjections with SquadsProjection = Pending }
                let authState = { authState with UnauthProjections = unauthProjections }
                let squadsProjectionCmd = InitializeSquadsProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
                let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" squadDtoDelta squadDic error)
                { state with AppState = Auth authState }, Cmd.batch [ squadsProjectionCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | Pending | Failed -> // note: silently ignore SquadsDeltaMsg if not Ready
            state, Cmd.none
    | SquadsProjectionMsg (PlayersDeltaMsg (deltaRvn, squadId, squadRvn, playerDtoDelta)), Unauth unauthState, Connected _ ->
        let unauthProjections = unauthState.UnauthProjections
        match unauthProjections.SquadsProjection with
        | Ready (rvn, squadDic) ->
            let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
            match squad with
            | Some squad ->
                match squad.PlayerDic |> applyPlayersDelta rvn deltaRvn playerDtoDelta with
                | Ok playerDic ->
                    squadDic.[squadId] <- { squad with Rvn = squadRvn ; PlayerDic = playerDic }
                    let unauthProjections = { unauthProjections with SquadsProjection = (deltaRvn, squadDic) |> Ready }
                    let unauthState = { unauthState with UnauthProjections = unauthProjections }
                    { state with AppState = Unauth unauthState }, Cmd.none
                | Error error ->
                    let unauthProjections = { unauthProjections with SquadsProjection = Pending }
                    let unauthState = { unauthState with UnauthProjections = unauthProjections }
                    let squadsProjectionCmd = InitializeSquadsProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
                    let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" playerDtoDelta squad.PlayerDic error)
                    { state with AppState = Unauth unauthState }, Cmd.batch [ squadsProjectionCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
            | None -> // note: silently ignore unknown squadId (should never happen)
                state, Cmd.none
        | Pending | Failed -> // note: silently ignore SquadsDeltaMsg if not Ready
            state, Cmd.none
    | SquadsProjectionMsg (PlayersDeltaMsg (deltaRvn, squadId, squadRvn, playerDtoDelta)), Auth authState, Connected _ ->
        let unauthProjections = authState.UnauthProjections
        match unauthProjections.SquadsProjection with
        | Ready (rvn, squadDic) ->
            let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
            match squad with
            | Some squad ->
                match squad.PlayerDic |> applyPlayersDelta rvn deltaRvn playerDtoDelta with
                | Ok playerDic ->
                    squadDic.[squadId] <- { squad with Rvn = squadRvn ; PlayerDic = playerDic }
                    let unauthPageStates = authState.UnauthPageStates
                    let squadsState = unauthPageStates.SquadsState
                    let addPlayersState =
                        match squadsState.AddPlayersState with
                        | Some addPlayersState when addPlayersState.SquadId = squadId ->
                            let newPlayerNameText = addPlayersState.NewPlayerNameText
                            if String.IsNullOrWhiteSpace newPlayerNameText |> not then
                                let newPlayerNameErrorText = validatePlayerName (squad.PlayerDic |> playerNames) (PlayerName newPlayerNameText)
                                { addPlayersState with NewPlayerNameErrorText = newPlayerNameErrorText } |> Some
                            else addPlayersState |> Some
                        | Some _ | None -> None
                    let changePlayerNameState =
                        match squadsState.ChangePlayerNameState with
                        | Some changePlayerNameState when changePlayerNameState.SquadId = squadId ->
                            let playerNameText = changePlayerNameState.PlayerNameText
                            if String.IsNullOrWhiteSpace playerNameText |> not then
                                let playerNameErrorText = validatePlayerName (squad.PlayerDic |> playerNames) (PlayerName playerNameText)
                                { changePlayerNameState with PlayerNameErrorText = playerNameErrorText } |> Some
                            else changePlayerNameState |> Some
                        | Some _ | None -> None
                    let squadsState = { squadsState with AddPlayersState = addPlayersState ; ChangePlayerNameState = changePlayerNameState }
                    let unauthPageStates = { unauthPageStates with SquadsState = squadsState }
                    let unauthProjections = { unauthProjections with SquadsProjection = (deltaRvn, squadDic) |> Ready }
                    let authState = { authState with UnauthPageStates = unauthPageStates ; UnauthProjections = unauthProjections }
                    { state with AppState = Auth authState }, Cmd.none
                | Error error ->
                    let unauthProjections = { unauthProjections with SquadsProjection = Pending }
                    let authState = { authState with UnauthProjections = unauthProjections }
                    let squadsProjectionCmd = InitializeSquadsProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
                    let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" playerDtoDelta squad.PlayerDic error)
                    { state with AppState = Auth authState }, Cmd.batch [ squadsProjectionCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
            | None -> // note: silently ignore unknown squadId (should never happen)
                state, Cmd.none
        | Pending | Failed -> // note: silently ignore SquadsDeltaMsg if not Ready
            state, Cmd.none
    | InitializeFixturesProjectionQryResult result, Unauth unauthState, Connected _ ->
        let unauthProjections = unauthState.UnauthProjections
        match unauthProjections.FixturesProjection with
        | Pending ->
            let state, fixturesProjection =
                match result with
                | Ok fixtureDtos ->
                    let fixtureDic = fixtureDtos |> fixtureDic
                    state, (initialRvn, fixtureDic) |> Ready
                | Error (OtherError errorText) ->
                    state |> addNotificationMessage (errorText |> dangerDismissableMessage), Failed
            let unauthProjections = { unauthProjections with FixturesProjection = fixturesProjection }
            let unauthState = { unauthState with UnauthProjections = unauthProjections }
            { state with AppState = Unauth unauthState }, Cmd.none
        | Failed | Ready _ ->
            state |> shouldNeverHappen (sprintf "Unexpected InitializeFixturesProjectionQryResult when not Pending -> %A" result)
    | InitializeFixturesProjectionQryResult result, Auth authState, Connected _ ->
        let unauthProjections = authState.UnauthProjections
        match unauthProjections.FixturesProjection with
        | Pending ->
            let state, fixturesProjection =
                match result with
                | Ok fixtureDtos ->
                    let fixtureDic = fixtureDtos |> fixtureDic
                    state, (initialRvn, fixtureDic) |> Ready
                | Error (OtherError errorText) ->
                    state |> addNotificationMessage (errorText |> dangerDismissableMessage), Failed
            let unauthProjections = { unauthProjections with FixturesProjection = fixturesProjection }
            let authState = { authState with UnauthProjections = unauthProjections }
            { state with AppState = Auth authState }, Cmd.none
        | Failed | Ready _ ->
            state |> shouldNeverHappen (sprintf "Unexpected InitializeFixturesProjectionQryResult when not Pending -> %A" result)
    | FixturesProjectionMsg (FixturesDeltaMsg (deltaRvn, fixtureDtoDelta)), Unauth unauthState, Connected _ ->
        let unauthProjections = unauthState.UnauthProjections
        match unauthProjections.FixturesProjection with
        | Ready (rvn, fixtureDic) ->
            match fixtureDic |> applyFixturesDelta rvn deltaRvn fixtureDtoDelta with
            | Ok fixtureDic ->
                let unauthProjections = { unauthProjections with FixturesProjection = (deltaRvn, fixtureDic) |> Ready }
                let unauthState = { unauthState with UnauthProjections = unauthProjections }
                { state with AppState = Unauth unauthState }, Cmd.none
            | Error error ->
                let unauthProjections = { unauthProjections with FixturesProjection = Pending }
                let unauthState = { unauthState with UnauthProjections = unauthProjections }
                let fixturesProjectionCmd = InitializeFixturesProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
                let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" fixtureDtoDelta fixtureDic error)
                { state with AppState = Unauth unauthState }, Cmd.batch [ fixturesProjectionCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | Pending | Failed -> // note: silently ignore FixturesDeltaMsg if not Ready
            state, Cmd.none
    | FixturesProjectionMsg (FixturesDeltaMsg (deltaRvn, fixtureDtoDelta)), Auth authState, Connected _ ->
        let unauthProjections = authState.UnauthProjections
        match unauthProjections.FixturesProjection with
        | Ready (rvn, fixtureDic) ->
            match fixtureDic |> applyFixturesDelta rvn deltaRvn fixtureDtoDelta with
            | Ok fixtureDic ->
                let unauthProjections = { unauthProjections with FixturesProjection = (deltaRvn, fixtureDic) |> Ready }
                let authState = { authState with UnauthProjections = unauthProjections }
                { state with AppState = Auth authState }, Cmd.none
            | Error error ->
                let unauthProjections = { unauthProjections with FixturesProjection = Pending }
                let authState = { authState with UnauthProjections = unauthProjections }
                let fixturesProjectionCmd = InitializeFixturesProjectionQry |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
                let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" fixtureDtoDelta fixtureDic error)
                { state with AppState = Auth authState }, Cmd.batch [ fixturesProjectionCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | Pending | Failed -> // note: silently ignore FixturesDeltaMsg if not Ready
            state, Cmd.none
    | InitializeDraftsProjectionQryResult _, Unauth _, Connected _ -> // note: silently ignore InitializeDraftsProjectionQryResult if Unauth
        state, Cmd.none
    | InitializeDraftsProjectionQryResult result, Auth authState, Connected _ ->
        let authProjections = authState.AuthProjections
        match authProjections.DraftsProjection with
        | Pending ->
            let state, draftsProjection =
                match result with
                | Ok (draftDtos, currentUserDraftDto) ->
                    let draftDic = draftDtos |> draftDic
                    state, (initialRvn, draftDic, currentUserDraftDto) |> Ready
                | Error error ->
                    state |> addNotificationMessage (error |> qryErrorText |> dangerDismissableMessage), Failed
            let authProjections = { authProjections with DraftsProjection = draftsProjection }
            let authState = { authState with AuthProjections = authProjections }
            { state with AppState = Auth authState }, Cmd.none
        | Failed | Ready _ ->
            state |> shouldNeverHappen (sprintf "Unexpected InitializeDraftsProjectionQryResult when not Pending -> %A" result)
    | DraftsProjectionMsg (DraftsDeltaMsg _), Unauth _, Connected _ -> // note: silently ignore DraftsDeltaMsg if Unauth
        state, Cmd.none
    | DraftsProjectionMsg (DraftsDeltaMsg (deltaRvn, draftDtoDelta)), Auth authState, Connected _ ->
        let authProjections = authState.AuthProjections
        match authProjections.DraftsProjection with
        | Ready (rvn, draftDic, currentUserDraftDto) ->
            match draftDic |> applyDraftsDelta rvn deltaRvn draftDtoDelta with
            | Ok draftDic ->
                let authProjections = { authProjections with DraftsProjection = (deltaRvn, draftDic, currentUserDraftDto) |> Ready }
                let authState = { authState with AuthProjections = authProjections }
                { state with AppState = Auth authState }, Cmd.none
            | Error error ->
                let authProjections = { authProjections with DraftsProjection = Pending }
                let authState = { authState with AuthProjections = authProjections }
                let authState, draftsProjectionCmd = InitializeDraftsProjectionQry |> UiAuthAppMsg |> sendAuthMsgCmd state.ConnectionState authState
                let state, shouldNeverHappenCmd = state |> shouldNeverHappen (sprintf "Unable to apply %A to %A -> %A" draftDtoDelta fixtureDic error)
                { state with AppState = Auth authState }, Cmd.batch [ draftsProjectionCmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
        | Pending | Failed -> // note: silently ignore DraftsDeltaMsg if not Ready
            state, Cmd.none
    | DraftsProjectionMsg (CurrentUserDraftDtoChangedMsg _), Unauth _, Connected _ -> // note: silently ignore CurrentUserDraftDtoChangedMsg if Unauth
        state, Cmd.none
    | DraftsProjectionMsg (CurrentUserDraftDtoChangedMsg (changeRvn, currentUserDraftDto)), Auth authState, Connected _ ->
        let authProjections = authState.AuthProjections
        match authProjections.DraftsProjection with
        | Ready (_, draftDic, _) ->
            let unauthPageStates = authState.UnauthPageStates
            let squadsState = unauthPageStates.SquadsState
            let pendingPicksState = squadsState.PendingPicksState
            let pendingPicks = pendingPicksState.PendingPicks
            let pendingPicksState =
                match currentUserDraftDto with
                | Some currentUserDraftDto ->
                    let userDraftPicks = currentUserDraftDto.UserDraftPickDtos |> List.map (fun userDraftPickDto -> userDraftPickDto.UserDraftPick)
                    let pendingPicks = pendingPicks |> List.filter (fun pendingPick ->
                        if pendingPick |> Squads.Common.isAdding && userDraftPicks |> List.contains pendingPick.UserDraftPick then false
                        else (pendingPick |> Squads.Common.isRemoving && userDraftPicks |> List.contains pendingPick.UserDraftPick |> not) |> not)
                    { pendingPicksState with PendingPicks = pendingPicks }
                | None -> { PendingPicks = [] ; PendingRvn = None }
            let squadsState = { squadsState with PendingPicksState = pendingPicksState }
            let authPageStates = authState.AuthPageStates
            let draftsState = authPageStates.DraftsState
            let removalPending = draftsState.RemovalPending
            let removalPending =
                match currentUserDraftDto, removalPending with
                | Some currentUserDraftDto, Some (_, Rvn pendingRvn) ->
                    let (Rvn rvn) = currentUserDraftDto.Rvn
                    if pendingRvn <= rvn then None else removalPending
                | _ -> removalPending
            let changePriorityPending, lastPriorityChanged = draftsState.ChangePriorityPending, draftsState.LastPriorityChanged
            let changePriorityPending, lastPriorityChanged =
                match currentUserDraftDto, changePriorityPending with
                | Some currentUserDraftDto, Some (userDraftPick, priorityChange, Rvn pendingRvn) ->
                    let (Rvn rvn) = currentUserDraftDto.Rvn
                    if pendingRvn <= rvn then None, (userDraftPick, priorityChange) |> Some else changePriorityPending, lastPriorityChanged
                | _ -> changePriorityPending, lastPriorityChanged
            let draftsState = { draftsState with RemovalPending = removalPending ; ChangePriorityPending = changePriorityPending ; LastPriorityChanged = lastPriorityChanged }
            let unauthPageStates = { unauthPageStates with SquadsState = squadsState }
            let authPageStates = { authPageStates with DraftsState = draftsState }
            let authProjections = { authProjections with DraftsProjection = (changeRvn, draftDic, currentUserDraftDto) |> Ready }
            let authState = { authState with UnauthPageStates = unauthPageStates ; AuthPageStates = authPageStates ; AuthProjections = authProjections }
            { state with AppState = Auth authState }, Cmd.none
        | Pending | Failed -> // note: silently ignore CurrentUserDraftDtoChangedMsg if not Ready
            state, Cmd.none
    | _, _, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected ServerAppMsg when %A (%A) -> %A" state.AppState state.ConnectionState serverAppMsg)

let private handleServerMsg serverMsg state =
    match serverMsg, state.AppState with
    | Waff, _ -> // note: silently ignored
        state, Cmd.none
    | ServerAppMsg serverAppMsg, _ ->
        state |> handleServerAppMsg serverAppMsg
    | ServerNewsMsg (InitializeNewsProjectionQryResult result), Unauth _ ->
        state, result |> InitializeNewsProjectionQryResult |> ReceiveServerNewsMsg |> NewsInput |> UnauthPageInput |> UnauthInput |> AppInput |> Cmd.ofMsg
    | ServerNewsMsg (MorePostsQryResult result), Unauth _ ->
        state, result |> MorePostsQryResult |> ReceiveServerNewsMsg |> NewsInput |> UnauthPageInput |> UnauthInput |> AppInput |> Cmd.ofMsg
    | ServerNewsMsg (NewsProjectionMsg newsProjectionMsg), Unauth _ ->
        state, newsProjectionMsg |> NewsProjectionMsg |> ReceiveServerNewsMsg |> NewsInput |> UnauthPageInput |> UnauthInput |> AppInput |> Cmd.ofMsg
    | ServerNewsMsg _, Unauth _ -> // note: silently ignore other ServerNewsMsg/s if Unauth
        state, Cmd.none
    | ServerNewsMsg serverNewsMsg, Auth _ ->
        state, serverNewsMsg |> ReceiveServerNewsMsg |> NewsInput |> UPageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | ServerSquadsMsg _, Unauth _ -> // note: silently ignore ServerSquadsMsg/s if Unauth
        state, Cmd.none
    | ServerSquadsMsg serverSquadsMsg, Auth _ ->
        state, serverSquadsMsg |> ReceiveServerSquadsMsg |> SquadsInput |> UPageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | ServerFixturesMsg _, Unauth _ -> // note: silently ignore ServerFixturesMsg/s if Unauth
        state, Cmd.none
    | ServerFixturesMsg serverFixturesMsg, Auth _ ->
        state, serverFixturesMsg |> ReceiveServerFixturesMsg |> FixturesInput |> UPageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | ServerUserAdminMsg _, Unauth _ -> // note: silently ignore ServerUserAdminMsg if Unauth
        state, Cmd.none
    | ServerUserAdminMsg serverUserAdminMsg, Auth _ ->
        state, serverUserAdminMsg |> ReceiveServerUserAdminMsg |> UserAdminInput |> APageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | ServerDraftAdminMsg _, Unauth _ -> // note: silently ignore ServerDraftAdminMsg if Unauth
        state, Cmd.none
    | ServerDraftAdminMsg serverDraftAdminMsg, Auth _ ->
        state, serverDraftAdminMsg |> ReceiveServerDraftAdminMsg |> DraftAdminInput |> APageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | ServerDraftsMsg _, Unauth _ -> // note: silently ignore ServerDraftsMsg if Unauth
        state, Cmd.none
    | ServerDraftsMsg serverDraftsMsg, Auth _ ->
        state, serverDraftsMsg |> ReceiveServerDraftsMsg |> DraftsInput |> APageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | ServerChatMsg _, Unauth _ -> // note: silently ignore ServerChatMsg if Unauth
        state, Cmd.none
    | ServerChatMsg serverChatMsg, Auth _ ->
        state, serverChatMsg |> ReceiveServerChatMsg |> ChatInput |> APageInput |> PageInput |> AuthInput |> AppInput |> Cmd.ofMsg
    | _, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected ServerMsg when %A -> %A" state.AppState serverMsg)

let private handleReadingPreferencesInput (result:Result<Preferences option, exn>) (state:State) =
    match result with
    | Ok (Some preferences) ->
        let state = { state with UseDefaultTheme = preferences.UseDefaultTheme ; SessionId = preferences.SessionId }
        setBodyClass state.UseDefaultTheme
        { state with AppState = (preferences.User, preferences.LastPage) |> Connecting }, Cmd.none
    | Ok None ->
        { state with AppState = (None, None) |> Connecting }, Cmd.none
    | Error exn ->
        let state, _ = state |> addDebugError (sprintf "ReadPreferencesResult -> %s" exn.Message) None // note: no need for toast
        state, None |> Ok |> ReadingPreferencesInput |> AppInput |> Cmd.ofMsg

let private handleConnectingInput ws state : State * Cmd<Input> = { state with ConnectionState = InitializingConnection ws }, Cmd.none

let private handleUnauthInput unauthInput (unauthState:UnauthState) state =
    match unauthInput, unauthState.SignInState with
    | ShowUnauthPage unauthPage, _ ->
        if unauthState.CurrentUnauthPage <> unauthPage then
            // TODO-ONGOING: Initialize other "optional" pages (if required) - and toggle "IsCurrent" for other relevant pages...
            let newsCmd =
                if unauthPage <> NewsPage && unauthState.CurrentUnauthPage = NewsPage then false |> ToggleNewsIsCurrentPage |> Cmd.ofMsg
                else if unauthPage = NewsPage && unauthState.CurrentUnauthPage <> NewsPage then true |> ToggleNewsIsCurrentPage |> Cmd.ofMsg
                else Cmd.none
            let unauthState = { unauthState with CurrentUnauthPage = unauthPage }
            let newsCmd = newsCmd |> Cmd.map (NewsInput >> UnauthPageInput >> UnauthInput >> AppInput)
            let state = { state with AppState = Unauth unauthState }
            state, Cmd.batch [ newsCmd ; state |> writePreferencesCmd ]
        else state, Cmd.none
    | UnauthPageInput (NewsInput (News.Common.AddNotificationMessage notificationMessage)), _ ->
        state |> addNotificationMessage notificationMessage, Cmd.none
    | UnauthPageInput (NewsInput News.Common.ShowMarkdownSyntaxModal), _ ->
        state |> shouldNeverHappen "Unexpected NewsInput ShowMarkdownSyntaxModal when Unauth"
    | UnauthPageInput (NewsInput (News.Common.SendUiUnauthMsg uiUnauthMsg)), _ ->
        let cmd = uiUnauthMsg |> sendUnauthMsgCmd state.ConnectionState
        state, cmd
    | UnauthPageInput (NewsInput (News.Common.SendUiAuthMsg _)), _ ->
        state |> shouldNeverHappen "Unexpected NewsInput SendUiAuthMsg when Unauth"
    | UnauthPageInput (NewsInput newsInput), _ ->
        let newsState = unauthState.UnauthPageStates.NewsState
        let newsState, newsCmd, _ = newsState |> News.State.transition newsInput
        let unauthPageStates = { unauthState.UnauthPageStates with NewsState = newsState }
        let newsCmd = newsCmd |> Cmd.map (NewsInput >> UnauthPageInput >> UnauthInput >> AppInput)
        { state with AppState = Unauth { unauthState with UnauthPageStates = unauthPageStates } }, newsCmd
    | UnauthPageInput (ScoresInput scoresInput), _ ->
        let scoresState = unauthState.UnauthPageStates.ScoresState
        let scoresState, scoresCmd, _ = scoresState |> Scores.State.transition scoresInput
        let unauthPageStates = { unauthState.UnauthPageStates with ScoresState = scoresState }
        let scoresCmd = scoresCmd |> Cmd.map (ScoresInput >> UnauthPageInput >> UnauthInput >> AppInput)
        { state with AppState = Unauth { unauthState with UnauthPageStates = unauthPageStates } }, scoresCmd
    | UnauthPageInput (SquadsInput (Squads.Common.AddNotificationMessage notificationMessage)), _ ->
        state |> addNotificationMessage notificationMessage, Cmd.none
    | UnauthPageInput (SquadsInput (Squads.Common.SendUiAuthMsg _)), _ ->
        state |> shouldNeverHappen "Unexpected SquadsInput SendUiAuthMsg when Unauth"
    | UnauthPageInput (SquadsInput squadsInput), _ ->
        let squadsState = unauthState.UnauthPageStates.SquadsState
        let squadsState, squadsCmd, _ = squadsState |> Squads.State.transition squadsInput None unauthState.UnauthProjections.SquadsProjection None None
        let unauthPageStates = { unauthState.UnauthPageStates with SquadsState = squadsState }
        let squadsCmd = squadsCmd |> Cmd.map (SquadsInput >> UnauthPageInput >> UnauthInput >> AppInput)
        { state with AppState = Unauth { unauthState with UnauthPageStates = unauthPageStates } }, squadsCmd
    | UnauthPageInput (FixturesInput (Fixtures.Common.AddNotificationMessage notificationMessage)), _ ->
        state |> addNotificationMessage notificationMessage, Cmd.none
    | UnauthPageInput (FixturesInput (Fixtures.Common.SendUiAuthMsg _)), _ ->
        state |> shouldNeverHappen "Unexpected FixturesInput SendUiAuthMsg when Unauth"
    | UnauthPageInput (FixturesInput fixturesInput), _ ->
        let fixturesState = unauthState.UnauthPageStates.FixturesState
        let fixturesState, fixturesCmd, _ =
            fixturesState |> Fixtures.State.transition fixturesInput unauthState.UnauthProjections.FixturesProjection unauthState.UnauthProjections.SquadsProjection
        let unauthPageStates = { unauthState.UnauthPageStates with FixturesState = fixturesState }
        let fixturesCmd = fixturesCmd |> Cmd.map (FixturesInput >> UnauthPageInput >> UnauthInput >> AppInput)
        { state with AppState = Unauth { unauthState with UnauthPageStates = unauthPageStates } }, fixturesCmd
    | ShowSignInModal, None ->
        let unauthState = { unauthState with SignInState = defaultSignInState None None |> Some }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput (UserNameTextChanged userNameText), Some signInState ->
        let signInState = { signInState with UserNameText = userNameText ; UserNameErrorText = validateUserName [] (UserName userNameText) }
        let unauthState = { unauthState with SignInState = signInState |> Some }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput (PasswordTextChanged passwordText), Some signInState ->
        let signInState = { signInState with PasswordText = passwordText ; PasswordErrorText = validatePassword (Password passwordText) }
        let unauthState = { unauthState with SignInState = signInState |> Some }
        { state with AppState = Unauth unauthState }, Cmd.none
    | SignInInput SignIn, Some signInState -> // note: assume no need to validate UserNameText or PasswordText (i.e. because App.Render.renderSignInModal will ensure that SignIn can only be dispatched when valid)
        let signInState = { signInState with SignInStatus = SignInPending |> Some }
        let unauthState = { unauthState with SignInState = signInState |> Some }
        let signInCmdParams = state.SessionId, UserName (signInState.UserNameText.Trim ()), Password (signInState.PasswordText.Trim ())
        let cmd = signInCmdParams |> SignInCmd |> UiUnauthAppMsg |> sendUnauthMsgCmd state.ConnectionState
        { state with AppState = Unauth unauthState }, cmd
    | SignInInput CancelSignIn, Some signInState ->
        match signInState.SignInStatus with
        | Some SignInPending -> state |> shouldNeverHappen "Unexpected CancelSignIn when SignInPending"
        | Some _ | None ->
            let unauthState = { unauthState with SignInState = None }
            { state with AppState = Unauth unauthState }, Cmd.none
    | _, _ ->
        state |> shouldNeverHappen (sprintf "Unexpected UnauthInput when SignIsState is %A -> %A" unauthState.SignInState unauthInput)

let private handleAuthInput authInput authState state =
    match authInput, authState.ChangePasswordState, authState.SigningOut with
    | ShowPage page, None, false ->
        if authState.CurrentPage <> page then
            match page, authState.AuthUser.Permissions.UserAdminPermissions, page, authState.AuthUser.Permissions.DraftAdminPermissions with
            | AuthPage UserAdminPage, None, _, _ -> // note: would expect "Permissions mismatch" AutoSignInCmdResult error instead
                let state, cmd = state |> shouldNeverHappen "Unexpected ShowPage UserAdminPage when UserAdminPermissions is None"
                state, cmd, false
            | _, _, AuthPage DraftAdminPage, None -> // note: would expect "Permissions mismatch" AutoSignInCmdResult error instead
                let state, cmd = state |> shouldNeverHappen "Unexpected ShowPage DraftAdminPage when DraftAdminPermissions is None"
                state, cmd, false
            | _ ->
                // TODO-ONGOING: Initialize other "optional" pages (if required) - and toggle "IsCurrent" for other relevant pages...
                let newsCmd =
                    if page <> UnauthPage NewsPage && authState.CurrentPage = UnauthPage NewsPage then false |> ToggleNewsIsCurrentPage |> Cmd.ofMsg
                    else if page = UnauthPage NewsPage && authState.CurrentPage <> UnauthPage NewsPage then true |> ToggleNewsIsCurrentPage |> Cmd.ofMsg
                    else Cmd.none
                let userAdminState, userAdminCmd =
                    match page, authState.AuthPageStates.UserAdminState with
                    | AuthPage UserAdminPage, None ->
                        let userAdminState, userAdminCmd = UserAdmin.State.initialize ()
                        userAdminState |> Some, userAdminCmd
                    | _, _ -> authState.AuthPageStates.UserAdminState, Cmd.none
                let draftAdminState, draftAdminCmd =
                    match page, authState.AuthPageStates.DraftAdminState with
                    | AuthPage DraftAdminPage, None ->
                        let draftAdminState, draftAdminCmd = DraftAdmin.State.initialize authState.AuthUser
                        draftAdminState |> Some, draftAdminCmd
                    | _, _ -> authState.AuthPageStates.DraftAdminState, Cmd.none
                let chatCmd =
                    if page <> AuthPage ChatPage && authState.CurrentPage = AuthPage ChatPage then false |> ToggleChatIsCurrentPage |> Cmd.ofMsg
                    else if page = AuthPage ChatPage && authState.CurrentPage <> AuthPage ChatPage then true |> ToggleChatIsCurrentPage |> Cmd.ofMsg
                    else Cmd.none
                let authPageStates = { authState.AuthPageStates with UserAdminState = userAdminState ; DraftAdminState = draftAdminState }
                let authState = { authState with CurrentPage = page ; AuthPageStates = authPageStates }
                let newsCmd = newsCmd |> Cmd.map (NewsInput >> UPageInput >> PageInput >> AuthInput >> AppInput)
                let userAdminCmd = userAdminCmd |> Cmd.map (UserAdminInput >> APageInput >> PageInput >> AuthInput >> AppInput)
                let draftAdminCmd = draftAdminCmd |> Cmd.map (DraftAdminInput >> APageInput >> PageInput >> AuthInput >> AppInput)
                let chatCmd = chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
                let state = { state with AppState = Auth authState }
                state, Cmd.batch [ newsCmd ; userAdminCmd ; draftAdminCmd ; chatCmd ; state |> writePreferencesCmd ], true
        else state, Cmd.none, true
    | PageInput (UPageInput (NewsInput (News.Common.AddNotificationMessage notificationMessage))), _, false ->
        state |> addNotificationMessage notificationMessage, Cmd.none, false
    | PageInput (UPageInput (NewsInput News.Common.ShowMarkdownSyntaxModal)), None, false ->
        { state with StaticModal = MarkdownSyntax |> Some }, Cmd.none, true
    | PageInput (UPageInput (NewsInput (News.Common.SendUiUnauthMsg uiUnauthMsg))), _, false ->
        let cmd = uiUnauthMsg |> sendUnauthMsgCmd state.ConnectionState
        state, cmd, false
    | PageInput (UPageInput (NewsInput (News.Common.SendUiAuthMsg uiAuthMsg))), _, false ->
        let authState, cmd = uiAuthMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | PageInput (UPageInput (NewsInput newsInput)), _, _ ->
        let newsState = authState.UnauthPageStates.NewsState
        let newsState, newsCmd, isUserNonApiActivity = newsState |> News.State.transition newsInput
        let unauthPageStates = { authState.UnauthPageStates with NewsState = newsState }
        let newsCmd = newsCmd |> Cmd.map (NewsInput >> UPageInput >> PageInput >> AuthInput >> AppInput)
        { state with AppState = Auth { authState with UnauthPageStates = unauthPageStates } }, newsCmd, isUserNonApiActivity
    | PageInput (UPageInput (ScoresInput scoresInput)), _, _ ->
        let scoresState = authState.UnauthPageStates.ScoresState
        let scoresState, scoresCmd, isUserNonApiActivity = scoresState |> Scores.State.transition scoresInput
        let unauthPageStates = { authState.UnauthPageStates with ScoresState = scoresState }
        let scoresCmd = scoresCmd |> Cmd.map (ScoresInput >> UPageInput >> PageInput >> AuthInput >> AppInput)
        { state with AppState = Auth { authState with UnauthPageStates = unauthPageStates } }, scoresCmd, isUserNonApiActivity
    | PageInput (UPageInput (SquadsInput (Squads.Common.AddNotificationMessage notificationMessage))), _, false ->
        state |> addNotificationMessage notificationMessage, Cmd.none, false
    | PageInput (UPageInput (SquadsInput (Squads.Common.SendUiAuthMsg uiAuthMsg))), _, false ->
        let authState, cmd = uiAuthMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | PageInput (UPageInput (SquadsInput squadsInput)), _, _ ->
        let squadsState = authState.UnauthPageStates.SquadsState
        let squadsState, squadsCmd, isUserNonApiActivity =
            let draftDic, currentUserDraftDto =
                match authState.AuthProjections.DraftsProjection with
                | Ready (_, draftDic, currentUserDraftDto) -> draftDic |> Some, currentUserDraftDto
                | Pending | Failed -> None, None
            squadsState |> Squads.State.transition squadsInput (authState.AuthUser |> Some) authState.UnauthProjections.SquadsProjection draftDic currentUserDraftDto
        let unauthPageStates = { authState.UnauthPageStates with SquadsState = squadsState }
        let squadsCmd = squadsCmd |> Cmd.map (SquadsInput >> UPageInput >> PageInput >> AuthInput >> AppInput)
        { state with AppState = Auth { authState with UnauthPageStates = unauthPageStates } }, squadsCmd, isUserNonApiActivity
    | PageInput (UPageInput (FixturesInput (Fixtures.Common.AddNotificationMessage notificationMessage))), _, false ->
        state |> addNotificationMessage notificationMessage, Cmd.none, false
    | PageInput (UPageInput (FixturesInput (Fixtures.Common.SendUiAuthMsg uiAuthMsg))), _, false ->
        let authState, cmd = uiAuthMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | PageInput (UPageInput (FixturesInput fixturesInput)), _, _ ->
        let fixturesState = authState.UnauthPageStates.FixturesState
        let fixturesState, fixturesCmd, isUserNonApiActivity =
            fixturesState |> Fixtures.State.transition fixturesInput authState.UnauthProjections.FixturesProjection authState.UnauthProjections.SquadsProjection
        let unauthPageStates = { authState.UnauthPageStates with FixturesState = fixturesState }
        let fixturesCmd = fixturesCmd |> Cmd.map (FixturesInput >> UPageInput >> PageInput >> AuthInput >> AppInput)
        { state with AppState = Auth { authState with UnauthPageStates = unauthPageStates } }, fixturesCmd, isUserNonApiActivity
    | PageInput (APageInput (UserAdminInput (UserAdmin.Common.AddNotificationMessage notificationMessage))), _, false ->
        state |> addNotificationMessage notificationMessage, Cmd.none, false
    | PageInput (APageInput (UserAdminInput (UserAdmin.Common.SendUiAuthMsg uiAuthMsg))), _, false ->
        let authState, cmd = uiAuthMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | PageInput (APageInput (UserAdminInput userAdminInput)), _, _ ->
        match authState.AuthPageStates.UserAdminState with
        | Some userAdminState ->
            let userAdminState, userAdminCmd, isUserNonApiActivity = userAdminState |> UserAdmin.State.transition userAdminInput authState.UnauthProjections.UsersProjection
            let authPageStates = { authState.AuthPageStates with UserAdminState = userAdminState |> Some }
            let userAdminCmd = userAdminCmd |> Cmd.map (UserAdminInput >> APageInput >> PageInput >> AuthInput >> AppInput)
            { state with AppState = Auth { authState with AuthPageStates = authPageStates } }, userAdminCmd, isUserNonApiActivity
        | None ->
            let state, cmd = state |> shouldNeverHappen "Unexpected UserAdminInput when AuthPageStates.UserAdminState is None"
            state, cmd, false
    | PageInput (APageInput (DraftAdminInput (DraftAdmin.Common.AddNotificationMessage notificationMessage))), _, false ->
        state |> addNotificationMessage notificationMessage, Cmd.none, false
    | PageInput (APageInput (DraftAdminInput (DraftAdmin.Common.SendUiAuthMsg uiAuthMsg))), _, false ->
        let authState, cmd = uiAuthMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | PageInput (APageInput (DraftAdminInput draftAdminInput)), _, _ ->
        match authState.AuthPageStates.DraftAdminState with
        | Some draftAdminState ->
            let draftAdminState, draftAdminCmd, isUserNonApiActivity = draftAdminState |> DraftAdmin.State.transition draftAdminInput authState.AuthUser authState.AuthProjections.DraftsProjection
            let authPageStates = { authState.AuthPageStates with DraftAdminState = draftAdminState |> Some }
            let draftAdminCmd = draftAdminCmd |> Cmd.map (DraftAdminInput >> APageInput >> PageInput >> AuthInput >> AppInput)
            { state with AppState = Auth { authState with AuthPageStates = authPageStates } }, draftAdminCmd, isUserNonApiActivity
        | None ->
            let state, cmd = state |> shouldNeverHappen "Unexpected DraftAdminInput when AuthPageStates.DraftAdminState is None"
            state, cmd, false
    | PageInput (APageInput (DraftsInput (Drafts.Common.AddNotificationMessage notificationMessage))), _, false ->
        state |> addNotificationMessage notificationMessage, Cmd.none, false
    | PageInput (APageInput (DraftsInput (Drafts.Common.SendUiAuthMsg uiAuthMsg))), _, false ->
        let authState, cmd = uiAuthMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | PageInput (APageInput (DraftsInput draftsInput)), _, _ ->
        let draftsState = authState.AuthPageStates.DraftsState
        let draftsState, draftsCmd, isUserNonApiActivity =
            let currentUserDraftDto =
                match authState.AuthProjections.DraftsProjection with
                | Ready (_, _, currentUserDraftDto) -> currentUserDraftDto
                | Pending | Failed -> None
            draftsState |> Drafts.State.transition draftsInput (authState.AuthUser |> Some) authState.UnauthProjections.SquadsProjection currentUserDraftDto
        let authPageStates = { authState.AuthPageStates with DraftsState = draftsState }
        let draftsCmd = draftsCmd |> Cmd.map (DraftsInput >> APageInput >> PageInput >> AuthInput >> AppInput)
        { state with AppState = Auth { authState with AuthPageStates = authPageStates } }, draftsCmd, isUserNonApiActivity
    | PageInput (APageInput (ChatInput (Chat.Common.AddNotificationMessage notificationMessage))), _, false ->
        state |> addNotificationMessage notificationMessage, Cmd.none, false
    | PageInput (APageInput (ChatInput Chat.Common.ShowMarkdownSyntaxModal)), None, false ->
        { state with StaticModal = MarkdownSyntax |> Some }, Cmd.none, true
    | PageInput (APageInput (ChatInput (Chat.Common.SendUiAuthMsg uiAuthMsg))), _, false ->
        let authState, cmd = uiAuthMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | PageInput (APageInput (ChatInput chatInput)), _, _ ->
        let chatState, chatCmd, isUserNonApiActivity = authState.AuthPageStates.ChatState |> Chat.State.transition chatInput
        let authPageStates = { authState.AuthPageStates with ChatState = chatState }
        let chatCmd = chatCmd |> Cmd.map (ChatInput >> APageInput >> PageInput >> AuthInput >> AppInput)
        { state with AppState = Auth { authState with AuthPageStates = authPageStates } }, chatCmd, isUserNonApiActivity
    | ShowChangePasswordModal, None, false ->
        let authState = { authState with ChangePasswordState = defaultChangePasswordState None None |> Some }
        { state with AppState = Auth authState }, Cmd.none, true
    | ChangePasswordInput (NewPasswordTextChanged newPasswordText), Some changePasswordState, false ->
        let newPasswordErrorText = validatePassword (Password newPasswordText)
        let confirmPasswordErrorText =
            if String.IsNullOrWhiteSpace changePasswordState.ConfirmPasswordText then changePasswordState.ConfirmPasswordErrorText
            else validateConfirmPassword (Password newPasswordText) (Password changePasswordState.ConfirmPasswordText)
        let changePasswordState = { changePasswordState with NewPasswordText = newPasswordText ; NewPasswordErrorText = newPasswordErrorText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        let authState = { authState with ChangePasswordState = changePasswordState |> Some }
        { state with AppState = Auth authState }, Cmd.none, true
    | ChangePasswordInput (ConfirmPasswordTextChanged confirmPasswordText), Some changePasswordState, false ->
        let confirmPasswordErrorText = validateConfirmPassword (Password changePasswordState.NewPasswordText) (Password confirmPasswordText)
        let changePasswordState = { changePasswordState with ConfirmPasswordText = confirmPasswordText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        let authState = { authState with ChangePasswordState = changePasswordState |> Some }
        { state with AppState = Auth authState }, Cmd.none, true
    | ChangePasswordInput ChangePassword, Some changePasswordState, false -> // note: assume no need to validate NewPasswordText or ConfirmPasswordText (i.e. because App.Render.renderChangePasswordModal will ensure that ChangePassword can only be dispatched when valid)
        let changePasswordState = { changePasswordState with ChangePasswordStatus = ChangePasswordPending |> Some }
        let authState = { authState with ChangePasswordState = changePasswordState |> Some }
        let changePasswordCmdParams = authState.AuthUser.Rvn, Password (changePasswordState.NewPasswordText.Trim ())
        let authState, cmd = changePasswordCmdParams |> ChangePasswordCmd |> UiAuthAppMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth authState }, cmd, false
    | ChangePasswordInput CancelChangePassword, Some changePasswordState, false ->
        match changePasswordState.MustChangePasswordReason, changePasswordState.ChangePasswordStatus with
        | Some _, _ ->
            let authState = { authState with ChangePasswordState = None }
            { state with AppState = Auth authState }, SignOut |> AuthInput |> AppInput |> Cmd.ofMsg, true
        | None, Some ChangePasswordPending ->
            let state, cmd = state |> shouldNeverHappen "Unexpected CancelChangePassword when ChangePasswordPending"
            state, cmd, false
        | None, Some _ | None, None ->
            let authState = { authState with ChangePasswordState = None }
            { state with AppState = Auth authState }, Cmd.none, true
    | SignOut, None, false ->
        let authState, cmd = SignOutCmd |> UiAuthAppMsg |> sendAuthMsgCmd state.ConnectionState authState
        { state with AppState = Auth { authState with SigningOut = true } }, cmd, false
    | _, _, false ->
        let state, cmd = state |> shouldNeverHappen (sprintf "Unexpected AuthInput when not SigningOut and ChangePasswordState is %A -> %A" authState.ChangePasswordState authInput)
        state, cmd, false
    | _, _, true ->
        let state, cmd = state |> shouldNeverHappen (sprintf "Unexpected AuthInput when SigningOut and ChangePasswordState is %A -> %A" authState.ChangePasswordState authInput)
        state, cmd, false

let private handleAppInput appInput state =
    match appInput, state.AppState with
    | ReadingPreferencesInput result, ReadingPreferences ->
        let state, cmd = state |> handleReadingPreferencesInput result
        state, cmd, false
    | ConnectingInput ws, Connecting _ ->
        let state, cmd = state |> handleConnectingInput ws
        state, cmd, false
    | UnauthInput unauthInput, Unauth unauthState ->
        let state, cmd = state |> handleUnauthInput unauthInput unauthState
        state, cmd, false
    | UnauthInput (UnauthPageInput unauthInput), Auth authState ->
        state |> handleAuthInput (unauthInput |> UPageInput |> PageInput) authState
    | AuthInput authInput, Auth authState ->
        state |> handleAuthInput authInput authState
    | _, _ ->
        let state, cmd = state |> shouldNeverHappen (sprintf "Unexpected AppInput when %A -> %A" state.AppState appInput)
        state, cmd, false

let transition input state =
    let state, cmd, isUserNonApiActivity =
        match input with
        | Tick ->
            // Note: Only sending Wiff messages to server to see if this resolves issue with WebSocket "timeouts" for MS Edge (only seen with Azure, not dev-server).
            let lastWiff, cmd =
                match state.ConnectionState with
                | Connected connectedState ->
                    let now = DateTimeOffset.UtcNow
                    if (now.DateTime - state.LastWiff.DateTime).TotalSeconds * 1.<second> >= WIFF_INTERVAL then now, Wiff |> sendMsg connectedState.Ws
                    else state.LastWiff, Cmd.none
                | NotConnected | InitializingConnection _ -> state.LastWiff, Cmd.none
            { state with Ticks = state.Ticks + 1<tick> ; LastWiff = lastWiff }, cmd, false
        | AddNotificationMessage notificationMessage ->
            state |> addNotificationMessage notificationMessage, Cmd.none, false
        | DismissNotificationMessage notificationId ->
            state |> removeNotificationMessage notificationId, Cmd.none, true
        | ToggleTheme ->
            let state = { state with UseDefaultTheme = (state.UseDefaultTheme |> not) }
            setBodyClass state.UseDefaultTheme
            state, state |> writePreferencesCmd, true
        | ToggleNavbarBurger ->
            { state with NavbarBurgerIsActive = (state.NavbarBurgerIsActive |> not) }, Cmd.none, true
        | ShowStaticModal staticModal ->
            { state with StaticModal = staticModal |> Some }, Cmd.none, true
        | HideStaticModal ->
            { state with StaticModal = None }, Cmd.none, true
        | WritePreferencesResult (Ok _) ->
            state, Cmd.none, false
        | WritePreferencesResult (Error exn) -> // note: no need for toast
            let state, cmd = state |> addDebugError (sprintf "WritePreferencesResult -> %s" exn.Message) None
            state, cmd, false
        | WsError wsError ->
            let state, cmd = state |> handleWsError wsError
            state, cmd, false
        | HandleServerMsg serverMsg ->
            let state, cmd = state |> handleServerMsg serverMsg
            state, cmd, false
        | AppInput appInput ->
            state |> handleAppInput appInput
    let appState, userNonApiActivityCmd =
        match state.AppState, isUserNonApiActivity with
        | Auth authState, true ->
            let authState, userNonApiActivityCmd =
                if (DateTimeOffset.UtcNow - authState.LastUserActivity).TotalSeconds * 1.<second> < LAST_ACTIVITY_THROTTLE then authState, Cmd.none
                else UserNonApiActivity |> sendAuthMsgCmd state.ConnectionState authState // note: updates authState.LastUserActivity
            Auth authState, userNonApiActivityCmd
        | _, _ ->
            state.AppState, Cmd.none
    { state with AppState = appState }, Cmd.batch [ cmd ; userNonApiActivityCmd ]
