module Aornota.Sweepstake2026.Ui.Pages.UserAdmin.State

open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Common.Toasts
open Aornota.Sweepstake2026.Ui.Pages.UserAdmin.Common
open Aornota.Sweepstake2026.Ui.Shared

open System

open Elmish

let initialize () = { CreateUsersState = None ; ResetPasswordState = None ; ChangeUserTypeState = None }, Cmd.none

let private userRvnOrInitial (userDic:UserDic) userId =
    if userId |> userDic.ContainsKey then
        let _, userAuthDto = userDic.[userId]
        match userAuthDto with | Some userAuthDto -> userAuthDto.Rvn | None -> initialRvn
    else initialRvn

let private defaultCreateUsersState userTypes userType createUserStatus = {
    UserTypes = userTypes
    NewUserId = UserId.Create ()
    NewUserNameText = String.Empty
    NewUserNameErrorText = None
    NewPasswordKey = Guid.NewGuid ()
    NewPasswordText = String.Empty
    NewPasswordErrorText = None
    ConfirmPasswordKey = Guid.NewGuid ()
    ConfirmPasswordText = String.Empty
    ConfirmPasswordErrorText = None
    NewUserType = userType
    CreateUserStatus = createUserStatus }

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private handleCreateUserCmdResult (result:Result<UserName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.CreateUsersState with
    | Some createUsersState ->
        match createUsersState.CreateUserStatus with
        | Some CreateUserPending ->
            match result with
            | Ok userName ->
                let (UserName userName) = userName
                let createUsersState = defaultCreateUsersState createUsersState.UserTypes createUsersState.NewUserType None
                { state with CreateUsersState = createUsersState |> Some }, sprintf "<strong>%s</strong> has been added" userName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "CreateUserCmdResult error -> %A" error) (error |> cmdErrorText)
                let createUsersState = { createUsersState with CreateUserStatus = errorText |> CreateUserFailed |> Some }
                { state with CreateUsersState = createUsersState |> Some }, "Unable to add user" |> errorToastCmd
        | Some (CreateUserFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected CreateUserCmdResult when CreateUserStatus is not CreateUserPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected CreateUserCmdResult when CreateUsersState is None -> %A" result)

let private handleResetPasswordCmdResult (result:Result<UserName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.ResetPasswordState with
    | Some resetPasswordState ->
        match resetPasswordState.ResetPasswordStatus with
        | Some ResetPasswordPending ->
            match result with
            | Ok userName ->
                let (UserName userName) = userName
                { state with ResetPasswordState = None }, sprintf "Password has been reset for <strong>%s</strong>" userName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ResetPasswordCmdResult error -> %A" error) (error |> cmdErrorText)
                let resetPasswordState = { resetPasswordState with ResetPasswordStatus = errorText |> ResetPasswordFailed |> Some }
                { state with ResetPasswordState = resetPasswordState |> Some }, "Unable to reset password" |> errorToastCmd
        | Some (ResetPasswordFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ResetPasswordCmdResult when ResetPasswordStatus is not ResetPasswordPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ResetPasswordCmdResult when ResetPasswordState is None -> %A" result)

let private handleChangeUserTypeCmdResult (result:Result<UserName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.ChangeUserTypeState with
    | Some changeUserTypeState ->
        match changeUserTypeState.ChangeUserTypeStatus with
        | Some ChangeUserTypePending ->
            match result with
            | Ok userName ->
                let (UserName userName) = userName
                { state with ChangeUserTypeState = None }, sprintf "Type has been changed for <strong>%s</strong>" userName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangeUserTypeCmdResult error -> %A" error) (error |> cmdErrorText)
                let changeUserTypeState = { changeUserTypeState with ChangeUserTypeStatus = errorText |> ChangeUserTypeFailed |> Some }
                { state with ChangeUserTypeState = changeUserTypeState |> Some }, "Unable to change user type" |> errorToastCmd
        | Some (ChangeUserTypeFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangeUserTypeCmdResult when ChangeUserTypeStatus is not ChangeUserTypePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangeUserTypeCmdResult when ChangeUserTypeState is None -> %A" result)

let private handleServerUserAdminMsg serverUserAdminMsg state : State * Cmd<Input> =
    match serverUserAdminMsg with
    | CreateUserCmdResult result ->
        state |> handleCreateUserCmdResult result
    | ResetPasswordCmdResult result ->
        state |> handleResetPasswordCmdResult result
    | ChangeUserTypeCmdResult result ->
        state |> handleChangeUserTypeCmdResult result

let private handleCreateUsersInput createUsersInput userDic state : State * Cmd<Input> * bool =
    match createUsersInput, state.CreateUsersState with
    | NewUserNameTextChanged newUserNameText, Some createUsersState ->
        let userNames = userDic |> userNames
        let newUserNameErrorText = validateUserName userNames (UserName newUserNameText)
        let createUsersState = { createUsersState with NewUserNameText = newUserNameText ; NewUserNameErrorText = newUserNameErrorText }
        { state with CreateUsersState = createUsersState |> Some }, Cmd.none, true
    | CreateUsersInput.NewPasswordTextChanged newPasswordText, Some createUsersState ->
        let newPasswordErrorText = validatePassword (Password newPasswordText)
        let confirmPasswordErrorText =
            if String.IsNullOrWhiteSpace createUsersState.ConfirmPasswordText then createUsersState.ConfirmPasswordErrorText
            else validateConfirmPassword (Password newPasswordText) (Password createUsersState.ConfirmPasswordText)
        let createUsersState = { createUsersState with NewPasswordText = newPasswordText ; NewPasswordErrorText = newPasswordErrorText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        { state with CreateUsersState = createUsersState |> Some }, Cmd.none, true
    | CreateUsersInput.ConfirmPasswordTextChanged confirmPasswordText, Some createUsersState ->
        let confirmPasswordErrorText = validateConfirmPassword (Password createUsersState.NewPasswordText) (Password confirmPasswordText)
        let createUsersState = { createUsersState with ConfirmPasswordText = confirmPasswordText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        { state with CreateUsersState = createUsersState |> Some }, Cmd.none, true
    | NewUserTypeChanged newUserType, Some createUsersState ->
        let createUsersState = { createUsersState with NewUserType = newUserType }
        { state with CreateUsersState = createUsersState |> Some }, Cmd.none, true
    | CreateUser, Some createUsersState -> // note: assume no need to validate NewUserNameText or NewPasswordText or ConfirmPasswordText (i.e. because UserAdmin.Render.renderCreateUsersModal will ensure that CreateUser can only be dispatched when valid)
        let createUsersState = { createUsersState with CreateUserStatus = CreateUserPending |> Some }
        let createUserCmdParams =
            createUsersState.NewUserId, UserName (createUsersState.NewUserNameText.Trim ()), Password (createUsersState.NewPasswordText.Trim ()), createUsersState.NewUserType
        let cmd = createUserCmdParams |> CreateUserCmd |> UiAuthUserAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with CreateUsersState = createUsersState |> Some }, cmd, true
    | CancelCreateUsers, Some createUsersState ->
        match createUsersState.CreateUserStatus with
        | Some CreateUserPending ->
            state, shouldNeverHappenCmd "Unexpected CancelCreateUsers when CreateUserPending", false
        | Some (CreateUserFailed _) | None ->
            { state with CreateUsersState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected CreateUsersInput when CreateUsersState is None -> %A" createUsersInput), false

let private handleResetPasswordInput resetPasswordInput userDic state : State * Cmd<Input> * bool =
    match resetPasswordInput, state.ResetPasswordState with
    | ResetPasswordInput.NewPasswordTextChanged newPasswordText, Some resetPasswordState ->
        let newPasswordErrorText = validatePassword (Password newPasswordText)
        let confirmPasswordErrorText =
            if String.IsNullOrWhiteSpace resetPasswordState.ConfirmPasswordText then resetPasswordState.ConfirmPasswordErrorText
            else validateConfirmPassword (Password newPasswordText) (Password resetPasswordState.ConfirmPasswordText)
        let resetPasswordState = { resetPasswordState with NewPasswordText = newPasswordText ; NewPasswordErrorText = newPasswordErrorText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        { state with ResetPasswordState = resetPasswordState |> Some }, Cmd.none, true
    | ResetPasswordInput.ConfirmPasswordTextChanged confirmPasswordText, Some resetPasswordState ->
        let confirmPasswordErrorText = validateConfirmPassword (Password resetPasswordState.NewPasswordText) (Password confirmPasswordText)
        let resetPasswordState = { resetPasswordState with ConfirmPasswordText = confirmPasswordText ; ConfirmPasswordErrorText = confirmPasswordErrorText }
        { state with ResetPasswordState = resetPasswordState |> Some }, Cmd.none, true
    | ResetPassword, Some resetPasswordState -> // note: assume no need to validate NewUserNameText or NewPasswordText or ConfirmPasswordText (i.e. because UserAdmin.Render.renderCreateUsersModal will ensure that CreateUser can only be dispatched when valid)
        let resetPasswordState = { resetPasswordState with ResetPasswordStatus = ResetPasswordPending |> Some }
        let userId = resetPasswordState.UserId
        let currentRvn = userId |> userRvnOrInitial userDic
        let resetPasswordCmdParams = userId, currentRvn, Password (resetPasswordState.NewPasswordText.Trim ())
        let cmd = resetPasswordCmdParams |> ResetPasswordCmd |> UiAuthUserAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ResetPasswordState = resetPasswordState |> Some }, cmd, true
    | CancelResetPassword, Some resetPasswordState ->
        match resetPasswordState.ResetPasswordStatus with
        | Some ResetPasswordPending ->
            state, shouldNeverHappenCmd "Unexpected CancelResetPassword when ResetPasswordPending", false
        | Some (ResetPasswordFailed _) | None ->
            { state with ResetPasswordState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ResetPasswordInput when ResetPasswordState is None -> %A" resetPasswordInput), false

let private handleChangeUserTypeInput changeUserTypeInput userDic state : State * Cmd<Input> * bool =
    match changeUserTypeInput, state.ChangeUserTypeState with
    | UserTypeChanged userType, Some changeUserTypeState ->
        let changeUserTypeState = { changeUserTypeState with UserType = userType |> Some }
        { state with ChangeUserTypeState = changeUserTypeState |> Some }, Cmd.none, true
    | ChangeUserType, Some changeUserTypeState -> // note: assume no need to validate UserType (i.e. because UserAdmin.Render.renderChangeUserTypeModal will ensure that ChangeUserType can only be dispatched when valid)
        match changeUserTypeState.UserType with
        | Some userType ->
            let changeUserTypeState = { changeUserTypeState with ChangeUserTypeStatus = ChangeUserTypePending |> Some }
            let userId = changeUserTypeState.UserId
            let currentRvn = userId |> userRvnOrInitial userDic
            let changeUserTypeCmdParams = userId, currentRvn, userType
            let cmd = changeUserTypeCmdParams |> ChangeUserTypeCmd |> UiAuthUserAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ChangeUserTypeState = changeUserTypeState |> Some }, cmd, true
        | None -> // note: should never happen
            state, Cmd.none, false
    | CancelChangeUserType, Some changeUserTypeState ->
        match changeUserTypeState.ChangeUserTypeStatus with
        | Some ChangeUserTypePending ->
            state, shouldNeverHappenCmd "Unexpected CancelChangeUserType when ChangeUserTypePending", false
        | Some (ChangeUserTypeFailed _) | None ->
            { state with ChangeUserTypeState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangeUserTypeInput when ChangeUserTypeState is None -> %A" changeUserTypeInput), false

let transition input (usersProjection:Projection<_ * UserDic>) state =
    let state, cmd, isUserNonApiActivity =
        match input, usersProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerUserAdminMsg serverUserAdminMsg, Ready _ ->
            let state, cmd = state |> handleServerUserAdminMsg serverUserAdminMsg
            state, cmd, false
        | ShowCreateUsersModal userTypes, Ready _ ->
            let createUsersState = defaultCreateUsersState userTypes Pleb None
            { state with CreateUsersState = createUsersState |> Some }, Cmd.none, true
        | CreateUsersInput createUsersInput, Ready (_, userDic) ->
            state |> handleCreateUsersInput createUsersInput userDic
        | ShowResetPasswordModal userId, Ready _ -> // note: no need to check for unknown userId (should never happen)
            let resetPasswordState = {
                UserId = userId
                NewPasswordKey = Guid.NewGuid ()
                NewPasswordText = String.Empty
                NewPasswordErrorText = None
                ConfirmPasswordKey = Guid.NewGuid ()
                ConfirmPasswordText = String.Empty
                ConfirmPasswordErrorText = None
                ResetPasswordStatus = None }
            { state with ResetPasswordState = resetPasswordState |> Some }, Cmd.none, true
        | ResetPasswordInput resetPasswordInput, Ready (_, userDic) ->
            state |> handleResetPasswordInput resetPasswordInput userDic
        | ShowChangeUserTypeModal (userId, userTypes), Ready _ -> // note: no need to check for unknown userId (should never happen)
            let changeUserTypeState = { UserId = userId ; UserTypes = userTypes ; UserType = None ; ChangeUserTypeStatus = None }
            { state with ChangeUserTypeState = changeUserTypeState |> Some }, Cmd.none, true
        | ChangeUserTypeInput changeUserTypeInput, Ready (_, userDic) ->
            state |> handleChangeUserTypeInput changeUserTypeInput userDic
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" usersProjection input), false
    state, cmd, isUserNonApiActivity
