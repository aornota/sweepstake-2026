module Aornota.Sweepstake2026.Ui.Pages.UserAdmin.Common

open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications

open System

type CreateUsersInput =
    | NewUserNameTextChanged of newUserNameText : string
    | NewPasswordTextChanged of newPasswordText : string
    | ConfirmPasswordTextChanged of confirmPasswordText : string
    | NewUserTypeChanged of newUserType : UserType
    | CreateUser
    | CancelCreateUsers

type ResetPasswordInput =
    | NewPasswordTextChanged of newPasswordText : string
    | ConfirmPasswordTextChanged of confirmPasswordText : string
    | ResetPassword
    | CancelResetPassword

type ChangeUserTypeInput =
    | UserTypeChanged of userType : UserType
    | ChangeUserType
    | CancelChangeUserType

type Input =
    | AddNotificationMessage of notificationMessage : NotificationMessage
    | SendUiAuthMsg of uiAuthMsg : UiAuthMsg
    | ReceiveServerUserAdminMsg of serverUserAdminMsg : ServerUserAdminMsg
    | ShowCreateUsersModal of userTypes : UserType list
    | CreateUsersInput of createUsersInput : CreateUsersInput
    | ShowResetPasswordModal of userId : UserId
    | ResetPasswordInput of resetPasswordInput : ResetPasswordInput
    | ShowChangeUserTypeModal of userId : UserId * userTypes : UserType list
    | ChangeUserTypeInput of changeUserTypeInput : ChangeUserTypeInput

type CreateUserStatus =
    | CreateUserPending
    | CreateUserFailed of errorText : string

type CreateUsersState = {
    UserTypes : UserType list
    NewUserId : UserId
    NewUserNameText : string
    NewUserNameErrorText : string option
    NewPasswordKey : Guid
    NewPasswordText : string
    NewPasswordErrorText : string option
    ConfirmPasswordKey : Guid
    ConfirmPasswordText : string
    ConfirmPasswordErrorText : string option
    NewUserType : UserType
    CreateUserStatus : CreateUserStatus option }

type ResetPasswordStatus =
    | ResetPasswordPending
    | ResetPasswordFailed of errorText : string

type ResetPasswordState = {
    UserId : UserId
    NewPasswordKey : Guid
    NewPasswordText : string
    NewPasswordErrorText : string option
    ConfirmPasswordKey : Guid
    ConfirmPasswordText : string
    ConfirmPasswordErrorText : string option
    ResetPasswordStatus : ResetPasswordStatus option }

type ChangeUserTypeStatus =
    | ChangeUserTypePending
    | ChangeUserTypeFailed of errorText : string

type ChangeUserTypeState = {
    UserId : UserId
    UserTypes : UserType list
    UserType : UserType option
    ChangeUserTypeStatus : ChangeUserTypeStatus option }

type State = {
    CreateUsersState : CreateUsersState option
    ResetPasswordState : ResetPasswordState option
    ChangeUserTypeState : ChangeUserTypeState option }
