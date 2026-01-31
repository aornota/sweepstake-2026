module Aornota.Sweepstake2026.Common.Domain.User

open Aornota.Sweepstake2026.Common.Revision

open System

type UserId = | UserId of guid : Guid with static member Create () = Guid.NewGuid () |> UserId

type UserName = | UserName of userName : string
type Password = | Password of password : string

type UserType = | SuperUser | Administrator | Pleb | PersonaNonGrata

type UserTarget = | NotSelf of userTypes : UserType list

type UserAdminPermissions = {
    CreateUserPermission : UserType list
    ResetPasswordPermission : UserTarget option
    ChangeUserTypePermission : (UserTarget * UserType list) option }

type DraftAdminPermissions = {
    ProcessDraftPermission : bool }

type NewsPermissions = {
    CreatePostPermission : bool
    EditOrRemovePostPermission : UserId option }

type SquadPermissions = {
    SquadProjectionAuthQryPermission : bool
    CreateSquadPermission : bool
    AddOrEditPlayerPermission : bool
    WithdrawPlayerPermission : bool
    EliminateSquadPermission : bool }

type FixturePermissions = {
    CreateFixturePermission : bool
    ConfirmFixturePermission : bool }

type Permissions = {
    ChangePasswordPermission : UserId option
    UserAdminPermissions : UserAdminPermissions option
    DraftAdminPermissions : DraftAdminPermissions option
    ResultsAdminPermission: bool
    NewsPermissions : NewsPermissions option
    SquadPermissions : SquadPermissions option
    FixturePermissions : FixturePermissions option
    DraftPermission : UserId option
    ChatPermission : bool }

type MustChangePasswordReason =
    | FirstSignIn
    | PasswordReset

type Jwt = | Jwt of jwt : string

type AuthUser = {
    UserId : UserId
    Rvn : Rvn
    UserName : UserName
    UserType : UserType
    Permissions : Permissions
    MustChangePasswordReason : MustChangePasswordReason option
    Jwt : Jwt }

type UserUnauthDto = { UserId : UserId ; UserName : UserName }
type UserAuthDto = { Rvn : Rvn ; UserType : UserType ; LastActivity : DateTimeOffset option }

type UserDto = UserUnauthDto * UserAuthDto option

let permissions userId userType =
    let changePasswordPermission = match userType with | SuperUser | Administrator | Pleb -> userId |> Some | PersonaNonGrata -> None
    let createUserPermission, resetPasswordPermission, changeUserTypePermission =
        match userType with
        | SuperUser ->
            let createUserPermission = [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ]
            let resetPasswordPermission = NotSelf [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ] |> Some
            let changeUserTypePermission = (NotSelf [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ], [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ]) |> Some
            createUserPermission, resetPasswordPermission, changeUserTypePermission
        | Administrator -> [ Pleb ], NotSelf [ Pleb ] |> Some, None
        | Pleb | PersonaNonGrata -> [], None, None
    let userAdminPermissions =
        match createUserPermission, resetPasswordPermission, changeUserTypePermission with
        | [], None, None -> None
        | _ -> { CreateUserPermission = createUserPermission ; ResetPasswordPermission = resetPasswordPermission ; ChangeUserTypePermission = changeUserTypePermission } |> Some
    let draftAdminPermissions =
        match userType with
        | SuperUser -> { ProcessDraftPermission = true } |> Some
        | Administrator -> { ProcessDraftPermission = false } |> Some
        | Pleb | PersonaNonGrata -> None
    let resultsAdminPermission = match userType with | SuperUser | Administrator -> true | Pleb | PersonaNonGrata -> false
    let createPostPermission, editOrRemovePostPermission = match userType with | SuperUser | Administrator -> true, userId |> Some | Pleb | PersonaNonGrata -> false, None
    let newsPermissions =
        match createPostPermission, editOrRemovePostPermission with
        | false, None -> None
        | _ -> { CreatePostPermission = createPostPermission ; EditOrRemovePostPermission = editOrRemovePostPermission } |> Some
    let squadProjectionAuthQryPermission, createSquadPermission, addOrEditPlayerPermission, withdrawPlayerPermission, eliminateSquadPermission =
        match userType with
        | SuperUser -> true, true, true, true, true
        | Administrator -> true, false, true, true, true
        | Pleb -> true, false, false, false, false
        | PersonaNonGrata -> false, false, false, false, false
    let squadPermissions =
        match squadProjectionAuthQryPermission, createSquadPermission, addOrEditPlayerPermission, withdrawPlayerPermission, eliminateSquadPermission with
        | false, false, false, false, false -> None
        | _ -> { SquadProjectionAuthQryPermission = squadProjectionAuthQryPermission ; CreateSquadPermission = createSquadPermission ; AddOrEditPlayerPermission = addOrEditPlayerPermission
                 WithdrawPlayerPermission = withdrawPlayerPermission ; EliminateSquadPermission = eliminateSquadPermission } |> Some
    let createFixturePermission, confirmFixturePermission = match userType with | SuperUser -> true, true | Administrator -> false, true | Pleb | PersonaNonGrata -> false, false
    let fixturePermissions =
        match createFixturePermission, confirmFixturePermission with
        | false, false -> None
        | _ -> { CreateFixturePermission = createFixturePermission ; ConfirmFixturePermission = confirmFixturePermission } |> Some
    let draftPermission = match userType with | SuperUser | Administrator | Pleb -> userId |> Some | PersonaNonGrata -> None
    let chatPermission = match userType with | SuperUser | Administrator | Pleb -> true | PersonaNonGrata -> false
    {
        ChangePasswordPermission = changePasswordPermission
        UserAdminPermissions = userAdminPermissions
        DraftAdminPermissions = draftAdminPermissions
        ResultsAdminPermission = resultsAdminPermission
        NewsPermissions = newsPermissions
        SquadPermissions = squadPermissions
        FixturePermissions  = fixturePermissions
        DraftPermission = draftPermission
        ChatPermission = chatPermission
    }

let validateUserName (userNames:UserName list) (UserName userName) =
    if String.IsNullOrWhiteSpace userName then "User name must not be blank" |> Some
    else if (userName.Trim ()).Length < 3 then "User name must be at least 3 characters" |> Some
    else if userNames |> List.map (fun (UserName userName) -> (userName.ToLower ()).Trim ()) |> List.contains ((userName.ToLower ()).Trim ()) then "User name already in use" |> Some
    else None
let validatePassword (Password password) =
    if String.IsNullOrWhiteSpace password then "Password must not be blank" |> Some
    else if (password.Trim ()).Length < 6 then "Password must be at least 6 characters" |> Some
    else None
let validateConfirmPassword (Password newPassword) (Password confirmPassword) =
    if newPassword <> confirmPassword then "Confirmation password must match new password" |> Some
    else validatePassword (Password confirmPassword)
