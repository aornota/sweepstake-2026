module Aornota.Sweepstake2026.Ui.Pages.UserAdmin.Render

open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Ui.Common.LazyViewOrHMR
open Aornota.Sweepstake2026.Ui.Pages.UserAdmin.Common
open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common
open Aornota.Sweepstake2026.Ui.Shared
open Aornota.Sweepstake2026.Ui.Theme.Common
open Aornota.Sweepstake2026.Ui.Theme.Render.Bulma
open Aornota.Sweepstake2026.Ui.Theme.Shared
open Aornota.Sweepstake2026.Common.Domain.User // note: after Aornota.Sweepstake2026.Ui.Render.Bulma to avoid collision with Icon.Password

open System

module RctH = Fable.React.Helpers

let [<Literal>] private RECENTLY_ACTIVE = 5.<minute>

let private cutoff (after:int<second>) = float (after * -1) |> DateTimeOffset.UtcNow.AddSeconds

let private (|Self|RecentlyActive|SignedIn|NotSignedIn|PersonaNonGrata|) (authUserId:UserId, userId:UserId, userAuthDto:UserAuthDto) =
    if userId = authUserId then Self
    else if userAuthDto.UserType = UserType.PersonaNonGrata then PersonaNonGrata
    else
        match userAuthDto.LastActivity with
        | Some lastApi ->
            let recentlyActiveCutoff = cutoff (int (RECENTLY_ACTIVE |> minutesToSeconds) * 1<second>)
            if lastApi > recentlyActiveCutoff then RecentlyActive else SignedIn
        | None -> NotSignedIn

let private semantic authUserId (userId, userAuthDto) =
    match userAuthDto with
    | Some userAuthDto ->
        match (authUserId, userId, userAuthDto) with | Self -> Link | RecentlyActive -> Success | SignedIn -> Primary | NotSignedIn -> Dark | PersonaNonGrata -> Light
    | None -> Light // note: should never happen

let private userTypes = [ SuperUser ; Administrator ; Pleb ; PersonaNonGrata ] |> List.map (fun userType -> userType, Guid.NewGuid())

let private userTypeSortOrder userType =
    match userType with
    | Some userType -> match userType with | SuperUser -> 1 | Administrator -> 2 | Pleb -> 3 | UserType.PersonaNonGrata -> 4
    | None -> 5 // note: should never happen
let private userTypeElement userType =
    match userType with
    | Some SuperUser -> strongEm "Benevolent dictator"
    | Some Administrator -> strong "Administrator"
    | Some Pleb -> str "User"
    | Some UserType.PersonaNonGrata -> em "Persona non grata"
    | None -> str UNKNOWN // note: should never happen

let private userTypeRadios theme selectedUserType allowedUserTypes currentUserType disableAll dispatch =
    let onChange userType = (fun _ -> userType |> dispatch)
    userTypes
    |> List.sortBy (fun (userType, _) -> userType |> Some |> userTypeSortOrder)
    |> List.map (fun (userType, key) ->
        let selected, allowed, current = userType |> Some = selectedUserType, allowedUserTypes |> List.contains userType, userType |> Some = currentUserType
        let semantic =
            if selected then
                if allowed then Success else Warning
            else if current then Light
            else Link
        let disabled = disableAll || not allowed || current
        let onChange = if selected || disabled then ignore else userType |> onChange
        let radioData = { radioDefaultSmall with RadioSemantic = semantic |> Some ; HasBackgroundColour = selected || current }
        radioInline theme radioData key (userType |> Some |> userTypeElement) selected disabled onChange)

let private renderCreateUsersModal (useDefaultTheme, userDic:UserDic, createUsersState:CreateUsersState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Add user/s" ] |> para theme paraCentredSmall ]
    let onDismiss = match createUsersState.CreateUserStatus with | Some CreateUserPending -> None | Some _ | None -> (fun _ -> CancelCreateUsers |> dispatch) |> Some
    let userNames = userDic |> userNames
    let isCreatingUser, createUserInteraction, onEnter =
        let createUser = (fun _ -> CreateUser |> dispatch)
        match createUsersState.CreateUserStatus with
        | Some CreateUserPending -> true, Loading, ignore
        | Some (CreateUserFailed _) | None ->
            let validUserName = validateUserName userNames (UserName createUsersState.NewUserNameText)
            let validPassword = validatePassword (Password createUsersState.NewPasswordText)
            let validConfirmPassword = validateConfirmPassword (Password createUsersState.NewPasswordText) (Password createUsersState.ConfirmPasswordText)
            match validUserName, validPassword, validConfirmPassword with
            | None, None, None -> false, Clickable (createUser, None), createUser
            | _ -> false, NotEnabled None, ignore
    let errorText = match createUsersState.CreateUserStatus with | Some (CreateUserFailed errorText) -> errorText |> Some | Some CreateUserPending | None -> None
    let (UserId newUserKey) = createUsersState.NewUserId
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter the name, password (twice) and type for the new user" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme newUserKey createUsersState.NewUserNameText (iconUserSmall |> Some) false createUsersState.NewUserNameErrorText [] true isCreatingUser
                (NewUserNameTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme createUsersState.NewPasswordKey createUsersState.NewPasswordText (iconPasswordSmall |> Some) true createUsersState.NewPasswordErrorText []
                false isCreatingUser (CreateUsersInput.NewPasswordTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme createUsersState.ConfirmPasswordKey createUsersState.ConfirmPasswordText (iconPasswordSmall |> Some) true createUsersState.ConfirmPasswordErrorText []
                false isCreatingUser (CreateUsersInput.ConfirmPasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            yield! userTypeRadios theme (createUsersState.NewUserType |> Some) createUsersState.UserTypes None isCreatingUser (NewUserTypeChanged >> dispatch) ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Add user" ] |> button theme { buttonLinkSmall with Interaction = createUserInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderResetPasswordModal (useDefaultTheme, userDic:UserDic, resetPasswordState:ResetPasswordState) dispatch =
    let theme = getTheme useDefaultTheme
    let (UserName userName) = resetPasswordState.UserId |> userName userDic
    let titleText = sprintf "Reset password for %s" userName
    let title = [ [ strong titleText ] |> para theme paraCentredSmall ]
    let onDismiss = match resetPasswordState.ResetPasswordStatus with | Some ResetPasswordPending -> None | Some _ | None -> (fun _ -> CancelResetPassword |> dispatch) |> Some
    let isResettingPassword, resetPasswordInteraction, onEnter =
        let resetPassword = (fun _ -> ResetPassword |> dispatch)
        match resetPasswordState.ResetPasswordStatus with
        | Some ResetPasswordPending -> true, Loading, ignore
        | Some (ResetPasswordFailed _) | None ->
            let validPassword = validatePassword (Password resetPasswordState.NewPasswordText)
            let validConfirmPassword = validateConfirmPassword (Password resetPasswordState.NewPasswordText) (Password resetPasswordState.ConfirmPasswordText)
            match validPassword, validConfirmPassword with
            | None, None -> false, Clickable (resetPassword, None), resetPassword
            | _ -> false, NotEnabled None, ignore
    let errorText = match resetPasswordState.ResetPasswordStatus with | Some (ResetPasswordFailed errorText) -> errorText |> Some | Some ResetPasswordPending | None -> None
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter the new password (twice) for the user" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme resetPasswordState.NewPasswordKey resetPasswordState.NewPasswordText (iconPasswordSmall |> Some) true resetPasswordState.NewPasswordErrorText []
                true isResettingPassword (ResetPasswordInput.NewPasswordTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme resetPasswordState.ConfirmPasswordKey resetPasswordState.ConfirmPasswordText (iconPasswordSmall |> Some) true resetPasswordState.ConfirmPasswordErrorText []
                false isResettingPassword (ResetPasswordInput.ConfirmPasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Reset password" ] |> button theme { buttonLinkSmall with Interaction = resetPasswordInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderChangeUserTypeModal (useDefaultTheme, userDic:UserDic, changeUserTypeState:ChangeUserTypeState) dispatch =
    let theme = getTheme useDefaultTheme
    let userId = changeUserTypeState.UserId
    let currentUserType, titleText =
        match userId |> userType userDic with
        | Some userType ->
            let (UserName userName) = userId |> userName userDic
            userType |> Some, sprintf "Change type for %s" userName
        | None -> None, "Change user type" // note: should never happen
    let title = [ [ strong titleText ] |> para theme paraCentredSmall ]
    let onDismiss = match changeUserTypeState.ChangeUserTypeStatus with | Some ChangeUserTypePending -> None | Some _ | None -> (fun _ -> CancelChangeUserType |> dispatch) |> Some
    let isChangingUserType, changeUserTypeInteraction =
        let changeUserType = (fun _ -> ChangeUserType |> dispatch)
        match changeUserTypeState.ChangeUserTypeStatus with
        | Some ChangeUserTypePending -> true, Loading
        | Some (ChangeUserTypeFailed _) | None ->
            let isValid = match changeUserTypeState.UserType with | Some userType -> userType |> Some <> currentUserType | None -> false
            if isValid |> not then false, NotEnabled None
            else false, Clickable (changeUserType, None)
    let errorText = match changeUserTypeState.ChangeUserTypeStatus with | Some (ChangeUserTypeFailed errorText) -> errorText |> Some | Some ChangeUserTypePending | None -> None
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please choose the new type for the user" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            yield! userTypeRadios theme changeUserTypeState.UserType changeUserTypeState.UserTypes currentUserType isChangingUserType (UserTypeChanged >> dispatch) ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Change type" ] |> button theme { buttonLinkSmall with Interaction = changeUserTypeInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderUsers (useDefaultTheme, userDic:UserDic, authUser) dispatch =
    let theme = getTheme useDefaultTheme
    let resetPassword userId userType =
        match authUser.Permissions.UserAdminPermissions with
        | Some userAdminPermissions ->
            match userAdminPermissions.ResetPasswordPermission with
            | Some (NotSelf userTypes) ->
                match userType with
                | Some userType ->
                    if userId <> authUser.UserId && userTypes |> List.contains userType then
                        let onClick = (fun _ -> userId |> ShowResetPasswordModal |> dispatch)
                        [ [ str "Reset password" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal onClick) |> Some
                    else None
                | None -> None
            | None -> None
        | None -> None // note: should never happen
    let changeUserType userId userType =
        match authUser.Permissions.UserAdminPermissions with
        | Some userAdminPermissions ->
            match userAdminPermissions.ChangeUserTypePermission with
            | Some (NotSelf userTypes, newUserTypes) ->
                match userType with
                | Some userType ->
                    if userId <> authUser.UserId && userTypes |> List.contains userType then
                        let onClick = (fun _ -> (userId, newUserTypes) |> ShowChangeUserTypeModal |> dispatch)
                        [ [ str "Change type" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal onClick) |> Some
                    else None
                | None -> None
            | None -> None
        | None -> None // note: should never happen
    let userRow (userId, UserName userName, userType, semantic) =
        let userName = [ str userName ] |> tag theme { tagDefault with TagSemantic = semantic |> Some ; IsRounded = false }
        tr false [
            td [ [ userName ] |> para theme paraDefaultSmallest ]
            td [ [ userType |> userTypeElement ] |> para theme paraCentredSmallest ]
            td [ RctH.ofOption (changeUserType userId userType) ]
            td [ RctH.ofOption (resetPassword userId userType) ] ]
    let users =
        userDic
        |> List.ofSeq
        |> List.map (fun (KeyValue (userId, (userName, userAuthDto))) ->
            let userType = userId |> userType userDic
            let semantic = (userId, userAuthDto) |> semantic authUser.UserId
            userId, userName, userType, semantic)
        |> List.sortBy (fun (_, userName, userType, _) ->
            userType |> userTypeSortOrder, userName)
    let userRows = users |> List.map (fun (userId, userName, userType, semantic) -> (userId, userName, userType, semantic) |> userRow)
    div divCentred [
        if userDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
                thead [
                    tr false [
                        th [ [ strong "User name" ] |> para theme paraDefaultSmallest ]
                        th [ [ strong "Type" ] |> para theme paraCentredSmallest ]
                        th []
                        th [] ] ]
                tbody [ yield! userRows ] ]
        else yield [ str "There are no users" ] |> para theme paraCentredSmallest ] // note: should never happen

let private createUsers theme authUser dispatch =
    match authUser.Permissions.UserAdminPermissions with
    | Some userAdminPermissions ->
        let userTypes = userAdminPermissions.CreateUserPermission
        match userTypes with
        | _ :: _ ->
            let onClick = (fun _ -> userTypes |> ShowCreateUsersModal |> dispatch)
            [ [ str "Add user/s" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal onClick) |> Some
        | [] -> None
    | None -> None // note: should never happen

let render (useDefaultTheme, state, authUser, usersProjection:Projection<_ * UserDic>, hasModal) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ strong "User administration" ] |> para theme paraCentredSmall
        yield hr theme false
        match usersProjection with
        | Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, userDic) ->
            match hasModal, state.CreateUsersState with
            | false, Some createUsersState ->
                yield div divDefault [ lazyViewOrHMR2 renderCreateUsersModal (useDefaultTheme, userDic, createUsersState) (CreateUsersInput >> dispatch) ]
            | _ -> ()
            match hasModal, state.ResetPasswordState with
            | false, Some resetPasswordState ->
                yield div divDefault [ lazyViewOrHMR2 renderResetPasswordModal (useDefaultTheme, userDic, resetPasswordState) (ResetPasswordInput >> dispatch) ]
            | _ -> ()
            match hasModal, state.ChangeUserTypeState with
            | false, Some changeUserTypeState ->
                yield div divDefault [ lazyViewOrHMR2 renderChangeUserTypeModal (useDefaultTheme, userDic, changeUserTypeState) (ChangeUserTypeInput >> dispatch) ]
            | _ -> ()
            yield lazyViewOrHMR2 renderUsers (useDefaultTheme, userDic, authUser) dispatch
            yield RctH.ofOption (createUsers theme authUser dispatch) ]
