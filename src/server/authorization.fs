module Aornota.Sweepstake2026.Server.Authorization

open Aornota.Sweepstake2026.Common.Domain.User

type MetaToken = private | MetaToken

type ChangePasswordToken private (userId) =
    new (_:MetaToken, userId:UserId) = ChangePasswordToken userId
    member __.UserId = userId

type CreateUserToken private (userTypes) =
    new (_:MetaToken, userTypes:UserType list) = CreateUserToken userTypes
    member __.UserTypes = userTypes
type ResetPasswordToken private (userTarget) =
    new (_:MetaToken, userTarget:UserTarget) = ResetPasswordToken userTarget
    member __.UserTarget = userTarget
type ChangeUserTypeToken private (userTarget, userTypes) =
    new (_:MetaToken, userTarget:UserTarget, userTypes:UserType list) = ChangeUserTypeToken (userTarget, userTypes)
    member __.UserTarget = userTarget
    member __.UserTypes = userTypes

type DraftAdminToken private () =
    new (_:MetaToken) = DraftAdminToken ()
type ProcessDraftToken private () =
    new (_:MetaToken) = ProcessDraftToken ()

type ResultsAdminToken private () =
    new (_:MetaToken) = ResultsAdminToken ()

type CreatePostToken private () =
    new (_:MetaToken) = CreatePostToken ()
type EditOrRemovePostToken private (userId) =
    new (_:MetaToken, userId:UserId) = EditOrRemovePostToken userId
    member __.UserId = userId

type SquadsProjectionAuthQryToken private () =
    new (_:MetaToken) = SquadsProjectionAuthQryToken ()
type CreateSquadToken private () =
    new (_:MetaToken) = CreateSquadToken ()
type AddOrEditPlayerToken private () =
    new (_:MetaToken) = AddOrEditPlayerToken ()
type WithdrawPlayerToken private () =
    new (_:MetaToken) = WithdrawPlayerToken ()
type EliminateSquadToken private () =
    new (_:MetaToken) = EliminateSquadToken ()

type CreateFixtureToken private () =
    new (_:MetaToken) = CreateFixtureToken ()
type ConfirmFixtureToken private () =
    new (_:MetaToken) = ConfirmFixtureToken ()

type DraftToken private (userId) =
    new (_:MetaToken, userId:UserId) = DraftToken userId
    member __.UserId = userId

type ChatToken private () =
    new (_:MetaToken) = ChatToken ()

type private ValidatedUserTokens = {
    ChangePasswordToken : ChangePasswordToken option
    CreateUserToken : CreateUserToken option
    ResetPasswordToken : ResetPasswordToken option
    ChangeUserTypeToken : ChangeUserTypeToken option
    DraftAdminToken : DraftAdminToken option
    ProcessDraftToken : ProcessDraftToken option
    ResultsAdminToken : ResultsAdminToken option
    CreatePostToken : CreatePostToken option
    EditOrRemovePostToken : EditOrRemovePostToken option
    SquadsProjectionAuthQryToken : SquadsProjectionAuthQryToken option
    CreateSquadToken : CreateSquadToken option
    AddOrEditPlayerToken : AddOrEditPlayerToken option
    WithdrawPlayerToken : WithdrawPlayerToken option
    EliminateSquadToken : EliminateSquadToken option
    CreateFixtureToken : CreateFixtureToken option
    ConfirmFixtureToken : ConfirmFixtureToken option
    DraftToken : DraftToken option
    ChatToken : ChatToken option }

type UserTokens private (vut:ValidatedUserTokens) =
    new (permissions:Permissions) =
        let changePasswordToken = match permissions.ChangePasswordPermission with | Some userId -> (MetaToken, userId) |> ChangePasswordToken |> Some | None -> None
        let createUserToken, resetPasswordToken, changeUserTypeToken =
            match permissions.UserAdminPermissions with
            | Some userAdminPermissions ->
                let createUserToken = (MetaToken, userAdminPermissions.CreateUserPermission) |> CreateUserToken |> Some
                let resetPasswordToken =
                    match userAdminPermissions.ResetPasswordPermission with
                    | Some userTarget -> (MetaToken, userTarget) |> ResetPasswordToken |> Some
                    | None -> None
                let changeUserTypeToken =
                    match userAdminPermissions.ChangeUserTypePermission with
                    | Some (userTarget, userTypes) -> (MetaToken, userTarget, userTypes) |> ChangeUserTypeToken |> Some
                    | None -> None
                createUserToken, resetPasswordToken, changeUserTypeToken
            | None -> None, None, None
        let draftAdminToken, processDraftToken =
            match permissions.DraftAdminPermissions with
            | Some draftAdminPermissions ->
                let draftAdminToken = MetaToken |> DraftAdminToken |> Some
                let processDraftToken = if draftAdminPermissions.ProcessDraftPermission then MetaToken |> ProcessDraftToken |> Some else None
                draftAdminToken, processDraftToken
            | None -> None, None
        let resultsAdminToken = if permissions.ResultsAdminPermission then MetaToken |> ResultsAdminToken |> Some else None
        let createPostToken, editOrRemovePostToken =
            match permissions.NewsPermissions with
            | Some newsPermissions ->
                let createPostToken = if newsPermissions.CreatePostPermission then MetaToken |> CreatePostToken |> Some else None
                let editOrRemovePostToken = match newsPermissions.EditOrRemovePostPermission with | Some userId -> (MetaToken, userId) |> EditOrRemovePostToken |> Some | None -> None
                createPostToken, editOrRemovePostToken
            | None -> None, None
        let squadsProjectionAuthQryToken, createSquadToken, addOrEditPlayerToken, withdrawPlayerToken, eliminateSquadToken =
            match permissions.SquadPermissions with
            | Some squadPermissions ->
                let squadsProjectionAuthQryToken = if squadPermissions.SquadProjectionAuthQryPermission then MetaToken |> SquadsProjectionAuthQryToken |> Some else None
                let createSquadToken = if squadPermissions.CreateSquadPermission then MetaToken |> CreateSquadToken |> Some else None
                let addOrEditPlayerToken = if squadPermissions.AddOrEditPlayerPermission then MetaToken |> AddOrEditPlayerToken |> Some else None
                let withdrawPlayerToken = if squadPermissions.WithdrawPlayerPermission then MetaToken |> WithdrawPlayerToken |> Some else None
                let eliminateSquadToken = if squadPermissions.EliminateSquadPermission then MetaToken |> EliminateSquadToken |> Some else None
                squadsProjectionAuthQryToken, createSquadToken, addOrEditPlayerToken, withdrawPlayerToken, eliminateSquadToken
            | None -> None, None, None, None, None
        let createFixtureToken, confirmFixtureToken =
            match permissions.FixturePermissions with
            | Some fixturePermissions ->
                let createFixtureToken = if fixturePermissions.CreateFixturePermission then MetaToken |> CreateFixtureToken |> Some else None
                let confirmFixtureToken = if fixturePermissions.ConfirmFixturePermission then MetaToken |> ConfirmFixtureToken |> Some else None
                createFixtureToken, confirmFixtureToken
            | None -> None, None
        let draftToken = match permissions.DraftPermission with | Some userId -> (MetaToken, userId) |> DraftToken |> Some | None -> None
        let chatToken = if permissions.ChatPermission then MetaToken |> ChatToken |> Some else None
        UserTokens {
            ChangePasswordToken = changePasswordToken
            CreateUserToken = createUserToken
            ResetPasswordToken = resetPasswordToken
            ChangeUserTypeToken = changeUserTypeToken
            DraftAdminToken = draftAdminToken
            ProcessDraftToken = processDraftToken
            ResultsAdminToken = resultsAdminToken
            CreatePostToken = createPostToken
            EditOrRemovePostToken = editOrRemovePostToken
            SquadsProjectionAuthQryToken = squadsProjectionAuthQryToken
            CreateSquadToken = createSquadToken
            AddOrEditPlayerToken = addOrEditPlayerToken
            WithdrawPlayerToken = withdrawPlayerToken
            EliminateSquadToken = eliminateSquadToken
            CreateFixtureToken = createFixtureToken
            ConfirmFixtureToken = confirmFixtureToken
            DraftToken = draftToken
            ChatToken = chatToken }
    member __.ChangePasswordToken = vut.ChangePasswordToken
    member __.CreateUserToken = vut.CreateUserToken
    member __.ResetPasswordToken = vut.ResetPasswordToken
    member __.ChangeUserTypeToken = vut.ChangeUserTypeToken
    member __.DraftAdminToken = vut.DraftAdminToken
    member __.ProcessDraftToken = vut.ProcessDraftToken
    member __.ResultsAdminToken = vut.ResultsAdminToken
    member __.CreatePostToken = vut.CreatePostToken
    member __.EditOrRemovePostToken = vut.EditOrRemovePostToken
    member __.SquadsProjectionAuthQryToken = vut.SquadsProjectionAuthQryToken
    member __.CreateSquadToken = vut.CreateSquadToken
    member __.AddOrEditPlayerToken = vut.AddOrEditPlayerToken
    member __.WithdrawPlayerToken = vut.WithdrawPlayerToken
    member __.EliminateSquadToken = vut.EliminateSquadToken
    member __.CreateFixtureToken = vut.CreateFixtureToken
    member __.ConfirmFixtureToken = vut.ConfirmFixtureToken
    member __.DraftToken = vut.DraftToken
    member __.ChatToken = vut.ChatToken
