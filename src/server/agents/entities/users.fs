module Aornota.Sweepstake2026.Server.Agents.Entities.Users

(* Broadcasts: TODO:SendMsg
               UsersRead
   Subscribes: UsersEventsRead *)

open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Agents.Persistence
open Aornota.Sweepstake2026.Server.Authorization
open Aornota.Sweepstake2026.Server.Common.Helpers
open Aornota.Sweepstake2026.Server.Events.UserEvents
open Aornota.Sweepstake2026.Server.Jwt
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic
open System.Security.Cryptography
open System.Text

type private UsersInput =
    | IsAwaitingStart of reply : AsyncReplyChannel<bool>
    | Start of reply : AsyncReplyChannel<unit>
    | Reset of reply : AsyncReplyChannel<unit>
    | OnUsersEventsRead of usersEvents : (UserId * (Rvn * UserEvent) list) list
    | HandleSignInCmd of userName : UserName * password : Password
        * reply : AsyncReplyChannel<Result<AuthUser, SignInCmdError<string>>>
    | HandleAutoSignInCmd of userId : UserId * permissionsFromJwt : Permissions
        * reply : AsyncReplyChannel<Result<AuthUser, AutoSignInCmdError<string>>>
    | HandleChangePasswordCmd of token : ChangePasswordToken * auditUserId : UserId * currentRvn : Rvn * password : Password
        * reply : AsyncReplyChannel<Result<Rvn, AuthCmdError<string>>>
    | HandleCreateUserCmd of token : CreateUserToken * auditUserId : UserId * userId : UserId * userName : UserName * password : Password * userType : UserType
        * reply : AsyncReplyChannel<Result<UserName, AuthCmdError<string>>>
    | HandleResetPasswordCmd of token : ResetPasswordToken * auditUserId : UserId * userId : UserId * currentRvn : Rvn * password : Password
        * reply : AsyncReplyChannel<Result<UserName, AuthCmdError<string>>>
    | HandleChangeUserTypeCmd of token : ChangeUserTypeToken * auditUserId : UserId * userId : UserId * currentRvn : Rvn * userType : UserType
        * reply : AsyncReplyChannel<Result<UserName, AuthCmdError<string>>>

type private User = { Rvn : Rvn ; UserName : UserName ; PasswordSalt : Salt ; PasswordHash : Hash ; UserType : UserType ; MustChangePasswordReason : MustChangePasswordReason option }
type private UserDic = Dictionary<UserId, User>

let [<Literal>] private NOT_PERMITTED = "You are not permitted to access this system"

let private log category = (Entity Entity.Users, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private rng = RandomNumberGenerator.Create ()
let private sha512 = SHA512.Create ()
let private encoding = Encoding.UTF8

let private salt () =
    let bytes : byte [] = Array.zeroCreate 32
    rng.GetBytes bytes
    Salt (Convert.ToBase64String bytes)

let private hash (Password password) (Salt salt) =
    let bytes = encoding.GetBytes (sprintf "%s|%s" password salt) |> sha512.ComputeHash // note: password is therefore case-sensitive
    Hash (Convert.ToBase64String bytes)

let private authUser userId rvn userName userType permissions jwt mustChangePasswordReason =
    { UserId = userId ; Rvn = rvn ; UserName = userName ; UserType = userType ; Permissions = permissions ; Jwt = jwt ; MustChangePasswordReason = mustChangePasswordReason }

let private applyUserEvent source idAndUserResult (nextRvn, userEvent:UserEvent) =
    let otherError errorText = otherError (sprintf "%s#applyUserEvent" source) errorText
    match idAndUserResult, userEvent with
    | Ok (userId, _), _ when userId <> userEvent.UserId -> // note: should never happen
        ifDebug (sprintf "UserId mismatch for %A -> %A" userId userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" userId nextRvn userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, Some user), _ when validateNextRvn (Some user.Rvn) nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" userId user.Rvn nextRvn userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, None), UserCreated (_, userName, passwordSalt, passwordHash, userType) ->
        (userId, { Rvn = nextRvn ; UserName = userName ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash ; UserType = userType ; MustChangePasswordReason = FirstSignIn |> Some } |> Some) |> Ok
    | Ok (userId, None), _ -> // note: should never happen
        ifDebug (sprintf "Invalid initial UserEvent for %A -> %A" userId userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, Some user), UserCreated _ -> // note: should never happen
        ifDebug (sprintf "Invalid non-initial UserEvent for %A (%A) -> %A" userId user userEvent) UNEXPECTED_ERROR |> otherError
    | Ok (userId, Some user), PasswordChanged (_, passwordSalt, passwordHash) ->
        (userId, { user with Rvn = nextRvn ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash ; MustChangePasswordReason = None } |> Some) |> Ok
    | Ok (userId, Some user), PasswordReset (_, passwordSalt, passwordHash) ->
        let mustChangePasswordReason = match user.MustChangePasswordReason with | Some FirstSignIn -> FirstSignIn |> Some | Some _ | None -> MustChangePasswordReason.PasswordReset |> Some
        (userId, { user with Rvn = nextRvn ; PasswordSalt = passwordSalt ; PasswordHash = passwordHash ; MustChangePasswordReason = mustChangePasswordReason } |> Some) |> Ok
    | Ok (userId, Some user), UserTypeChanged (_, userType) ->
        (userId, { user with Rvn = nextRvn ; UserType = userType } |> Some) |> Ok
    | Error error, _ -> error |> Error

let private initializeUsers source (usersEvents:(UserId * (Rvn * UserEvent) list) list) =
    let source = sprintf "%s#initializeUsers" source
    let userDic = UserDic ()
    let results =
        usersEvents
        |> List.map (fun (userId, events) ->
            match events with
            | _ :: _ -> events |> List.fold (fun idAndUserResult (rvn, userEvent) -> applyUserEvent source idAndUserResult (rvn, userEvent)) (Ok (userId, None))
            | [] -> ifDebug (sprintf "No UserEvents for %A" userId) UNEXPECTED_ERROR |> otherError source) // note: should never happen
    results
    |> List.choose (fun idAndUserResult -> match idAndUserResult with | Ok (userId, Some user) -> (userId, user) |> Some | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (userId, user) -> userDic.Add (userId, user))
    let errors =
        results
        |> List.choose (fun idAndUserResult ->
            match idAndUserResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> ifDebug (sprintf "%s: applyUserEvent returned Ok (_, None)" source) UNEXPECTED_ERROR |> OtherError |> Some // note: should never happen
            | Error error -> error |> Some)
    userDic, errors

let private updateUser userId user (userDic:UserDic) = if userId |> userDic.ContainsKey then userDic.[userId] <- user

let private tryFindUser userId onError (userDic:UserDic) =
    if userId |> userDic.ContainsKey then (userId, userDic.[userId]) |> Ok else ifDebug (sprintf "%A does not exist" userId) UNEXPECTED_ERROR |> onError

let private tryApplyUserEvent source userId user nextRvn userEvent =
    match applyUserEvent source (Ok (userId, user)) (nextRvn, userEvent) with
    | Ok (_, Some user) -> (user, nextRvn, userEvent) |> Ok
    | Ok (_, None) -> ifDebug "applyUserEvent returned Ok (_, None)" UNEXPECTED_ERROR |> otherCmdError source // note: should never happen
    | Error otherError -> otherError |> OtherAuthCmdError |> Error

let private tryWriteUserEventAsync auditUserId rvn userEvent (user:User) = async {
    let! result = (auditUserId, rvn, userEvent) |> persistence.WriteUserEventAsync
    return match result with | Ok _ -> (userEvent.UserId, user) |> Ok | Error persistenceError -> persistenceError |> AuthCmdPersistenceError |> Error }

type Users () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> true |> reply.Reply ; return! awaitingStart ()
            | Start reply ->
                "Start when awaitingStart -> pendingOnUsersEventsRead" |> Info |> log
                () |> reply.Reply
                return! pendingOnUsersEventsRead ()
            | Reset _ -> "Reset when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUsersEventsRead _ -> "OnUsersEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleSignInCmd _ -> "HandleSignInCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleAutoSignInCmd _ -> "HandleAutoSignInCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleChangePasswordCmd _ -> "HandleChangePasswordCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleCreateUserCmd _ -> "HandleCreateUserCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleResetPasswordCmd _ -> "HandleResetPasswordCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleChangeUserTypeCmd _ -> "HandleChangeUserTypeCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnUsersEventsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! pendingOnUsersEventsRead ()
            | Start _ -> "Start when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | Reset _ -> "Reset when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | OnUsersEventsRead usersEvents ->
                let source = "OnUsersEventsRead"
                let users, errors = initializeUsers source usersEvents
                errors |> List.iter (fun (OtherError errorText) -> errorText |> Danger |> log)
                sprintf "%s (%i user/s) when pendingOnUsersEventsRead -> managingUsers (%i user/s)" source usersEvents.Length users.Count |> Info |> log
                let usersRead =
                    users
                    |> List.ofSeq
                    |> List.map (fun (KeyValue (userId, user)) -> { UserId = userId ; Rvn = user.Rvn ; UserName = user.UserName ; UserType = user.UserType })
                usersRead |> UsersRead |> broadcaster.Broadcast
                return! managingUsers users
            | HandleSignInCmd _ -> "HandleSignInCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleAutoSignInCmd _ -> "HandleAutoSignInCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleChangePasswordCmd _ -> "HandleChangePasswordCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleCreateUserCmd _ -> "HandleCreateUserCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleResetPasswordCmd _ -> "HandleResetPasswordCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead ()
            | HandleChangeUserTypeCmd _ -> "HandleChangeUserTypeCmd when pendingOnUsersEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersEventsRead () }
        and managingUsers userDic = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! managingUsers userDic
            | Start _ -> sprintf "Start when managingUsers (%i user/s)" userDic.Count |> IgnoredInput |> Agent |> log ; return! managingUsers userDic
            | Reset reply ->
                sprintf "Reset when managingUsers (%i user/s) -> pendingOnUsersEventsRead" userDic.Count |> Info |> log
                () |> reply.Reply
                return! pendingOnUsersEventsRead ()
            | OnUsersEventsRead _ -> sprintf "OnUsersEventsRead when managingUsers (%i user/s)" userDic.Count |> IgnoredInput |> Agent |> log ; return! managingUsers userDic
            | HandleSignInCmd (userName, password, reply) ->
                let source = "HandleSignInCmd"
                let invalidCredentialsError errorText = errorText |> InvalidCredentials |> Error
                sprintf "%s for %A when managingUsers (%i user/s)" source userName userDic.Count |> Verbose |> log
                let result =
                    match validateUserName [] userName with | None -> () |> Ok | Some errorText -> errorText |> Some |> invalidCredentialsError
                    |> Result.bind (fun _ -> match validatePassword password with | None -> () |> Ok | Some errorText -> errorText |> Some |> invalidCredentialsError)
                    |> Result.bind (fun _ ->
                        let matches = userDic |> List.ofSeq |> List.choose (fun (KeyValue (userId, user)) -> if user.UserName = userName then (userId, user) |> Some else None)
                        match matches with
                        | [ userId, user ] ->
                            if hash password user.PasswordSalt <> user.PasswordHash then ifDebug ("Incorrect password" |> Some) None |> invalidCredentialsError
                            else
                                if user.UserType = PersonaNonGrata then NOT_PERMITTED |> Some |> invalidCredentialsError
                                else
                                    let permissions = permissions userId user.UserType
                                    match (userId, permissions) |> toJwt with
                                    | Ok jwt -> authUser userId user.Rvn user.UserName user.UserType permissions jwt user.MustChangePasswordReason |> Ok
                                    | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> SignInCmdJwtError |> Error
                        | _ :: _ -> ifDebug (sprintf "Multiple matches for %A" userName |> Some) None |> invalidCredentialsError
                        | [] -> ifDebug (sprintf "No matches for %A" userName |> Some) None |> invalidCredentialsError)
                result |> logResult source (fun (authUser:AuthUser) -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! managingUsers userDic
            | HandleAutoSignInCmd (userId, permissionsFromJwt, reply) ->
                let source = "HandleAutoSignInCmd"
                sprintf "%s for %A when managingUsers (%i user/s)" source userId userDic.Count |> Verbose |> log
                let result =
                    userDic |> tryFindUser userId (OtherError >> OtherAutoSignInCmdError >> Error)
                    |> Result.bind (fun (userId, user) ->
                        if user.UserType = PersonaNonGrata then NOT_PERMITTED |> OtherError |> OtherAutoSignInCmdError |> Error
                        else
                            let permissions = permissions userId user.UserType
                            if permissions <> permissionsFromJwt then
                                let errorText = sprintf "HandleAutoSignInCmd: Permissions mismatch -> user %A vs. Jwt %A" permissions permissionsFromJwt
                                ifDebug errorText UNEXPECTED_ERROR |> OtherError |> OtherAutoSignInCmdError |> Error
                            else
                                match (userId, permissions) |> toJwt with
                                | Ok jwt -> authUser userId user.Rvn user.UserName user.UserType permissions jwt user.MustChangePasswordReason |> Ok
                                | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> AutoSignInCmdJwtError |> Error)
                result |> logResult source (fun (authUser:AuthUser) -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! managingUsers userDic
            | HandleChangePasswordCmd (changePasswordToken, auditUserId, currentRvn, Password password, reply) ->
                let source = "HandleChangePasswordCmd"
                sprintf "%s for %A (%A) when managingUsers (%i user/s)" source auditUserId currentRvn userDic.Count |> Verbose |> log
                let password = Password (password.Trim ())
                let result =
                    if changePasswordToken.UserId = auditUserId then () |> Ok else NotAuthorized |> AuthCmdAuthznError |> Error
                    |> Result.bind (fun _ -> userDic |> tryFindUser auditUserId (otherCmdError source))
                    |> Result.bind (fun (userId, user) -> match validatePassword password with | None -> (userId, user) |> Ok | Some errorText -> errorText |> otherCmdError source)
                    |> Result.bind (fun (userId, user) ->
                        if hash password user.PasswordSalt <> user.PasswordHash then (userId, user) |> Ok
                        else "New password must not be the same as your current password" |> otherCmdError source)
                    |> Result.bind (fun (userId, user) ->
                        let salt = salt ()
                        (auditUserId, salt, hash password salt) |> PasswordChanged |> tryApplyUserEvent source userId (Some user) (incrementRvn currentRvn))
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, user) -> user.Rvn) |> reply.Reply
                match result with | Ok (userId, user) -> userDic |> updateUser userId user | Error _ -> ()
                return! managingUsers userDic
            | HandleCreateUserCmd (createUserToken, auditUserId, userId, UserName userName, Password password, userType, reply) ->
                let source = "HandleCreateUserCmd"
                sprintf "%s for %A (%A %A) when managingUsers (%i user/s)" source userId userName userType userDic.Count |> Verbose |> log
                let userName = UserName (userName.Trim ())
                let password = Password (password.Trim ())
                let result =
                    if createUserToken.UserTypes |> List.contains userType then () |> Ok else NotAuthorized |> AuthCmdAuthznError |> Error
                    |> Result.bind (fun _ ->
                        if userId |> userDic.ContainsKey |> not then () |> Ok
                        else ifDebug (sprintf "%A already exists" userId) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun _ ->
                        let userNames = userDic |> List.ofSeq |> List.map (fun (KeyValue (_, user)) -> user.UserName)
                        match validateUserName userNames userName with | None -> () |> Ok | Some errorText -> errorText |> otherCmdError source)
                    |> Result.bind (fun _ -> match validatePassword password with | None -> () |> Ok | Some errorText -> errorText |> otherCmdError source)
                    |> Result.bind (fun _ ->
                        let salt = salt ()
                        (userId, userName, salt, hash password salt, userType) |> UserCreated |> tryApplyUserEvent source userId None initialRvn)
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (userId, user) -> sprintf "Audit%A %A %A" auditUserId userId user |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, user) -> user.UserName) |> reply.Reply
                match result with | Ok (userId, user) -> (userId, user) |> userDic.Add | Error _ -> ()
                return! managingUsers userDic
            | HandleResetPasswordCmd (resetPasswordToken, auditUserId, userId, currentRvn, Password password, reply) ->
                let source = "HandleResetPasswordCmd"
                sprintf "%s for %A (%A) when managingUsers (%i user/s)" source userId currentRvn userDic.Count |> Verbose |> log
                let password = Password (password.Trim ())
                let result =
                    userDic |> tryFindUser userId (otherCmdError source)
                    |> Result.bind (fun (userId, user) ->
                        match resetPasswordToken.UserTarget with
                        | NotSelf userTypes ->
                            if userId = auditUserId then NotAuthorized |> AuthCmdAuthznError |> Error
                            else if userTypes |> List.contains user.UserType |> not then NotAuthorized |> AuthCmdAuthznError |> Error
                            else (userId, user) |> Ok)
                    |> Result.bind (fun (userId, user) -> match validatePassword password with | None -> (userId, user) |> Ok | Some errorText -> errorText |> otherCmdError source)
                    // Note: Do not check if password is the same as the current password (cf. HandleChangePasswordCmd) as this would be a leak of security information.
                    |> Result.bind (fun (userId, user) ->
                        let salt = salt ()
                        (userId, salt, hash password salt) |> PasswordReset |> tryApplyUserEvent source userId (Some user) (incrementRvn currentRvn))
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, user) -> user.UserName) |> reply.Reply
                match result with | Ok (userId, user) -> userDic |> updateUser userId user | Error _ -> ()
                return! managingUsers userDic
            | HandleChangeUserTypeCmd (changeUserTypeToken, auditUserId, userId, currentRvn, userType, reply) ->
                let source = "HandleChangeUserTypeCmd"
                sprintf "%s %A for %A (%A) when managingUsers (%i user/s)" source userType userId currentRvn userDic.Count |> Verbose |> log
                let result =
                    userDic |> tryFindUser userId (otherCmdError source)
                    |> Result.bind (fun (userId, user) ->
                        match changeUserTypeToken.UserTarget with
                        | NotSelf userTypes ->
                            if userId = auditUserId then NotAuthorized |> AuthCmdAuthznError |> Error
                            else if userTypes |> List.contains user.UserType |> not then NotAuthorized |> AuthCmdAuthznError |> Error
                            else (userId, user) |> Ok)
                    |> Result.bind (fun (userId, user) ->
                        if changeUserTypeToken.UserTypes |> List.contains userType |> not then NotAuthorized |> AuthCmdAuthznError |> Error else (userId, user) |> Ok)
                    |> Result.bind (fun (userId, user) ->
                        if userType <> user.UserType then (userId, user) |> Ok
                        else ifDebug (sprintf "New UserType must not be the same as current UserType (%A)" user.UserType) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (userId, user) ->
                        (userId, userType) |> UserTypeChanged |> tryApplyUserEvent source userId (Some user) (incrementRvn currentRvn))
                let! result = match result with | Ok (user, rvn, userEvent) -> tryWriteUserEventAsync auditUserId rvn userEvent user | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (userId, user) -> Some (sprintf "Audit%A %A %A" auditUserId userId user)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, user) -> user.UserName) |> reply.Reply
                match result with | Ok (userId, user) -> userDic |> updateUser userId user | Error _ -> ()
                return! managingUsers userDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Entity Entity.Users |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member self.Start () =
        if IsAwaitingStart |> agent.PostAndReply then
            // Note: Not interested in UserEventWritten events (since Users agent causes these in the first place - and will already have maintained its internal state accordingly).
            let onEvent = (fun event -> match event with | UsersEventsRead usersEvents -> usersEvents |> self.OnUsersEventsRead | _ -> ())
            let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
            sprintf "agent subscribed to UsersEventsRead broadcasts -> %A" subscriptionId |> Info |> log
            Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
        else
            "agent has already been started" |> Info |> log
    member __.Reset () = Reset |> agent.PostAndReply // note: not async (since need to reset agents deterministically)
    member __.OnUsersEventsRead usersEvents = usersEvents |> OnUsersEventsRead |> agent.Post
    member __.HandleSignInCmdAsync (userName, password) = (fun reply -> (userName, password, reply) |> HandleSignInCmd) |> agent.PostAndAsyncReply
    member __.HandleAutoSignInCmdAsync (userId, permissionsFromJwt) = (fun reply -> (userId, permissionsFromJwt, reply) |> HandleAutoSignInCmd) |> agent.PostAndAsyncReply
    member __.HandleCreateUserCmdAsync (token, auditUserId, userId, userName, password, userType) =
        (fun reply -> (token, auditUserId, userId, userName, password, userType, reply) |> HandleCreateUserCmd) |> agent.PostAndAsyncReply
    member __.HandleChangePasswordCmdAsync (token, auditUserId, currentRvn, password) =
        (fun reply -> (token, auditUserId, currentRvn, password, reply) |> HandleChangePasswordCmd) |> agent.PostAndAsyncReply
    member __.HandleResetPasswordCmdAsync (token, auditUserId, userId, currentRvn, password) =
        (fun reply -> (token, auditUserId, userId, currentRvn, password, reply) |> HandleResetPasswordCmd) |> agent.PostAndAsyncReply
    member __.HandleChangeUserTypeCmdAsync (token, auditUserId, userId, currentRvn, userType) =
        (fun reply -> (token, auditUserId, userId, currentRvn, userType, reply) |> HandleChangeUserTypeCmd) |> agent.PostAndAsyncReply

let users = Users ()
