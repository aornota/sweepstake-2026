module Aornota.Sweepstake2026.Server.Agents.Projections.Users

(* Broadcasts: SendMsg
   Subscribes: UsersRead
               UserEventWritten (UserCreated | PasswordChanged | PasswordReset | UserTypeChanged)
               UserSignedIn | UserActivity | UserSignedOut
               ConnectionsSignedOut | Disconnected *)

open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Common.DeltaHelper
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Events.UserEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private UserAdminInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnUsersRead of usersRead : UserRead list
    | OnUserCreated of userId : UserId * rvn : Rvn * userName : UserName * userType : UserType
    | OnUserTypeChanged of userId : UserId * rvn : Rvn * userType : UserType
    | OnOtherUserEvent of userId : UserId * rvn : Rvn
    | OnUserSignedInOrOut of userId : UserId * signedIn : bool
    | OnUserActivity of userId : UserId
    | SignOutConnections of connectionIds : ConnectionId list
    | RemoveConnection of connectionId : ConnectionId
    | HandleInitializeUsersProjectionUnauthQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<UserUnauthDto list, OtherError<string>>>
    | HandleInitializeUsersProjectionAuthQry of connectionId : ConnectionId * userId : UserId
        * reply : AsyncReplyChannel<Result<UserDto list, AuthQryError<string>>>

type private User = { Rvn : Rvn ; UserName : UserName ; UserType : UserType ; LastActivity : DateTimeOffset option }
type private UserDic = Dictionary<UserId, User>

type private Projectee = { LastRvn : Rvn ; UserId : UserId option }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { UserDic : UserDic }

type private StateChangeType =
    | Initialization of userDic : UserDic
    | UserChange of userDic : UserDic * state : State

type private UserUnauthDtoDic = Dictionary<UserId, UserUnauthDto>
type private UserDtoDic = Dictionary<UserId, UserDto>

let [<Literal>] private LAST_ACTIVITY_THROTTLE = 10.<second>

let private log category = (Projection Users, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private userUnauthDto (userId, user:User) = { UserId = userId ; UserName = user.UserName }

let private userDto (userId, user:User) : UserDto = (userId, user) |> userUnauthDto, { Rvn = user.Rvn ; UserType = user.UserType ; LastActivity = user.LastActivity } |> Some

let private userUnauthDtoDic (userDic:UserDic) =
    let userUnauthDtoDic = UserUnauthDtoDic ()
    userDic |> List.ofSeq |> List.iter (fun (KeyValue (userId, user)) -> (userId, (userId, user) |> userUnauthDto) |> userUnauthDtoDic.Add)
    userUnauthDtoDic

let private userDtoDic (userDic:UserDic) =
    let userDtoDic = UserDtoDic ()
    userDic |> List.ofSeq |> List.iter (fun (KeyValue (userId, user)) -> (userId, (userId, user) |> userDto) |> userDtoDic.Add)
    userDtoDic

let private usersProjectionUnauth state = state.UserDic |> List.ofSeq |> List.map (fun (KeyValue (userId, user)) -> (userId, user) |> userUnauthDto)

let private usersProjectionAuth (_:UserId ) state = state.UserDic |> List.ofSeq |> List.map (fun (KeyValue (userId, user)) -> (userId, user) |> userDto)

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendUserUnauthDtoDelta (projecteeDic:ProjecteeDic) userUnauthDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        match projectee.UserId with
        | Some _ -> ()
        | None ->
            let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
            sprintf "sendUserDtoDeltaUnauth -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
            (projectee.LastRvn, userUnauthDtoDelta) |> UsersDeltaUnauthMsg |> UsersProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ]
            (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private sendUserDtoDelta (projecteeDic:ProjecteeDic) userDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        match projectee.UserId with
        | Some _ ->
            let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
            sprintf "sendUserDtoDeltaAuth -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
            (projectee.LastRvn, userDtoDelta) |> UsersDeltaAuthMsg |> UsersProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ]
            (connectionId, projectee) |> updatedProjecteeDic.Add
        | None -> ())
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private sendUserSignedInOrOut (projecteeDic:ProjecteeDic) (userName, signedIn) =
    let connectionIds = projecteeDic |> List.ofSeq |> List.choose (fun (KeyValue (connectionId, projectee)) -> match projectee.UserId with | Some _ -> connectionId |> Some | None -> None)
    userName |> (if signedIn then UserSignedInAuthMsg else UserSignedOutAuthMsg) |> UsersProjectionMsg |> ServerAppMsg |> sendMsg connectionIds

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization userDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { UserDic = UserDic userDic }
        | UserChange (userDic, state) ->
            let previousUserUnauthDtoDic = state.UserDic |> userUnauthDtoDic
            let userUnauthDtoDic = userDic |> userUnauthDtoDic
            let userUnauthDtoDelta = userUnauthDtoDic |> delta previousUserUnauthDtoDic
            if userUnauthDtoDelta |> isEmpty |> not then
                sprintf "%s -> UserUnauthDto delta %A -> %i (potential) projectee/s" source userUnauthDtoDelta projecteeDic.Count |> Info |> log
                userUnauthDtoDelta |> sendUserUnauthDtoDelta projecteeDic
            let previousUserDtoDic = state.UserDic |> userDtoDic
            let userDtoDic = userDic |> userDtoDic
            let userDtoDelta = userDtoDic |> delta previousUserDtoDic
            if userDtoDelta |> isEmpty |> not then
                sprintf "%s -> UserDto delta %A -> %i (potential) projectee/s" source userDtoDelta projecteeDic.Count |> Info |> log
                userDtoDelta |> sendUserDtoDelta projecteeDic
            if userUnauthDtoDelta |> isEmpty |> not || userDtoDelta |> isEmpty |> not then
                sprintf "%s -> updated" source |> Info |> log
                { state with UserDic = UserDic userDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

type Users () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingOnUsersRead (0 chat users) (0 chat messages) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingOnUsersRead ()
            | OnUsersRead _ -> "OnUsersRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserCreated _ -> "OnUserCreated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserTypeChanged _ -> "OnUserTypeChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnOtherUserEvent _ -> "OnOtherUserEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserSignedInOrOut _ -> "OnUserSignedInOrOut when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnUserActivity _ -> "OnUserActivity when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | SignOutConnections _ -> "SignOutConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnection _ -> "RemoveConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeUsersProjectionUnauthQry _ -> "HandleInitializeUsersProjectionUnauthQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeUsersProjectionAuthQry _ -> "HandleInitializeUsersProjectionAuthQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnUsersRead () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUsersRead usersRead ->
                let source = "OnUsersRead"
                sprintf "%s (%i user/s) when pendingOnUsersRead" source usersRead.Length |> Info |> log
                let userDic = UserDic ()
                usersRead |> List.iter (fun userRead -> (userRead.UserId, { Rvn = userRead.Rvn ; UserName = userRead.UserName ; UserType = userRead.UserType ; LastActivity = None }) |> userDic.Add)
                let projecteeDic = ProjecteeDic ()
                let state = userDic |> Initialization |> updateState source projecteeDic
                return! projectingUsers state userDic projecteeDic
            | OnUserCreated _ -> "OnUserCreated when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserTypeChanged _ -> "OnUserTypeChanged when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnOtherUserEvent _ -> "OnOtherUserEvent when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserSignedInOrOut _ -> "OnUserSignedInOrOut when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | OnUserActivity _ -> "OnUserActivity when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | SignOutConnections _ -> "SignOutConnections when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | RemoveConnection _ -> "RemoveConnection when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | HandleInitializeUsersProjectionUnauthQry _ -> "HandleInitializeUsersProjectionUnauthQry when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead ()
            | HandleInitializeUsersProjectionAuthQry _ -> "HandleInitializeUsersProjectionAuthQry when pendingOnUsersRead" |> IgnoredInput |> Agent |> log ; return! pendingOnUsersRead () }
        and projectingUsers state userDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingUsers" |> IgnoredInput |> Agent |> log ; return! projectingUsers state userDic projecteeDic
            | OnUsersRead _ -> "OnUsersRead when projectingUsers" |> IgnoredInput |> Agent |> log ; return! projectingUsers state userDic projecteeDic
            | OnUserCreated (userId, rvn, userName, userType) ->
                let source = "OnUserCreated"
                sprintf "%s (%A %A %A) when projectingUsers (%i user/s) (%i projectee/s)" source userId userName userType userDic.Count projecteeDic.Count |> Info |> log
                if userId |> userDic.ContainsKey |> not then // note: silently ignore already-known userId (should never happen)
                    (userId, { UserName = userName ; Rvn = rvn ; UserType = userType ; LastActivity = None }) |> userDic.Add
                sprintf "%s when projectingUsers -> %i user/s)" source userDic.Count |> Info |> log
                let state = (userDic, state) |> UserChange |> updateState source projecteeDic
                return! projectingUsers state userDic projecteeDic
            | OnUserTypeChanged (userId, rvn, userType) ->
                let source = "OnUserTypeChanged"
                sprintf "%s (%A %A) when projectingUsers (%i user/s) (%i projectee/s)" source userId userType userDic.Count projecteeDic.Count |> Info |> log
                if userId |> userDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                    let user = userDic.[userId]
                    userDic.[userId] <- { user with Rvn = rvn ; UserType = userType }
                let state = (userDic, state) |> UserChange |> updateState source projecteeDic
                return! projectingUsers state userDic projecteeDic
            | OnOtherUserEvent (userId, rvn) ->
                let source = "OnOtherUserEvent"
                sprintf "%s (%A) when projectingUsers (%i user/s) (%i projectee/s)" source userId userDic.Count projecteeDic.Count |> Info |> log
                if userId |> userDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                    let user = userDic.[userId]
                    userDic.[userId] <- { user with Rvn = rvn }
                let state = (userDic, state) |> UserChange |> updateState source projecteeDic
                return! projectingUsers state userDic projecteeDic
            | OnUserSignedInOrOut (userId, signedIn) ->
                let source = "OnUserSignedInOrOut"
                sprintf "%s (%A %b) when projectingUsers (%i user/s) (%i projectee/s)" source userId signedIn userDic.Count projecteeDic.Count |> Info |> log
                if userId |> userDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                    let user = userDic.[userId]
                    (user.UserName, signedIn) |> sendUserSignedInOrOut projecteeDic
                    userDic.[userId] <- { user with LastActivity = match signedIn with | true -> DateTimeOffset.UtcNow |> Some | false -> None }
                let state = (userDic, state) |> UserChange |> updateState source projecteeDic
                return! projectingUsers state userDic projecteeDic
            | OnUserActivity userId ->
                let source = "OnUserActivity"
                sprintf "%s (%A) when projectingUsers (%i user/s) (%i projectee/s)" source userId userDic.Count projecteeDic.Count |> Info |> log
                let updated =
                    if userId |> userDic.ContainsKey then // note: silently ignore unknown userId (should never happen)
                        let user = userDic.[userId]
                        let now = DateTimeOffset.UtcNow
                        let throttled = match user.LastActivity with | Some lastActivity -> (now - lastActivity).TotalSeconds * 1.<second> < LAST_ACTIVITY_THROTTLE | None -> false
                        if throttled |> not then userDic.[userId] <- { user with LastActivity = now |> Some }
                        else sprintf "%s throttled for %A" source userId |> Info |> log
                        throttled |> not
                    else false
                let state = if updated then (userDic, state) |> UserChange |> updateState source projecteeDic else state
                return! projectingUsers state userDic projecteeDic
            | SignOutConnections connectionIds ->
                let source = "SignOutConnections"
                sprintf "%s (%A) when projectingUsers (%i user/s) (%i projectee/s)" source connectionIds userDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId ->
                    if connectionId |> projecteeDic.ContainsKey then // note: silently ignore unknown connectionIds
                        let projectee = projecteeDic.[connectionId]
                        projecteeDic.[connectionId] <- { projectee with UserId = None })
                return! projectingUsers state userDic projecteeDic
            | RemoveConnection connectionId ->
                let source = "RemoveConnection"
                sprintf "%s (%A) when projectingUsers (%i user/s) (%i projectee/s)" source connectionId userDic.Count projecteeDic.Count |> Info |> log
                if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore // note: silently ignore unknown connectionId
                sprintf "%s when projectingUsers -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingUsers state userDic projecteeDic
            | HandleInitializeUsersProjectionUnauthQry (connectionId, reply) ->
                let source = "HandleInitializeUsersProjectionUnauthQry"
                sprintf "%s for %A when projectingUsers (%i user/s) (%i projectee/s)" source connectionId userDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn ; UserId = None }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingUsers -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> usersProjectionUnauth |> Ok
                result |> logResult source (fun userDtos -> sprintf "%i user/s" userDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingUsers state userDic projecteeDic
            | HandleInitializeUsersProjectionAuthQry (connectionId, userId, reply) ->
                let source = "HandleInitializeUsersProjectionAuthQry"
                sprintf "%s for %A when projectingUsers (%i user/s) (%i projectee/s)" source connectionId userDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn ; UserId = userId |> Some }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingUsers -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> usersProjectionAuth userId |> Ok
                result |> logResult source (fun userDtos -> sprintf "%i user/s" userDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingUsers state userDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Users |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | UsersRead usersRead -> usersRead |> OnUsersRead |> agent.Post
            | UserEventWritten (rvn, userEvent) ->
                match userEvent with
                | UserCreated (userId, userName, _, _, userType) -> (userId, rvn, userName, userType) |> OnUserCreated |> agent.Post
                | PasswordChanged (userId, _, _) -> (userId, rvn) |> OnOtherUserEvent |> agent.Post
                | PasswordReset (userId, _, _) -> (userId, rvn) |> OnOtherUserEvent |> agent.Post
                | UserTypeChanged (userId, userType) -> (userId, rvn, userType) |> OnUserTypeChanged |> agent.Post
            | UserSignedIn userId -> (userId, true) |> OnUserSignedInOrOut |> agent.Post
            | UserSignedOut userId -> (userId, false) |> OnUserSignedInOrOut |> agent.Post
            | UserActivity userId -> userId |> OnUserActivity |> agent.Post
            | ConnectionsSignedOut connectionIds -> connectionIds |> SignOutConnections |> agent.Post
            | Disconnected connectionId -> connectionId |> RemoveConnection |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to Tick | UsersRead | UserEventWritten | UserSignedIn | UserApi | UserSignedOut | ConnectionsSignedOut | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeUsersProjectionUnauthQryAsync connectionId =
        (fun reply -> (connectionId, reply) |> HandleInitializeUsersProjectionUnauthQry) |> agent.PostAndAsyncReply
    member __.HandleInitializeUsersProjectionAuthQryAsync (connectionId, userId) =
        (fun reply -> (connectionId, userId, reply) |> HandleInitializeUsersProjectionAuthQry) |> agent.PostAndAsyncReply

let users = Users ()
