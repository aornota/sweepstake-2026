module Aornota.Sweepstake2026.Server.Agents.Connections

(* Broadcasts: UserSignedIn | UserActivity | UserSignedOut
               ConnectionsSignedOut | Disconnected
   Subscribes: SendMsg *)

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Json
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Server.Agents
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Authorization
open Aornota.Sweepstake2026.Server.Common.Helpers
open Aornota.Sweepstake2026.Server.Common.JsonConverter
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Jwt
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic
open System.Net.WebSockets
open System.Text
open System.Threading

open FSharp.Control.Tasks.ContextInsensitive

type private ConnectionsInput =
    | Start of serverStarted : DateTimeOffset * reply : AsyncReplyChannel<unit>
    | OnSendMsg of serverMsg : ServerMsg * connectionIds : ConnectionId list
    | AddConnection of connectionId : ConnectionId * ws : WebSocket
    | RemoveConnection of connectionId : ConnectionId
    | OnReceiveUiMsgError of connectionId : ConnectionId * exn : exn
    | OnDeserializeUiMsgError of connectionId : ConnectionId * exn : exn
    | HandleUiMsg of connectionId : ConnectionId * uiMsg : UiMsg

type private SignedInUser = { UserName : UserName ; Permissions : Permissions ; UserTokens : UserTokens ; LastApi : DateTimeOffset }

type private SignedInUserDic = Dictionary<UserId, SignedInUser>

type private SignedInSession = UserId * SessionId

type private Connection = WebSocket * SignedInSession option

type private ConnectionDic = Dictionary<ConnectionId, Connection>

let private log category = (Connections, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private encoding = Encoding.UTF8

let private hasConnections userId (connectionDic:ConnectionDic) =
    let forUserId =
        connectionDic
        |> List.ofSeq
        |> List.filter (fun (KeyValue (_, (_, signedInSession))) -> match signedInSession with | Some (otherUserId, _) when otherUserId = userId -> true | Some _ | None -> false)
    forUserId.Length > 0

let private addSignedInUser (authUser:AuthUser) (signedInUserDic:SignedInUserDic) = async {
    if authUser.UserId |> signedInUserDic.ContainsKey |> not then // note: silently ignore if already in signedInUsers
        let signedInUser = { UserName = authUser.UserName ; Permissions = authUser.Permissions ; UserTokens = authUser.Permissions |> UserTokens ; LastApi = DateTimeOffset.UtcNow }
        (authUser.UserId, signedInUser) |> signedInUserDic.Add
        authUser.UserId |> UserSignedIn |> broadcaster.Broadcast }

let private removeSignedInUser userId (signedInUserDic:SignedInUserDic) =
    if userId |> signedInUserDic.Remove then userId |> UserSignedOut |> broadcaster.Broadcast
    // Note: Silently ignore non-signed-in user.

let private removeConnection connectionId (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) =
    match connectionId |> connectionDic.TryGetValue with
    | true, (_, signedInSession) ->
        connectionId |> connectionDic.Remove |> ignore
        connectionId |> Disconnected |> broadcaster.Broadcast
        match signedInSession with
        | Some (userId, _) -> if connectionDic |> hasConnections userId |> not then signedInUserDic |> removeSignedInUser userId
        | None -> ()
    | false, _ -> // note: should never happen
        sprintf "removeConnection -> %A not found in connections" connectionId |> Danger |> log

let private sendMsg (serverMsg:ServerMsg) connectionIds (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    let (Json json) = serverMsg |> toJson
    let buffer = encoding.GetBytes json
    let segment = ArraySegment<byte> (buffer)
    let rec send (recipients:(ConnectionId * WebSocket) list) failedConnectionIds = async {
        let trySendMessage (ws:WebSocket) = task {
            if ws.State = WebSocketState.Open then
                try
                    do! ws.SendAsync (segment, WebSocketMessageType.Text, true, CancellationToken.None)
                    return true
                with _ -> return false
            else return false }
        match recipients with
        | (connectionId, ws) :: t ->
            let! success = trySendMessage ws |> Async.AwaitTask
            return! send t (if success then failedConnectionIds else connectionId :: failedConnectionIds)
        | [] -> return failedConnectionIds }
    let recipients =
        connectionIds |> List.choose (fun connectionId -> match connectionId |> connectionDic.TryGetValue with | true, (ws, _) -> (connectionId, ws) |> Some | false, _ -> None)
    let! failedConnectionIds = [] |> send recipients
    failedConnectionIds |> List.iter (fun connectionId -> (connectionDic, signedInUserDic) |> removeConnection connectionId)
    return () }

let private autoSignOut autoSignOutReason userId onlySessionId exceptConnectionId (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    let otherConnections =
        connectionDic |> List.ofSeq |> List.choose (fun (KeyValue (connectionId, (ws, (signedInSession)))) ->
            let signOut =
                match signedInSession with
                | Some (otherUserId, sessionId) ->
                    if otherUserId = userId then
                        match onlySessionId with
                        | Some onlySessionId when onlySessionId = sessionId -> true
                        | Some _ -> false
                        | None -> true
                    else false
                | None -> false
            let signOut =
                if signOut then
                    match exceptConnectionId with
                    | Some exceptConnectionId when exceptConnectionId <> connectionId -> true // note: this check should be superfluous (as calling code will have ensured that exceptConnectionId no longer has a SignedInSession)
                    | Some _ -> false
                    | None -> true
                else signOut
            if signOut then (ws, connectionId) |> Some
            else None)
    match otherConnections with
    | _ :: _ ->
        otherConnections |> List.iter (fun (ws, otherConnectionId) -> connectionDic.[otherConnectionId] <- (ws, None))
        let otherConnectionIds = otherConnections |> List.map snd
        otherConnectionIds |> ConnectionsSignedOut |> broadcaster.Broadcast
        do! (connectionDic, signedInUserDic) |> sendMsg (autoSignOutReason |> AutoSignOutMsg |> ServerAppMsg) otherConnectionIds
    | [] -> ()
    // Note: No need to broadcast UserSignedOut (if appropriate) since handled by removeSignedInUser.
    if connectionDic |> hasConnections userId |> not then signedInUserDic |> removeSignedInUser userId
    return () }

let private ifConnection source connectionId fWithWs (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    match connectionId |> connectionDic.TryGetValue with
    | true, (ws, _) ->
        do! ws |> fWithWs
    | false, _ -> // note: should never happen
        sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log }

let private ifNoSignedInSession source connectionId fWithWs (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    match connectionId |> connectionDic.TryGetValue with
    | true, (ws, signedInSession) ->
        match signedInSession with
        | Some _ -> // note: should never happen
            sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A already has a signed-in session" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log
        | None -> do! ws |> fWithWs
    | false, _ -> // note: should never happen
        sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log }

let private ifSignedInSession source connectionId fWithConnection (connectionDic:ConnectionDic, signedInUserDic:SignedInUserDic) = async {
    match connectionId |> connectionDic.TryGetValue with
    | true, (ws, signedInSession) ->
        match signedInSession with
        | Some signedInSession -> do! (ws, signedInSession) |> fWithConnection
        | None -> // note: should never happen
            sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A does not have a signed-in session" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log
    | false, _ -> // note: should never happen
        sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" source connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log }

let private tokensForAuthApi source (otherError, jwtError) updateLastApi userId jwt (signedInUserDic:SignedInUserDic) =
    // Note: If successful [and if requested], updates SignedInUser.LastApi and broadcasts UserActivity.
    match jwt |> fromJwt with
    | Ok (userIdFromJwt, permissionsFromJwt) ->
        if userIdFromJwt <> userId then // note: should never happen
            let errorText = sprintf "%s -> UserId mismatch -> SignedInSession %A vs. userIdFromJwt %A" source userId userIdFromJwt
            ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
        else
            match userIdFromJwt |> signedInUserDic.TryGetValue with
            | true, signedInUser ->
                if signedInUser.Permissions <> permissionsFromJwt then // note: should never happen
                    let errorText = sprintf "%s -> Permissions mismatch -> SignedInUser %A vs. permissionsFromJwt %A" source signedInUser.Permissions permissionsFromJwt
                    ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
                else
                    if updateLastApi then
                        let signedInUser = { signedInUser with LastApi = DateTimeOffset.UtcNow }
                        signedInUserDic.[userIdFromJwt] <- signedInUser
                        userIdFromJwt |> UserActivity |> broadcaster.Broadcast
                    signedInUser.UserTokens |> Ok
            | false, _ -> // note: should never happen
                let errorText = sprintf "%s -> No SignedInUser for %A" source userIdFromJwt
                ifDebug errorText UNEXPECTED_ERROR |> OtherError |> otherError |> Error
    | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> jwtError |> Error

let private tokensForAuthCmdApi source updateLastApi userId jwt signedInUsers = tokensForAuthApi source (OtherAuthCmdError, AuthCmdJwtError) updateLastApi userId jwt signedInUsers
let private tokensForAuthQryApi source userId jwt signedInUsers = tokensForAuthApi source (OtherAuthQryError, AuthQryJwtError) true userId jwt signedInUsers

type Connections () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start (serverStarted, reply) ->
                "Start when awaitingStart -> managingConnections (0 connections)" |> Info |> log
                () |> reply.Reply
                return! managingConnections serverStarted (ConnectionDic ()) (SignedInUserDic ())
            | AddConnection _ -> "AddConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnection _ -> "RemoveConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnReceiveUiMsgError _ -> "OnReceiveUiMsgError when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDeserializeUiMsgError _ -> "OnDeserializeUiMsgError when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleUiMsg _ -> "HandleUiMsg when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSendMsg _ -> "SendMsg when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and managingConnections serverStarted connectionDic signedInUserDic = async {
            let! input = inbox.Receive ()
            (* TEMP-NMB...
            do! ifDebugSleepAsync 100 500 *)
            match input with
            | Start _ -> "Start when managingConnections" |> IgnoredInput |> Agent |> log ; return! managingConnections serverStarted connectionDic signedInUserDic
            | OnSendMsg (serverMsg, connectionIds) ->
                sprintf "SendMsg %A (%i ConnectionId/s) when managingConnections (%i connection/s) (%i signed-in user/s)" serverMsg connectionIds.Length connectionDic.Count signedInUserDic.Count |> Verbose |> log
                do! (connectionDic, signedInUserDic) |> sendMsg serverMsg connectionIds
                return! managingConnections serverStarted connectionDic signedInUserDic
            | AddConnection (connectionId, ws) ->
                sprintf "AddConnection %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                let otherConnectionCount, signedInUserCount = connectionDic.Count, signedInUserDic.Count
                if connectionId |> connectionDic.ContainsKey then // note: should never happen
                    sprintf "AddConnection when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (already in use)" connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log
                else
                    (connectionId, (ws, None)) |> connectionDic.Add
                    let serverMsg = (serverStarted, otherConnectionCount, signedInUserCount) |> ConnectedMsg |> ServerAppMsg
                    do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                return! managingConnections serverStarted connectionDic signedInUserDic
            | RemoveConnection connectionId ->
                sprintf "RemoveConnection %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                match connectionId |> connectionDic.TryGetValue with
                | true, _ ->
                    // Note: No need to broadcast Disconnected - and UserSignedOut (if appropriate) - since handled by removeConnection.
                    (connectionDic, signedInUserDic) |> removeConnection connectionId
                | false, _ -> // note: should never happen
                    sprintf "RemoveConnection when managingConnections (%i connection/s) (%i signed-in user/s) -> %A is not valid (not in use)" connectionDic.Count signedInUserDic.Count connectionId |> Danger |> log
                return! managingConnections serverStarted connectionDic signedInUserDic
            | OnReceiveUiMsgError (connectionId, exn) ->
                sprintf "OnReceiveUiMsgError for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Danger |> log
                let serverMsg = exn.Message |> ReceiveUiMsgError |> ServerUiMsgErrorMsg |> ServerAppMsg
                do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                return! managingConnections serverStarted connectionDic signedInUserDic
            | OnDeserializeUiMsgError (connectionId, exn) ->
                sprintf "OnDeserializeUiMsgError for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Danger |> log
                let serverMsg = exn.Message |> DeserializeUiMsgError |> ServerUiMsgErrorMsg |> ServerAppMsg
                do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                return! managingConnections serverStarted connectionDic signedInUserDic
            | HandleUiMsg (connectionId, uiMsg) ->
                match uiMsg with
                | Wiff -> // note: logged - but otherwise ignored
                    sprintf "Wiff for %A when managingConnections (%i connection/s) (%i signed-in user/s)" connectionId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiUnauthMsg (UiUnauthAppMsg (SignInCmd (sessionId, userName, password))) ->
                    let source = "SignInCmd"
                    sprintf "%s for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source userName connectionId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun ws -> async {
                        let! result =
                            if debugFakeError () then sprintf "Fake %s error -> %A (%A)" source userName sessionId |> OtherError |> OtherSignInCmdError |> Error |> thingAsync
                            else (userName, password) |> Entities.Users.users.HandleSignInCmdAsync
                        let result =
                            result
                            |> Result.bind (fun authUser ->
                                // Note: No need to broadcast UserSignedIn (if appropriate) since handled by addSignedInUser.
                                signedInUserDic |> addSignedInUser authUser |> Async.RunSynchronously
                                connectionDic.[connectionId] <- (ws, (authUser.UserId, sessionId) |> Some)
                                authUser |> Ok)
                        let serverMsg = result |> SignInCmdResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun authUser -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifNoSignedInSession source connectionId fWithWs
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiUnauthMsg (UiUnauthAppMsg (AutoSignInCmd (sessionId, jwt))) ->
                    let source = "AutoSignInCmd"
                    sprintf "%s for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source sessionId jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun ws -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAutoSignInCmdError |> Error
                            else
                                // Note: Similar logic to tokensForAuthApi.
                                match jwt |> fromJwt with
                                | Ok (userIdFromJwt, permissionsFromJwt) ->
                                    match userIdFromJwt |> signedInUserDic.TryGetValue with
                                    | true, signedInUser ->
                                        if signedInUser.Permissions <> permissionsFromJwt then
                                            let errorText = sprintf "%s -> Permissions mismatch -> SignedInUser %A vs. permissionsFromJwt %A" source signedInUser.Permissions permissionsFromJwt
                                            ifDebug errorText UNEXPECTED_ERROR |> OtherError |> OtherAutoSignInCmdError |> Error
                                        else (userIdFromJwt, permissionsFromJwt) |> Ok
                                    | false, _ -> (userIdFromJwt, permissionsFromJwt) |> Ok
                                | Error errorText -> ifDebug errorText UNEXPECTED_ERROR |> JwtError |> AutoSignInCmdJwtError |> Error
                        let! result =
                            match result with
                            | Ok (userIdFromJwt, permissionsFromJwt) -> (userIdFromJwt, permissionsFromJwt) |> Entities.Users.users.HandleAutoSignInCmdAsync
                            | Error error -> error |> Error |> thingAsync
                        let result =
                            result
                            |> Result.bind (fun authUser ->
                                // Note: No need to broadcast UserSignedIn (if appropriate) since handled by addSignedInUser.
                                signedInUserDic |> addSignedInUser authUser |> Async.RunSynchronously
                                connectionDic.[connectionId] <- (ws, (authUser.UserId, sessionId) |> Some)
                                authUser |> Ok)
                        let serverMsg = result |> AutoSignInCmdResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun authUser -> sprintf "%A %A" authUser.UserName authUser.UserId |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifNoSignedInSession source connectionId fWithWs
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiUnauthMsg (UiUnauthAppMsg InitializeUsersProjectionUnauthQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "InitializeUsersProjectionUnauthQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun _ -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source connectionId |> OtherError |> Error
                            else () |> Ok
                        let! result =
                            match result with
                            | Ok _ -> connectionId |> Projections.Users.users.HandleInitializeUsersProjectionUnauthQryAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeUsersProjectionUnauthQryResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun userDtos -> sprintf "%i user/s" userDtos.Length |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifConnection source connectionId fWithWs
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiUnauthMsg (UiUnauthAppMsg InitializeSquadsProjectionQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "InitializeSquadsProjectionQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun _ -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source connectionId |> OtherError |> Error
                            else () |> Ok
                        let! result =
                            match result with
                            | Ok _ -> connectionId |> Projections.Squads.squads.HandleInitializeSquadsProjectionQryAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeSquadsProjectionQryResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun squadDtos -> sprintf "%i squad/s" squadDtos.Length |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifConnection source connectionId fWithWs
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiUnauthMsg (UiUnauthAppMsg InitializeFixturesProjectionQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "InitializeFixturesProjectionQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun _ -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source connectionId |> OtherError |> Error
                            else () |> Ok
                        let! result =
                            match result with
                            | Ok _ -> connectionId |> Projections.Fixtures.fixtures.HandleInitializeFixturesProjectionQryAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeFixturesProjectionQryResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun fixtureDtos -> sprintf "%i fixtures/s" fixtureDtos.Length |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifConnection source connectionId fWithWs
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiUnauthMsg (UiUnauthNewsMsg InitializeNewsProjectionQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "InitializeNewsProjectionQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun _ -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source connectionId |> OtherError |> Error
                            else () |> Ok
                        let! result =
                            match result with
                            | Ok _ -> connectionId |> Projections.News.news.HandleInitializeNewsProjectionQryAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeNewsProjectionQryResult |> ServerNewsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun (postDtos, hasMorePosts) -> sprintf "%i post/s (%b)" postDtos.Length hasMorePosts |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifConnection source connectionId fWithWs
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiUnauthMsg (UiUnauthNewsMsg MorePostsQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "MorePostsQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithWs = (fun _ -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source connectionId |> OtherError |> Error
                            else () |> Ok
                        let! result =
                            match result with
                            | Ok _ -> connectionId |> Projections.News.news.HandleMorePostsQryAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> MorePostsQryResult |> ServerNewsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun (_, postDtos, hasMorePosts) -> sprintf "%i post/s (%b)" postDtos.Length hasMorePosts |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifConnection source connectionId fWithWs
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UserNonApiActivity) ->
                    let source = "UserNonApiActivity"
                    sprintf "%s for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async { userId |> UserActivity |> broadcaster.Broadcast })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthAppMsg SignOutCmd) ->
                    let source = "SignOutCmd"
                    sprintf "%s for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (ws, (userId, sessionId)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source false userId jwt // note: if successful, does *not* update SignedInUser.LastApi (nor broadcast UserActivity)
                            |> Result.bind (fun _ ->
                                connectionDic.[connectionId] <- (ws, None) // note: connectionId will be in connections (otherwise ifSignedInSession would bypass fWithConnection)
                                [ connectionId ] |> ConnectionsSignedOut |> broadcaster.Broadcast
                                () |> Ok)
                        let serverMsg = result |> SignOutCmdResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun _ -> sprintf "%A" userId |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                        match result with
                        | Ok _ -> do! (connectionDic, signedInUserDic) |> autoSignOut None userId (sessionId |> Some) (connectionId |> Some)
                        | Error _ -> () })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthAppMsg (ChangePasswordCmd (currentRvn, password))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "ChangePasswordCmd"
                    sprintf "%s (%A) for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source currentRvn jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChangePasswordToken with
                                | Some changePasswordToken -> (changePasswordToken, userId, currentRvn, password) |> Entities.Users.users.HandleChangePasswordCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ChangePasswordCmdResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthAppMsg InitializeUsersProjectionAuthQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "InitializeUsersProjectionAuthQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthQryError |> Error
                            else signedInUserDic |> tokensForAuthQryApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok _ ->
                                (connectionId, userId) |> Projections.Users.users.HandleInitializeUsersProjectionAuthQryAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeUsersProjectionAuthQryResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun userDtos -> sprintf "%i user/s" userDtos.Length |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthAppMsg InitializeDraftsProjectionQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "InitializeDraftsProjectionQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthQryError |> Error
                            else signedInUserDic |> tokensForAuthQryApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.DraftToken with
                                | Some draftToken -> (draftToken, connectionId, userId) |> Projections.Drafts.drafts.HandleInitializeDraftsProjectionQryAsync
                                | None -> NotAuthorized |> AuthQryAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeDraftsProjectionQryResult |> ServerAppMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun (draftDtos, currentUserDraftDto) -> sprintf "%i draft/s (%A)" draftDtos.Length currentUserDraftDto |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthUserAdminMsg (CreateUserCmd (userId, userName, password, userType))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "CreateUserCmd"
                    sprintf "%s (%A %A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source userId userName userType connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.CreateUserToken with
                                | Some createUserToken ->
                                    (createUserToken, auditUserId, userId, userName, password, userType) |> Entities.Users.users.HandleCreateUserCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> CreateUserCmdResult |> ServerUserAdminMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthUserAdminMsg (ResetPasswordCmd (userId, currentRvn, password))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "ResetPasswordCmd"
                    sprintf "%s for (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source userId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ResetPasswordToken with
                                | Some resetPasswordToken ->
                                    (resetPasswordToken, auditUserId, userId, currentRvn, password) |> Entities.Users.users.HandleResetPasswordCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ResetPasswordCmdResult |> ServerUserAdminMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                        match result with
                        | Ok _ -> do! (connectionDic, signedInUserDic) |> autoSignOut (PasswordReset |> Some) userId None None
                        | Error _ -> () })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthUserAdminMsg (ChangeUserTypeCmd (userId, currentRvn, userType))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "ChangeUserTypeCmd"
                    sprintf "%s (%A) for (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source userType userId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChangeUserTypeToken with
                                | Some changeUserTypeToken ->
                                    (changeUserTypeToken, auditUserId, userId, currentRvn, userType) |> Entities.Users.users.HandleChangeUserTypeCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ChangeUserTypeCmdResult |> ServerUserAdminMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                        match result with
                        | Ok _ -> do! (connectionDic, signedInUserDic) |> autoSignOut (userType = PersonaNonGrata |> PermissionsChanged |> Some) userId None None
                        | Error _ -> () })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthDraftAdminMsg InitializeUserDraftSummaryProjectionQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "InitializeUserDraftSummaryProjectionQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthQryError |> Error
                            else signedInUserDic |> tokensForAuthQryApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.DraftAdminToken with
                                | Some draftAdminToken -> (draftAdminToken, connectionId, userId) |> Projections.UserDraftSummary.userDraftSummary.HandleInitializeUserDraftSummaryProjectionQryAsync
                                | None -> NotAuthorized |> AuthQryAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeUserDraftSummaryProjectionQryResult |> ServerDraftAdminMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (fun userDraftSummaryDtos -> sprintf "%i user draft/s" userDraftSummaryDtos.Length |> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthDraftAdminMsg (ProcessDraftCmd (draftId, currentRvn))) ->
                    let source = "ProcessDraftCmd"
                    sprintf "%s for (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source draftId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                            |> Result.bind (fun userTokens ->
                                match userTokens.ProcessDraftToken with
                                | Some processDraftToken ->
                                    (processDraftToken, auditUserId, draftId, currentRvn, connectionId) |> Entities.Drafts.drafts.HandleProcessDraftCmd |> Ok
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error)
                        match result with
                        | Ok _ -> ()
                        | Error error ->
                            let serverMsg = error |> Error |> ProcessDraftCmdResult |> ServerDraftAdminMsg
                            do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                            error |> Error |> logResult source (sprintf "%A" >> Some) })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthNewsMsg (CreatePostCmd (postId, postType, messageText))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "CreatePostCmd"
                    sprintf "%s (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source postId postType connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.CreatePostToken with
                                | Some createPostToken ->
                                    (createPostToken, auditUserId, postId, postType, messageText) |> Entities.News.news.HandleCreatePostCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> CreatePostCmdResult |> ServerNewsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthNewsMsg (ChangePostCmd (postId, currentRvn, messageText))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "ChangePostCmd"
                    sprintf "%s for (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source postId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.EditOrRemovePostToken with
                                | Some editOrRemovePostToken ->
                                    (editOrRemovePostToken, auditUserId, postId, currentRvn, messageText) |> Entities.News.news.HandleChangePostCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ChangePostCmdResult |> ServerNewsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthNewsMsg (RemovePostCmd (postId, currentRvn))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "CreatePostCmd"
                    sprintf "%s for (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source postId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.EditOrRemovePostToken with
                                | Some editOrRemovePostToken ->
                                    (editOrRemovePostToken, auditUserId, postId, currentRvn) |> Entities.News.news.HandleRemovePostCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> RemovePostCmdResult |> ServerNewsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthSquadsMsg (AddPlayerCmd (squadId, currentRvn, playerId, playerName, playerType))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "AddPlayerCmd"
                    sprintf "%s (%A %A %A) for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source playerId playerName playerType squadId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.AddOrEditPlayerToken with
                                | Some addOrEditPlayerToken ->
                                    (addOrEditPlayerToken, auditUserId, squadId, currentRvn, playerId, playerName, playerType) |> Entities.Squads.squads.HandleAddPlayerCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> AddPlayerCmdResult |> ServerSquadsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthSquadsMsg (ChangePlayerNameCmd (squadId, currentRvn, playerId, playerName))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "ChangePlayerNameCmd"
                    sprintf "%s (%A %A) for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source playerId playerName squadId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.AddOrEditPlayerToken with
                                | Some addOrEditPlayerToken ->
                                    (addOrEditPlayerToken, auditUserId, squadId, currentRvn, playerId, playerName) |> Entities.Squads.squads.HandleChangePlayerNameCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ChangePlayerNameCmdResult |> ServerSquadsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthSquadsMsg (ChangePlayerTypeCmd (squadId, currentRvn, playerId, playerType))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "ChangePlayerTypeCmd"
                    sprintf "%s (%A %A) for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source playerId playerType squadId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.AddOrEditPlayerToken with
                                | Some addOrEditPlayerToken ->
                                    (addOrEditPlayerToken, auditUserId, squadId, currentRvn, playerId, playerType) |> Entities.Squads.squads.HandleChangePlayerTypeCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ChangePlayerTypeCmdResult |> ServerSquadsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthSquadsMsg (WithdrawPlayerCmd (squadId, currentRvn, playerId))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "WithdrawPlayerCmd"
                    sprintf "%s (%A) for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source playerId squadId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.WithdrawPlayerToken with
                                | Some withdrawPlayerToken ->
                                    (withdrawPlayerToken, auditUserId, squadId, currentRvn, playerId) |> Entities.Squads.squads.HandleWithdrawPlayerCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> WithdrawPlayerCmdResult |> ServerSquadsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthSquadsMsg (EliminateSquadCmd (squadId, currentRvn))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "EliminateSquadCmd"
                    sprintf "%s for %A (%A) when managingConnections (%i connection/s) (%i signed-in user/s)" source squadId currentRvn connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.EliminateSquadToken with
                                | Some eliminateSquadToken ->
                                    (eliminateSquadToken, auditUserId, squadId, currentRvn) |> Entities.Squads.squads.HandleEliminateSquadCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> EliminateSquadCmdResult |> ServerSquadsMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthSquadsMsg (AddToDraftCmd (draftId, currentRvn, userDraftPick))) ->
                    let source = "AddToDraftCmd"
                    sprintf "%s for %A (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source draftId currentRvn userDraftPick connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                            |> Result.bind (fun userTokens ->
                                match userTokens.DraftToken with
                                | Some draftToken ->
                                    (draftToken, auditUserId, draftId, currentRvn, userDraftPick, connectionId) |> Entities.Drafts.drafts.HandleAddToDraftCmd |> Ok
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error)
                        match result with
                        | Ok _ -> ()
                        | Error error ->
                            let serverMsg = (userDraftPick, error) |> Error |> AddToDraftCmdResult |> ServerSquadsMsg
                            do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                            error |> Error |> logResult source (sprintf "%A" >> Some) })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthSquadsMsg (UiAuthSquadsMsg.RemoveFromDraftCmd (draftId, currentRvn, userDraftPick))) ->
                    let source = "UiAuthSquadsMsg.RemoveFromDraftCmd"
                    sprintf "%s for %A (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source draftId currentRvn userDraftPick connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                            |> Result.bind (fun userTokens ->
                                match userTokens.DraftToken with
                                | Some draftToken ->
                                    let toServerMsg = (ServerSquadsMsg.RemoveFromDraftCmdResult >> ServerSquadsMsg)
                                    (draftToken, auditUserId, draftId, currentRvn, userDraftPick, toServerMsg, connectionId) |> Entities.Drafts.drafts.HandleRemoveFromDraftCmd |> Ok
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error)
                        match result with
                        | Ok _ -> ()
                        | Error error ->
                            let serverMsg = (userDraftPick, error) |> Error |> ServerSquadsMsg.RemoveFromDraftCmdResult |> ServerSquadsMsg
                            do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                            error |> Error |> logResult source (sprintf "%A" >> Some) })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthSquadsMsg (FreePickCmd (draftId, currentRvn, draftPick))) ->
                    let source = "FreePickCmd"
                    sprintf "%s for %A (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source draftId currentRvn draftPick connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                            |> Result.bind (fun userTokens ->
                                match userTokens.DraftToken with
                                | Some draftToken ->
                                    (draftToken, auditUserId, draftId, currentRvn, draftPick, connectionId) |> Entities.Drafts.drafts.HandleFreePickCmd |> Ok
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error)
                        match result with
                        | Ok _ -> ()
                        | Error error ->
                            let serverMsg = error |> Error |> FreePickCmdResult |> ServerSquadsMsg
                            do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                            error |> Error |> logResult source (sprintf "%A" >> Some) })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthFixturesMsg (ConfirmParticipantCmd (fixtureId, currentRvn, role, squadId))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "ConfirmParticipantCmd"
                    sprintf "%s for %A (%A %A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source fixtureId currentRvn role squadId connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ConfirmFixtureToken with
                                | Some confirmFixtureToken ->
                                    (confirmFixtureToken, auditUserId, fixtureId, currentRvn, role, squadId) |> Entities.Fixtures.fixtures.HandleConfirmParticipantCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> ConfirmParticipantCmdResult |> ServerFixturesMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthFixturesMsg (AddMatchEventCmd (fixtureId, currentRvn, matchEvent))) ->
                    let source = "AddMatchEventCmd"
                    sprintf "%s for %A (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source fixtureId currentRvn matchEvent connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                            |> Result.bind (fun userTokens ->
                                match userTokens.ResultsAdminToken with
                                | Some resultsAdminToken ->
                                    (resultsAdminToken, auditUserId, fixtureId, currentRvn, matchEvent, connectionId) |> Entities.Fixtures.fixtures.HandleAddMatchEventCmd |> Ok
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error)
                        match result with
                        | Ok _ -> ()
                        | Error error ->
                            let serverMsg = error |> Error |> AddMatchEventCmdResult |> ServerFixturesMsg
                            do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                            error |> Error |> logResult source (sprintf "%A" >> Some) })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthFixturesMsg (RemoveMatchEventCmd (fixtureId, currentRvn, matchEventId, matchEvent))) ->
                    let source = "AddMatchEventCmd"
                    sprintf "%s for %A (%A %A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source fixtureId currentRvn matchEventId matchEvent connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                            |> Result.bind (fun userTokens ->
                                match userTokens.ResultsAdminToken with
                                | Some resultsAdminToken ->
                                    (resultsAdminToken, auditUserId, fixtureId, currentRvn, matchEventId, matchEvent, connectionId) |> Entities.Fixtures.fixtures.HandleRemoveMatchEventCmd |> Ok
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error)
                        match result with
                        | Ok _ -> ()
                        | Error error ->
                            let serverMsg = error |> Error |> RemoveMatchEventCmdResult |> ServerFixturesMsg
                            do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                            error |> Error |> logResult source (sprintf "%A" >> Some) })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthDraftsMsg (ChangePriorityCmd (draftId, currentRvn, userDraftPick, priorityChange))) ->
                    let source = "ChangePriorityCmd"
                    sprintf "%s for %A (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source draftId currentRvn userDraftPick connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                            |> Result.bind (fun userTokens ->
                                match userTokens.DraftToken with
                                | Some draftToken ->
                                    (draftToken, auditUserId, draftId, currentRvn, userDraftPick, priorityChange, connectionId)
                                    |> Entities.Drafts.drafts.HandleChangePriorityCmd |> Ok
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error)
                        match result with
                        | Ok _ -> ()
                        | Error error ->
                            let serverMsg = (userDraftPick, error) |> Error |> ChangePriorityCmdResult |> ServerDraftsMsg
                            do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                            error |> Error |> logResult source (sprintf "%A" >> Some) })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthDraftsMsg (UiAuthDraftsMsg.RemoveFromDraftCmd (draftId, currentRvn, userDraftPick))) ->
                    let source = "UiAuthDraftsMsg.RemoveFromDraftCmd"
                    sprintf "%s for %A (%A %A) when managingConnections (%i connection/s) (%i signed-in user/s)" source draftId currentRvn userDraftPick connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (auditUserId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true auditUserId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                            |> Result.bind (fun userTokens ->
                                match userTokens.DraftToken with
                                | Some draftToken ->
                                    let toServerMsg = (ServerDraftsMsg.RemoveFromDraftCmdResult >> ServerDraftsMsg)
                                    (draftToken, auditUserId, draftId, currentRvn, userDraftPick, toServerMsg, connectionId) |> Entities.Drafts.drafts.HandleRemoveFromDraftCmd |> Ok
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error)
                        match result with
                        | Ok _ -> ()
                        | Error error ->
                            let serverMsg = (userDraftPick, error) |> Error |> ServerDraftsMsg.RemoveFromDraftCmdResult |> ServerDraftsMsg
                            do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                            error |> Error |> logResult source (sprintf "%A" >> Some) })
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthChatMsg InitializeChatProjectionQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "InitializeChatProjectionQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthQryError |> Error
                            else signedInUserDic |> tokensForAuthQryApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChatToken with
                                | Some chatProjectionQryToken -> (chatProjectionQryToken, connectionId) |> Projections.Chat.chat.HandleInitializeChatProjectionQryAsync
                                | None -> NotAuthorized |> AuthQryAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> InitializeChatProjectionQryResult |> ServerChatMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthChatMsg MoreChatMessagesQry) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "MoreChatMessagesQry"
                    sprintf "%s when managingConnections (%i connection/s) (%i signed-in user/s)" source connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthQryError |> Error
                            else signedInUserDic |> tokensForAuthQryApi source userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChatToken with
                                | Some chatProjectionQryToken -> (chatProjectionQryToken, connectionId) |> Projections.Chat.chat.HandleMoreChatMessagesQryAsync
                                | None -> NotAuthorized |> AuthQryAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> MoreChatMessagesQryResult |> ServerChatMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic
                | UiAuthMsg (jwt, UiAuthChatMsg (SendChatMessageCmd (chatMessageId, messageText))) -> // TODO-SOON: Switch to "non-async" (cf. ProcessDraftCmd &c.)...
                    let source = "SendChatMessageCmd"
                    sprintf "%s %A for %A when managingConnections (%i connection/s) (%i signed-in user/s)" source chatMessageId jwt connectionDic.Count signedInUserDic.Count |> Verbose |> log
                    let fWithConnection = (fun (_, (userId, _)) -> async {
                        let result =
                            if debugFakeError () then sprintf "Fake %s error -> %A" source jwt |> OtherError |> OtherAuthCmdError |> Error
                            else signedInUserDic |> tokensForAuthCmdApi source true userId jwt // note: if successful, updates SignedInUser.LastApi (and broadcasts UserActivity)
                        let! result =
                            match result with
                            | Ok userTokens ->
                                match userTokens.ChatToken with
                                | Some sendChatMessageToken -> (sendChatMessageToken, userId, chatMessageId, messageText) |> Projections.Chat.chat.HandleSendChatMessageCmdAsync
                                | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync
                            | Error error -> error |> Error |> thingAsync
                        let serverMsg = result |> SendChatMessageCmdResult |> ServerChatMsg
                        do! (connectionDic, signedInUserDic) |> sendMsg serverMsg [ connectionId ]
                        result |> logResult source (sprintf "%A" >> Some) }) // note: log success/failure here (rather than assuming that calling code will do so)
                    do! (connectionDic, signedInUserDic) |> ifSignedInSession source connectionId fWithConnection
                    return! managingConnections serverStarted connectionDic signedInUserDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Source.Connections |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start (serverStarted) =
        // TODO-NMB-LOW: Subscribe to Tick, e.g. to auto-sign out "expired" sessions? and/or to "purge" connections (i.e. via sending Waff to all connections)?...
        let onEvent = (fun event ->
            match event with
            | Signal.SendMsg (serverMsg, connectionIds) -> (serverMsg, connectionIds) |> OnSendMsg |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to SendMsg broadcasts -> %A" subscriptionId |> Info |> log
        (fun reply -> (serverStarted, reply) |>  Start) |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.AddConnection (connectionId, ws) = (connectionId, ws) |> AddConnection |> agent.Post
    member __.RemoveConnection connectionId = connectionId |> RemoveConnection |> agent.Post
    member __.OnReceiveUiMsgError (connectionId, exn) = (connectionId, exn) |> OnReceiveUiMsgError |> agent.Post
    member __.OnDeserializeUiMsgError (connectionId, exn) = (connectionId, exn) |> OnDeserializeUiMsgError |> agent.Post
    member __.HandleUiMsg (connectionId, uiMsg) = (connectionId, uiMsg) |> HandleUiMsg |> agent.Post

let connections = Connections ()
