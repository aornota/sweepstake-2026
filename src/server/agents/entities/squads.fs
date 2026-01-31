module Aornota.Sweepstake2026.Server.Agents.Entities.Squads

(* Broadcasts: TODO:SendMsg
               SquadsRead
   Subscribes: SquadsEventsRead *)

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Squad
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
open Aornota.Sweepstake2026.Server.Events.SquadEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private SquadsInput =
    | IsAwaitingStart of reply : AsyncReplyChannel<bool>
    | Start of reply : AsyncReplyChannel<unit>
    | Reset of reply : AsyncReplyChannel<unit>
    | OnSquadsEventsRead of squadsEvents : (SquadId * (Rvn * SquadEvent) list) list
    | HandleCreateSquadCmd of token : CreateSquadToken * auditUserId : UserId * squadId : SquadId * squadName : SquadName * group : Group * seeding : Seeding option * coachName : CoachName
        * reply : AsyncReplyChannel<Result<SquadName, AuthCmdError<string>>>
    | HandleAddPlayerCmd of token : AddOrEditPlayerToken * auditUserId : UserId * squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
        * reply : AsyncReplyChannel<Result<Rvn * PlayerName, AuthCmdError<string>>>
    | HandleChangePlayerNameCmd of token : AddOrEditPlayerToken * auditUserId : UserId * squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerName : PlayerName
        * reply : AsyncReplyChannel<Result<PlayerName * PlayerName, AuthCmdError<string>>>
    | HandleChangePlayerTypeCmd of token : AddOrEditPlayerToken * auditUserId : UserId * squadId : SquadId * currentRvn : Rvn * playerId : PlayerId * playerType : PlayerType
        * reply : AsyncReplyChannel<Result<PlayerName, AuthCmdError<string>>>
    | HandleWithdrawPlayerCmd of token : WithdrawPlayerToken * auditUserId : UserId * squadId : SquadId * currentRvn : Rvn * playerId : PlayerId
        * reply : AsyncReplyChannel<Result<PlayerName, AuthCmdError<string>>>
    | HandleEliminateSquadCmd of token : EliminateSquadToken * auditUserId : UserId * squadId : SquadId * currentRvn : Rvn
        * reply : AsyncReplyChannel<Result<SquadName, AuthCmdError<string>>>

type private Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus }
type private PlayerDic = Dictionary<PlayerId, Player>

type private Squad = { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding option ; CoachName : CoachName ; Eliminated : bool ; Players : Dictionary<PlayerId, Player> }
type private SquadDic = Dictionary<SquadId, Squad>

let private log category = (Entity Entity.Squads, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private nonWithdrawnCount (players:Dictionary<PlayerId, Player>) =
    players |> List.ofSeq |> List.filter (fun (KeyValue (_, player)) -> match player.PlayerStatus with | Active -> true | Withdrawn _ -> false) |> List.length

let private applySquadEvent source idAndSquadResult (nextRvn, squadEvent:SquadEvent) =
    let otherError errorText = otherError (sprintf "%s#applySquadEvent" source) errorText
    match idAndSquadResult, squadEvent with
    | Ok (squadId, _), _ when squadId <> squadEvent.SquadId -> // note: should never happen
        ifDebug (sprintf "SquadId mismatch for %A -> %A" squadId squadEvent) UNEXPECTED_ERROR |> otherError
    | Ok (squadId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" squadId nextRvn squadEvent) UNEXPECTED_ERROR |> otherError
    | Ok (squadId, Some squad), _ when validateNextRvn (Some squad.Rvn) nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" squadId squad.Rvn nextRvn squadEvent) UNEXPECTED_ERROR |> otherError
    | Ok (squadId, None), SquadCreated (_, squadName, group, seeding, coachName) ->
        (squadId, { Rvn = nextRvn ; SquadName = squadName ; Group = group ; Seeding = seeding ; CoachName = coachName ; Eliminated = false ; Players = Dictionary<PlayerId, Player> () } |> Some) |> Ok
    | Ok (squadId, None), _ -> // note: should never happen
        ifDebug (sprintf "Invalid initial SquadEvent for %A -> %A" squadId squadEvent) UNEXPECTED_ERROR |> otherError
    | Ok (squadId, Some squad), SquadCreated _ -> // note: should never happen
        ifDebug (sprintf "Invalid non-initial SquadEvent for %A (%A) -> %A" squadId squad squadEvent) UNEXPECTED_ERROR |> otherError
    | Ok (squadId, Some squad), PlayerAdded (_, playerId, playerName, playerType) ->
        if squad.Players |> nonWithdrawnCount >= MAX_PLAYERS_PER_SQUAD then
            ifDebug (sprintf "%s -> %A" squadIsFullText squadEvent) UNEXPECTED_ERROR |> otherError
        else if playerId |> squad.Players.ContainsKey then // note: should never happen
            ifDebug (sprintf "%A already exists for %A -> %A" playerId squadId squadEvent) UNEXPECTED_ERROR |> otherError
        else
            (playerId, { PlayerName = playerName ; PlayerType = playerType ; PlayerStatus = Active }) |> squad.Players.Add
            (squadId, { squad with Rvn = nextRvn } |> Some) |> Ok
    | Ok (squadId, Some squad), PlayerNameChanged (_, playerId, playerName) ->
        if playerId |> squad.Players.ContainsKey |> not then // note: should never happen
            ifDebug (sprintf "%A does not exist for %A -> %A" playerId squadId squadEvent) UNEXPECTED_ERROR |> otherError
        else
            let player = squad.Players.[playerId]
            squad.Players.[playerId] <- { player with PlayerName = playerName }
            (squadId, { squad with Rvn = nextRvn } |> Some) |> Ok
    | Ok (squadId, Some squad), PlayerTypeChanged (_, playerId, playerType) ->
        if playerId |> squad.Players.ContainsKey |> not then // note: should never happen
            ifDebug (sprintf "%A does not exist for %A -> %A" playerId squadId squadEvent) UNEXPECTED_ERROR |> otherError
        else
            let player = squad.Players.[playerId]
            squad.Players.[playerId] <- { player with PlayerType = playerType }
            (squadId, { squad with Rvn = nextRvn } |> Some) |> Ok
    | Ok (squadId, Some squad), PlayerWithdrawn (_, playerId, dateWithdrawn) ->
        if playerId |> squad.Players.ContainsKey |> not then // note: should never happen
            ifDebug (sprintf "%A does not exist for %A -> %A" playerId squadId squadEvent) UNEXPECTED_ERROR |> otherError
        else
            let player = squad.Players.[playerId]
            squad.Players.[playerId] <- { player with PlayerStatus = dateWithdrawn |> Withdrawn }
            (squadId, { squad with Rvn = nextRvn } |> Some) |> Ok
    | Ok (squadId, Some squad), SquadEliminated _ ->
        (squadId, { squad with Rvn = nextRvn ; Eliminated = true } |> Some) |> Ok
    | Error error, _ -> error |> Error

let private initializeSquads source (squadsEvents:(SquadId * (Rvn * SquadEvent) list) list) =
    let source = sprintf "%s#initializeSquads" source
    let squadDic = SquadDic ()
    let results =
        squadsEvents
        |> List.map (fun (squadId, events) ->
            match events with
            | _ :: _ -> events |> List.fold (fun idAndSquadResult (rvn, squadEvent) -> applySquadEvent source idAndSquadResult (rvn, squadEvent)) (Ok (squadId, None))
            | [] -> ifDebug (sprintf "No SquadEvents for %A" squadId) UNEXPECTED_ERROR |> otherError source) // note: should never happen
    results
    |> List.choose (fun idAndSquadResult -> match idAndSquadResult with | Ok (squadId, Some squad) -> (squadId, squad) |> Some | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (squadId, squad) -> squadDic.Add (squadId, squad))
    let errors =
        results
        |> List.choose (fun idAndSquadResult ->
            match idAndSquadResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> ifDebug (sprintf "%s: applySquadEvent returned Ok (_, None)" source) UNEXPECTED_ERROR |> OtherError |> Some // note: should never happen
            | Error error -> error |> Some)
    squadDic, errors

let private tryFindSquad squadId onError (squadDic:SquadDic) =
    if squadId |> squadDic.ContainsKey then (squadId, squadDic.[squadId]) |> Ok else ifDebug (sprintf "%A does not exist" squadId) UNEXPECTED_ERROR |> onError

let private tryFindPlayer playerId onError (playerDic:PlayerDic) =
    if playerId |> playerDic.ContainsKey then (playerId, playerDic.[playerId]) |> Ok else ifDebug (sprintf "%A does not exist" playerId) UNEXPECTED_ERROR |> onError

let private tryApplySquadEvent source squadId squad nextRvn thing squadEvent =
    match applySquadEvent source (Ok (squadId, squad)) (nextRvn, squadEvent) with
    | Ok (_, Some squad) -> (squad, nextRvn, squadEvent, thing) |> Ok
    | Ok (_, None) -> ifDebug "applySquadEvent returned Ok (_, None)" UNEXPECTED_ERROR |> otherCmdError source // note: should never happen
    | Error otherError -> otherError |> OtherAuthCmdError |> Error

let private tryWriteSquadEventAsync auditUserId rvn squadEvent (squad:Squad) thing = async {
    let! result = (auditUserId, rvn, squadEvent) |> persistence.WriteSquadEventAsync
    return match result with | Ok _ -> (squadEvent.SquadId, squad, thing) |> Ok | Error persistenceError -> persistenceError |> AuthCmdPersistenceError |> Error }

type Squads () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> true |> reply.Reply ; return! awaitingStart ()
            | Start reply ->
                "Start when awaitingStart -> pendingOnSquadsEventsRead" |> Info |> log
                () |> reply.Reply
                return! pendingOnSquadsEventsRead ()
            | Reset _ -> "Reset when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSquadsEventsRead _ -> "OnSquadsEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleCreateSquadCmd _ -> "HandleCreateSquadCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleAddPlayerCmd _ -> "HandleAddPlayerCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleChangePlayerNameCmd _ -> "HandleChangePlayerNameCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleChangePlayerTypeCmd _ -> "HandleChangePlayerTypeCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleWithdrawPlayerCmd _ -> "HandleWithdrawPlayerCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleEliminateSquadCmd _ -> "HandleEliminateSquadCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnSquadsEventsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! pendingOnSquadsEventsRead ()
            | Start _ -> "Start when pendingOnSquadsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsEventsRead ()
            | Reset _ -> "Reset when pendingOnSquadsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsEventsRead ()
            | OnSquadsEventsRead squadsEvents ->
                let source = "OnSquadsEventsRead"
                let squads, errors = initializeSquads source squadsEvents
                errors |> List.iter (fun (OtherError errorText) -> errorText |> Danger |> log)
                sprintf "%s (%i squad/s) when pendingOnSquadsEventsRead -> managingSquads (%i squad/s)" source squadsEvents.Length squads.Count |> Info |> log
                let squadsRead =
                    squads
                    |> List.ofSeq
                    |> List.map (fun (KeyValue (squadId, squad)) ->
                        let playersRead =
                            squad.Players
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (playerId, player)) ->
                                { PlayerId = playerId ; PlayerName = player.PlayerName ; PlayerType = player.PlayerType ; PlayerStatus = player.PlayerStatus })
                        { SquadId = squadId ; Rvn = squad.Rvn ; SquadName = squad.SquadName ; Group = squad.Group ; Seeding = squad.Seeding ; CoachName = squad.CoachName ; Eliminated = squad.Eliminated ; PlayersRead = playersRead })
                squadsRead |> SquadsRead |> broadcaster.Broadcast
                return! managingSquads squads
            | HandleCreateSquadCmd _ -> "HandleCreateSquadCmd when pendingOnSquadsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsEventsRead ()
            | HandleAddPlayerCmd _ -> "HandleAddPlayerCmd when pendingOnSquadsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsEventsRead ()
            | HandleChangePlayerNameCmd _ -> "HandleChangePlayerNameCmd when pendingOnSquadsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsEventsRead ()
            | HandleChangePlayerTypeCmd _ -> "HandleChangePlayerTypeCmd when pendingOnSquadsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsEventsRead ()
            | HandleWithdrawPlayerCmd _ -> "HandleWithdrawPlayerCmd when pendingOnSquadsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsEventsRead ()
            | HandleEliminateSquadCmd _ -> "HandleEliminateSquadCmd when pendingOnSquadsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnSquadsEventsRead () }
        and managingSquads squadDic = async {
            let! input = inbox.Receive ()
            match input with
            | IsAwaitingStart reply -> false |> reply.Reply ; return! managingSquads squadDic
            | Start _ -> sprintf "Start when managingSquads (%i squad/s)" squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingSquads squadDic
            | Reset reply ->
                sprintf "Reset when managingSquads (%i squad/s) -> pendingOnSquadsEventsRead" squadDic.Count |> Info |> log
                () |> reply.Reply
                return! pendingOnSquadsEventsRead ()
            | OnSquadsEventsRead _ -> sprintf "OnSquadsEventsRead when managingSquads (%i squad/s)" squadDic.Count |> IgnoredInput |> Agent |> log ; return! managingSquads squadDic
            | HandleCreateSquadCmd (_, auditUserId, squadId, SquadName squadName, group, seeding, CoachName coachName, reply) ->
                let source = "HandleCreateSquadCmd"
                sprintf "%s for %A (%A %A) when managingSquads (%i squad/s)" source squadId squadName group squadDic.Count |> Verbose |> log
                let squadName = SquadName (squadName.Trim ())
                let coachName = CoachName (coachName.Trim ())
                let result =
                    if squadId |> squadDic.ContainsKey |> not then () |> Ok
                    else ifDebug (sprintf "%A already exists" squadId) UNEXPECTED_ERROR |> otherCmdError source
                    |> Result.bind (fun _ ->
                        let squadNames = squadDic |> List.ofSeq |> List.map (fun (KeyValue (_, squad)) -> squad.SquadName)
                        match validateSquadName squadNames squadName with | None -> () |> Ok | Some errorText -> errorText |> otherCmdError source)
                    |> Result.bind (fun _ -> match validateCoachName coachName with | None -> () |> Ok | Some errorText -> errorText |> otherCmdError source)
                    |> Result.bind (fun _ ->
                        (squadId, squadName, group, seeding, coachName) |> SquadCreated |> (tryApplySquadEvent source squadId None initialRvn ()))
                let! result = match result with | Ok (squad, rvn, squadEvent, _) -> tryWriteSquadEventAsync auditUserId rvn squadEvent squad () | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (squadId, squad, _) -> sprintf "Audit%A %A %A" auditUserId squadId squad |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, squad, _) -> squad.SquadName) |> reply.Reply
                match result with | Ok (squadId, squad, _) -> (squadId, squad) |> squadDic.Add | Error _ -> ()
                return! managingSquads squadDic
            | HandleAddPlayerCmd (_, auditUserId, squadId, currentRvn, playerId, PlayerName playerName, playerType, reply) ->
                let source = "HandleAddPlayerCmd"
                sprintf "%s for %A (%A %A %A) when managingSquads (%i squad/s)" source squadId playerId playerName playerType squadDic.Count |> Verbose |> log
                let playerName = PlayerName (playerName.Trim ())
                let result =
                    squadDic |> tryFindSquad squadId (otherCmdError source)
                    |> Result.bind (fun (squadId, squad) ->
                        if playerId |> squad.Players.ContainsKey |> not then (squadId, squad, playerId) |> Ok
                        else ifDebug (sprintf "%A already exists" playerId) UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (squadId, squad, playerId) ->
                        if squad.Players |> nonWithdrawnCount < MAX_PLAYERS_PER_SQUAD then (squadId, squad, playerId) |> Ok
                        else squadIsFullText |> otherCmdError source)
                    |> Result.bind (fun (squadId, squad, playerId) ->
                        let playerNames = squad.Players |> List.ofSeq |> List.map (fun (KeyValue (_, player)) -> player.PlayerName)
                        match validatePlayerName playerNames playerName with
                        | None -> (squadId, squad, playerId) |> Ok
                        | Some errorText -> errorText |> otherCmdError source)
                    |> Result.bind (fun (squadId, squad, playerId) ->
                        (squadId, playerId, playerName, playerType) |> PlayerAdded |> (tryApplySquadEvent source squadId (Some squad) (incrementRvn currentRvn) ()))
                let! result = match result with | Ok (squad, rvn, squadEvent, _) -> tryWriteSquadEventAsync auditUserId rvn squadEvent squad () | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (squadId, squad, _) -> sprintf "Audit%A %A %A" auditUserId squadId squad |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, squad, _) -> squad.Rvn, playerName) |> reply.Reply
                match result with | Ok (squadId, squad, _) -> squadDic.[squadId] <- squad | Error _ -> ()
                return! managingSquads squadDic
            | HandleChangePlayerNameCmd (_, auditUserId, squadId, currentRvn, playerId, PlayerName playerName, reply) ->
                let source = "HandleChangePlayerNameCmd"
                sprintf "%s for %A (%A %A) when managingSquads (%i squad/s)" source squadId playerId playerName squadDic.Count |> Verbose |> log
                let playerName = PlayerName (playerName.Trim ())
                let result =
                    squadDic |> tryFindSquad squadId (otherCmdError source)
                    |> Result.bind (fun (squadId, squad) ->
                        match squad.Players |> tryFindPlayer playerId (otherCmdError source) with
                        | Ok (playerId, player) -> (squadId, squad, playerId, player) |> Ok
                        | Error error -> error |> Error)
                    |> Result.bind (fun (squadId, squad, playerId, player) ->
                        let playerNames = squad.Players |> List.ofSeq |> List.map (fun (KeyValue (_, player)) -> player.PlayerName)
                        match validatePlayerName playerNames playerName with
                        | None -> (squadId, squad, playerId, player) |> Ok
                        | Some errorText -> errorText |> otherCmdError source)
                    |> Result.bind (fun (squadId, squad, playerId, player) ->
                        if playerName <> player.PlayerName then (squadId, squad, playerId, player) |> Ok
                        else "New player name must not be the same as the current player name" |> otherCmdError source)
                    |> Result.bind (fun (squadId, squad, playerId, player) ->
                        (squadId, playerId, playerName) |> PlayerNameChanged |> tryApplySquadEvent source squadId (Some squad) (incrementRvn currentRvn) player.PlayerName)
                let! result = match result with | Ok (squad, rvn, squadEvent, previousPlayerName) -> tryWriteSquadEventAsync auditUserId rvn squadEvent squad previousPlayerName | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (squadId, squad, _) -> sprintf "Audit%A %A %A" auditUserId squadId squad |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, _, previousPlayerName) -> previousPlayerName, playerName) |> reply.Reply
                match result with | Ok (squadId, squad, _) -> squadDic.[squadId] <- squad | Error _ -> ()
                return! managingSquads squadDic
            | HandleChangePlayerTypeCmd (_, auditUserId, squadId, currentRvn, playerId, playerType, reply) ->
                let source = "HandleChangePlayerTypeCmd"
                sprintf "%s for %A (%A %A) when managingSquads (%i squad/s)" source squadId playerId playerType squadDic.Count |> Verbose |> log
                let result =
                    squadDic |> tryFindSquad squadId (otherCmdError source)
                    |> Result.bind (fun (squadId, squad) ->
                        match squad.Players |> tryFindPlayer playerId (otherCmdError source) with
                        | Ok (playerId, player) -> (squadId, squad, playerId, player) |> Ok
                        | Error error -> error |> Error)
                    |> Result.bind (fun (squadId, squad, playerId, player) ->
                        if playerType <> player.PlayerType then (squadId, squad, playerId, player) |> Ok
                        else "New player type must not be the same as the current player type" |> otherCmdError source)
                    |> Result.bind (fun (squadId, squad, playerId, player) ->
                        (squadId, playerId, playerType) |> PlayerTypeChanged |> tryApplySquadEvent source squadId (Some squad) (incrementRvn currentRvn) player.PlayerName)
                let! result = match result with | Ok (squad, rvn, squadEvent, playerName) -> tryWriteSquadEventAsync auditUserId rvn squadEvent squad playerName | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (squadId, squad, _) -> sprintf "Audit%A %A %A" auditUserId squadId squad |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, _, playerName) -> playerName) |> reply.Reply
                match result with | Ok (squadId, squad, _) -> squadDic.[squadId] <- squad | Error _ -> ()
                return! managingSquads squadDic
            | HandleWithdrawPlayerCmd (_, auditUserId, squadId, currentRvn, playerId, reply) ->
                let source = "HandleWithdrawPlayerCmd"
                sprintf "%s for %A (%A) when managingSquads (%i squad/s)" source squadId playerId squadDic.Count |> Verbose |> log
                let result =
                    squadDic |> tryFindSquad squadId (otherCmdError source)
                    |> Result.bind (fun (squadId, squad) ->
                        match squad.Players |> tryFindPlayer playerId (otherCmdError source) with
                        | Ok (playerId, player) -> (squadId, squad, playerId, player) |> Ok
                        | Error error -> error |> Error)
                    |> Result.bind (fun (squadId, squad, playerId, player) ->
                        match player.PlayerStatus with
                        | Active -> (squadId, squad, playerId, player) |> Ok
                        | Withdrawn _ -> "Player has already been withdrawn" |> otherCmdError source)
                    |> Result.bind (fun (squadId, squad, playerId, player) ->
                        (squadId, playerId, (DateTimeOffset.UtcNow |> Some)) |> PlayerWithdrawn |> tryApplySquadEvent source squadId (Some squad) (incrementRvn currentRvn) player.PlayerName)
                let! result = match result with | Ok (squad, rvn, squadEvent, playerName) -> tryWriteSquadEventAsync auditUserId rvn squadEvent squad playerName | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (squadId, squad, _) -> sprintf "Audit%A %A %A" auditUserId squadId squad |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, _, playerName) -> playerName) |> reply.Reply
                match result with | Ok (squadId, squad, _) -> squadDic.[squadId] <- squad | Error _ -> ()
                return! managingSquads squadDic
            | HandleEliminateSquadCmd (_, auditUserId, squadId, currentRvn, reply) ->
                let source = "HandleEliminateSquadCmd"
                sprintf "%s for %A when managingSquads (%i squad/s)" source squadId squadDic.Count |> Verbose |> log
                let result =
                    squadDic |> tryFindSquad squadId (otherCmdError source)
                    |> Result.bind (fun (squadId, squad) ->
                        if squad.Eliminated |> not then (squadId, squad) |> Ok
                        else "Team has already been eliminated" |> otherCmdError source)
                    |> Result.bind (fun (squadId, squad) ->
                        squadId |> SquadEliminated |> (tryApplySquadEvent source squadId (Some squad) (incrementRvn currentRvn) ()))
                let! result = match result with | Ok (squad, rvn, squadEvent, _) -> tryWriteSquadEventAsync auditUserId rvn squadEvent squad () | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (squadId, squad, _) -> sprintf "Audit%A %A %A" auditUserId squadId squad |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> Result.map (fun (_, squad, _) -> squad.SquadName) |> reply.Reply
                match result with | Ok (squadId, squad, _) -> squadDic.[squadId] <- squad | Error _ -> ()
                return! managingSquads squadDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Entity Entity.Squads |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member self.Start () =
        if IsAwaitingStart |> agent.PostAndReply then
            // Note: Not interested in SquadEventWritten events (since Squads agent causes these in the first place - and will already have maintained its internal state accordingly).
            let onEvent = (fun event -> match event with | SquadsEventsRead squadsEvents -> squadsEvents |> self.OnSquadsEventsRead | _ -> ())
            let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
            sprintf "agent subscribed to SquadsEventsRead broadcasts -> %A" subscriptionId |> Info |> log
            Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
        else
            "agent has already been started" |> Info |> log
    member __.Reset () = Reset |> agent.PostAndReply // note: not async (since need to reset agents deterministically)
    member __.OnSquadsEventsRead squadsEvents = squadsEvents |> OnSquadsEventsRead |> agent.Post
    member __.HandleCreateSquadCmdAsync (token, auditUserId, squadId, squadName, group, seeding, coachName) =
        (fun reply -> (token, auditUserId, squadId, squadName, group, seeding, coachName, reply) |> HandleCreateSquadCmd) |> agent.PostAndAsyncReply
    member __.HandleAddPlayerCmdAsync (token, auditUserId, squadId, currentRvn, playerId, playerName, playerType) =
        (fun reply -> (token, auditUserId, squadId, currentRvn, playerId, playerName, playerType, reply) |> HandleAddPlayerCmd) |> agent.PostAndAsyncReply
    member __.HandleChangePlayerNameCmdAsync (token, auditUserId, squadId, currentRvn, playerId, playerName) =
        (fun reply -> (token, auditUserId, squadId, currentRvn, playerId, playerName, reply) |> HandleChangePlayerNameCmd) |> agent.PostAndAsyncReply
    member __.HandleChangePlayerTypeCmdAsync (token, auditUserId, squadId, currentRvn, playerId, playerType) =
        (fun reply -> (token, auditUserId, squadId, currentRvn, playerId, playerType, reply) |> HandleChangePlayerTypeCmd) |> agent.PostAndAsyncReply
    member __.HandleWithdrawPlayerCmdAsync (token, auditUserId, squadId, currentRvn, playerId) =
        (fun reply -> (token, auditUserId, squadId, currentRvn, playerId, reply) |> HandleWithdrawPlayerCmd) |> agent.PostAndAsyncReply
    member __.HandleEliminateSquadCmdAsync (token, auditUserId, squadId, currentRvn) =
        (fun reply -> (token, auditUserId, squadId, currentRvn, reply) |> HandleEliminateSquadCmd) |> agent.PostAndAsyncReply

let squads = Squads ()
