module Aornota.Sweepstake2026.Server.Agents.Projections.Squads

(* Broadcasts: SendMsg
   Subscribes: SquadsRead
               SquadEventWritten (PlayerAdded | PlayerNameChanged | PlayerTypeChanged | PlayerWithdrawn | SquadEliminated)
               DraftsRead
               DraftEventWritten (Picked | FreePick)
               ConnectionsSignedOut | Disconnected *)

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Common.DeltaHelper
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Events.DraftEvents
open Aornota.Sweepstake2026.Server.Events.SquadEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private SquadsInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnSquadsRead of squadsRead : SquadRead list
    | OnPlayerAdded of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerName : PlayerName * playerType : PlayerType
    | OnPlayerNameChanged of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerName : PlayerName
    | OnPlayerTypeChanged of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * playerType : PlayerType
    | OnPlayerWithdrawn of squadId : SquadId * squadRvn : Rvn * playerId : PlayerId * dateWithdrawn : DateTimeOffset option
    | OnSquadEliminated of squadId : SquadId * squadRvn : Rvn
    | OnDraftsRead of draftsRead : DraftRead list
    | OnPicked of draftPick : DraftPick * draftOrdinal : DraftOrdinal option * userId : UserId * timestamp : DateTimeOffset
    | RemoveConnection of connectionId : ConnectionId
    | HandleInitializeSquadsProjectionQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<SquadDto list, OtherError<string>>>

type private Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus ; PickedBy : PickedBy option }
type private PlayerDic = Dictionary<PlayerId, Player>

type private Squad =
    { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding option ; CoachName : CoachName ; Eliminated : bool ; PlayerDic : PlayerDic ; PickedBy : PickedBy option }
type private SquadDic = Dictionary<SquadId, Squad>

type private Projectee = { LastRvn : Rvn }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { SquadDic : SquadDic }

type private StateChangeType =
    | Initialization of squadDic : SquadDic
    | SquadChange of squadDic : SquadDic * state : State
    | PlayerChange of squadId : SquadId * squadRvn : Rvn * playerDic : PlayerDic * state : State

type private PlayerDtoDic = Dictionary<PlayerId, PlayerDto>
type private SquadOnlyDtoDic = Dictionary<SquadId, SquadOnlyDto>

let private log category = (Projection Squads, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private playerDto (playerId, player:Player) =
    match player.PlayerStatus with
    | Active | Withdrawn (Some _) ->
        { PlayerDto.PlayerId = playerId ; PlayerName = player.PlayerName ; PlayerType = player.PlayerType ; PlayerStatus = player.PlayerStatus ; PickedBy = player.PickedBy } |> Some
    | Withdrawn None -> None

let private playerDtoDic (playerDic:PlayerDic) =
    let playerDtoDic = PlayerDtoDic ()
    playerDic |> List.ofSeq |> List.iter (fun (KeyValue (playerId, player)) ->
        match (playerId, player) |> playerDto with | Some playerDto -> (playerDto.PlayerId, playerDto) |> playerDtoDic.Add | None -> ())
    playerDtoDic

let private squadOnlyDto (squadId, squad:Squad) =
    { SquadId = squadId ; Rvn = squad.Rvn ; SquadName = squad.SquadName ; Group = squad.Group ; Seeding = squad.Seeding ; CoachName = squad.CoachName ; Eliminated = squad.Eliminated
      PickedBy = squad.PickedBy }

let private squadOnlyDtoDic (squadDic:SquadDic) =
    let squadOnlyDtoDic = SquadOnlyDtoDic ()
    squadDic |> List.ofSeq |> List.iter (fun (KeyValue (squadId, squad)) ->
        let squadOnlyDto = (squadId, squad) |> squadOnlyDto
        (squadOnlyDto.SquadId, squadOnlyDto) |> squadOnlyDtoDic.Add)
    squadOnlyDtoDic

let private squadDto (squadId, squad:Squad) =
    let playerDtos = squad.PlayerDic |> List.ofSeq |> List.choose (fun (KeyValue (playerId, player)) -> (playerId, player) |> playerDto)
    { SquadOnlyDto = (squadId, squad) |> squadOnlyDto ; PlayerDtos = playerDtos }

let private squadDtos state = state.SquadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> (squadId, squad) |> squadDto)

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendPlayerDtoDelta squadId squadRvn (projecteeDic:ProjecteeDic) playerDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendPlayerDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, squadId, squadRvn, playerDtoDelta) |> PlayersDeltaMsg |> SquadsProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private sendSquadOnlyDtoDelta (projecteeDic:ProjecteeDic) squadOnlyDtoDelta =
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn }
        sprintf "sendSquadOnlyDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
        (projectee.LastRvn, squadOnlyDtoDelta) |> SquadsDeltaMsg |> SquadsProjectionMsg |> ServerAppMsg |> sendMsg [ connectionId ]
        (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private copySquadDic (squadDic:SquadDic) =
    let copiedSquadDic = SquadDic ()
    squadDic |> List.ofSeq |> List.iter (fun (KeyValue (squadId, squad)) ->
        let squad = { squad with PlayerDic = PlayerDic squad.PlayerDic }
        (squadId, squad) |> copiedSquadDic.Add)
    copiedSquadDic

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization squadDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { SquadDic = squadDic |> copySquadDic }
        | SquadChange (squadDic, state) ->
            let previousSquadOnlyDtoDic = state.SquadDic |> squadOnlyDtoDic
            let squadOnlyDtoDic = squadDic |> squadOnlyDtoDic
            let squadOnlyDtoDelta = squadOnlyDtoDic |> delta previousSquadOnlyDtoDic
            if squadOnlyDtoDelta |> isEmpty |> not then
                sprintf "%s -> SquadOnlyDto delta %A -> %i projectee/s" source squadOnlyDtoDelta projecteeDic.Count |> Info |> log
                squadOnlyDtoDelta |> sendSquadOnlyDtoDelta projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with SquadDic = squadDic |> copySquadDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
        | PlayerChange (squadId, squadRvn, playerDic, state) ->
            let squadDic = state.SquadDic
            if squadId |> squadDic.ContainsKey then
                let squad = squadDic.[squadId]
                let previousPlayerDtoDic = squad.PlayerDic |> playerDtoDic
                let playerDtoDic = playerDic |> playerDtoDic
                let playerDtoDelta = playerDtoDic |> delta previousPlayerDtoDic
                if playerDtoDelta |> isEmpty |> not then
                    sprintf "%s -> PlayerDto delta %A -> %i projectee/s" source playerDtoDelta projecteeDic.Count |> Info |> log
                    playerDtoDelta |> sendPlayerDtoDelta squadId squadRvn projecteeDic
                    let squad = { squad with Rvn = squadRvn ; PlayerDic = PlayerDic playerDic }
                    squadDic.[squadId] <- squad
                    sprintf "%s -> updated" source |> Info |> log
                    state
                else
                    sprintf "%s -> unchanged" source |> Info |> log
                    state
            else // note: should never happen
                state
    newState

let private ifAllRead source (squadsRead:(SquadRead list) option, draftsRead:(DraftRead list) option) =
    match squadsRead, draftsRead with
    | Some squadsRead, Some draftsRead ->
        let draftPicks = draftsRead |> List.map (fun draftRead -> draftRead.DraftPicks) |> List.collect id
        let teamPickedBy squadId =
            let pickedBy = draftPicks |> List.tryFind (fun (draftPick, _) -> match draftPick with | TeamPicked pickedSquadId when pickedSquadId = squadId -> true | _ -> false)
            match pickedBy with | Some (_, userIdAndDraftOrdinal) -> userIdAndDraftOrdinal |> Some | None -> None
        let playerPickedBy (squadId, playerId) =
            let pickedBy = draftPicks |> List.tryFind (fun (draftPick, _) ->
                match draftPick with | PlayerPicked (pickedSquadId, pickedPlayerId) when pickedSquadId = squadId && pickedPlayerId = playerId -> true | _ -> false)
            match pickedBy with | Some (_, userIdAndDraftOrdinal) -> userIdAndDraftOrdinal |> Some | None -> None
        let squadDic = SquadDic ()
        squadsRead |> List.iter (fun squadRead ->
            let playerDic = PlayerDic ()
            squadRead.PlayersRead |> List.iter (fun playerRead ->
                let playerPickedBy = (squadRead.SquadId, playerRead.PlayerId) |> playerPickedBy
                let player = { PlayerName = playerRead.PlayerName ; PlayerType = playerRead.PlayerType ; PlayerStatus = playerRead.PlayerStatus ; PickedBy = playerPickedBy }
                (playerRead.PlayerId, player) |> playerDic.Add)
            let teamPickedBy = squadRead.SquadId |> teamPickedBy
            let squad = {
                Rvn = squadRead.Rvn ; SquadName = squadRead.SquadName ; Group = squadRead.Group ; Seeding = squadRead.Seeding ; CoachName = squadRead.CoachName
                Eliminated = squadRead.Eliminated ; PlayerDic = playerDic ; PickedBy = teamPickedBy }
            (squadRead.SquadId, squad) |> squadDic.Add)
        let projecteeDic = ProjecteeDic ()
        let state = squadDic |> Initialization |> updateState source projecteeDic
        (state, squadDic, projecteeDic) |> Some
    | _ -> None

type Squads () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingAllRead (0 squads) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingAllRead None None
            | OnSquadsRead _ -> "OnSquadsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerAdded _ -> "OnPlayerAdded when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerNameChanged _ -> "OnPlayerNameChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerTypeChanged _ -> "OnPlayerTypeChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPlayerWithdrawn _ -> "OnPlayerWithdrawn when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnSquadEliminated _ -> "OnSquadEliminated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnDraftsRead _ -> "OnDraftsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPicked _ -> "OnPicked when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnection _ -> "RemoveConnection when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeSquadsProjectionQry _ -> "HandleInitializeSquadsProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingAllRead squadsRead draftsRead = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead
            | OnSquadsRead squadsRead ->
                let source = "OnSquadsRead"
                sprintf "%s (%i squad/s) when pendingAllRead" source squadsRead.Length |> Info |> log
                let squadsRead = squadsRead |> Some
                match (squadsRead, draftsRead) |> ifAllRead source with
                | Some (state, squadDic, projecteeDic) ->
                    return! projectingSquads state squadDic projecteeDic
                | None -> return! pendingAllRead squadsRead draftsRead
            | OnPlayerAdded _ -> "OnPlayerAdded when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead
            | OnPlayerNameChanged _ -> "OnPlayerNameChanged when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead
            | OnPlayerTypeChanged _ -> "OnPlayerTypeChanged when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead
            | OnPlayerWithdrawn _ -> "OnPlayerWithdrawn when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead
            | OnSquadEliminated _ -> "OnSquadEliminated when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead
            | OnDraftsRead draftsRead ->
                let source = "OnDraftsRead"
                sprintf "%s (%i draft/s) when pendingAllRead" source draftsRead.Length |> Info |> log
                let draftsRead = draftsRead |> Some
                match (squadsRead, draftsRead) |> ifAllRead source with
                | Some (state, squadDic, projecteeDic) ->
                    return! projectingSquads state squadDic projecteeDic
                | None -> return! pendingAllRead squadsRead draftsRead
            | OnPicked _ -> "OnPicked when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead
            | RemoveConnection _ -> "RemoveConnection when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead
            | HandleInitializeSquadsProjectionQry _ -> "HandleInitializeSquadsProjectionQry when pendingAllRead" |> IgnoredInput |> Agent |> log ; return! pendingAllRead squadsRead draftsRead }
        and projectingSquads state squadDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads state squadDic projecteeDic
            | OnSquadsRead _ -> "OnSquadsRead when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads state squadDic projecteeDic
            | OnPlayerAdded (squadId, squadRvn, playerId, playerName, playerType) ->
                let source = "OnPlayerAdded"
                sprintf "%s (%A %A %A %A %A) when projectingSquads (%i squad/s) (%i projectee/s)" source squadId squadRvn playerId playerName playerType squadDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey |> not then // note: silently ignore already-known playerId (should never happen)
                            (playerId, { PlayerName = playerName ; PlayerType = playerType ; PlayerStatus = Active ; PickedBy = None }) |> playerDic.Add
                            (squadId, squadRvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads state squadDic projecteeDic
            | OnPlayerNameChanged (squadId, squadRvn, playerId, playerName) ->
                let source = "OnPlayerNameChanged"
                sprintf "%s (%A %A %A %A) when projectingSquads (%i squad/s) (%i projectee/s)" source squadId squadRvn playerId playerName squadDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey then // note: silently ignore unknown playerId (should never happen)
                            let player = playerDic.[playerId]
                            playerDic.[playerId] <- { player with PlayerName = playerName }
                            (squadId, squadRvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads state squadDic projecteeDic
            | OnPlayerTypeChanged (squadId, squadRvn, playerId, playerType) ->
                let source = "OnPlayerTypeChanged"
                sprintf "%s (%A %A %A %A) when projectingSquads (%i squad/s) (%i projectee/s)" source squadId squadRvn playerId playerType squadDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey then // note: silently ignore unknown playerId (should never happen)
                            let player = playerDic.[playerId]
                            playerDic.[playerId] <- { player with PlayerType = playerType }
                            (squadId, squadRvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads state squadDic projecteeDic
            | OnPlayerWithdrawn (squadId, squadRvn, playerId, dateWithdrawn) ->
                let source = "OnPlayerWithdrawn"
                sprintf "%s (%A %A %A) when projectingSquads (%i squad/s) (%i projectee/s)" source squadId squadRvn playerId squadDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        let playerDic = squad.PlayerDic
                        if playerId |> playerDic.ContainsKey then // note: silently ignore unknown playerId (should never happen)
                            let player = playerDic.[playerId]
                            playerDic.[playerId] <- { player with PlayerStatus = dateWithdrawn |> Withdrawn }
                            (squadId, squadRvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                        else state
                    else state
                return! projectingSquads state squadDic projecteeDic
            | OnSquadEliminated (squadId, squadRvn) ->
                let source = "OnSquadEliminated"
                sprintf "%s (%A %A) when projectingSquads (%i squad/s) (%i projectee/s)" source squadId squadRvn squadDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                        let squad = squadDic.[squadId]
                        squadDic.[squadId] <- { squad with Rvn = squadRvn ; Eliminated = true }
                        (squadDic, state) |> SquadChange |> updateState source projecteeDic
                    else state
                return! projectingSquads state squadDic projecteeDic
            | OnDraftsRead _ -> "OnDraftsRead when projectingSquads" |> IgnoredInput |> Agent |> log ; return! projectingSquads state squadDic projecteeDic
            | OnPicked (draftPick, draftOrdinal, userId, timestamp) ->
                let source = "OnPicked"
                sprintf "%s (%A %A) when projectingSquads (%i squad/s) (%i projectee/s)" source draftPick userId squadDic.Count projecteeDic.Count |> Info |> log
                let state =
                    match draftPick with
                    | TeamPicked squadId ->
                        if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                            let squad = squadDic.[squadId]
                            squadDic.[squadId] <- { squad with PickedBy = (userId, draftOrdinal, timestamp) |> Some }
                            (squadDic, state) |> SquadChange |> updateState source projecteeDic
                        else state
                    | PlayerPicked (squadId, playerId) ->
                        if squadId |> squadDic.ContainsKey then // note: silently ignore unknown squadId (should never happen)
                            let squad = squadDic.[squadId]
                            let playerDic = squad.PlayerDic
                            if playerId |> playerDic.ContainsKey then // note: silently ignore unknown playerId (should never happen)
                                let player = playerDic.[playerId]
                                playerDic.[playerId] <- { player with PickedBy = (userId, draftOrdinal, timestamp) |> Some }
                                (squadId, squad.Rvn, playerDic, state) |> PlayerChange |> updateState source projecteeDic
                            else state
                        else state
                return! projectingSquads state squadDic projecteeDic
            | RemoveConnection connectionId ->
                let source = "RemoveConnection"
                sprintf "%s (%A) when projectingSquads (%i squad/s) (%i projectee/s)" source connectionId squadDic.Count projecteeDic.Count |> Info |> log
                if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore // note: silently ignore unknown connectionIds
                sprintf "%s when projectingChat -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingSquads state squadDic projecteeDic
            | HandleInitializeSquadsProjectionQry (connectionId, reply) ->
                let source = "HandleInitializeSquadsProjectionQry"
                sprintf "%s for %A when projectingSquads (%i squad/s) (%i projectee/s)" source connectionId squadDic.Count projecteeDic.Count |> Info |> log
                let projectee = { LastRvn = initialRvn }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingSquads -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = state |> squadDtos |> Ok
                result |> logResult source (fun squadDtos -> sprintf "%i squad/s" squadDtos.Length |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingSquads state squadDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.Squads |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | SquadsRead squadsRead -> squadsRead |> OnSquadsRead |> agent.Post
            | SquadEventWritten (rvn, userEvent) ->
                match userEvent with
                | SquadCreated _ -> () // note: no need to handle since cannot happen once SquadsRead
                | PlayerAdded (squadId, playerId, playerName, playerType) -> (squadId, rvn, playerId, playerName, playerType) |> OnPlayerAdded |> agent.Post
                | PlayerNameChanged (squadId, playerId, playerName) -> (squadId, rvn, playerId, playerName) |> OnPlayerNameChanged |> agent.Post
                | PlayerTypeChanged (squadId, playerId, playerType) -> (squadId, rvn, playerId, playerType) |> OnPlayerTypeChanged |> agent.Post
                | PlayerWithdrawn (squadId, playerId, dateWithdrawn) -> (squadId, rvn, playerId, dateWithdrawn) |> OnPlayerWithdrawn |> agent.Post
                | SquadEliminated squadId -> (squadId, rvn) |> OnSquadEliminated |> agent.Post
            | DraftsRead draftsRead -> draftsRead |> OnDraftsRead |> agent.Post
            | DraftEventWritten (_, draftEvent) ->
                match draftEvent with
                | DraftEvent.Picked (_, draftOrdinal, draftPick, userId, timestamp) -> (draftPick, draftOrdinal |> Some, userId, timestamp) |> OnPicked |> agent.Post
                | FreePick (_, draftPick, userId, timestamp) -> (draftPick, None, userId, timestamp) |> OnPicked |> agent.Post
                | _ -> ()
            | Disconnected connectionId -> connectionId |> RemoveConnection |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to SquadsRead | SquadEventWritten (subset) | DraftsRead | DraftEventWritten (subset) | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeSquadsProjectionQryAsync (connectionId) =
        (fun reply -> (connectionId, reply) |> HandleInitializeSquadsProjectionQry) |> agent.PostAndAsyncReply

let squads = Squads ()
