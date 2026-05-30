module Aornota.Sweepstake2026.Ui.Shared

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg

open System
open System.Collections.Generic

type Projection<'a> =
    | Pending
    | Failed
    | Ready of data : 'a

type User = UserName * UserAuthDto option
type UserDic = Dictionary<UserId, User>

type Player = { PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus ; PickedBy : PickedBy option }
type PlayerDic = Dictionary<PlayerId, Player>

type Squad = { Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding option ; CoachName : CoachName ; Eliminated : bool ; PlayerDic : PlayerDic ; PickedBy : PickedBy option }
type SquadDic = Dictionary<SquadId, Squad>

type Fixture = { Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset ; MatchResult : MatchResult option ; CustomMessage : (UserId * Markdown) option }
type FixtureDic = Dictionary<FixtureId, Fixture>

type Draft = { Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus ; ProcessingDetails : ProcessingDetails option }
type DraftDic = Dictionary<DraftId, Draft>

type UserDraftPickDic = Dictionary<UserDraftPick, int>

type MissingMatchEvent =
    | CleanSheetMissing of squadId : SquadId
    | ManOfTheMatchMissing
    | PenaltyShootoutMissing

type FixtureStatus =
    | NotStarted
    | NotConfirmed
    | DetailsPending
    | DetailsOverdue
    | DetailsMissing of missingMatchEvents : MissingMatchEvent list
    | DetailsEntered

let [<Literal>] UNKNOWN = "<unknown>"
let [<Literal>] RESULT_PENDING = "Result pending"
let [<Literal>] RESULT_OVERDUE = "Result overdue"
let [<Literal>] RESULT_HAS_MISSING_DETAILS = "Result has missing details"

let cmdErrorText error = match error with | AuthCmdJwtError _ | AuthCmdAuthznError _ | AuthCmdPersistenceError _ -> UNEXPECTED_ERROR | OtherAuthCmdError (OtherError errorText) -> errorText
let qryErrorText error = match error with | AuthQryJwtError _ | AuthQryAuthznError _ -> UNEXPECTED_ERROR | OtherAuthQryError (OtherError errorText) -> errorText

let userName (userDic:UserDic) userId =
    if userId |> userDic.ContainsKey then
        let userName, _ = userDic.[userId]
        userName
    else UserName UNKNOWN
let userType (userDic:UserDic) userId =
    if userId |> userDic.ContainsKey then
        let _, userAuthDto = userDic.[userId]
        match userAuthDto with | Some userAuthDto -> userAuthDto.UserType |> Some | None -> None
    else None
let userNames (userDic:UserDic) = userDic |> List.ofSeq |> List.map (fun (KeyValue (_, (userName, _))) -> userName)

let squadName (squadDic:SquadDic) squadId = if squadId |> squadDic.ContainsKey then squadDic.[squadId].SquadName else SquadName UNKNOWN
let seedingText seeding = match seeding with | Some (Seeding seeding) when seeding <= MAX_SEEDS_FOR_SCORING -> sprintf "%i" seeding | _ -> "N/A"

let playerName (squadDic:SquadDic) (squadId, playerId) =
    if squadId |> squadDic.ContainsKey then
        let playerDic = squadDic.[squadId].PlayerDic
        if playerId |> playerDic.ContainsKey then playerDic.[playerId].PlayerName else PlayerName UNKNOWN
    else PlayerName UNKNOWN
let playerType (squadDic:SquadDic) (squadId, playerId) =
    if squadId |> squadDic.ContainsKey then
        let playerDic = squadDic.[squadId].PlayerDic
        if playerId |> playerDic.ContainsKey then playerDic.[playerId].PlayerType |> Some else None
    else None
let playerNames (playerDic:PlayerDic) = playerDic |> List.ofSeq |> List.map (fun (KeyValue (_, player)) -> player.PlayerName)

let currentDraft (draftDic:DraftDic) =
    let drafts = draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) -> draftId, draft) |> List.sortBy (fun (_, draft) ->
        match draft.DraftStatus with | Opened _ -> 1 | PendingProcessing false -> 2 | PendingProcessing true -> 3 | PendingOpen _ -> 4 | FreeSelection -> 5 | _ -> 6)
    match drafts with
    | (draftId, draft) :: _ ->
        match draft.DraftStatus with
        | PendingOpen _ | Opened _ | PendingProcessing _ | FreeSelection -> (draftId, draft) |> Some
        | Processed | PendingFreeSelection -> None
    | [] -> None
let activeDraft (draftDic:DraftDic) =
    let drafts = draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) -> draftId, draft) |> List.sortBy (fun (_, draft) ->
        match draft.DraftStatus with | Opened _ -> 1 | PendingProcessing false -> 2 | PendingProcessing true -> 3 | _ -> 4)
    match drafts with
    | (draftId, draft) :: _ -> if draft.DraftStatus |> isActive then (draftId, draft) |> Some else None
    | [] -> None

let draftPickText (squadDic:SquadDic) draftPick =
    match draftPick with
    | TeamPicked squadId ->
        let (SquadName squadName) = squadId |> squadName squadDic
        squadName
    | PlayerPicked (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        playerName

let userDraftPickDic currentUserDraftDto =
    match currentUserDraftDto with
    | Some currentUserDraftDto ->
        let userDraftPickDic = UserDraftPickDic ()
        currentUserDraftDto.UserDraftPickDtos |> List.iter (fun userDraftPickDto -> (userDraftPickDto.UserDraftPick, userDraftPickDto.Rank) |> userDraftPickDic.Add)
        userDraftPickDic
    | None -> UserDraftPickDic ()
let userDraftPickText (squadDic:SquadDic) userDraftPick =
    match userDraftPick with
    | TeamPick squadId ->
        let (SquadName squadName) = squadId |> squadName squadDic
        squadName
    | PlayerPick (squadId, playerId) ->
        let (PlayerName playerName) = (squadId, playerId) |> playerName squadDic
        playerName

let pickedByUser (squadDic:SquadDic) userId =
    let squad =
        squadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> squadId, squad)
        |> List.choose (fun (squadId, squad) ->
            match squad.PickedBy with | Some (pickedByUserId, draftOrdinal, timestamp) when pickedByUserId = userId -> (squadId, squad, draftOrdinal, timestamp) |> Some | _ -> None)
    let squad = match squad with (squadId, squad, draftOrdinal, timestamp) :: _ -> (squadId, squad, draftOrdinal, timestamp) |> Some | [] -> None
    let playerDics =
        squadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> (squadId, squad), squad.PlayerDic)
    let players =
        playerDics |> List.map (fun ((squadId, squad), playerDic) ->
            playerDic |> List.ofSeq |> List.map (fun (KeyValue (playerId, player)) -> playerId, player)
            |> List.choose (fun (playerId, player) ->
                match player.PickedBy with
                | Some (pickedByUserId, draftOrdinal, timestamp) when pickedByUserId = userId -> (squadId, squad, playerId, player, draftOrdinal, timestamp) |> Some
                | _ -> None))
        |> List.collect id
    squad, players
let pickedCounts (squad:(_ * Squad * _ * _) option, players:(_ * _ * _ * Player * _ * _) list) =
    let teamCount = match squad with | Some _ -> 1 | None -> 0
    let goalkeeperCount = players |> List.filter (fun (_, _, _, player, _, _) -> match player.PlayerType, player.PlayerStatus with | Goalkeeper, Active -> true | _ -> false) |> List.length
    let outfieldPlayerCount =
        players |> List.filter (fun (_, _, _, player, _, _) ->
            match player.PlayerType, player.PlayerStatus with | Goalkeeper, _ -> false | _, Active -> true | _ -> false) |> List.length
    teamCount, goalkeeperCount, outfieldPlayerCount
let required (pickedTeamCount, pickedGoalkeeperCount, pickedOutfieldPlayerCount) =
    let required = [
        if pickedTeamCount < MAX_TEAM_PICKS then yield "1 team/coach"
        if pickedGoalkeeperCount < MAX_GOALKEEPER_PICKS then yield "1 goalkeeper"
        let requiredOutfieldPlayers = MAX_OUTFIELD_PLAYER_PICKS - pickedOutfieldPlayerCount
        if requiredOutfieldPlayers > 0 then
            let plural = if requiredOutfieldPlayers > 1 then "s" else String.Empty
            yield sprintf "%i outfield player%s" requiredOutfieldPlayers plural ]
    let items = required.Length
    if items > 0 then
        let required = required |> List.mapi (fun i item -> if i = 0 then item else if i + 1 < items then sprintf ", %s" item else sprintf " and %s" item)
        let required = required |> List.fold (fun text item -> sprintf "%s%s" text item) String.Empty
        required |> Some
    else None
let stillRequired (pickedTeamCount, pickedGoalkeeperCount, pickedOutfieldPlayerCount) =
    match (pickedTeamCount, pickedGoalkeeperCount, pickedOutfieldPlayerCount) |> required with
    | Some required -> sprintf "%s still required" required |> Some
    | None -> None

let teamPoints (fixtureDic:FixtureDic) squadId pickedDate =
    let teamScoreEvents =
        fixtureDic |> List.ofSeq |> List.choose (fun (KeyValue (_, fixture)) ->
            match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with
            | Confirmed homeSquadId, Confirmed awaySquadId, Some matchResult ->
                if homeSquadId = squadId then (fixture.KickOff, matchResult.HomeScoreEvents.TeamScoreEvents) |> Some
                else if awaySquadId = squadId then (fixture.KickOff, matchResult.AwayScoreEvents.TeamScoreEvents) |> Some
                else None
            | _ -> None)
        |> List.map (fun (kickOff, events) -> events |> List.map (fun (_, points) -> kickOff, points))
        |> List.collect id
    let points = teamScoreEvents |> List.sumBy snd
    let pickedPoints =
        match pickedDate with
        | Some pickedDate -> teamScoreEvents |> List.filter (fun (kickOff, _) -> kickOff > pickedDate) |> List.sumBy snd |> Some
        | None -> None
    points, pickedPoints
let playerPoints (fixtureDic:FixtureDic) (squadId, playerId) pickedDate =
    let playerScoreEvents =
        fixtureDic |> List.ofSeq |> List.choose (fun (KeyValue (_, fixture)) ->
            match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with
            | Confirmed homeSquadId, Confirmed awaySquadId, Some matchResult ->
                if homeSquadId = squadId then
                    let playerScoreEvents =
                        matchResult.HomeScoreEvents.PlayerScoreEvents |> List.choose (fun (forPlayerId, events) -> if forPlayerId = playerId then events |> Some else None) |> List.collect id
                    (fixture.KickOff, playerScoreEvents) |> Some
                else if awaySquadId = squadId then
                    let playerScoreEvents =
                        matchResult.AwayScoreEvents.PlayerScoreEvents |> List.choose (fun (forPlayerId, events) -> if forPlayerId = playerId then events |> Some else None) |> List.collect id
                    (fixture.KickOff, playerScoreEvents) |> Some
                else None
            | _ -> None)
        |> List.map (fun (kickOff, events) -> events |> List.map (fun (_, points) -> kickOff, points))
        |> List.collect id
    let points = playerScoreEvents |> List.sumBy snd
    let pickedPoints =
        match pickedDate with
        | Some pickedDate -> playerScoreEvents |> List.filter (fun (kickOff, _) -> kickOff > pickedDate) |> List.sumBy snd |> Some
        | None -> None
    points, pickedPoints

let stageText includeMatchNumber stage =
    match stage with
    | Group group -> group |> groupText
    | RoundOf32 matchNumber ->
        let text = "Round of 32"
        if includeMatchNumber then sprintf "%s (match %i)" text matchNumber else text
    | RoundOf16 matchNumber ->
        let text = "Round of 16"
        if includeMatchNumber then sprintf "%s (match %i)" text matchNumber else text
    | QuarterFinal quarterFinalOrdinal -> sprintf "Quarter-final %i" quarterFinalOrdinal
    | SemiFinal semiFinalOrdinal -> sprintf "Semi-final %i" semiFinalOrdinal
    | ThirdPlacePlayOff -> "Third place play-off"
    | Final -> "Final"

let confirmedFixtureDetails (squadDic:SquadDic) fixture =
    match fixture.HomeParticipant, fixture.AwayParticipant, fixture.MatchResult with
    | Confirmed homeSquadId, Confirmed awaySquadId, Some matchResult ->
        let matchOutcome, homeScoreEvents, awayScoreEvents, matchEvents = matchResult.MatchOutcome, matchResult.HomeScoreEvents, matchResult.AwayScoreEvents, matchResult.MatchEvents
        let winnerSquadId, penaltyShootoutText =
            match matchOutcome.PenaltyShootoutOutcome with
            | Some penaltyShootoutOutcome ->
                if penaltyShootoutOutcome.HomeScore > penaltyShootoutOutcome.AwayScore then
                    let (SquadName squadName) = homeSquadId |> squadName squadDic
                    let penaltyShootoutText = sprintf "%s win %i - %i on penalities" squadName penaltyShootoutOutcome.HomeScore penaltyShootoutOutcome.AwayScore
                    homeSquadId |> Some, penaltyShootoutText |> Some
                else if penaltyShootoutOutcome.AwayScore > penaltyShootoutOutcome.HomeScore then
                    let (SquadName squadName) = awaySquadId |> squadName squadDic
                    let penaltyShootoutText = sprintf "%s win %i - %i on penalities" squadName penaltyShootoutOutcome.AwayScore penaltyShootoutOutcome.HomeScore
                    awaySquadId |> Some, penaltyShootoutText |> Some
                else None, None // note: should never happen
            | None ->
                if matchOutcome.HomeGoals > matchOutcome.AwayGoals then homeSquadId |> Some, None
                else if matchOutcome.AwayGoals > matchOutcome.HomeGoals then awaySquadId |> Some, None
                else None, None
        let homeIsWinner, homeName, homeGoals =
            let (SquadName squadName) = homeSquadId |> squadName squadDic
            homeSquadId |> Some = winnerSquadId, squadName, matchOutcome.HomeGoals
        let awayIsWinner, awayName, awayGoals =
            let (SquadName squadName) = awaySquadId |> squadName squadDic
            awaySquadId |> Some = winnerSquadId, squadName, matchOutcome.AwayGoals
        let teams = (homeSquadId, homeName, awaySquadId, awayName) |> Some
        let result = (homeIsWinner, homeGoals, awayIsWinner, awayGoals, penaltyShootoutText, homeScoreEvents, awayScoreEvents, matchEvents) |> Some
        teams, result
    | Confirmed homeSquadId, Confirmed awaySquadId, None ->
        let (SquadName homeName), (SquadName awayName) = homeSquadId |> squadName squadDic, awaySquadId |> squadName squadDic
        (homeSquadId, homeName, awaySquadId, awayName) |> Some, None
    | _ -> None, None

let concatenate (items:string list) =
    match items with
    | [] -> "NO ITEMS"
    | h :: t ->
        let rec concatenate (acc:string) (remaining:string list) =
            match remaining with
            | [] -> acc
            | [ item ] -> concatenate (sprintf "%s; and %s" acc item) []
            | h :: t -> concatenate (sprintf "%s; %s" acc h) t
        concatenate h t

let isKnockout fixture = match fixture.Stage with | Group _ -> false | _ -> true

let missingMatchEventText (squadDic:SquadDic) missingMatchEvent =
    match missingMatchEvent with
    | CleanSheetMissing squadId ->
        let (SquadName squadName) = squadId |> squadName squadDic
        sprintf "clean sheet for %s" squadName
    | ManOfTheMatchMissing -> "man-of-the-match"
    | PenaltyShootoutMissing -> "penalty shootout"

let fixtureStatus (fixtureDic:FixtureDic) fixtureId =
    if fixtureId |> fixtureDic.ContainsKey then
        let fixture = fixtureDic.[fixtureId]
        let local, now = fixture.KickOff.LocalDateTime, DateTime.Now
        if local <= now then
            let isOverdue =
                match isKnockout fixture, (now - local).TotalHours with
                | false, elapsed when elapsed > 2 -> true
                | true, elapsed when elapsed > 3. -> true
                | _ -> false
            match fixture.HomeParticipant, fixture.AwayParticipant, isOverdue, fixture.MatchResult with
            | Unconfirmed _, _, _, _ | _, Unconfirmed _, _, _ -> Some NotConfirmed
            | _, _, false, None -> Some DetailsPending
            | _, _, true, None -> Some DetailsOverdue
            | _, _, _, Some matchResult ->
                let matchEvents = matchResult.MatchEvents |> List.map snd
                let missingMatchEvents =
                    [
                        let homeSquadId = match fixture.HomeParticipant with | Confirmed squadId -> Some squadId | _ -> None
                        match homeSquadId, matchResult.MatchOutcome.AwayGoals with
                        | Some homeSquadId, 0u ->
                            match matchEvents |> List.filter (fun matchEvent -> match matchEvent with | CleanSheet (squadId, _) when squadId = homeSquadId -> true | _ -> false) with
                            | [] -> CleanSheetMissing homeSquadId
                            | _ -> ()
                        | _ -> ()
                        let awaySquadId = match fixture.AwayParticipant with | Confirmed squadId -> Some squadId | _ -> None
                        match awaySquadId, matchResult.MatchOutcome.HomeGoals with
                        | Some awaySquadId, 0u ->
                            match matchEvents |> List.filter (fun matchEvent -> match matchEvent with | CleanSheet (squadId, _) when squadId = awaySquadId -> true | _ -> false) with
                            | [] -> CleanSheetMissing awaySquadId
                            | _ -> ()
                        | _ -> ()
                        match matchEvents |> List.filter (fun matchEvent -> match matchEvent with | ManOfTheMatch _ -> true | _ -> false) with
                        | [] -> ManOfTheMatchMissing
                        | _ -> ()
                        match fixture |> isKnockout, matchResult.MatchOutcome.HomeGoals, matchResult.MatchOutcome.AwayGoals, matchResult.MatchOutcome.PenaltyShootoutOutcome with
                        | true, home, away, None when home = away -> PenaltyShootoutMissing
                        | _ -> ()
                    ]
                match missingMatchEvents with
                | [] -> Some DetailsEntered
                | _ -> Some (DetailsMissing missingMatchEvents)
        else Some NotStarted
    else None
