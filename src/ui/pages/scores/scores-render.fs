module Aornota.Sweepstake2026.Ui.Pages.Scores.Render

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Ui.Common.LazyViewOrHMR
open Aornota.Sweepstake2026.Ui.Common.TimestampHelper
open Aornota.Sweepstake2026.Ui.Pages.Scores.Common
open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common
open Aornota.Sweepstake2026.Ui.Theme.Common
open Aornota.Sweepstake2026.Ui.Theme.Render.Bulma
open Aornota.Sweepstake2026.Ui.Theme.Shared
open Aornota.Sweepstake2026.Common.Domain.Squad // note: after Aornota.Sweepstake2026.Ui.Render.Bulma to avoid collision with Icon.Forward
open Aornota.Sweepstake2026.Ui.Shared // note: after Aornota.Sweepstake2026.Common.Domain.Squad to avoid collition with Squad[Only]Dto

open System

module RctH = Fable.React.Helpers

let private renderStandings (useDefaultTheme, users:(UserId * UserName) list, squadDic:SquadDic, fixtureDic:FixtureDic) dispatch =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let userRow userCount (rank, tieCount, rankChange, userId, UserName userName, squad, players, points, pointsChange) =
            let payout =
                let paraPayout = { paraDefaultSmallest with ParaAlignment = RightAligned }
                let payout =
                    match rank, tieCount with
                    | 1, 1 -> 50. |> Some
                    | 1, 2 -> 40. |> Some
                    | 1, 3 -> 33.33 |> Some
                    | 1, 4 -> 25. |> Some
                    | 1, _ -> None // note: unlikely to happen
                    | 2, 1 -> 30. |> Some
                    | 2, 2 -> 25. |> Some
                    | 2, 3 -> 16.66 |> Some
                    | 2, 4 -> 12.5 |> Some
                    | 2, _ -> None // note: unlikely to happen
                    | 3, 1 -> 20. |> Some
                    | 3, 2 -> 10. |> Some
                    | 3, 3 -> 6.66 |> Some
                    | 3, _ -> None // note: unlikely to happen
                    | rank, 1 when rank = userCount -> 10. |> Some
                    | rank, 2 when rank = userCount - 1 -> 5. |> Some
                    | rank, n when rank = userCount - (n - 1) -> None // note: unlikely to happen
                    | _ -> None
                match payout with
                // TODO-NMB-2021: Show payouts...
                | Some payout -> [ str (sprintf "£%.2f" payout) ] |> para theme paraPayout |> Some
                | None -> None
            let rankText = if tieCount > 1 then sprintf "=%i." rank else sprintf "%i." rank
            let rankChange =
                match rankChange with
                | Some rankChange ->
                    if rankChange > 0 then
                        let contents = div divDefault [ icon iconCollapseUpSmall ; strong (sprintf " +%i" rankChange) ]
                        [ contents ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Success } |> Some
                    else if rankChange < 0 then
                        let contents = div divDefault [ icon iconExpandDownSmall ; em (sprintf " %i" rankChange) ]
                        [ contents ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger } |> Some
                    else None
                | None -> None
            let onClick = (fun _ -> userId |> Some |> ShowSweepstaker |> dispatch)
            let squad, eliminated =
                match squad with
                | Some squad ->
                    let (SquadName squadName), (CoachName coachName) = squad.SquadName, squad.CoachName
                    let squadText = sprintf "%s (%s)" squadName coachName
                    let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
                    [ str squadText ] |> para theme paraDefaultSmallest |> Some, eliminated
                | None -> None, None
            let playerCount playerType =
                players
                |> List.filter (fun (_, _, player) -> match player.PlayerStatus with | Active -> player.PlayerType = playerType | Withdrawn _ -> false)
                |> List.length
            let defenderCount, midfielderCount, forwardCount = Defender |> playerCount, Midfielder |> playerCount, Forward |> playerCount
            let formation = sprintf "%i-%i-%i" defenderCount midfielderCount forwardCount
            let playersRemaining =
                players
                |> List.filter (fun (_, _, player) -> match player.PlayerStatus with | Active -> true | Withdrawn _ -> false)
                |> List.filter (fun (squadId, _, _) -> if squadId |> squadDic.ContainsKey then squadDic.[squadId].Eliminated |> not else false)
                |> List.length
            let pointsText = sprintf "%i" (int points)
            let score = if points > 0<point> then strong pointsText else if points < 0<point> then em pointsText else str pointsText
            let pointsChange =
                match pointsChange with
                | Some pointsChange ->
                    let paraPointsChange = { paraDefaultSmallest with ParaAlignment = RightAligned }
                    if pointsChange > 0<point> then [ strong (sprintf "+%i" (int pointsChange)) ] |> para theme paraPointsChange |> Some
                    else if pointsChange < 0<point> then [ em (sprintf "%i" (int pointsChange)) ] |> para theme paraPointsChange |> Some
                    else None
                | None -> None
            tr false [
                td [ RctH.ofOption payout ]
                td [ [ em rankText ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                td [ RctH.ofOption rankChange ]
                td [ [ [ str userName ] |> para theme paraDefaultSmallest ] |> link theme (Internal onClick) ]
                td [ RctH.ofOption squad ]
                td [ RctH.ofOption eliminated ]
                td [ [ str formation ] |> para theme paraCentredSmallest ]
                td [ [ str (sprintf "%i" playersRemaining ) ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                td [ [ score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                td [ RctH.ofOption pointsChange ] ]
        let resultKickOffs = fixtureDic |> List.ofSeq |> List.choose (fun (KeyValue (_, fixture)) -> match fixture.MatchResult with | Some _ -> fixture.KickOff |> Some | None -> None)
        let latestResult = match resultKickOffs with | _ :: _ -> resultKickOffs |> List.max |> Some | [] -> None
        let previousRankAndPoints =
            match latestResult with
            | Some latestResult ->
                let previousFixtureDic = FixtureDic ()
                fixtureDic |> List.ofSeq |> List.iter (fun (KeyValue (fixtureId, fixture)) -> if fixture.KickOff < latestResult then (fixtureId, fixture) |> previousFixtureDic.Add)
                let mutable previousRank = 1
                let mutable previousPoints : int<point> option = None
                users
                |> List.map (fun (userId, userName) ->
                    let squad, players = userId |> pickedByUser squadDic
                    let teamPoints =
                        match squad with
                        | Some (squadId, _, _, timestamp) ->
                            let _, teamPoints = teamPoints previousFixtureDic squadId (timestamp |> Some)
                            teamPoints
                        | None -> None
                    let teamPoints = match teamPoints with | Some teamPoints -> teamPoints | None -> 0<point>
                    let playerPoints =
                        players
                        |> List.choose (fun (squadId, _, playerId, _, _, timestamp) ->
                            let _, playerPoints = playerPoints previousFixtureDic (squadId, playerId) (timestamp |> Some)
                            playerPoints)
                        |> List.sum
                    let points = teamPoints + playerPoints
                    userId, userName, points)
                |> List.sortBy (fun (_, userName, points) -> -points, userName)
                |> List.mapi (fun i (userId, _, points) ->
                    let rank =
                        if i = 0 then
                            previousPoints <- points |> Some
                            i + 1
                        else
                            let rank =
                                if points |> Some = previousPoints then previousRank
                                else
                                    previousRank <- i + 1
                                    previousPoints <- points |> Some
                                    i + 1
                            rank
                    rank, userId, points)
            | None -> []
        let previousRankAndPoints forUserId =
            match previousRankAndPoints |> List.tryFind (fun (_, userId, _) -> userId = forUserId) with
            | Some (rank, _, points) -> (rank, points) |> Some
            | None -> None
        let users =
            let mutable previousRank = 1
            let mutable previousPoints : int<point> option = None
            users
            |> List.map (fun (userId, userName) ->
                let squad, players = userId |> pickedByUser squadDic
                let squad, teamPoints =
                    match squad with
                    | Some (squadId, squad, _, timestamp) ->
                        let _, teamPoints = teamPoints fixtureDic squadId (timestamp |> Some)
                        squad |> Some, teamPoints
                    | None -> None, None
                let teamPoints = match teamPoints with | Some teamPoints -> teamPoints | None -> 0<point>
                let playerPoints =
                    players
                    |> List.choose (fun (squadId, _, playerId, _, _, timestamp) ->
                        let _, playerPoints = playerPoints fixtureDic (squadId, playerId) (timestamp |> Some)
                        playerPoints)
                    |> List.sum
                let points = teamPoints + playerPoints
                let players = players |> List.map (fun (squadId, _, playerId, player, _, _) -> squadId, playerId, player)
                userId, userName, squad, players, points)
            |> List.sortBy (fun (_, userName, _, _, points) -> -points, userName)
            |> List.mapi (fun i (userId, userName, squadId, squad, points) ->
                let rank =
                    if i = 0 then
                        previousPoints <- points |> Some
                        i + 1
                    else
                        let rank =
                            if points |> Some = previousPoints then previousRank
                            else
                                previousRank <- i + 1
                                previousPoints <- points |> Some
                                i + 1
                        rank
                rank, userId, userName, squadId, squad, points)
            |> List.map (fun (rank, userId, userName, squadId, squad, points) ->
                match userId |> previousRankAndPoints with
                | Some (previousRank, previousPoints) ->
                    let rankChange, pointsChange = previousRank - rank, points - previousPoints
                    rank, rankChange |> Some, userId, userName, squadId, squad, points, pointsChange |> Some
                | None -> rank, None, userId, userName, squadId, squad, points, None)
        let ranks = users |> List.map (fun (rank, _, _, _, _, _, _, _) -> rank)
        let users =
            users
            |> List.map (fun (rank, rankChange, userId, userName, squadId, squad, points, pointsChange) ->
                let tieCount = ranks |> List.filter (fun otherRank -> otherRank = rank) |> List.length
                rank, tieCount, rankChange, userId, userName, squadId, squad, points, pointsChange)
        let userCount = users.Length
        let userRows = users |> List.map (userRow userCount)
        yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
            thead [
                tr false [
                    th []
                    th []
                    th []
                    th [ [ strong "Name" ] |> para theme paraDefaultSmallest ]
                    th [ [ strong "Team/coach"] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ strong "Formation" ] |> para theme paraCentredSmallest ]
                    th [ [ strong "Players remaining" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                    th [ [ strong "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ]
                    th [] ] ]
            tbody [ yield! userRows ] ] ]

let private filterTabs currentScoresFilter dispatch =
    let isActive filter =
        match filter, currentScoresFilter with
        | Sweepstaker _, Sweepstaker _ -> true
        | Best _, Best _ -> true
        | BestUnpicked _, BestUnpicked _ -> true
        | _ -> false
    let filterText filter = match filter with | Sweepstaker _ -> "Sweepstakers" | Best _ -> "Best..." | BestUnpicked _ -> "Best unpicked..."
    let onClick filter =
        match filter with
        | Sweepstaker _ -> (fun _ -> None |> ShowSweepstaker |> dispatch )
        | Best _ -> (fun _ -> None |> ShowBest |> dispatch )
        | BestUnpicked _ -> (fun _ -> None |> ShowBestUnpicked |> dispatch )
    let filters = [ None |> Sweepstaker ; None |> Best ; None |> BestUnpicked ]
    filters |> List.map (fun filter -> { IsActive = filter |> isActive ; TabText = filter |> filterText ; TabLinkType = Internal (filter |> onClick) } )

let private sweepstakerTabs users currentUserId dispatch =
    users |> List.map (fun (userId, UserName userName) ->
        { IsActive = userId = currentUserId ; TabText = userName ; TabLinkType = Internal (fun _ -> userId |> Some |> ShowSweepstaker |> dispatch) })

let private bestTabs currentBest dispatch =
    let bestText best =
        match best with
        | Teams -> "Teams/coaches"
        | Players -> "Players"
        | Goalkeepers -> "Goalkeepers"
        | Defenders -> "Defenders"
        | Midfielders -> "Midfielders"
        | Forwards -> "Forwards"
    let bests = [ Teams ; Players ; Goalkeepers ; Defenders ; Midfielders ; Forwards ]
    bests |> List.map (fun best -> { IsActive = best = currentBest ; TabText = best |> bestText ; TabLinkType = Internal (fun _ -> best |> Some |> dispatch) })

let private customAgo (timestamp:DateTime) =
#if TICK
    timestamp |> ago
#else
    sprintf "on %s" (timestamp |> dateAndTimeText)
#endif

let private pickedIn theme (draftOrdinal, timestamp:DateTimeOffset) =
    match draftOrdinal with
    | Some draftOrdinal -> [ str (sprintf "%s" (draftOrdinal |> draftText)) ] |> para theme paraCentredSmallest |> Some
    | None -> [ str (sprintf "Free pick (%s)" (customAgo timestamp.LocalDateTime)) ] |> para theme paraCentredSmallest |> Some

let private scoreText score =
    let scoreText = sprintf "%i" score
    if score > 0 then strong scoreText else str scoreText

let private score (points:int<point>) (pickedByPoints:int<point> option) pickedByUserId (userDic:UserDic) =
    let pointsOnly =
        let pointsText = sprintf "%i" (int points)
        if points > 0<point> then strong pointsText else if points < 0<point> then em pointsText else str pointsText
    match pickedByPoints, pickedByUserId with
    | Some pickedByPoints, Some pickedByUserId ->
        if points = pickedByPoints then pointsOnly
        else
            let (UserName userName) = pickedByUserId |> userName userDic
            let pickedByPointsText = sprintf "%i" (int pickedByPoints)
            let pickedByPoints = if pickedByPoints > 0<point> then strong pickedByPointsText else if pickedByPoints < 0<point> then em pickedByPointsText else str pickedByPointsText
            let pickedByUser = str (sprintf " for %s)" userName)
            div divDefault [ pointsOnly ; str " (" ; pickedByPoints ; pickedByUser ]
    | _ -> pointsOnly

let private renderSweepstakerSquad (useDefaultTheme, userId, squadId, squad, draftOrdinal, timestamp, userDic:UserDic, fixtureDic:FixtureDic) =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let (SquadName squadName), (CoachName coachName) = squad.SquadName, squad.CoachName
        let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
        let points, pickedByPoints = teamPoints fixtureDic squadId (timestamp |> Some)
        let score = score points pickedByPoints (userId |> Some) userDic
        yield table theme false { tableDefault with IsNarrow = true } [
            thead [
                tr false [
                    th [ [ strong "Team"] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ strong "Seeding" ] |> para theme paraCentredSmallest ]
                    th [ [ strong "Coach" ] |> para theme paraDefaultSmallest ]
                    th [ [ strong "Picked in" ] |> para theme paraCentredSmallest ]
                    th [ [ strong "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
            tbody [
                tr false [
                    td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                    td [ RctH.ofOption eliminated ]
                    td [ [ str (squad.Seeding |> seedingText) ] |> para theme paraCentredSmallest ]
                    td [ [ str coachName ] |> para theme paraDefaultSmallest ]
                    td [ RctH.ofOption ((draftOrdinal, timestamp) |> pickedIn theme) ]
                    td [ [ score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ] ] ]

let private renderSweepstakerPlayers (useDefaultTheme, userId, players:(SquadId * Squad * PlayerId * Player * DraftOrdinal option * DateTimeOffset) list, userDic:UserDic, fixtureDic:FixtureDic) =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let playerRow (squadId, squad, playerId, player, draftOrdinal, timestamp) =
            let (SquadName squadName), (PlayerName playerName), playerTypeText = squad.SquadName, player.PlayerName, player.PlayerType |> playerTypeText
            let withdrawn =
                    match player.PlayerStatus with
                    | Withdrawn _ -> [ [ str "Withdrawn" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some
                    | Active -> None
            let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
            let points, pickedByPoints = playerPoints fixtureDic (squadId, playerId) (timestamp |> Some)
            let score = score points pickedByPoints (userId |> Some) userDic
            tr false [
                td [ [ str playerName ] |> para theme paraDefaultSmallest ]
                td [ RctH.ofOption withdrawn ]
                td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                td [ RctH.ofOption eliminated ]
                td [ [ str playerTypeText ] |> para theme paraCentredSmallest ]
                td [ RctH.ofOption ((draftOrdinal, timestamp) |> pickedIn theme) ]
                td [ [ score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ]
        let players =
            players |> List.sortBy (fun (_, squad, _, player, _, _) ->
                let active = match squad.Eliminated, player.PlayerStatus with | true, _ -> 1 | false, Withdrawn _ -> 1 | false, Active -> 0
                active, player.PlayerType, squad.SquadName, player.PlayerName)
        let playerRows = players |> List.map playerRow
        yield table theme false { tableDefault with IsNarrow = true } [
            thead [
                tr false [
                    th [ [ strong "Player" ] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ strong "Team"] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ strong "Position" ] |> para theme paraCentredSmallest ]
                    th [ [ strong "Picked in" ] |> para theme paraCentredSmallest ]
                    th [ [ strong "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
            tbody [ yield! playerRows ] ] ]

let private pickedByTag theme (userDic:UserDic) (authUser:AuthUser option) (pickedBy:PickedBy option) =
    match pickedBy with
    | Some (userId, draftOrdinal, timestamp) ->
        let (UserName userName) = userId |> userName userDic
        let pickedBy =
            match draftOrdinal with
            | Some draftOrdinal -> [ div divDefault [ strong userName ; str (sprintf " (%s)" (draftOrdinal |> draftTextLower)) ] ]
            | None -> [ div divDefault [ strong userName ; str (sprintf " (%s)" (customAgo timestamp.LocalDateTime)) ] ]
        let tagData = match authUser with | Some authUser when authUser.UserId = userId -> tagSuccess | Some _ | None -> tagPrimary
        pickedBy |> tag theme { tagData with IsRounded = false } |> Some
    | None -> None

let private renderBestTeams (useDefaultTheme, unpickedOnly, squadDic:SquadDic, userDic:UserDic, fixtureDic:FixtureDic, authUser) =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let squadRow (squad, points, pickedByPoints, pickedByUserId) =
            let (SquadName squadName), (CoachName coachName) = squad.SquadName, squad.CoachName
            let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
            let pickedByTag = if unpickedOnly |> not then squad.PickedBy |> pickedByTag theme userDic authUser else None
            let score = score points pickedByPoints pickedByUserId userDic
            tr false [
                td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                td [ RctH.ofOption eliminated ]
                td [ [ str (squad.Seeding |> seedingText) ] |> para theme paraCentredSmallest ]
                td [ [ str coachName ] |> para theme paraDefaultSmallest ]
                td [ RctH.ofOption pickedByTag ]
                td [ [ score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ]
        let squads =
            squadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> squadId, squad)
            |> List.filter (fun (_, squad) -> match squad.PickedBy with | Some _ -> unpickedOnly |> not | None -> true)
            |> List.map (fun (squadId, squad) ->
                let pickedByUserId, pickedDate = match squad.PickedBy with | Some (userId, _, date) -> userId |> Some, date |> Some | None -> None, None
                let points, pickedByPoints = teamPoints fixtureDic squadId pickedDate
                squad, points, pickedByPoints, pickedByUserId)
            |> List.filter (fun (_, points, _, _) -> points > 0<point>)
            |> List.sortBy (fun (squad, points, _, _) -> -points, squad.SquadName)
        let squadRows = squads |> List.map squadRow
        let pickedByHeader = if unpickedOnly |> not then [ strong "Picked by" ] |> para theme paraDefaultSmallest |> Some else None
        yield table theme false { tableDefault with IsNarrow = true} [
            thead [
                tr false [
                    th [ [ strong "Team"] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ strong "Seeding" ] |> para theme paraCentredSmallest ]
                    th [ [ strong "Coach" ] |> para theme paraDefaultSmallest ]
                    th [ RctH.ofOption pickedByHeader ]
                    th [ [ strong "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
            tbody [ yield! squadRows ] ] ]

let private renderBestPlayers (useDefaultTheme, playerType, unpickedOnly, squadDic:SquadDic, userDic:UserDic, fixtureDic:FixtureDic, authUser) =
    let theme = getTheme useDefaultTheme
    div divCentred [
        let playerRow (squad, player, points, pickedByPoints, pickedByUserId) =
            let (SquadName squadName), (PlayerName playerName) = squad.SquadName, player.PlayerName
            let withdrawn =
                    match player.PlayerStatus with
                    | Withdrawn _ -> [ [ str "Withdrawn" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some
                    | Active -> None
            let eliminated = if squad.Eliminated then [ [ str "Eliminated" ] |> tag theme { tagWarning with IsRounded = false } ] |> para theme paraDefaultSmallest |> Some else None
            let playerTypeText = match playerType with | Some _ -> None | None -> [ str (player.PlayerType |> playerTypeText) ] |> para theme paraCentredSmallest |> Some
            let pickedByTag = if unpickedOnly |> not then player.PickedBy |> pickedByTag theme userDic authUser else None
            let score = score points pickedByPoints pickedByUserId userDic
            tr false [
                td [ [ str playerName ] |> para theme paraDefaultSmallest ]
                td [ RctH.ofOption withdrawn ]
                td [ [ str squadName ] |> para theme paraDefaultSmallest ]
                td [ RctH.ofOption eliminated ]
                td [ RctH.ofOption playerTypeText ]
                td [ RctH.ofOption pickedByTag ]
                td [ [ score ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ]
        let squads = squadDic |> List.ofSeq |> List.map (fun (KeyValue (squadId, squad)) -> squadId, squad)
        let players =
            squads |> List.map (fun (squadId, squad) ->
                squad.PlayerDic |> List.ofSeq |> List.map (fun (KeyValue (playerId, player)) -> playerId, player)
                |> List.filter (fun (_, player) -> match player.PickedBy with | Some _ -> unpickedOnly |> not | None -> true)
                |> List.filter (fun (_, player) -> match playerType with | Some playerType -> playerType = player.PlayerType | None -> true)
                |> List.map (fun (playerId, player) ->
                    let pickedByUserId, pickedDate = match player.PickedBy with | Some (userId, _, date) -> userId |> Some, date |> Some | None -> None, None
                    let points, pickedByPoints = playerPoints fixtureDic (squadId, playerId) pickedDate
                    squad, player, points, pickedByPoints, pickedByUserId))
            |> List.collect id
            |> List.filter (fun (_, _, points, _, _) -> points > 0<point>)
            |> List.sortBy (fun (squad, player, points, _, _) ->
                let active = match squad.Eliminated, player.PlayerStatus with | true, _ -> 1 | false, Withdrawn _ -> 1 | false, Active -> 0
                -points, active, squad.SquadName, player.PlayerName)
        let playerRows = players |> List.map playerRow
        let playerTypeHeader = match playerType with | Some _ -> None | None -> [ strong "Position" ] |> para theme paraCentredSmallest |> Some
        let pickedByHeader = if unpickedOnly |> not then [ strong "Picked by" ] |> para theme paraDefaultSmallest |> Some else None
        yield table theme false { tableDefault with IsNarrow = true } [
            thead [
                tr false [
                    th [ [ strong "Player" ] |> para theme paraDefaultSmallest ]
                    th []
                    th [ [ strong "Team" ] |> para theme paraDefaultSmallest ]
                    th []
                    th [ RctH.ofOption playerTypeHeader ]
                    th [ RctH.ofOption pickedByHeader ]
                    th [ [ strong "Score" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
            tbody [ yield! playerRows ] ] ]

let private renderBest (useDefaultTheme, best, unpickedOnly, squadDic, userDic, fixtureDic, authUser) =
    match best with
    | Teams -> renderBestTeams (useDefaultTheme, unpickedOnly, squadDic, userDic, fixtureDic, authUser)
    | Players -> renderBestPlayers (useDefaultTheme, None, unpickedOnly, squadDic, userDic, fixtureDic, authUser)
    | Goalkeepers -> renderBestPlayers (useDefaultTheme, Goalkeeper |> Some, unpickedOnly, squadDic, userDic, fixtureDic, authUser)
    | Defenders -> renderBestPlayers (useDefaultTheme, Defender |> Some, unpickedOnly, squadDic, userDic, fixtureDic, authUser)
    | Midfielders -> renderBestPlayers (useDefaultTheme, Midfielder |> Some, unpickedOnly, squadDic, userDic, fixtureDic, authUser)
    | Forwards -> renderBestPlayers (useDefaultTheme, Forward |> Some, unpickedOnly, squadDic, userDic, fixtureDic, authUser)

let render (useDefaultTheme, state, authUser:AuthUser option, usersProjection:Projection<_ * UserDic>, squadsProjection:Projection<_ * SquadDic>, fixturesProjection:Projection<_ * FixtureDic>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ strong "Scores" ] |> para theme paraCentredSmall
        yield hr theme false
        match usersProjection, squadsProjection, fixturesProjection with
        | Pending, _, _ | _, Pending, _ | _, _, Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed, _, _ | _, Failed, _ | _, _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, userDic), Ready (_, squadDic), Ready (_, fixtureDic) ->
            let currentScoresFilter = state.CurrentScoresFilter
            let users =
                squadDic |> List.ofSeq |> List.map (fun (KeyValue (_, squad)) ->
                    let users =
                        squad.PlayerDic |> List.ofSeq |> List.choose (fun (KeyValue (_, player)) -> player.PickedBy)
                        |> List.map (fun (userId, _, _) -> userId)
                    let squadUserId = match squad.PickedBy with | Some (userId, _, _) -> [ userId ] | None -> []
                    squadUserId @ users)
                |> List.collect id |> List.distinct |> List.map (fun userId -> userId, userId |> userName userDic) |> List.sortBy snd
            let filterTabs = filterTabs currentScoresFilter dispatch
            yield lazyViewOrHMR2 renderStandings (useDefaultTheme, users, squadDic, fixtureDic) dispatch
            yield br
            yield div divCentred [ tabs theme { tabsDefault with TabsSize = Normal ; Tabs = filterTabs } ]
            match currentScoresFilter with
            | Sweepstaker currentUserId ->
                let currentUserId = match users |> List.filter (fun (userId, _) -> userId |> Some = currentUserId) with | _ :: _ -> currentUserId | [] -> None
                let currentUserId =
                    match currentUserId with
                    | Some currentUserId -> currentUserId |> Some
                    | None -> match users |> List.sortBy snd with | (userId, _) :: _ -> userId |> Some | [] -> None
                match currentUserId with
                | Some currentUserId ->
                    let sweepstakerTabs = sweepstakerTabs users currentUserId dispatch
                    let squad, players = currentUserId |> pickedByUser squadDic
                    let pickedCounts = (squad, players) |> pickedCounts
                    let stillRequired = pickedCounts |> stillRequired
                    yield div divCentred [ tabs theme { tabsDefault with Tabs = sweepstakerTabs } ]
                    yield br
                    match stillRequired with
                    | Some stillRequired ->
                        yield [ strong stillRequired ] |> para theme paraCentredSmallest
                        yield br
                    | None -> ()
                    match squad with
                    | Some (squadId, squad, draftOrdinal, timestamp) ->
                        yield lazyViewOrHMR renderSweepstakerSquad (useDefaultTheme, currentUserId, squadId, squad, draftOrdinal, timestamp, userDic, fixtureDic)
                    | None -> ()
                    yield br
                    match players with
                    | _ :: _ ->
                        yield lazyViewOrHMR renderSweepstakerPlayers (useDefaultTheme, currentUserId, players, userDic, fixtureDic)
                    | [] -> ()
                | None -> yield [ br ; str "Coming soon" ] |> para theme paraCentredSmaller // note: should never happen
            | Best best ->
                let best = match best with | Some best -> best | None -> Teams
                let bestTabs = bestTabs best (ShowBest >> dispatch)
                yield div divCentred [ tabs theme { tabsDefault with Tabs = bestTabs } ]
                yield br
                yield lazyViewOrHMR renderBest (useDefaultTheme, best, false, squadDic, userDic, fixtureDic, authUser)
            | BestUnpicked best ->
                let best = match best with | Some best -> best | None -> Teams
                let bestTabs = bestTabs best (ShowBestUnpicked >> dispatch)
                yield div divCentred [ tabs theme { tabsDefault with Tabs = bestTabs } ]
                yield br
                yield lazyViewOrHMR renderBest (useDefaultTheme, best, true, squadDic, userDic, fixtureDic, authUser) ]
