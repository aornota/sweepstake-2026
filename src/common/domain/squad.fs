module Aornota.Sweepstake2026.Common.Domain.Squad

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Revision

open System

type SquadId = | SquadId of guid : Guid with static member Create () = Guid.NewGuid () |> SquadId

type SquadName = | SquadName of squadName : string
type CoachName = | CoachName of coachName : string

type Seeding = | Seeding of seeding : int

type PlayerId = | PlayerId of guid : Guid with static member Create () = Guid.NewGuid () |> PlayerId

type PlayerName = | PlayerName of playerName : string

type PlayerType = | Goalkeeper | Defender | Midfielder | Forward

type PlayerStatus = | Active | Withdrawn of dateWithdrawn : DateTimeOffset option

type PickedBy = UserId * DraftOrdinal option * DateTimeOffset

type PlayerDto = { PlayerId : PlayerId ; PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus ; PickedBy : PickedBy option }

type SquadOnlyDto = { SquadId : SquadId ; Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding option ; CoachName : CoachName ; Eliminated : bool ; PickedBy : PickedBy option }

type SquadDto = { SquadOnlyDto : SquadOnlyDto ; PlayerDtos : PlayerDto list }

let [<Literal>] MAX_PLAYERS_PER_SQUAD = 26

let squadIsFullText = sprintf "Squad contains the maximum of %i non-withdrawn players" MAX_PLAYERS_PER_SQUAD

let playerTypeText playerType = match playerType with | Goalkeeper -> "Goalkeeper" | Defender -> "Defender" | Midfielder -> "Midfielder" | Forward -> "Forward"

let validateSquadName (squadNames:SquadName list) (SquadName squadName) =
    if String.IsNullOrWhiteSpace squadName then "Squad name must not be blank" |> Some
    else if (squadName.Trim ()).Length < 4 then "Squad name must be at least 4 characters" |> Some
    else if squadNames |> List.map (fun (SquadName squadName) -> (squadName.ToLower ()).Trim ()) |> List.contains ((squadName.ToLower ()).Trim ()) then "Squad name already in use" |> Some
    else None
let validateCoachName (CoachName coachName) =
    if String.IsNullOrWhiteSpace coachName then "Coach name must not be blank" |> Some
    else if (coachName.Trim ()).Length < 4 then "Coach name must be at least 4 characters" |> Some
    else None
let validatePlayerName (playerNames:PlayerName list) (PlayerName playerName) =
    if String.IsNullOrWhiteSpace playerName then "Player name must not be blank" |> Some
    else if (playerName.Trim ()).Length < 4 then "Player name must be at least 4 characters" |> Some
    else if playerNames |> List.map (fun (PlayerName playerName) -> (playerName.ToLower ()).Trim ()) |> List.contains ((playerName.ToLower ()).Trim ()) then "Player name already in use" |> Some
    else None
