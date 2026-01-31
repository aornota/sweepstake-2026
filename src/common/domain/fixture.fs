module Aornota.Sweepstake2026.Common.Domain.Fixture

open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnitsOfMeasure

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Squad

open System

type FixtureId = | FixtureId of guid : Guid with static member Create () = Guid.NewGuid () |> FixtureId

type Role = | Home | Away

type Stage =
    | Group of group : Group
    | RoundOf16 of matchNumber : uint32
    | QuarterFinal of quarterFinalOrdinal : uint32
    | SemiFinal of semiFinalOrdinal : uint32
    | Final

type Unconfirmed =
    | Winner of stage : Stage
    | RunnerUp of group : Group
    | ThirdPlace of groups : Group list

type Participant =
    | Confirmed of squadId : SquadId
    | Unconfirmed of unconfirmed : Unconfirmed

type MatchEventId = | MatchEventId of guid : Guid with static member Create () = Guid.NewGuid () |> MatchEventId

type PenaltyOutcome =
    | Scored
    | Missed
    | Saved of savedBy : SquadId * PlayerId

type MatchEvent =
    | Goal of squadId : SquadId * playerId : PlayerId * assistedBy : PlayerId option
    | OwnGoal of squadId : SquadId * playerId : PlayerId
    | Penalty of squadId : SquadId * playerId : PlayerId * penaltyOutcome : PenaltyOutcome
    | YellowCard of squadId : SquadId * playerId : PlayerId
    | RedCard of squadId : SquadId * playerId : PlayerId
    | CleanSheet of squadId : SquadId * playerId : PlayerId // note: not currently catering for "shared" clean sheet points
    | PenaltyShootout of homeScore : uint32 * awayScore : uint32
    | ManOfTheMatch of squadId : SquadId * playerId : PlayerId

type PenaltyShootoutOutcome = { HomeScore : uint32 ; AwayScore : uint32 }
type MatchOutcome = { HomeGoals : uint32 ; AwayGoals : uint32 ; PenaltyShootoutOutcome : PenaltyShootoutOutcome option }

type Card =
    | Yellow
    | SecondYellow
    | Red

type TeamScoreEvent =
    | MatchWon
    | MatchDrawn
    | PlayerCard of playerId : PlayerId * card : Card

type PlayerScoreEvent =
    | GoalScored
    | GoalAssisted
    | OwnGoalScored
    | PenaltyScored
    | PenaltyMissed
    | Card of card : Card
    | PenaltySaved
    | CleanSheetKept
    | ManOfTheMatchAwarded

type ScoreEvents = { TeamScoreEvents : (TeamScoreEvent * int<point>) list ; PlayerScoreEvents : (PlayerId * (PlayerScoreEvent * int<point>) list) list }

type MatchResult = { MatchOutcome : MatchOutcome ; HomeScoreEvents : ScoreEvents ; AwayScoreEvents : ScoreEvents ; MatchEvents : (MatchEventId * MatchEvent) list }

type FixtureDto =
    { FixtureId : FixtureId ; Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset ; MatchResult : MatchResult option }
