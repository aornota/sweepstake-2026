module Aornota.Sweepstake2026.Server.Signal

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Events.DraftEvents
open Aornota.Sweepstake2026.Server.Events.FixtureEvents
open Aornota.Sweepstake2026.Server.Events.NewsEvents
open Aornota.Sweepstake2026.Server.Events.SquadEvents
open Aornota.Sweepstake2026.Server.Events.UserDraftEvents
open Aornota.Sweepstake2026.Server.Events.UserEvents

open System

type UserRead = { UserId : UserId ; Rvn : Rvn ; UserName : UserName ; UserType : UserType }

type NewsRead = { PostId : PostId ; Rvn : Rvn ; UserId : UserId ; PostType : PostType ; MessageText : Markdown ; Timestamp : DateTimeOffset ; Removed : bool }

type PlayerRead = { PlayerId : PlayerId ; PlayerName : PlayerName ; PlayerType : PlayerType ; PlayerStatus : PlayerStatus }
type SquadRead = { SquadId : SquadId ; Rvn : Rvn ; SquadName : SquadName ; Group : Group ; Seeding : Seeding option ; CoachName : CoachName ; Eliminated : bool ; PlayersRead : PlayerRead list }

type MatchEventRead = { MatchEventId : MatchEventId ; MatchEvent : MatchEvent }
type FixtureRead =
    { FixtureId : FixtureId ; Rvn : Rvn ; Stage : Stage ; HomeParticipant : Participant ; AwayParticipant : Participant ; KickOff : DateTimeOffset ; MatchEventsRead : MatchEventRead list }

type DraftRead =
    { DraftId : DraftId ; Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus ; DraftPicks : (DraftPick * PickedBy) list ; ProcessingEvents : ProcessingEvent list }

type UserDraftPickRead = { UserDraftPick : UserDraftPick ; Rank : int }
type UserDraftRead = { UserDraftId : UserDraftId ; Rvn : Rvn ; UserDraftKey : UserDraftKey ; UserDraftPicksRead : UserDraftPickRead list }

type Signal =
    | Tick of ticks : int<tick> * secondsPerTick : int<second/tick>
    | SendMsg of serverMsg : ServerMsg * connectionIds : ConnectionId list
    | UsersEventsRead of usersEvents : (UserId * (Rvn * UserEvent) list) list
    | UsersRead of usersRead : UserRead list
    | UserEventWritten of rvn : Rvn * userEvent : UserEvent
    | NewsEventsRead of newsEvents : (PostId * (Rvn * NewsEvent) list) list
    | NewsRead of newsRead : NewsRead list
    | NewsEventWritten of rvn : Rvn * newsEvent : NewsEvent
    | SquadsEventsRead of squadsEvents : (SquadId * (Rvn * SquadEvent) list) list
    | SquadsRead of squadRead : SquadRead list
    | SquadEventWritten of rvn : Rvn * squadEvent : SquadEvent
    | FixturesEventsRead of fixturesEvents : (FixtureId * (Rvn * FixtureEvent) list) list
    | FixturesRead of fixturesRead : FixtureRead list
    | FixtureEventWritten of rvn : Rvn * fixtureEvent : FixtureEvent
    | DraftsEventsRead of draftsEvents : (DraftId * (Rvn * DraftEvent) list) list
    | DraftsRead of draftsRead : DraftRead list
    | DraftEventWritten of rvn : Rvn * draftEvent : DraftEvent
    | UserDraftsEventsRead of userDraftsEvents : (UserDraftId * (Rvn * UserDraftEvent) list) list
    | UserDraftsRead of userDraftsRead : UserDraftRead list
    | UserDraftEventWritten of rvn : Rvn * userDraftEvent : UserDraftEvent
    | UserSignedIn of userId : UserId
    | UserSignedOut of userId : UserId
    | UserActivity of userId : UserId
    | ConnectionsSignedOut of connectionIds : ConnectionId list
    | Disconnected of connectionId : ConnectionId
