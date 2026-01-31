module Aornota.Sweepstake2026.Server.Events.FixtureEvents

open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.Squad

open System

type FixtureEvent =
    | FixtureCreated of fixtureId : FixtureId * stage : Stage * homeParticipant : Participant * awayParticipant : Participant * kickOff : DateTimeOffset
    | ParticipantConfirmed of fixtureId : FixtureId * role : Role * squadId : SquadId
    | MatchEventAdded of fixtureId : FixtureId * matchEventId : MatchEventId * matchEvent : MatchEvent
    | MatchEventRemoved of fixtureId : FixtureId * matchEventId : MatchEventId
    with
        member self.FixtureId =
            match self with
            | FixtureCreated (fixtureId, _, _, _, _) -> fixtureId
            | ParticipantConfirmed (fixtureId, _, _) -> fixtureId
            | MatchEventAdded (fixtureId, _, _) -> fixtureId
            | MatchEventRemoved (fixtureId, _) -> fixtureId
