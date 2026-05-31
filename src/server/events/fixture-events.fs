module Aornota.Sweepstake2026.Server.Events.FixtureEvents

open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Markdown

open System

type FixtureEvent =
    | FixtureCreated of fixtureId : FixtureId * stage : Stage * homeParticipant : Participant * awayParticipant : Participant * kickOff : DateTimeOffset
    | ParticipantConfirmed of fixtureId : FixtureId * role : Role * squadId : SquadId
    | MatchEventAdded of fixtureId : FixtureId * matchEventId : MatchEventId * matchEvent : MatchEvent
    | MatchEventRemoved of fixtureId : FixtureId * matchEventId : MatchEventId
    | CustomMessageAdded of fixtureId : FixtureId * userId : UserId * customMessage : Markdown
    | CustomMessageChanged of fixtureId : FixtureId  * userId : UserId * customMessage : Markdown
    | CustomMessageRemoved of fixtureId : FixtureId
    with
        member self.FixtureId =
            match self with
            | FixtureCreated (fixtureId, _, _, _, _) -> fixtureId
            | ParticipantConfirmed (fixtureId, _, _) -> fixtureId
            | MatchEventAdded (fixtureId, _, _) -> fixtureId
            | MatchEventRemoved (fixtureId, _) -> fixtureId
            | CustomMessageAdded (fixtureId, _, _) -> fixtureId
            | CustomMessageChanged (fixtureId, _, _) -> fixtureId
            | CustomMessageRemoved fixtureId -> fixtureId
