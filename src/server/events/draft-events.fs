module Aornota.Sweepstake2026.Server.Events.DraftEvents

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User

open System

type DraftEvent =
    | DraftCreated of draftId : DraftId * draftOrdinal : DraftOrdinal * draftType : DraftType
    | DraftOpened of draftId : DraftId
    | DraftPendingProcessing of draftId : DraftId
    | DraftProcessed of draftId : DraftId
    | DraftFreeSelection of draftId : DraftId
    | ProcessingStarted of draftId : DraftId * seed : int
    | WithdrawnPlayersIgnored of draftId : DraftId * ignored : (UserId * (SquadId * PlayerId) list) list
    | RoundStarted of draftId : DraftId * round : uint32
    | AlreadyPickedIgnored of draftId : DraftId * ignored : (UserId * DraftPick list) list
    | NoLongerRequiredIgnored of draftId : DraftId * ignored : (UserId * DraftPick list) list
    | UncontestedPick of draftId : DraftId * draftPick : DraftPick * userId : UserId
    | ContestedPick of draftId : DraftId * draftPick : DraftPick * userDetails : (UserId * uint32 * float option) list * winner : UserId
    | PickPriorityChanged of draftId : DraftId * userId : UserId * pickPriority : uint32
    | Picked of draftId : DraftId * draftOrdinal : DraftOrdinal * draftPick : DraftPick * userId : UserId * timestamp : DateTimeOffset
    | FreePick of draftId : DraftId * draftPick : DraftPick * userId : UserId * timestamp : DateTimeOffset
    with
        member self.DraftId =
            match self with
            | DraftCreated (draftId, _, _) -> draftId
            | DraftOpened draftId -> draftId
            | DraftPendingProcessing draftId -> draftId
            | DraftProcessed draftId -> draftId
            | DraftFreeSelection draftId -> draftId
            | ProcessingStarted (draftId, _) -> draftId
            | WithdrawnPlayersIgnored (draftId, _) -> draftId
            | RoundStarted (draftId, _) -> draftId
            | AlreadyPickedIgnored (draftId, _) -> draftId
            | NoLongerRequiredIgnored (draftId, _) -> draftId
            | UncontestedPick (draftId, _, _) -> draftId
            | ContestedPick (draftId, _, _, _) -> draftId
            | PickPriorityChanged (draftId, _, _) -> draftId
            | Picked (draftId, _, _, _, _) -> draftId
            | FreePick (draftId, _, _, _) -> draftId
