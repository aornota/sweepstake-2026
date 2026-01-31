module Aornota.Sweepstake2026.Common.Domain.Draft

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Revision

open System

type DraftId = | DraftId of guid : Guid with static member Create () = Guid.NewGuid () |> DraftId

type DraftType =
    | Constrained of starts : DateTimeOffset * ends : DateTimeOffset
    | Unconstrained

type DraftStatus =
    | PendingOpen of starts : DateTimeOffset * ends : DateTimeOffset
    | Opened of ends : DateTimeOffset
    | PendingProcessing of processingStarted : bool
    | Processed
    | PendingFreeSelection
    | FreeSelection

type DraftPick =
    | TeamPicked of squadId : SquadId
    | PlayerPicked of squadId : SquadId * playerId : PlayerId

type UserDraftPick =
    | TeamPick of squadId : SquadId
    | PlayerPick of squadId : SquadId * playerId : PlayerId

type UserDraftPickDto = { UserDraftPick : UserDraftPick ; Rank : int }

type ProcessingEvent =
    | ProcessingStarted of seed : int
    | WithdrawnPlayersIgnored of ignored : (UserId * (SquadId * PlayerId) list) list
    | RoundStarted of round : uint32
    | AlreadyPickedIgnored of ignored : (UserId * DraftPick list) list
    | NoLongerRequiredIgnored of ignored : (UserId * DraftPick list) list
    | UncontestedPick of draftPick : DraftPick * userId : UserId
    | ContestedPick of draftPick : DraftPick * userDetails : (UserId * uint32 * float option) list * winner : UserId
    | PickPriorityChanged of userId : UserId * pickPriority : uint32
    | Picked of draftOrdinal : DraftOrdinal * draftPick : DraftPick * userId : UserId * timestamp : DateTimeOffset

type ProcessingDetails = { UserDraftPicks : (UserId * UserDraftPickDto list) list ; ProcessingEvents : ProcessingEvent list }

type DraftDto = { DraftId : DraftId ; Rvn : Rvn ; DraftOrdinal : DraftOrdinal ; DraftStatus : DraftStatus ; ProcessingDetails : ProcessingDetails option }

type PriorityChange = | Increase | Decrease

type UserDraftKey = UserId * DraftId

type CurrentUserDraftDto = { UserDraftKey : UserDraftKey ; Rvn : Rvn ; UserDraftPickDtos : UserDraftPickDto list }

type UserDraftSummaryDto = { UserDraftKey : UserDraftKey ; PickCount : int }

let [<Literal>] MAX_TEAM_PICKS = 1
let [<Literal>] MAX_GOALKEEPER_PICKS = 1
let [<Literal>] MAX_OUTFIELD_PLAYER_PICKS = 10

let draftText (DraftOrdinal draftOrdinal) =
    if draftOrdinal = 1 then "First draft"
    else if draftOrdinal = 2 then "Second draft"
    else if draftOrdinal = 4 then "Third draft"
    else sprintf "Draft #%i" draftOrdinal
let draftTextLower (DraftOrdinal draftOrdinal) =
    if draftOrdinal = 1 then "first draft"
    else if draftOrdinal = 2 then "second draft"
    else if draftOrdinal = 4 then "third draft"
    else sprintf "draft #%i" draftOrdinal

let defaultDraftStatus draftType = match draftType with | Constrained (starts, ends) -> (starts, ends) |> PendingOpen | Unconstrained -> PendingFreeSelection

let isActive draftStatus = match draftStatus with | Opened _ | PendingProcessing _ -> true | PendingOpen _ | Processed | PendingFreeSelection | FreeSelection -> false
