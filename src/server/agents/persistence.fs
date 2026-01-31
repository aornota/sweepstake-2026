module Aornota.Sweepstake2026.Server.Agents.Persistence

(* Broadcasts: UsersEventsRead | UserEventWritten
               NewsEventsRead | NewsEventWritten
               SquadsEventsRead | SquadEventWritten
               FixturesEventsRead | FixtureEventWritten
               DraftsEventsRead | DraftEventWritten
               UserDraftsEventsRead | UserDraftEventWritten
   Subscribes: N/A *)

open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Json
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Common.Helpers
open Aornota.Sweepstake2026.Server.Common.JsonConverter
open Aornota.Sweepstake2026.Server.Events.DraftEvents
open Aornota.Sweepstake2026.Server.Events.FixtureEvents
open Aornota.Sweepstake2026.Server.Events.NewsEvents
open Aornota.Sweepstake2026.Server.Events.SquadEvents
open Aornota.Sweepstake2026.Server.Events.UserDraftEvents
open Aornota.Sweepstake2026.Server.Events.UserEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic
open System.IO
open System.Text

type ReadLockId = private | ReadLockId of guid : Guid

type EntityType = // note: not private since used in default-data.fs
    | Users
    | News
    | Squads
    | Fixtures
    | Drafts
    | UserDrafts

type private PersistenceInput =
    | Start of reply : AsyncReplyChannel<unit>
    | AcquireReadLock of readLockId : ReadLockId * acquiredBy : string * disposable : IDisposable * reply : AsyncReplyChannel<ReadLockId * IDisposable>
    | ReleaseReadLock of readLockId : ReadLockId
    | ReadUsersEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteUserEvent of auditUserId : UserId * rvn : Rvn * userEvent : UserEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | ReadNewsEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteNewsEvent of auditUserId : UserId * rvn : Rvn * userEvent : NewsEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | ReadSquadsEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteSquadEvent of auditUserId : UserId * rvn : Rvn * squadEvent : SquadEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | ReadFixturesEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteFixtureEvent of auditUserId : UserId * rvn : Rvn * fixtureEvent : FixtureEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | ReadDraftsEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteDraftEvent of auditUserId : UserId * rvn : Rvn * draftEvent : DraftEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | ReadUserDraftsEvents of readLockId : ReadLockId * reply : AsyncReplyChannel<Result<unit, PersistenceError>>
    | WriteUserDraftEvent of auditUserId : UserId * rvn : Rvn * userDraftEvent : UserDraftEvent * reply : AsyncReplyChannel<Result<unit, PersistenceError>>

type PersistedEvent = { Rvn : Rvn ; TimestampUtc : DateTime ; EventJson : Json ; AuditUserId : UserId } // note: *not* private because this breaks deserialization

type private ReadLockDic = Dictionary<ReadLockId, string>

let [<Literal>] private PERSISTENCE_ROOT = "./persisted"
let [<Literal>] private EVENTS_EXTENSION = "events"

let private log category = (Persistence, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let directory entityType = // note: not private since used in default-data.fs
    let entityTypeDir =
        match entityType with
        | Users -> "users"
        | News -> "news"
        | Squads -> "squads"
        | Fixtures -> "fixtures"
        | Drafts -> "drafts"
        | UserDrafts -> "user-drafts"
    sprintf "%s/%s" PERSISTENCE_ROOT entityTypeDir

let private encoding = Encoding.UTF8

let private persistenceError source errorText = ifDebugSource source errorText |> PersistenceError |> Error

let private readEvents<'a> entityType =
    let eventsExtensionWithDot = sprintf ".%s" EVENTS_EXTENSION
    let readFile (fileName:string) = // note: silently ignore non-{Guid}.EVENTS_EXTENSION files
        let fileInfo = FileInfo fileName
        match if fileInfo.Extension = eventsExtensionWithDot then fileInfo.Name.Substring (0, fileInfo.Name.Length - eventsExtensionWithDot.Length) |> Some else None with
        | Some possibleGuid ->
            match Guid.TryParse possibleGuid with
            | true, id ->
                let events =
                    File.ReadAllLines (fileInfo.FullName, encoding)
                    |> List.ofArray
                    |> List.map (fun line ->
                        let persistedEvent = Json line |> fromJson<PersistedEvent>
                        persistedEvent.Rvn, persistedEvent.EventJson |> fromJson<'a>)
                (id, events) |> Some
            | false, _ -> None
        | None -> None
    let entityTypeDir = directory entityType
    if Directory.Exists entityTypeDir then
        try
            Directory.GetFiles (entityTypeDir, sprintf "*%s" eventsExtensionWithDot)
            |> List.ofArray
            |> List.choose readFile
            |> Ok
        with exn -> ifDebug exn.Message UNEXPECTED_ERROR |> persistenceError (sprintf "Persistence.readEvents<%s>" typeof<'a>.Name)
    else [] |> Ok

let private writeEvent source entityType (entityId:Guid) rvn eventJson auditUserId =
    let source = sprintf "%s#writeEvent" source
    let entityTypeDir = directory entityType
    let fileName = sprintf "%s/%s.%s" entityTypeDir (entityId.ToString ()) EVENTS_EXTENSION
    let (Json json) = { Rvn = rvn ; TimestampUtc = DateTime.UtcNow ; EventJson = eventJson ; AuditUserId = auditUserId } |> toJson
    try
        if Directory.Exists entityTypeDir |> not then Directory.CreateDirectory entityTypeDir |> ignore
        if File.Exists fileName then
            let lineCount = (File.ReadAllLines fileName).Length
            if validateNextRvn ((Rvn lineCount) |> Some) rvn |> not then
                ifDebug (sprintf "File %s contains %i lines (Rvns) when writing %A (%A)" fileName lineCount rvn eventJson) UNEXPECTED_ERROR |> persistenceError source
            else
                File.AppendAllLines (fileName, [ json ], encoding)
                () |> Ok
        else
            if rvn <> initialRvn then
                ifDebug (sprintf "No existing file %s when writing %A (%A)" fileName rvn eventJson) UNEXPECTED_ERROR |> persistenceError source
            else
                File.WriteAllLines (fileName, [ json ], encoding)
                () |> Ok
    with exn -> ifDebug exn.Message UNEXPECTED_ERROR |> persistenceError source

type Persistence () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> notLockedForReading" |> Info |> log
                () |> reply.Reply
                return! notLockedForReading ()
            | AcquireReadLock _ -> "AcquireReadLock when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReleaseReadLock _ -> "ReleaseReadLock when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReadUsersEvents _ -> "ReadUsersEvents when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | WriteUserEvent _ -> "WriteUserEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReadNewsEvents _ -> "ReadNewsEvents when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | WriteNewsEvent _ -> "WriteNewsEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReadSquadsEvents _ -> "ReadSquadsEvents when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | WriteSquadEvent _ -> "WriteSquadEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReadFixturesEvents _ -> "ReadFixturesEvents when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | WriteFixtureEvent _ -> "WriteFixtureEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReadDraftsEvents _ -> "ReadDraftsEvents when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | WriteDraftEvent _ -> "WriteDraftEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | ReadUserDraftsEvents _ -> "ReadUserDraftsEvents when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | WriteUserDraftEvent _ -> "WriteUserDraftEvent when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and notLockedForReading () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when notLockedForReading" |> IgnoredInput |> Agent |> log ; return! notLockedForReading ()
            | AcquireReadLock (readLockId, acquiredBy, disposable, reply) ->
                sprintf "AcquireReadLock %A for '%s' when notLockedForReading -> lockedForReading (1 lock)" readLockId acquiredBy |> Info |> log
                (readLockId, disposable) |> reply.Reply
                let readLockDic = ReadLockDic ()
                (readLockId, acquiredBy) |> readLockDic.Add
                return! lockedForReading readLockDic
            | ReleaseReadLock _ -> "ReleaseReadLock when notLockedForReading" |> formatIgnoredInput |> Danger |> log ; return! notLockedForReading ()
            | ReadUsersEvents (readLockId, reply) ->
                let errorText = sprintf "ReadUsersEvents (%A) when notLockedForReading" readLockId
                errorText |> formatIgnoredInput |> Danger |> log
                errorText |> PersistenceError |> Error |> reply.Reply
                return! notLockedForReading ()
            | WriteUserEvent (auditUserId, rvn, userEvent, reply) ->
                let source = "WriteUserEvent"
                sprintf "%s when notLockedForReading -> Audit%A %A %A" source auditUserId rvn userEvent |> Verbose |> log
                let (UserId userId) = userEvent.UserId
                let result =
                    match writeEvent source Users userId rvn (toJson userEvent) auditUserId with
                    | Ok _ ->
                        (rvn, userEvent) |> UserEventWritten |> broadcaster.Broadcast
                        () |> Ok
                    | Error error -> error |> Error
                result |> logResult source (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn userEvent)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading ()
            | ReadNewsEvents (readLockId, reply) ->
                let errorText = sprintf "ReadNewsEvents (%A) when notLockedForReading" readLockId
                errorText |> formatIgnoredInput |> Danger |> log
                errorText |> PersistenceError |> Error |> reply.Reply
                return! notLockedForReading ()
            | WriteNewsEvent (auditUserId, rvn, newsEvent, reply) ->
                let source = "WriteNewsEvent"
                sprintf "%s when notLockedForReading -> Audit%A %A %A" source auditUserId rvn newsEvent |> Verbose |> log
                let (PostId postId) = newsEvent.PostId
                let result =
                    match writeEvent source News postId rvn (toJson newsEvent) auditUserId with
                    | Ok _ ->
                        (rvn, newsEvent) |> NewsEventWritten |> broadcaster.Broadcast
                        () |> Ok
                    | Error error -> error |> Error
                result |> logResult source (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn newsEvent)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading ()
            | ReadSquadsEvents (readLockId, reply) ->
                let errorText = sprintf "ReadSquadsEvents (%A) when notLockedForReading" readLockId
                errorText |> formatIgnoredInput |> Danger |> log
                errorText |> PersistenceError |> Error |> reply.Reply
                return! notLockedForReading ()
            | WriteSquadEvent (auditUserId, rvn, squadEvent, reply) ->
                let source = "WriteSquadEvent"
                sprintf "%s when notLockedForReading -> Audit%A %A %A" source auditUserId rvn squadEvent |> Verbose |> log
                let (SquadId squadId) = squadEvent.SquadId
                let result =
                    match writeEvent source Squads squadId rvn (toJson squadEvent) auditUserId with
                    | Ok _ ->
                        (rvn, squadEvent) |> SquadEventWritten |> broadcaster.Broadcast
                        () |> Ok
                    | Error error -> error |> Error
                result |> logResult source (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn squadEvent)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading ()
            | ReadFixturesEvents (readLockId, reply) ->
                let errorText = sprintf "ReadFixturesEvents (%A) when notLockedForReading" readLockId
                errorText |> formatIgnoredInput |> Danger |> log
                errorText |> PersistenceError |> Error |> reply.Reply
                return! notLockedForReading ()
            | WriteFixtureEvent (auditUserId, rvn, fixtureEvent, reply) ->
                let source = "WriteFixtureEvent"
                sprintf "%s when notLockedForReading -> Audit%A %A %A" source auditUserId rvn fixtureEvent |> Verbose |> log
                let (FixtureId fixtureId) = fixtureEvent.FixtureId
                let result =
                    match writeEvent source Fixtures fixtureId rvn (toJson fixtureEvent) auditUserId with
                    | Ok _ ->
                        (rvn, fixtureEvent) |> FixtureEventWritten |> broadcaster.Broadcast
                        () |> Ok
                    | Error error -> error |> Error
                result |> logResult source (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn fixtureEvent)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading ()
            | ReadDraftsEvents (readLockId, reply) ->
                let errorText = sprintf "ReadDraftsEvents (%A) when notLockedForReading" readLockId
                errorText |> formatIgnoredInput |> Danger |> log
                errorText |> PersistenceError |> Error |> reply.Reply
                return! notLockedForReading ()
            | WriteDraftEvent (auditUserId, rvn, draftEvent, reply) ->
                let source = "WriteDraftEvent"
                sprintf "%s when notLockedForReading -> Audit%A %A %A" source auditUserId rvn draftEvent |> Verbose |> log
                let (DraftId draftId) = draftEvent.DraftId
                let result =
                    match writeEvent source Drafts draftId rvn (toJson draftEvent) auditUserId with
                    | Ok _ ->
                        (rvn, draftEvent) |> DraftEventWritten |> broadcaster.Broadcast
                        () |> Ok
                    | Error error -> error |> Error
                result |> logResult source (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn draftEvent)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading ()
            | ReadUserDraftsEvents (readLockId, reply) ->
                let errorText = sprintf "ReadUserDraftsEvents (%A) when notLockedForReading" readLockId
                errorText |> formatIgnoredInput |> Danger |> log
                errorText |> PersistenceError |> Error |> reply.Reply
                return! notLockedForReading ()
            | WriteUserDraftEvent (auditUserId, rvn, userDraftEvent, reply) ->
                let source = "WriteUserDraftEvent"
                sprintf "%s when notLockedForReading -> Audit%A %A %A" source auditUserId rvn userDraftEvent |> Verbose |> log
                let (UserDraftId userDraftId) = userDraftEvent.UserDraftId
                let result =
                    match writeEvent source UserDrafts userDraftId rvn (toJson userDraftEvent) auditUserId with
                    | Ok _ ->
                        (rvn, userDraftEvent) |> UserDraftEventWritten |> broadcaster.Broadcast
                        () |> Ok
                    | Error error -> error |> Error
                result |> logResult source (fun _ -> Some (sprintf "Audit%A %A %A" auditUserId rvn userDraftEvent)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! notLockedForReading () }
        and lockedForReading readLockDic = async {
            // Note: Scan (rather than Receive) in order to leave "skipped" inputs (e.g. WriteUserEvent) on the queue - though also ignore-but-consume some inputs (e.g. Start).
            return! inbox.Scan (fun input ->
                match input with
                | Start _ -> sprintf "Start when lockedForReading (%i lock/s)" readLockDic.Count |> IgnoredInput |> Agent |> log ; lockedForReading readLockDic |> Some
                | AcquireReadLock (readLockId, acquiredBy, disposable, reply) ->
                    let source = "AcquireReadLock"
                    if readLockId |> readLockDic.ContainsKey |> not then
                        let previousCount = readLockDic.Count
                        (readLockId, acquiredBy) |> readLockDic.Add
                        sprintf "%s %A for '%s' when lockedForReading (%i lock/s) -> lockedForReading (%i lock/s)" source readLockId acquiredBy previousCount readLockDic.Count |> Info |> log
                    else // note: should never happen
                        sprintf "%s for '%s' when lockedForReading (%i lock/s) -> %A is not valid (already in use)" source acquiredBy readLockDic.Count readLockId |> Danger |> log
                    (readLockId, disposable) |> reply.Reply
                    lockedForReading readLockDic |> Some
                | ReleaseReadLock readLockId ->
                    let source = "ReleaseReadLock"
                    if readLockId |> readLockDic.ContainsKey then
                        let previousCount = readLockDic.Count
                        readLockId |> readLockDic.Remove |> ignore
                        if readLockDic.Count = 0 then
                            sprintf "%s %A when lockedForReading (%i lock/s) -> 0 locks -> notLockedForReading" source readLockId previousCount |> Info |> log
                            notLockedForReading () |> Some
                        else
                            sprintf "%s %A when lockedForReading (%i lock/s) -> lockedForReading (%i lock/s)" source readLockId previousCount readLockDic.Count |> Info |> log
                            lockedForReading readLockDic |> Some
                    else // note: should never happen
                        sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLockDic.Count readLockId |> Danger |> log
                        lockedForReading readLockDic |> Some
                | ReadUsersEvents (readLockId, reply) ->
                    let source = "ReadUsersEvents"
                    sprintf "%s (%A) when lockedForReading (%i lock/s)" source readLockId readLockDic.Count |> Verbose |> log
                    let result =
                        if readLockId |> readLockDic.ContainsKey |> not then // note: should never happen
                            let errorText = sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLockDic.Count readLockId
                            ifDebug errorText UNEXPECTED_ERROR |> PersistenceError |> Error
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents Users with
                            | Ok usersEvents ->
                                (usersEvents |> List.map (fun (id, userEvents) -> UserId id, userEvents)) |> UsersEventsRead |> broadcaster.Broadcast
                                usersEvents |> Ok
                            | Error error -> error |> Error)
                    let successText = (fun usersEvents ->
                        let eventsCount = usersEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        sprintf "%i UserEvent/s for %i UserId/s" eventsCount usersEvents.Length |> Some)
                    result |> logResult source successText // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    lockedForReading readLockDic |> Some
                | WriteUserEvent _ -> sprintf "WriteUserEvent when lockedForReading (%i lock/s)" readLockDic.Count |> SkippedInput |> Agent |> log ; None
                | ReadNewsEvents (readLockId, reply) ->
                    let source = "ReadNewsEvents"
                    sprintf "%s (%A) when lockedForReading (%i lock/s)" source readLockId readLockDic.Count |> Verbose |> log
                    let result =
                        if readLockId |> readLockDic.ContainsKey |> not then // note: should never happen
                            let errorText = sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLockDic.Count readLockId
                            ifDebug errorText UNEXPECTED_ERROR |> PersistenceError |> Error
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents News with
                            | Ok newsEvents ->
                                (newsEvents |> List.map (fun (id, newsEvents) -> PostId id, newsEvents)) |> NewsEventsRead |> broadcaster.Broadcast
                                newsEvents |> Ok
                            | Error error -> error |> Error)
                    let successText = (fun newsEvents ->
                        let eventsCount = newsEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        sprintf "%i NewsEvent/s for %i PostId/s" eventsCount newsEvents.Length |> Some)
                    result |> logResult source successText // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    lockedForReading readLockDic |> Some
                | WriteNewsEvent _ -> sprintf "WriteNewsEvent when lockedForReading (%i lock/s)" readLockDic.Count |> SkippedInput |> Agent |> log ; None
                | ReadSquadsEvents (readLockId, reply) ->
                    let source = "ReadSquadsEvents"
                    sprintf "%s (%A) when lockedForReading (%i lock/s)" source readLockId readLockDic.Count |> Verbose |> log
                    let result =
                        if readLockId |> readLockDic.ContainsKey |> not then // note: should never happen
                            let errorText = sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLockDic.Count readLockId
                            ifDebug errorText UNEXPECTED_ERROR |> PersistenceError |> Error
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents Squads with
                            | Ok squadsEvents ->
                                (squadsEvents |> List.map (fun (id, squadsEvents) -> SquadId id, squadsEvents)) |> SquadsEventsRead |> broadcaster.Broadcast
                                squadsEvents |> Ok
                            | Error error -> error |> Error)
                    let successText = (fun squadsEvents ->
                        let eventsCount = squadsEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        sprintf "%i SquadEvent/s for %i SquadId/s" eventsCount squadsEvents.Length |> Some)
                    result |> logResult source successText // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    lockedForReading readLockDic |> Some
                | WriteSquadEvent _ -> sprintf "WriteSquadEvent when lockedForReading (%i lock/s)" readLockDic.Count |> SkippedInput |> Agent |> log ; None
                | ReadFixturesEvents (readLockId, reply) ->
                    let source = "ReadFixturesEvents"
                    sprintf "%s (%A) when lockedForReading (%i lock/s)" source readLockId readLockDic.Count |> Verbose |> log
                    let result =
                        if readLockId |> readLockDic.ContainsKey |> not then // note: should never happen
                            let errorText = sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLockDic.Count readLockId
                            ifDebug errorText UNEXPECTED_ERROR |> PersistenceError |> Error
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents Fixtures with
                            | Ok fixturesEvents ->
                                (fixturesEvents |> List.map (fun (id, fixturesEvents) -> FixtureId id, fixturesEvents)) |> FixturesEventsRead |> broadcaster.Broadcast
                                fixturesEvents |> Ok
                            | Error error -> error |> Error)
                    let successText = (fun fixturesEvents ->
                        let eventsCount = fixturesEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        sprintf "%i FixtureEvent/s for %i FixtureId/s" eventsCount fixturesEvents.Length |> Some)
                    result |> logResult source successText // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    lockedForReading readLockDic |> Some
                | WriteFixtureEvent _ -> sprintf "WriteFixtureEvent when lockedForReading (%i lock/s)" readLockDic.Count |> SkippedInput |> Agent |> log ; None
                | ReadDraftsEvents (readLockId, reply) ->
                    let source = "ReadDraftsEvents"
                    sprintf "%s (%A) when lockedForReading (%i lock/s)" source readLockId readLockDic.Count |> Verbose |> log
                    let result =
                        if readLockId |> readLockDic.ContainsKey |> not then // note: should never happen
                            let errorText = sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLockDic.Count readLockId
                            ifDebug errorText UNEXPECTED_ERROR |> PersistenceError |> Error
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents Drafts with
                            | Ok draftsEvents ->
                                (draftsEvents |> List.map (fun (id, draftsEvents) -> DraftId id, draftsEvents)) |> DraftsEventsRead |> broadcaster.Broadcast
                                draftsEvents |> Ok
                            | Error error -> error |> Error)
                    let successText = (fun draftsEvents ->
                        let eventsCount = draftsEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        sprintf "%i DraftEvent/s for %i DraftId/s" eventsCount draftsEvents.Length |> Some)
                    result |> logResult source successText // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    lockedForReading readLockDic |> Some
                | WriteDraftEvent _ -> sprintf "WriteDraftEvent when lockedForReading (%i lock/s)" readLockDic.Count |> SkippedInput |> Agent |> log ; None
                | ReadUserDraftsEvents (readLockId, reply) ->
                    let source = "ReadUserDraftsEvents"
                    sprintf "%s (%A) when lockedForReading (%i lock/s)" source readLockId readLockDic.Count |> Verbose |> log
                    let result =
                        if readLockId |> readLockDic.ContainsKey |> not then // note: should never happen
                            let errorText = sprintf "%s when lockedForReading (%i lock/s) -> %A is not valid (not in use)" source readLockDic.Count readLockId
                            ifDebug errorText UNEXPECTED_ERROR |> PersistenceError |> Error
                        else Ok ()
                        |> Result.bind (fun _ ->
                            match readEvents UserDrafts with
                            | Ok userDraftsEvents ->
                                (userDraftsEvents |> List.map (fun (id, userDraftsEvents) -> UserDraftId id, userDraftsEvents)) |> UserDraftsEventsRead |> broadcaster.Broadcast
                                userDraftsEvents |> Ok
                            | Error error -> error |> Error)
                    let successText = (fun userDraftsEvents ->
                        let eventsCount = userDraftsEvents |> List.sumBy (fun (_, events) -> events |> List.length)
                        sprintf "%i UserDraftEvent/s for %i UserDraftId/s" eventsCount userDraftsEvents.Length |> Some)
                    result |> logResult source successText // note: log success/failure here (rather than assuming that calling code will do so)
                    result |> discardOk |> reply.Reply
                    lockedForReading readLockDic |> Some
                | WriteUserDraftEvent _ -> sprintf "WriteUserDraftEvent when lockedForReading (%i lock/s)" readLockDic.Count |> SkippedInput |> Agent |> log ; None) }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Source.Persistence |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    // TODO-NMB-MEDIUM: Subscribe to Tick [in Start] - then periodically "auto-backup" everything in PERSISTENCE_ROOT?...
    member __.Start () = Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.AcquireReadLockAsync acquiredBy = (fun reply ->
        let readLockId = Guid.NewGuid () |> ReadLockId
        (readLockId, acquiredBy, { new IDisposable with member __.Dispose () = ReleaseReadLock readLockId |> agent.Post }, reply) |> AcquireReadLock) |> agent.PostAndAsyncReply
    member __.ReadUsersEventsAsync readLockId = (fun reply -> (readLockId, reply) |> ReadUsersEvents) |> agent.PostAndAsyncReply
    member __.WriteUserEventAsync (auditUserId, rvn, userEvent) = (fun reply -> (auditUserId, rvn, userEvent, reply) |> WriteUserEvent) |> agent.PostAndAsyncReply
    member __.ReadNewsEventsAsync readLockId = (fun reply -> (readLockId, reply) |> ReadNewsEvents) |> agent.PostAndAsyncReply
    member __.WriteNewsEventAsync (auditUserId, rvn, newsEvent) = (fun reply -> (auditUserId, rvn, newsEvent, reply) |> WriteNewsEvent) |> agent.PostAndAsyncReply
    member __.ReadSquadsEventsAsync readLockId = (fun reply -> (readLockId, reply) |> ReadSquadsEvents) |> agent.PostAndAsyncReply
    member __.WriteSquadEventAsync (auditUserId, rvn, squadEvent) = (fun reply -> (auditUserId, rvn, squadEvent, reply) |> WriteSquadEvent) |> agent.PostAndAsyncReply
    member __.ReadFixturesEventsAsync readLockId = (fun reply -> (readLockId, reply) |> ReadFixturesEvents) |> agent.PostAndAsyncReply
    member __.WriteFixtureEventAsync (auditUserId, rvn, fixtureEvent) = (fun reply -> (auditUserId, rvn, fixtureEvent, reply) |> WriteFixtureEvent) |> agent.PostAndAsyncReply
    member __.ReadDraftsEventsAsync readLockId = (fun reply -> (readLockId, reply) |> ReadDraftsEvents) |> agent.PostAndAsyncReply
    member __.WriteDraftEventAsync (auditUserId, rvn, draftEvent) = (fun reply -> (auditUserId, rvn, draftEvent, reply) |> WriteDraftEvent) |> agent.PostAndAsyncReply
    member __.ReadUserDraftsEventsAsync readLockId = (fun reply -> (readLockId, reply) |> ReadUserDraftsEvents) |> agent.PostAndAsyncReply
    member __.WriteUserDraftEventAsync (auditUserId, rvn, userDraftEvent) = (fun reply -> (auditUserId, rvn, userDraftEvent, reply) |> WriteUserDraftEvent) |> agent.PostAndAsyncReply

let persistence = Persistence ()

let readPersistedEvents () =
    let acquiredBy = "readPersistedEvents"
    let (readLockId, readLock) = acquiredBy |> persistence.AcquireReadLockAsync |> Async.RunSynchronously
    use _disposable = readLock
    (* TEMP-NMB: Try calling WriteUserEventAsync in read lock (should be "skipped" but processed later)...
    "calling WriteUserEventAsync in read lock" |> Info |> log
    let userEvent = (UserId (Guid "ffffffff-ffff-ffff-ffff-ffffffffffff"), UserName "skippy", Salt "salt", Hash "hash", Pleb) |> UserCreated
    // Note: Need to call via Async.Start since WriteUserEventAsync will block (since input will be "skipped" when in read lock) until _disposable disposed (which will release the read lock).
    async {
        let! _ = (UserId Guid.Empty, initialRvn, userEvent) |> persistence.WriteUserEventAsync
        return () } |> Async.Start *)
    readLockId |> persistence.ReadUsersEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
    readLockId |> persistence.ReadNewsEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
    readLockId |> persistence.ReadSquadsEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
    readLockId |> persistence.ReadFixturesEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
    readLockId |> persistence.ReadDraftsEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
    readLockId |> persistence.ReadUserDraftsEventsAsync |> Async.RunSynchronously |> ignore // note: success/failure already logged by agent
