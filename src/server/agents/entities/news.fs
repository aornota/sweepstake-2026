module Aornota.Sweepstake2026.Server.Agents.Entities.News

(* Broadcasts: TODO:SendMsg
               NewsRead
   Subscribes: NewsEventsRead *)

open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Agents.Persistence
open Aornota.Sweepstake2026.Server.Authorization
open Aornota.Sweepstake2026.Server.Common.Helpers
open Aornota.Sweepstake2026.Server.Events.NewsEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private NewsInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnNewsEventsRead of newsEvents : (PostId * (Rvn * NewsEvent) list) list
    | HandleCreatePostCmd of token : CreatePostToken * auditUserId : UserId * postId : PostId * postType : PostType * messageText : Markdown
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleChangePostCmd of token : EditOrRemovePostToken * auditUserId : UserId * postId : PostId * currentRvn : Rvn * messageText : Markdown
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>
    | HandleRemovePostCmd of token : EditOrRemovePostToken * auditUserId : UserId * postId : PostId * currentRvn : Rvn
        * reply : AsyncReplyChannel<Result<unit, AuthCmdError<string>>>

type private Post = { Rvn : Rvn ; UserId : UserId ; PostType : PostType ; MessageText : Markdown ; Timestamp : DateTimeOffset ; Removed : bool }
type private PostDic = Dictionary<PostId, Post>

let private log category = (Entity Entity.News, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private applyNewsEvent source idAndPostResult (nextRvn, newsEvent:NewsEvent) =
    let otherError errorText = otherError (sprintf "%s#applyNewsEvent" source) errorText
    match idAndPostResult, newsEvent with
    | Ok (postId, _), _ when postId <> newsEvent.PostId -> // note: should never happen
        ifDebug (sprintf "PostId mismatch for %A -> %A" postId newsEvent) UNEXPECTED_ERROR |> otherError
    | Ok (postId, None), _ when validateNextRvn None nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid initial Rvn for %A -> %A (%A)" postId nextRvn newsEvent) UNEXPECTED_ERROR |> otherError
    | Ok (postId, Some post), _ when validateNextRvn (Some post.Rvn) nextRvn |> not -> // note: should never happen
        ifDebug (sprintf "Invalid next Rvn for %A (%A) -> %A (%A)" postId post.Rvn nextRvn newsEvent) UNEXPECTED_ERROR |> otherError
    | Ok (postId, None), PostCreated (_, userId, postType, messageText, timestamp) ->
        (postId, { Rvn = nextRvn ; UserId = userId ; PostType = postType ; MessageText = messageText ; Timestamp = timestamp ; Removed = false } |> Some) |> Ok
    | Ok (postId, None), _ -> // note: should never happen
        ifDebug (sprintf "Invalid initial NewsEvent for %A -> %A" postId newsEvent) UNEXPECTED_ERROR |> otherError
    | Ok (postId, Some post), PostCreated _ -> // note: should never happen
        ifDebug (sprintf "Invalid non-initial NewsEvent for %A (%A) -> %A" postId post newsEvent) UNEXPECTED_ERROR |> otherError
    | Ok (postId, Some post), PostChanged (_, messageText) ->
        (postId, { post with Rvn = nextRvn ; MessageText = messageText } |> Some) |> Ok
    | Ok (postId, Some post), PostRemoved _ ->
        (postId, { post with Rvn = nextRvn ; Removed = true } |> Some) |> Ok
    | Error error, _ -> error |> Error

let private initializePosts source (newsEvents:(PostId * (Rvn * NewsEvent) list) list) =
    let source = sprintf "%s#initializePosts" source
    let postDic = PostDic ()
    let results =
        newsEvents
        |> List.map (fun (postId, events) ->
            match events with
            | _ :: _ -> events |> List.fold (fun idAndPostResult (rvn, newsEvent) -> applyNewsEvent source idAndPostResult (rvn, newsEvent)) (Ok (postId, None))
            | [] -> ifDebug (sprintf "No NewsEvents for %A" postId) UNEXPECTED_ERROR |> otherError source) // note: should never happen
    results
    |> List.choose (fun idAndPostResult -> match idAndPostResult with | Ok (postId, Some post) -> (postId, post) |> Some | Ok (_, None) | Error _ -> None)
    |> List.iter (fun (postId, post) -> postDic.Add (postId, post))
    let errors =
        results
        |> List.choose (fun idAndPostResult ->
            match idAndPostResult with
            | Ok (_, Some _) -> None
            | Ok (_, None) -> ifDebug (sprintf "%s: applyPostEvent returned Ok (_, None)" source) UNEXPECTED_ERROR |> OtherError |> Some // note: should never happen
            | Error error -> error |> Some)
    postDic, errors

let private updatePost postId post (postDic:PostDic) = if postId |> postDic.ContainsKey then postDic.[postId] <- post

let private tryFindPost postId onError (postDic:PostDic) =
    if postId |> postDic.ContainsKey then (postId, postDic.[postId]) |> Ok else ifDebug (sprintf "%A does not exist" postId) UNEXPECTED_ERROR |> onError

let private tryApplyNewsEvent source postId post nextRvn newsEvent =
    match applyNewsEvent source (Ok (postId, post)) (nextRvn, newsEvent) with
    | Ok (_, Some post) -> (post, nextRvn, newsEvent) |> Ok
    | Ok (_, None) -> ifDebug "applyNewsEvent returned Ok (_, None)" UNEXPECTED_ERROR |> otherCmdError source // note: should never happen
    | Error otherError -> otherError |> OtherAuthCmdError |> Error

let private tryWriteNewsEventAsync auditUserId rvn newsEvent (post:Post) = async {
    let! result = (auditUserId, rvn, newsEvent) |> persistence.WriteNewsEventAsync
    return match result with | Ok _ -> (newsEvent.PostId, post) |> Ok | Error persistenceError -> persistenceError |> AuthCmdPersistenceError |> Error }

type News () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingOnNewsEventsRead" |> Info |> log
                () |> reply.Reply
                return! pendingOnNewsEventsRead ()
            | OnNewsEventsRead _ -> "OnNewsEventsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleCreatePostCmd _ -> "HandleCreatePostCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleChangePostCmd _ -> "HandleChangePostCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleRemovePostCmd _ -> "HandleRemovePostCmd when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingOnNewsEventsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingOnNewsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnNewsEventsRead ()
            | OnNewsEventsRead newsEvents ->
                let source = "OnNewsEventsRead"
                let posts, errors = initializePosts source newsEvents
                errors |> List.iter (fun (OtherError errorText) -> errorText |> Danger |> log)
                sprintf "%s (%i post/s) when pendingOnNewsEventsRead -> managingNews (%i post/s)" source newsEvents.Length posts.Count |> Info |> log
                let newsRead =
                    posts
                    |> List.ofSeq
                    |> List.map (fun (KeyValue (postId, post)) ->
                        { PostId = postId ; Rvn = post.Rvn ; UserId = post.UserId ; PostType = post.PostType ; MessageText = post.MessageText ; Timestamp = post.Timestamp ; Removed = post.Removed })
                newsRead |> NewsRead |> broadcaster.Broadcast
                return! managingNews posts
            | HandleCreatePostCmd _ -> "HandleCreatePostCmd when pendingOnNewsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnNewsEventsRead ()
            | HandleChangePostCmd _ -> "HandleChangePostCmd when pendingOnNewsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnNewsEventsRead ()
            | HandleRemovePostCmd _ -> "HandleRemovePostCmd when pendingOnNewsEventsRead" |> IgnoredInput |> Agent |> log ; return! pendingOnNewsEventsRead () }
        and managingNews postDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> sprintf "Start when managingNews (%i post/s)" postDic.Count |> IgnoredInput |> Agent |> log ; return! managingNews postDic
            | OnNewsEventsRead _ -> sprintf "OnNewsEventsRead when managingNews (%i post/s)" postDic.Count |> IgnoredInput |> Agent |> log ; return! managingNews postDic
            | HandleCreatePostCmd (_, auditUserId, postId, postType, Markdown messageText, reply) ->
                let source = "HandleCreatePostCmd"
                sprintf "%s for %A (%A) when managingNews (%i post/s)" source postId postType postDic.Count |> Verbose |> log
                let messageText = Markdown (messageText.Trim ())
                let result =
                    if postId |> postDic.ContainsKey |> not then () |> Ok else ifDebug (sprintf "%A already exists" postId) UNEXPECTED_ERROR |> otherCmdError source
                    |> Result.bind (fun _ -> match messageText |> validatePostMessageText with | Some errorText -> errorText |> otherCmdError source | None -> () |> Ok)
                    |> Result.bind (fun _ -> (postId, auditUserId, postType, messageText, DateTimeOffset.UtcNow) |> PostCreated |> tryApplyNewsEvent source postId None initialRvn)
                let! result = match result with | Ok (post, rvn, newsEvent) -> tryWriteNewsEventAsync auditUserId rvn newsEvent post | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (postId, post) -> sprintf "Audit%A %A %A" auditUserId postId post |> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                match result with | Ok (postId, post) -> (postId, post) |> postDic.Add | Error _ -> ()
                return! managingNews postDic
            | HandleChangePostCmd (editOrRemovePostToken, auditUserId, postId, currentRvn, Markdown messageText, reply) ->
                let source = "HandleChangePostCmd"
                sprintf "%s for %A (%A) when managingNews (%i post/s)" source postId currentRvn postDic.Count |> Verbose |> log
                let messageText = Markdown (messageText.Trim ())
                let result =
                    postDic |> tryFindPost postId (otherCmdError source)
                    |> Result.bind (fun (postId, post) -> if editOrRemovePostToken.UserId <> post.UserId then NotAuthorized |> AuthCmdAuthznError |> Error else (postId, post) |> Ok)
                    |> Result.bind (fun (postId, post) -> match messageText |> validatePostMessageText with | None -> (postId, post) |> Ok | Some errorText -> errorText |> otherCmdError source)
                    |> Result.bind (fun (postId, post) -> (postId, messageText) |> PostChanged |> tryApplyNewsEvent source postId (Some post) (incrementRvn currentRvn))
                let! result = match result with | Ok (post, rvn, newsEvent) -> tryWriteNewsEventAsync auditUserId rvn newsEvent post | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (postId, post) -> Some (sprintf "Audit%A %A %A" auditUserId postId post)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                match result with | Ok (postId, post) -> postDic |> updatePost postId post | Error _ -> ()
                return! managingNews postDic
            | HandleRemovePostCmd (editOrRemovePostToken, auditUserId, postId, currentRvn, reply) ->
                let source = "HandleRemovePostCmd"
                sprintf "%s for %A (%A) when managingNews (%i post/s)" source postId currentRvn postDic.Count |> Verbose |> log
                let result =
                    postDic |> tryFindPost postId (otherCmdError source)
                    |> Result.bind (fun (postId, post) -> if editOrRemovePostToken.UserId <> post.UserId then NotAuthorized |> AuthCmdAuthznError |> Error else (postId, post) |> Ok)

                    // TODO-SOON: Prevent removal of MatchResult-related posts?...

                    |> Result.bind (fun (postId, post) ->
                        if post.Removed |> not then (postId, post) |> Ok
                        else ifDebug "News post has already been removed" UNEXPECTED_ERROR |> otherCmdError source)
                    |> Result.bind (fun (postId, post) -> postId |> PostRemoved |> tryApplyNewsEvent source postId (Some post) (incrementRvn currentRvn))
                let! result = match result with | Ok (post, rvn, newsEvent) -> tryWriteNewsEventAsync auditUserId rvn newsEvent post | Error error -> error |> Error |> thingAsync
                result |> logResult source (fun (postId, post) -> Some (sprintf "Audit%A %A %A" auditUserId postId post)) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> discardOk |> reply.Reply
                match result with | Ok (postId, post) -> postDic |> updatePost postId post | Error _ -> ()
                return! managingNews postDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Entity Entity.News |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member self.Start () =
        // Note: Not interested in NewsEventWritten events (since News agent causes these in the first place - and will already have maintained its internal state accordingly).
        let onEvent = (fun event -> match event with | NewsEventsRead newsEvents -> newsEvents |> self.OnNewsEventsRead | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to NewsEventsRead broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.OnNewsEventsRead newsEvents = newsEvents |> OnNewsEventsRead |> agent.Post
    member __.HandleCreatePostCmdAsync (token, auditUserId, postId, postType, messageText) =
        (fun reply -> (token, auditUserId, postId, postType, messageText, reply) |> HandleCreatePostCmd) |> agent.PostAndAsyncReply
    member __.HandleChangePostCmdAsync (token, auditUserId, postId, currentRvn, messageText) =
        (fun reply -> (token, auditUserId, postId, currentRvn, messageText, reply) |> HandleChangePostCmd) |> agent.PostAndAsyncReply
    member __.HandleRemovePostCmdAsync (token, auditUserId, postId, currentRvn) =
        (fun reply -> (token, auditUserId, postId, currentRvn, reply) |> HandleRemovePostCmd) |> agent.PostAndAsyncReply

let news = News ()
