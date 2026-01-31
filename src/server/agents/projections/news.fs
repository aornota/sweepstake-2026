module Aornota.Sweepstake2026.Server.Agents.Projections.News

(* Broadcasts: SendMsg
   Subscribes: NewsRead
               NewsEventWritten (PostCreated | PostChanged | PostRemoved)
               Disconnected *)

open Aornota.Sweepstake2026.Common.Delta
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Common.DeltaHelper
open Aornota.Sweepstake2026.Server.Connection
open Aornota.Sweepstake2026.Server.Events.NewsEvents
open Aornota.Sweepstake2026.Server.Signal

open System
open System.Collections.Generic

type private NewsInput =
    | Start of reply : AsyncReplyChannel<unit>
    | OnNewsRead of newsRead : NewsRead list
    | OnPostCreated of postId : PostId * rvn : Rvn * userId : UserId * postType : PostType * messageText : Markdown * timestamp : DateTimeOffset
    | OnPostChanged of postId : PostId * rvn : Rvn * messageText : Markdown
    | OnPostRemoved of postId : PostId
    | RemoveConnections of connectionIds : ConnectionId list
    | HandleInitializeNewsProjectionQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<PostDto list * bool, OtherError<string>>>
    | HandleMorePostsQry of connectionId : ConnectionId
        * reply : AsyncReplyChannel<Result<Rvn * PostDto list * bool, OtherError<string>>>

type private Post = { Ordinal : int ; Rvn : Rvn ; UserId : UserId ; PostTypeDto : PostTypeDto ; Timestamp : DateTimeOffset }
type private PostDic = Dictionary<PostId, Post>

type private Projectee = { LastRvn : Rvn ; MinPostOrdinal : int option ; LastHasMorePosts : bool }
type private ProjecteeDic = Dictionary<ConnectionId, Projectee>

type private State = { PostDic : PostDic }

type private StateChangeType =
    | Initialization of postDic : PostDic
    | PostChange of postDic : PostDic * state : State

let [<Literal>] private POST_BATCH_SIZE = 5

let private log category = (Projection News, category) |> consoleLogger.Log

let private logResult source successText result =
    match result with
    | Ok ok ->
        let successText = match successText ok with | Some successText -> sprintf " -> %s" successText | None -> String.Empty
        sprintf "%s Ok%s" source successText |> Info |> log
    | Error error -> sprintf "%s Error -> %A" source error |> Danger |> log

let private postDto (postId, post:Post) = { PostId = postId ; Rvn = post.Rvn ; UserId = post.UserId ; PostTypeDto = post.PostTypeDto ; Timestamp = post.Timestamp }

let private postDtos state = state.PostDic |> List.ofSeq |> List.map (fun (KeyValue (postId, post)) -> (postId, post) |> postDto)

let private sendMsg connectionIds serverMsg = (serverMsg, connectionIds) |> SendMsg |> broadcaster.Broadcast

let private sendPostDelta removedOrdinals minPostOrdinal (projecteeDic:ProjecteeDic) (postDelta:Delta<PostId, Post>) =
    let isRelevant projecteeMinPostOrdinal ordinal =
        match projecteeMinPostOrdinal with
        | Some projecteeMinPostOrdinal when projecteeMinPostOrdinal > ordinal -> false
        | Some _ | None -> true
    let updatedProjecteeDic = ProjecteeDic ()
    projecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) ->
        let hasMorePosts =
            match projectee.MinPostOrdinal, minPostOrdinal with
            | Some projecteeMinPostOrdinal, Some minPostOrdinal when projecteeMinPostOrdinal > minPostOrdinal -> true
            | Some _, Some _ | Some _, None | None, Some _ | None, None -> false
        let addedPostDtos =
            postDelta.Added // note: no need to filter based on projectee.MinPostOrdinal
            |> List.map (fun (postId, post) -> postId, (postId, post) |> postDto)
        let changedPostDtos =
            postDelta.Changed
            |> List.filter (fun (_, post) -> post.Ordinal |> isRelevant projectee.MinPostOrdinal)
            |> List.map (fun (postId, post) -> postId, (postId, post) |> postDto)
        let removedPostIds =
            postDelta.Removed
            |> List.choose (fun postId ->
                match removedOrdinals |> List.tryFind (fun (removedPostId, _) -> removedPostId = postId) with
                | Some (_, ordinal) -> (postId, ordinal) |> Some
                | None -> None) // note: should never happen
            |> List.filter (fun (_, ordinal) -> ordinal |> isRelevant projectee.MinPostOrdinal)
            |> List.map fst
        let postDtoDelta = { Added = addedPostDtos ; Changed = changedPostDtos ; Removed = removedPostIds }
        if postDtoDelta |> isEmpty |> not || hasMorePosts <> projectee.LastHasMorePosts then
            let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn ; LastHasMorePosts = hasMorePosts }
            sprintf "sendPostDtoDelta -> %A (%A)" connectionId projectee.LastRvn |> Info |> log
            (projectee.LastRvn, postDtoDelta, hasMorePosts) |> PostsDeltaMsg |> NewsProjectionMsg |> ServerNewsMsg |> sendMsg [ connectionId ]
            (connectionId, projectee) |> updatedProjecteeDic.Add)
    updatedProjecteeDic |> List.ofSeq |> List.iter (fun (KeyValue (connectionId, projectee)) -> projecteeDic.[connectionId] <- projectee)

let private updateState source (projecteeDic:ProjecteeDic) stateChangeType =
    let source = sprintf "%s#updateState" source
    let newState =
        match stateChangeType with
        | Initialization postDic ->
            sprintf "%s -> initialized" source |> Info |> log
            { PostDic = PostDic postDic }
        | PostChange (postDic, state) ->
            let postDelta = postDic |> delta state.PostDic
            if postDelta |> isEmpty |> not then
                let removedOrdinals = postDelta.Removed |> List.choose (fun postId ->
                    if postId |> state.PostDic.ContainsKey |> not then None // note: ignore unknown postId (should never happen)
                    else
                        let post = state.PostDic.[postId]
                        (postId, post.Ordinal) |> Some)
                let minPostOrdinal =
                    if postDic.Count > 0 then postDic |> List.ofSeq |> List.map (fun (KeyValue (_, post)) -> post.Ordinal) |> List.min |> Some
                    else None
                sprintf "%s -> Post delta %A -> %i projectee/s" source postDelta projecteeDic.Count |> Info |> log
                postDelta |> sendPostDelta removedOrdinals minPostOrdinal projecteeDic
                sprintf "%s -> updated" source |> Info |> log
                { state with PostDic = PostDic postDic }
            else
                sprintf "%s -> unchanged" source |> Info |> log
                state
    newState

type News () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start reply ->
                "Start when awaitingStart -> pendingNewsRead (0 posts) (0 projectees)" |> Info |> log
                () |> reply.Reply
                return! pendingNewsRead ()
            | OnNewsRead _ -> "OnNewsRead when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPostCreated _ -> "OnPostCreated when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPostChanged _ -> "OnPostChanged when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | OnPostRemoved _ -> "OnPostRemoved when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | RemoveConnections _ -> "RemoveConnections when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleInitializeNewsProjectionQry _ -> "HandleInitializeNewsProjectionQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart ()
            | HandleMorePostsQry _ -> "HandleMorePostsQry when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and pendingNewsRead () = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when pendingNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingNewsRead ()
            | OnNewsRead newsRead ->
                let source = "OnNewsRead"
                sprintf "%s (%i post/s) when pendingNewsRead" source newsRead.Length |> Info |> log
                let postDic = PostDic ()
                newsRead
                |> List.filter (fun newsRead -> newsRead.Removed |> not)
                |> List.sortBy (fun newsRead -> newsRead.Timestamp)
                |> List.iteri (fun i newsRead ->
                    let postTypeDto =
                        match newsRead.PostType with
                        | Standard -> newsRead.MessageText |> StandardDto
                        | MatchResult fixtureId -> (newsRead.MessageText, fixtureId) |> MatchResultDto
                    let post = { Ordinal = i ; Rvn = newsRead.Rvn ; UserId = newsRead.UserId ; PostTypeDto = postTypeDto ; Timestamp = newsRead.Timestamp }
                    (newsRead.PostId, post) |> postDic.Add)
                let projecteeDic = ProjecteeDic ()
                let state = postDic |> Initialization |> updateState source projecteeDic
                return! projectingNews state postDic projecteeDic
            | OnPostCreated _ -> "OnPostCreated when pendingNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingNewsRead ()
            | OnPostChanged _ -> "OnPostChanged when pendingNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingNewsRead ()
            | OnPostRemoved _ -> "OnPostRemoved when pendingNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingNewsRead ()
            | RemoveConnections _ -> "RemoveConnections when pendingNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingNewsRead ()
            | HandleInitializeNewsProjectionQry _ -> "HandleInitializeNewsProjectionQry when pendingNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingNewsRead ()
            | HandleMorePostsQry _ -> "HandleMorePostsQry when pendingNewsRead" |> IgnoredInput |> Agent |> log ; return! pendingNewsRead () }
        and projectingNews state postDic projecteeDic = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when projectingNews" |> IgnoredInput |> Agent |> log ; return! projectingNews state postDic projecteeDic
            | OnNewsRead _ -> "OnNewsRead when projectingNews" |> IgnoredInput |> Agent |> log ; return! projectingNews state postDic projecteeDic
            | OnPostCreated (postId, rvn, userId, postType, messageText, timestamp) ->
                let source = "OnPostCreated"
                sprintf "%s (%A %A) when projectingNews (%i post/s) (%i projectee/s)" source postId userId postDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if postId |> postDic.ContainsKey |> not then // note: silently ignore already-known postId (should never happen)
                        let nextOrdinal =
                            if postDic.Count = 0 then 1
                            else (postDic |> List.ofSeq |> List.map (fun (KeyValue (_, post)) -> post.Ordinal) |> List.max) + 1
                        let postTypeDto =
                            match postType with
                            | Standard -> messageText |> StandardDto
                            | MatchResult fixtureId -> (messageText, fixtureId) |> MatchResultDto
                        let post = { Ordinal = nextOrdinal ; Rvn = rvn ; UserId = userId ; PostTypeDto = postTypeDto ; Timestamp = timestamp }
                        (postId, post) |> postDic.Add
                        (postDic, state) |> PostChange |> updateState source projecteeDic
                    else state
                return! projectingNews state postDic projecteeDic
            | OnPostChanged (postId, rvn, messageText) ->
                let source = "OnPostChanged"
                sprintf "%s (%A %A) when projectingNews (%i post/s) (%i projectee/s)" source postId rvn postDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if postId |> postDic.ContainsKey then // note: silently ignore unknown postId (should never happen)
                        let post = postDic.[postId]
                        let postTypeDto =
                            match post.PostTypeDto with
                            | StandardDto _ -> messageText |> StandardDto
                            | MatchResultDto (_, fixtureId) -> (messageText, fixtureId) |> MatchResultDto
                        postDic.[postId] <- { post with Rvn = rvn ; PostTypeDto = postTypeDto }
                        (postDic, state) |> PostChange |> updateState source projecteeDic
                    else state
                return! projectingNews state postDic projecteeDic
            | OnPostRemoved postId ->
                let source = "OnPostRemoved"
                sprintf "%s (%A) when projectingNews (%i post/s) (%i projectee/s)" source postId postDic.Count projecteeDic.Count |> Info |> log
                let state =
                    if postId |> postDic.ContainsKey then // note: silently ignore unknown postId (should never happen)
                        postId |> postDic.Remove |> ignore
                        (postDic, state) |> PostChange |> updateState source projecteeDic
                    else state
                return! projectingNews state postDic projecteeDic
            | RemoveConnections connectionIds ->
                let source = "RemoveConnections"
                sprintf "%s (%A) when projectingNews (%i post/s) (%i projectee/s)" source connectionIds postDic.Count projecteeDic.Count |> Info |> log
                connectionIds |> List.iter (fun connectionId -> if connectionId |> projecteeDic.ContainsKey then connectionId |> projecteeDic.Remove |> ignore) // note: silently ignore unknown connectionIds
                sprintf "%s when projectingNews -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                return! projectingNews state postDic projecteeDic
            | HandleInitializeNewsProjectionQry (connectionId, reply) ->
                let source = "HandleInitializeNewsProjectionQry"
                sprintf "%s for %A when projectingNews (%i post/s) (%i projectee/s)" source connectionId postDic.Count projecteeDic.Count |> Info |> log
                let initializedState, minPostOrdinal, hasMorePosts =
                    if postDic.Count <= POST_BATCH_SIZE then
                        let minPostOrdinal =
                            if postDic.Count > 0 then postDic |> List.ofSeq |> List.map (fun (KeyValue (_, post)) -> post.Ordinal) |> List.min |> Some
                            else None
                        state, minPostOrdinal, false
                    else
                        let initialPosts =
                            postDic
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (postId, post)) -> postId, post)
                            |> List.sortBy (fun (_, post) -> post.Ordinal) |> List.rev |> List.take POST_BATCH_SIZE
                        let minPostOrdinal = initialPosts |> List.map (fun (_, post) -> post.Ordinal) |> List.min |> Some
                        let initialPostDic = PostDic ()
                        initialPosts |> List.iter (fun (postId, post) -> (postId, post) |> initialPostDic.Add)
                        { state with PostDic = initialPostDic }, minPostOrdinal, true
                let projectee = { LastRvn = initialRvn ; MinPostOrdinal = minPostOrdinal ; LastHasMorePosts = hasMorePosts }
                // Note: connectionId might already be known, e.g. re-initialization.
                if connectionId |> projecteeDic.ContainsKey |> not then (connectionId, projectee) |> projecteeDic.Add else projecteeDic.[connectionId] <- projectee
                sprintf "%s when projectingNews -> %i projectee/s)" source projecteeDic.Count |> Info |> log
                let result = (initializedState |> postDtos, hasMorePosts) |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingNews state postDic projecteeDic
            | HandleMorePostsQry (connectionId, reply) ->
                let source = "HandleMorePostsQry"
                sprintf "%s for %A when projectingNews (%i post/s) (%i projectee/s)" source connectionId postDic.Count projecteeDic.Count |> Info |> log
                let result =
                    if connectionId |> projecteeDic.ContainsKey |> not then ifDebug (sprintf "%A does not exist" connectionId) UNEXPECTED_ERROR |> OtherError |> Error
                    else
                        let projectee = projecteeDic.[connectionId]
                        let morePosts =
                            postDic
                            |> List.ofSeq
                            |> List.map (fun (KeyValue (postId, post)) -> postId, post)
                            |> List.filter (fun (_, post) ->
                                match projectee.MinPostOrdinal with
                                | Some minPostOrdinal -> minPostOrdinal > post.Ordinal
                                | None -> true) // note: should never happen
                            |> List.sortBy (fun (_, post) -> post.Ordinal) |> List.rev
                        let morePosts, hasMorePosts =
                            if morePosts.Length <= POST_BATCH_SIZE then morePosts, false
                            else morePosts |> List.take POST_BATCH_SIZE, true
                        let minPostOrdinal =
                            if morePosts.Length > 0 then morePosts |> List.map (fun (_, post) -> post.Ordinal) |> List.min |> Some
                            else projectee.MinPostOrdinal // note: should never happen
                        let postDtos = morePosts |> List.map postDto
                        let projectee = { projectee with LastRvn = incrementRvn projectee.LastRvn ; MinPostOrdinal = minPostOrdinal ; LastHasMorePosts = hasMorePosts }
                        projecteeDic.[connectionId] <- projectee
                        (projectee.LastRvn, postDtos, hasMorePosts) |> Ok
                result |> logResult source (sprintf "%A" >> Some) // note: log success/failure here (rather than assuming that calling code will do so)
                result |> reply.Reply
                return! projectingNews state postDic projecteeDic }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Projection Projection.News |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start () =
        let onEvent = (fun event ->
            match event with
            | NewsRead newsRead -> newsRead |> OnNewsRead |> agent.Post
            | NewsEventWritten (rvn, newsEvent) ->
                match newsEvent with
                | PostCreated (postId, userId, postType, messageText, timestamp) -> (postId, rvn, userId, postType, messageText, timestamp) |> OnPostCreated |> agent.Post
                | PostChanged (postId, messageText) -> (postId, rvn, messageText) |> OnPostChanged |> agent.Post
                | PostRemoved postId -> postId |> OnPostRemoved |> agent.Post
            | Disconnected connectionId -> [ connectionId ] |> RemoveConnections |> agent.Post
            | _ -> ())
        let subscriptionId = onEvent |> broadcaster.SubscribeAsync |> Async.RunSynchronously
        sprintf "agent subscribed to NewsRead | NewsEventWritten | Disconnected broadcasts -> %A" subscriptionId |> Info |> log
        Start |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.HandleInitializeNewsProjectionQryAsync connectionId =
        (fun reply -> (connectionId, reply) |> HandleInitializeNewsProjectionQry) |> agent.PostAndAsyncReply
    member __.HandleMorePostsQryAsync connectionId =
        (fun reply -> (connectionId, reply) |> HandleMorePostsQry) |> agent.PostAndAsyncReply

let news = News ()
