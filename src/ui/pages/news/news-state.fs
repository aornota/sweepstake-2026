module Aornota.Sweepstake2026.Ui.Pages.News.State

open Aornota.Sweepstake2026.Common.Delta
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Json
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.JsonConverter
open Aornota.Sweepstake2026.Ui.Common.LocalStorage
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Common.Toasts
open Aornota.Sweepstake2026.Ui.Pages.News.Common
open Aornota.Sweepstake2026.Ui.Shared

open System

open Elmish

let [<Literal>] private NEWS_PREFERENCES_KEY = "sweepstake-2026-ui-news-preferences"

let private readPreferencesCmd =
    let readPreferences () = async {
        return Key NEWS_PREFERENCES_KEY |> readJson |> Option.map (fun (Json json) -> json |> fromJson<DateTimeOffset>) }
    Cmd.OfAsync.either readPreferences () (Ok >> ReadPreferencesResult) (Error >> ReadPreferencesResult)

let private writePreferencesCmd state =
    let writePreferences (lastNewsSeen:DateTimeOffset) = async {
        do lastNewsSeen |> toJson |> Json |> writeJson (Key NEWS_PREFERENCES_KEY) }
    match state.LastNewsSeen with
    | Some lastNewsSeen -> Cmd.OfAsync.either writePreferences lastNewsSeen (Ok >> WritePreferencesResult) (Error >> WritePreferencesResult)
    | None -> Cmd.none

let initialize isCurrentPage readPreferences lastNewsSeen : State * Cmd<Input> =
    let state = { NewsProjection = Pending ; PreferencesRead = readPreferences |> not ; LastNewsSeen = lastNewsSeen ; IsCurrentPage = isCurrentPage ; UnseenCount = 0 }
    let readPreferencesCmd = if readPreferences then readPreferencesCmd else Cmd.none
    let newsProjectionCmd = InitializeNewsProjectionQry |> UiUnauthNewsMsg |> SendUiUnauthMsg |> Cmd.ofMsg
    state, Cmd.batch [ readPreferencesCmd ; newsProjectionCmd ]

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private updateLastNewsSeen state =
    let lastNewsSeen = if state.IsCurrentPage && state.PreferencesRead then (DateTimeOffset.UtcNow.AddSeconds 10.) |> Some else state.LastNewsSeen
    let unseenCount =
        match state.PreferencesRead, state.NewsProjection with
        | true, Ready (_, newsDic, _) ->
            match lastNewsSeen with
            | Some lastNewsSeen -> newsDic |> List.ofSeq |> List.filter (fun (KeyValue (_, post)) -> post.Timestamp > lastNewsSeen) |> List.length
            | None -> newsDic.Count
        | _ -> state.UnseenCount
    let state = { state with LastNewsSeen = lastNewsSeen ; UnseenCount = unseenCount }
    state, state |> writePreferencesCmd

let private post (postDto:PostDto) = { Rvn = postDto.Rvn ; UserId = postDto.UserId ; Message = postDto.Message ; Timestamp = postDto.Timestamp ; Removed = false }

let private postDic (postDtos:PostDto list) =
    let postDic = PostDic ()
    postDtos |> List.iter (fun postDto ->
        let postId = postDto.PostId
        if postId |> postDic.ContainsKey |> not then // note: silently ignore duplicate postIds (should never happer)
            (postId, postDto |> post) |> postDic.Add)
    postDic

let private applyPostsDelta currentRvn deltaRvn (delta:Delta<PostId, PostDto>) (postDic:PostDic) =
    let postDic = PostDic postDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (postId, postDto) -> if postId |> postDic.ContainsKey then (postId, postDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (postId, postDto) -> (postId, postDto |> post) |> postDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (postId, postDto) -> if postId |> postDic.ContainsKey |> not then (postId, postDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (postId, postDto) -> postDic.[postId] <- (postDto |> post)) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun postId -> if postId |> postDic.ContainsKey |> not then postId |> Some else None)
        // Note: delta.Removed correspond to "removed" - but marked as such on client, rather than removed.
        if doNotExist.Length = 0 then delta.Removed |> List.iter (fun postId ->
            let post = postDic.[postId]
            postDic.[postId] <- { post with Removed = true }) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> postDic |> Ok)

let private handleCreatePostCmdResult (result:Result<unit, AuthCmdError<string>>) (rvn, postDic, readyState) state : State * Cmd<Input> =
    match readyState.AddPostState with
    | Some addPostState ->
        match addPostState.AddPostStatus with
        | Some AddPostPending ->
            match result with
            | Ok _ ->
                let readyState = { readyState with AddPostState = None }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Post has been added" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "AddPostCmdResult error -> %A" error) (error |> cmdErrorText)
                let addPostState = { addPostState with AddPostStatus = errorText |> AddPostFailed |> Some }
                let readyState = { readyState with AddPostState = addPostState |> Some }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Unable to add post" |> errorToastCmd
        | Some (AddPostFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AddPostCmdResult when AddPostStatus is not AddPostPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPostCmdResult when AddPostState is None -> %A" result)

let private handleChangePostCmdResult (result:Result<unit, AuthCmdError<string>>) (rvn, postDic, readyState) state : State * Cmd<Input> =
    match readyState.EditPostState with
    | Some editPostState ->
        match editPostState.EditPostStatus with
        | Some EditPostPending ->
            match result with
            | Ok _ ->
                let readyState = { readyState with EditPostState = None }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Post has been edited" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangePostCmdResult error -> %A" error) (error |> cmdErrorText)
                let editPostState = { editPostState with EditPostStatus = errorText |> EditPostFailed |> Some }
                let readyState = { readyState with EditPostState = editPostState |> Some }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Unable to edit post" |> errorToastCmd
        | Some (EditPostFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangePostCmdResult when EditPostStatus is not EditPostPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePostCmdResult when EditPostState is None -> %A" result)

let private handleRemovePostCmdResult (result:Result<unit, AuthCmdError<string>>) (rvn, postDic, readyState) state : State * Cmd<Input> =
    match readyState.RemovePostState with
    | Some removePostState ->
        match removePostState.RemovePostStatus with
        | Some RemovePostPending ->
            match result with
            | Ok _ ->
                let readyState = { readyState with RemovePostState = None }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Post has been removed" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "RemovePostCmdResult error -> %A" error) (error |> cmdErrorText)
                let removePostState = { removePostState with RemovePostStatus = errorText |> RemovePostFailed |> Some }
                let readyState = { readyState with RemovePostState = removePostState |> Some }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Unable to remove post" |> errorToastCmd
        | Some (RemovePostFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected RemovePostCmdResult when RemovePostStatus is not RemovePostPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemovePostCmdResult when RemovePostState is None -> %A" result)

let private handleAddCustomMessageCmdResult (result:Result<unit, AuthCmdError<string>>) (rvn, postDic, readyState) state : State * Cmd<Input> =
    match readyState.AddCustomMessageState with
    | Some addCustomMessageState ->
        match addCustomMessageState.AddCustomMessageStatus with
        | Some AddCustomMessagePending ->
            match result with
            | Ok _ ->
                let readyState = { readyState with AddCustomMessageState = None }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Custom message has been added" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "AddCustomMessageCmdResult error -> %A" error) (error |> cmdErrorText)
                let addCustomMessageState = { addCustomMessageState with AddCustomMessageStatus = errorText |> AddCustomMessageFailed |> Some }
                let readyState = { readyState with AddCustomMessageState = addCustomMessageState |> Some }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Unable to add custom message" |> errorToastCmd
        | Some (AddCustomMessageFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AddCustomMessageCmdResult when AddCustomMessageStatus is not AddCustomMessagePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddCustomMessageCmdResult when AddCustomMessageState is None -> %A" result)

let private handleChangeCustomMessageCmdResult (result:Result<unit, AuthCmdError<string>>) (rvn, postDic, readyState) state : State * Cmd<Input> =
    match readyState.EditCustomMessageState with
    | Some editCustomMessageState ->
        match editCustomMessageState.EditCustomMessageStatus with
        | Some EditCustomMessagePending ->
            match result with
            | Ok _ ->
                let readyState = { readyState with EditCustomMessageState = None }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Custom message has been edited" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangeCustomMessageCmdResult error -> %A" error) (error |> cmdErrorText)
                let editCustomMessageState = { editCustomMessageState with EditCustomMessageStatus = errorText |> EditCustomMessageFailed |> Some }
                let readyState = { readyState with EditCustomMessageState = editCustomMessageState |> Some }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Unable to edit custom message" |> errorToastCmd
        | Some (EditCustomMessageFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangeCustomMessageCmdResult when EditCustomMessageStatus is not EditCustomMessagePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangeCustomMessageCmdResult when EditCustomMessageState is None -> %A" result)

let private handleRemoveCustomMessageCmdResult (result:Result<unit, AuthCmdError<string>>) (rvn, postDic, readyState) state : State * Cmd<Input> =
    match readyState.RemoveCustomMessageState with
    | Some removeCustomMessageState ->
        match removeCustomMessageState.RemoveCustomMessageStatus with
        | Some RemoveCustomMessagePending ->
            match result with
            | Ok _ ->
                let readyState = { readyState with RemoveCustomMessageState = None }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Custom message has been removed" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "RemoveCustomMessageCmdResult error -> %A" error) (error |> cmdErrorText)
                let removeCustomMessageState = { removeCustomMessageState with RemoveCustomMessageStatus = errorText |> RemoveCustomMessageFailed |> Some }
                let readyState = { readyState with RemoveCustomMessageState = removeCustomMessageState |> Some }
                { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, "Unable to remove custom message" |> errorToastCmd
        | Some (RemoveCustomMessageFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected RemoveCustomMessageCmdResult when RemoveCustomMessageStatus is not RemoveCustomMessagePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemoveCustomMessageCmdResult when RemoveCustomMessageState is None -> %A" result)

let private handleServerNewsMsg serverNewsMsg state : State * Cmd<Input> =
    match serverNewsMsg, state.NewsProjection with
    | InitializeNewsProjectionQryResult (Ok (postDtos, hasMorePosts)), Pending ->
        let readyState = { HasMorePosts = hasMorePosts ; MorePostsPending = false ; AddPostState = None ; EditPostState = None ; RemovePostState = None ; AddCustomMessageState = None ; EditCustomMessageState = None ; RemoveCustomMessageState = None }
        let state = { state with NewsProjection = (initialRvn, postDtos |> postDic, readyState) |> Ready }
        state |> updateLastNewsSeen
    | InitializeNewsProjectionQryResult (Error (OtherError errorText)), Pending ->
        { state with NewsProjection = Failed }, errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | MorePostsQryResult (Ok (newRvn, postDtos, hasMorePosts)), Ready (rvn, postDic, readyState) ->
        if readyState.MorePostsPending |> not then // note: silently ignore unexpected result
            state, Cmd.none
        else if postDtos.Length = 0 then
            let readyState = { readyState with HasMorePosts = hasMorePosts ; MorePostsPending = false }
            { state with NewsProjection = (newRvn, postDic, readyState) |> Ready }, "Unable to retrieve more news posts<br><br>They have probably been removed" |> warningToastCmd
        else
            let addedPostDtos = postDtos |> List.map (fun postDto -> postDto.PostId, postDto)
            let postDtoDelta = { Added = addedPostDtos ; Changed = [] ; Removed = [] }
            match postDic |> applyPostsDelta rvn newRvn postDtoDelta with
            | Ok postDic ->
                let readyState = { readyState with HasMorePosts = hasMorePosts ; MorePostsPending = false }
                let state = { state with NewsProjection = (newRvn, postDic, readyState) |> Ready }
                state |> updateLastNewsSeen
            | Error error ->
                let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" postDtoDelta postDic error)
                let state, cmd = initialize state.IsCurrentPage false state.LastNewsSeen
                state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | MorePostsQryResult (Error error), Ready (rvn, postDic, readyState) ->
        if readyState.MorePostsPending |> not then // note: silently ignore unexpected result
            state, Cmd.none
        else
            let readyState = { readyState with MorePostsPending = false }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, shouldNeverHappenCmd (sprintf "MorePostsQryResult Error %A" error)
    | CreatePostCmdResult result, Ready (rvn, postDic, readyState) ->
        state |> handleCreatePostCmdResult result (rvn, postDic, readyState)
    | ChangePostCmdResult result, Ready (rvn, postDic, readyState) ->
        state |> handleChangePostCmdResult result (rvn, postDic, readyState)
    | RemovePostCmdResult result, Ready (rvn, postDic, readyState) ->
        state |> handleRemovePostCmdResult result (rvn, postDic, readyState)
    | AddCustomMessageCmdResult result, Ready (rvn, postDic, readyState) ->
        state |> handleAddCustomMessageCmdResult result (rvn, postDic, readyState)
    | ChangeCustomMessageCmdResult result, Ready (rvn, postDic, readyState) ->
        state |> handleChangeCustomMessageCmdResult result (rvn, postDic, readyState)
    | RemoveCustomMessageCmdResult result, Ready (rvn, postDic, readyState) ->
        state |> handleRemoveCustomMessageCmdResult result (rvn, postDic, readyState)
    | NewsProjectionMsg (PostsDeltaMsg (deltaRvn, postDtoDelta, hasMorePosts)), Ready (rvn, postDic, readyState) ->
        match postDic |> applyPostsDelta rvn deltaRvn postDtoDelta with
        | Ok postDic ->
            let readyState = { readyState with HasMorePosts = hasMorePosts }
            let state = { state with NewsProjection = (deltaRvn, postDic, readyState) |> Ready }
            state |> updateLastNewsSeen
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" postDtoDelta postDic error)
            let state, cmd = initialize state.IsCurrentPage false state.LastNewsSeen
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | NewsProjectionMsg _, _ -> // note: silently ignore NewsProjectionMsg if not Ready
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerNewsMsg when %A -> %A" state.NewsProjection serverNewsMsg)

let handleAddPostInput addPostInput (rvn, postDic, readyState) state : State * Cmd<Input> * bool =
    match addPostInput, readyState.AddPostState with
    | NewMessageChanged newMessage, Some addPostState ->
        let newMessageErrorText = validatePostMessage (Markdown newMessage)
        let addPostState = { addPostState with NewMessage = newMessage ; NewMessageErrorText = newMessageErrorText }
        let readyState = { readyState with AddPostState = addPostState |> Some }
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
    | AddPost, Some addPostState -> // note: assume no need to validate NewMessageText (i.e. because News.Render.renderAddPostModal will ensure that AddPost can only be dispatched when valid)
        let addPostState = { addPostState with AddPostStatus = AddPostPending |> Some }
        let readyState = { readyState with AddPostState = addPostState |> Some }
        let cmd = (addPostState.NewPostId, Markdown (addPostState.NewMessage.Trim ())) |> CreatePostCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, cmd, true
    | CancelAddPost, Some addPostState ->
        match addPostState.AddPostStatus with
        | Some AddPostPending ->
            state, shouldNeverHappenCmd "Unexpected CancelAddPost when AddPostPending", false
        | Some (AddPostFailed _) | None ->
            let readyState = { readyState with AddPostState = None }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPostInput when AddPostState is None -> %A" addPostInput), false

let handleEditPostInput editPostInput (rvn, postDic, readyState) state : State * Cmd<Input> * bool =
    match editPostInput, readyState.EditPostState with
    | MessageChanged message, Some editPostState ->
        let messageErrorText = validatePostMessage (Markdown message)
        let editPostState = { editPostState with Message = message ; MessageErrorText = messageErrorText }
        let readyState = { readyState with EditPostState = editPostState |> Some }
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
    | EditPost, Some editPostState -> // note: assume no need to validate MessageText (i.e. because News.Render.renderEditPostModal will ensure that EditPost can only be dispatched when valid)
        let editPostState = { editPostState with EditPostStatus = EditPostPending |> Some }
        let readyState = { readyState with EditPostState = editPostState |> Some }
        let postId = editPostState.PostId
        let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
        let currentRvn = match post with | Some post -> post.Rvn | None -> initialRvn
        let cmd = (postId, currentRvn, Markdown (editPostState.Message.Trim ())) |> ChangePostCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, cmd, true
    | CancelEditPost, Some editPostState ->
        match editPostState.EditPostStatus with
        | Some EditPostPending ->
            state, shouldNeverHappenCmd "Unexpected CancelEditPost when EditPostPending", false
        | Some (EditPostFailed _) | None ->
            let readyState = { readyState with EditPostState = None }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EditPostInput when EditPostState is None -> %A" editPostInput), false

let handleRemovePostInput removePostInput (rvn, postDic:PostDic, readyState) state : State * Cmd<Input> * bool =
    match removePostInput, readyState.RemovePostState with
    | ConfirmRemovePost, Some removePostState ->
        let removePostState = { removePostState with RemovePostStatus = RemovePostPending |> Some }
        let readyState = { readyState with RemovePostState = removePostState |> Some }
        let postId = removePostState.PostId
        let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
        let currentRvn = match post with | Some post -> post.Rvn | None -> initialRvn
        let cmd = (postId, currentRvn) |> RemovePostCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, cmd, true
    | CancelRemovePost, Some removePostState ->
        match removePostState.RemovePostStatus with
        | Some RemovePostPending ->
            state, shouldNeverHappenCmd "Unexpected CancelRemovePost when RemovePostPending", false
        | Some (RemovePostFailed _) | None ->
            let readyState = { readyState with RemovePostState = None }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemovePostInput when RemovePostState is None -> %A" removePostInput), false

let handleAddCustomMessageInput addCustomMessageInput (rvn, postDic, fixturesProjection:Projection<_ * FixtureDic>, readyState) state : State * Cmd<Input> * bool =
    match fixturesProjection, addCustomMessageInput, readyState.AddCustomMessageState with
    | Ready _, NewCustomMessageChanged newCustomMessage, Some addCustomMessageState ->
        let newCustomMessageErrorText = validateCustomMessage (Markdown newCustomMessage)
        let addCustomMessageState = { addCustomMessageState with NewCustomMessage = newCustomMessage ; NewCustomMessageErrorText = newCustomMessageErrorText }
        let readyState = { readyState with AddCustomMessageState = addCustomMessageState |> Some }
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
    | Ready (_, fixtureDic), AddCustomMessage, Some addCustomMessageState -> // note: assume no need to validate NewCustomMessage (i.e. because News.Render.renderAddCustomMessageModal will ensure that AddCustomMessage can only be dispatched when valid)
        let addCustomMessageState = { addCustomMessageState with AddCustomMessageStatus = AddCustomMessagePending |> Some }
        let readyState = { readyState with AddCustomMessageState = addCustomMessageState |> Some }
        let fixtureId = addCustomMessageState.FixtureId
        let fixture = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId] |> Some else None
        let currentRvn = match fixture with | Some fixture -> fixture.Rvn | None -> initialRvn
        let cmd = (fixtureId, currentRvn, Markdown (addCustomMessageState.NewCustomMessage.Trim ())) |> AddCustomMessageCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, cmd, true
    | Ready _, CancelAddCustomMessage, Some addCustomMessageState ->
        match addCustomMessageState.AddCustomMessageStatus with
        | Some AddCustomMessagePending ->
            state, shouldNeverHappenCmd "Unexpected CancelAddCustomMessage when AddCustomMessagePending", false
        | Some (AddCustomMessageFailed _) | None ->
            let readyState = { readyState with AddCustomMessageState = None }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, false
    | Ready _, _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddCustomMessageInput when AddCustomMessageState is None -> %A" addCustomMessageInput), false
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddCustomMessageInput when fixturesProjects is not Ready -> %A" addCustomMessageInput), false

let handleEditCustomMessageInput editCustomMessageInput (rvn, postDic, fixturesProjection:Projection<_ * FixtureDic>, readyState) state : State * Cmd<Input> * bool =
    match fixturesProjection, editCustomMessageInput, readyState.EditCustomMessageState with
    | Ready _, CustomMessageChanged customMessage, Some editCustomMessageState ->
        let customMessageErrorText = validateCustomMessage (Markdown customMessage)
        let editCustomMessageState = { editCustomMessageState with CustomMessage = customMessage ; CustomMessageErrorText = customMessageErrorText }
        let readyState = { readyState with EditCustomMessageState = editCustomMessageState |> Some }
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
    | Ready (_, fixtureDic), EditCustomMessage, Some editCustomMessageState -> // note: assume no need to validate CustomMessage (i.e. because News.Render.renderEditCustomMessageModal will ensure that EditCustomMessage can only be dispatched when valid)
        let editCustomMessageState = { editCustomMessageState with EditCustomMessageStatus = EditCustomMessagePending |> Some }
        let readyState = { readyState with EditCustomMessageState = editCustomMessageState |> Some }
        let fixtureId = editCustomMessageState.FixtureId
        let fixture = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId] |> Some else None
        let currentRvn = match fixture with | Some fixture -> fixture.Rvn | None -> initialRvn
        let cmd = (fixtureId, currentRvn, Markdown (editCustomMessageState.CustomMessage.Trim ())) |> ChangeCustomMessageCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, cmd, true
    | Ready _, CancelEditCustomMessage, Some editCustomMessageState ->
        match editCustomMessageState.EditCustomMessageStatus with
        | Some EditCustomMessagePending ->
            state, shouldNeverHappenCmd "Unexpected CancelEditCustomMessage when EditCustomMessagePending", false
        | Some (EditCustomMessageFailed _) | None ->
            let readyState = { readyState with EditCustomMessageState = None }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, false
    | Ready _, _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EditCustomMessageInput when EditCustomMessageState is None -> %A" editCustomMessageInput), false
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EditCustomMessageInput when fixturesProjects is not Ready -> %A" editCustomMessageInput), false

let handleRemoveCustomMessageInput removeCustomMessageInput (rvn, postDic:PostDic, fixturesProjection:Projection<_ * FixtureDic>, readyState) state : State * Cmd<Input> * bool =
    match fixturesProjection, removeCustomMessageInput, readyState.RemoveCustomMessageState with
    | Ready (_, fixtureDic), ConfirmRemoveCustomMessage, Some removeCustomMessageState ->
        let removeCustomMessageState = { removeCustomMessageState with RemoveCustomMessageStatus = RemoveCustomMessagePending |> Some }
        let readyState = { readyState with RemoveCustomMessageState = removeCustomMessageState |> Some }
        let fixtureId = removeCustomMessageState.FixtureId
        let fixture = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId] |> Some else None
        let currentRvn = match fixture with | Some fixture -> fixture.Rvn | None -> initialRvn
        let cmd = (fixtureId, currentRvn) |> RemoveCustomMessageCmd |> UiAuthNewsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, cmd, true
    | Ready _, CancelRemoveCustomMessage, Some removeCustomMessageState ->
        match removeCustomMessageState.RemoveCustomMessageStatus with
        | Some RemoveCustomMessagePending ->
            state, shouldNeverHappenCmd "Unexpected CancelRemoveCustomMessage when RemoveCustomMessagePending", false
        | Some (RemoveCustomMessageFailed _) | None ->
            let readyState = { readyState with RemoveCustomMessageState = None }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, false
    | Ready _, _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemoveCustomMessageInput when RemoveCustomMessageState is None -> %A" removeCustomMessageInput), false
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected RemoveCustomMessageInput when fixturesProjects is not Ready -> %A" removeCustomMessageInput), false

let transition (fixturesProjection:Projection<_ * FixtureDic>) input state =
    let state, cmd, isUserNonApiActivity =
        match input, state.NewsProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ShowMarkdownSyntaxModal, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiUnauthMsg _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReadPreferencesResult (Ok lastNewsSeen), _ ->
            let state = { state with PreferencesRead = true ; LastNewsSeen = lastNewsSeen }
            let state, cmd = state |> updateLastNewsSeen
            state, cmd, false
        | ReadPreferencesResult (Error _), _ -> // note: silently ignore error
            state, None |> Ok |> ReadPreferencesResult |> Cmd.ofMsg, false
        | WritePreferencesResult _, _ -> // note: nothing to do here
            state, Cmd.none, false
        | ReceiveServerNewsMsg serverNewsMsg, _ ->
            let state, cmd = state |> handleServerNewsMsg serverNewsMsg
            state, cmd, false
        | ToggleNewsIsCurrentPage isCurrentPage, _ ->
            let state = { state with IsCurrentPage = isCurrentPage }
            let state, cmd = state |> updateLastNewsSeen
            state, cmd, false
        | DismissPost postId, Ready (_, postDic, _) -> // note: silently ignore unknown postId (should never happen)
            if postId |> postDic.ContainsKey then postId |> postDic.Remove |> ignore
            state, Cmd.none, true
        | MorePosts, Ready (rvn, postDic, readyState) -> // note: assume no need to validate HasMorePosts (i.e. because News.Render.render will ensure that MorePosts can only be dispatched when true)
            let readyState = { readyState with MorePostsPending = true }
            let cmd = MorePostsQry |> UiUnauthNewsMsg |> SendUiUnauthMsg |> Cmd.ofMsg
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, cmd, false
        | ShowAddPostModal, Ready (rvn, postDic, readyState) ->
            let addPostState = { NewPostId = PostId.Create () ; NewMessage = String.Empty ; NewMessageErrorText = None ; AddPostStatus = None }
            let readyState = { readyState with AddPostState = addPostState |> Some }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
        | AddPostInput addPostInput, Ready (rvn, postDic, readyState) ->
            state |> handleAddPostInput addPostInput (rvn, postDic, readyState)
        | ShowEditPostModal postId, Ready (rvn, postDic, readyState) ->
            let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
            let message =
                match post with
                | Some post ->
                    let (Markdown message) = post.Message
                    message
                | None -> String.Empty // note: should never happen
            let editPostState = { PostId = postId ; Message = message ; MessageErrorText = None ; EditPostStatus = None }
            let readyState = { readyState with EditPostState = editPostState |> Some }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
        | EditPostInput editPostInput, Ready (rvn, postDic, readyState) ->
            state |> handleEditPostInput editPostInput (rvn, postDic, readyState)
        | ShowRemovePostModal postId, Ready (rvn, postDic, readyState) -> // note: no need to check for unknown postId (should never happen)
            let removePostState = { PostId = postId ; RemovePostStatus = None }
            let readyState = { readyState with RemovePostState = removePostState |> Some }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
        | RemovePostInput removePostInput, Ready (rvn, postDic, readyState) ->
            state |> handleRemovePostInput removePostInput (rvn, postDic, readyState)
        | ShowAddCustomMessageModal fixtureId, Ready (rvn, postDic, readyState) ->
            let addCustomMessageState = { FixtureId = fixtureId ; NewCustomMessage = String.Empty ; NewCustomMessageErrorText = None ; AddCustomMessageStatus = None }
            let readyState = { readyState with AddCustomMessageState = addCustomMessageState |> Some }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
        | AddCustomMessageInput addCustomMessageInput, Ready (rvn, postDic, readyState) ->
            state |> handleAddCustomMessageInput addCustomMessageInput (rvn, postDic, fixturesProjection, readyState)
        | ShowEditCustomMessageModal fixtureId, Ready (rvn, postDic, readyState) ->
            let fixture =
                match fixturesProjection with
                | Ready (_, fixtureDic) -> if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId] |> Some else None
                | _ -> None // note: should never happen
            let customMessage =
                match fixture with
                | Some fixture ->
                    match fixture.CustomMessage with
                    | Some (_, Markdown customMessage) -> customMessage
                    | None -> String.Empty // note: should never happen
                | None -> String.Empty // note: should never happen
            let editCustomMessageState = { FixtureId = fixtureId ; CustomMessage = customMessage ; CustomMessageErrorText = None ; EditCustomMessageStatus = None }
            let readyState = { readyState with EditCustomMessageState = editCustomMessageState |> Some }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
        | EditCustomMessageInput editCustomMessageInput, Ready (rvn, postDic, readyState) ->
            state |> handleEditCustomMessageInput editCustomMessageInput (rvn, postDic, fixturesProjection, readyState)
        | ShowRemoveCustomMessageModal fixtureId, Ready (rvn, postDic, readyState) -> // note: no need to check for unknown postId (should never happen)
            let removeCustomMessageState = { FixtureId = fixtureId ; RemoveCustomMessageStatus = None }
            let readyState = { readyState with RemoveCustomMessageState = removeCustomMessageState |> Some }
            { state with NewsProjection = (rvn, postDic, readyState) |> Ready }, Cmd.none, true
        | RemoveCustomMessageInput removeCustomMessageInput, Ready (rvn, postDic, readyState) ->
            state |> handleRemoveCustomMessageInput removeCustomMessageInput (rvn, postDic, fixturesProjection, readyState)
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" state.NewsProjection input), false
    state, cmd, isUserNonApiActivity
