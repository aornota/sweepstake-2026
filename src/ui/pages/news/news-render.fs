module Aornota.Sweepstake2026.Ui.Pages.News.Render

open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Ui.Common.LazyViewOrHMR
open Aornota.Sweepstake2026.Ui.Common.Render.Markdown
open Aornota.Sweepstake2026.Ui.Common.TimestampHelper
open Aornota.Sweepstake2026.Ui.Pages.News.Common
open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common
open Aornota.Sweepstake2026.Ui.Shared
open Aornota.Sweepstake2026.Ui.Theme.Common
open Aornota.Sweepstake2026.Ui.Theme.Render.Bulma
open Aornota.Sweepstake2026.Ui.Theme.Shared

open System

module RctH = Fable.React.Helpers

let [<Literal>] private REMOVED_MARKDOWN = "_This post has been removed_"

type private NewsType =
    | News of postId:PostId * post:Post
    | AutoFixture of fixtureId:FixtureId * fixture:Fixture

let private bold text = sprintf "**%s**" text
let private italic text = sprintf "_%s_" text
let private boldItalic = bold >> italic

let private concatenateLines (lines:string list) = String.Join (Environment.NewLine, lines |> Array.ofList)

let private renderAutoFixtureHeader theme squadDic (fixture:Fixture) = [
    let teams, result = fixture |> confirmedFixtureDetails squadDic
    let lines = [
        match teams with
        | Some (_, homeName, _, awayName) ->
            let stageText = fixture.Stage |> stageText false |> italic
            match result with
            | Some (homeIsWinner, homeGoals, awayIsWinner, awayGoals, penaltyShootoutText, _, _, _) ->
                let homeNameAndScore, awayNameAndScore = sprintf "%s %i" homeName homeGoals, sprintf "%i %s" awayGoals awayName
                yield sprintf "#### %s: %s - %s" stageText (if homeIsWinner then homeNameAndScore |> bold else homeNameAndScore) (if awayIsWinner then awayNameAndScore |> bold else awayNameAndScore)
                match penaltyShootoutText with
                | Some penaltyShootoutText -> yield penaltyShootoutText |> italic
                | None -> ()
            | None ->
                yield sprintf "#### %s: %s vs. %s" stageText homeName awayName
        | None -> () // should never happen
    ]
    yield lines |> concatenateLines |> Markdown |> notificationContentFromMarkdown theme
]

(*
_**16**_ points for **jem** (Ollie Watkins goal and man-of-the-match; Dutch yellow cards); _**13**_ points for **nourdine** (Xavi Simons goal and yellow card; Cole Palmer assist); _**9**_ points each for **rob** (English win, less yellow cards) and **rosie** (Harry Kane penalty); _**-2**_ points for **will** (Bukayo Saka yellow card); and _**-4**_ points for **highnam** (Jude Bellingham yellow card; Virgil van Dijk yellow card).
*)

let private renderAutoFixtureContent theme (userDic:UserDic) detailsEntered (squadDic:SquadDic) (fixture:Fixture) = [
    let nothingToSeeHere = "Nothing to see here"
    let teams, _ = fixture |> confirmedFixtureDetails squadDic
    let lines = [
        match teams with
        | Some (homeSquadId, _, awaySquadId, _) ->
            if homeSquadId |> squadDic.ContainsKey && awaySquadId |> squadDic.ContainsKey then
                match fixture.MatchResult with
                | Some matchResult ->
                    let homeSquad, awaySquad = squadDic.[homeSquadId], squadDic.[awaySquadId]
                    let teanScoreEvents = [
                        match homeSquad.PickedBy with
                        | Some (userId, _, pickedDate) when fixture.KickOff > pickedDate ->
                            yield! matchResult.HomeScoreEvents.TeamScoreEvents |> List.map (fun (event, points) -> userId, homeSquad, event, points)
                        | _ -> ()
                        match awaySquad.PickedBy with
                        | Some (userId, _, pickedDate) when fixture.KickOff > pickedDate ->
                            yield! matchResult.AwayScoreEvents.TeamScoreEvents |> List.map (fun (event, points) -> userId, awaySquad, event, points)
                        | _ -> ()
                    ]
                    let playerScoreEvents = [
                        yield!
                            matchResult.HomeScoreEvents.PlayerScoreEvents
                            |> List.choose (fun (playerId, items) ->
                                if playerId |> homeSquad.PlayerDic.ContainsKey then
                                    let player = homeSquad.PlayerDic.[playerId]
                                    match player.PickedBy with
                                    | Some (userId, _, pickedDate) when fixture.KickOff > pickedDate -> Some (userId, player, items)
                                    | _ -> None
                                else None)
                        yield!
                            matchResult.AwayScoreEvents.PlayerScoreEvents
                            |> List.choose (fun (playerId, items) ->
                                if playerId |> awaySquad.PlayerDic.ContainsKey then
                                    let player = awaySquad.PlayerDic.[playerId]
                                    match player.PickedBy with
                                    | Some (userId, _, pickedDate) when fixture.KickOff > pickedDate -> Some (userId, player, items)
                                    | _ -> None
                                 else None)
                        ]
                    let userTeamScores =
                        teanScoreEvents
                        |> List.groupBy (fun (userId, _, _, _) -> userId)
                        |> List.map (fun (userId, items) ->
                            let points = items |> List.sumBy (fun (_, _, _, points) -> points)

                            // TODO-NMB: Squad and TeamScoreEvent descriptions...

                            userId, points)
                    let userPlayerScores =
                        playerScoreEvents
                        |> List.groupBy (fun (userId, _, _) -> userId)
                        |> List.map (fun (userId, items) ->
                            let points = items |> List.sumBy (fun (_, _, subItems) -> subItems |> List.sumBy snd)

                            // TODO-NMB: Player and PlayerScoreEvent descriptions...

                            userId, points)
                    yield!
                        userTeamScores @ userPlayerScores
                        |> List.groupBy (fun (userId, _) -> userId)
                        |> List.map (fun (userId, items) -> userId |> userName userDic, items |> List.sumBy snd)
                        |> List.sortBy snd
                        |> List.rev
                        |> List.map (fun (UserName userName, points) -> sprintf "- %s points for %s" (sprintf "%i" points |> boldItalic) (userName |> bold))
                | None -> ()
            else () // should never happen
        | None -> () // should never happen
    ]
    match lines with
    | [] ->
        if detailsEntered then yield nothingToSeeHere |> Markdown |> notificationContentFromMarkdown theme
        else yield (sprintf "%s...yet" nothingToSeeHere) |> italic |> Markdown |> notificationContentFromMarkdown theme
    | _ -> yield lines |> concatenateLines |> Markdown |> notificationContentFromMarkdown theme
]

let private renderAddPostModal (useDefaultTheme, addPostState:AddPostState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Add post" ] |> para theme paraCentredSmall ]
    let onDismiss = match addPostState.AddPostStatus with | Some AddPostPending -> None | Some _ | None -> (fun _ -> CancelAddPost |> AddPostInput |> dispatch) |> Some
    let isAddingPost, addPostInteraction =
        match addPostState.AddPostStatus with
        | Some AddPostPending -> true, Loading
        | Some (AddPostFailed _) | None ->
            match validatePostMessageText (Markdown addPostState.NewMessageText) with
            | Some _ -> false, NotEnabled None
            | None -> false, Clickable ((fun _ -> AddPost |> AddPostInput |> dispatch), None)
    let errorText = match addPostState.AddPostStatus with | Some (AddPostFailed errorText) -> errorText |> Some | Some AddPostPending | None -> None
    let (PostId newPostKey), newMessageText = addPostState.NewPostId, addPostState.NewMessageText
    let helpInfo = [
        str "News posts are persisted and public. You can use "
        [ str "Markdown syntax" ] |> link theme (Internal (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
        str " to format your message. A preview of your message will appear below." ; br; br ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
            yield textArea theme newPostKey newMessageText addPostState.NewMessageErrorText helpInfo true isAddingPost (NewMessageTextChanged >> AddPostInput >> dispatch)
            if String.IsNullOrWhiteSpace newMessageText |> not then
                yield notification theme notificationBlack [ Markdown newMessageText |> notificationContentFromMarkdown theme ] ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Add post" ] |> button theme { buttonLinkSmall with Interaction = addPostInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderEditPostModal (useDefaultTheme, editPostState:EditPostState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Edit post" ] |> para theme paraCentredSmall ]
    let onDismiss = match editPostState.EditPostStatus with | Some EditPostPending -> None | Some _ | None -> (fun _ -> CancelEditPost |> EditPostInput |> dispatch) |> Some
    let isEditingPost, editPostInteraction =
        match editPostState.EditPostStatus with
        | Some EditPostPending -> true, Loading
        | Some (EditPostFailed _) | None ->
            match validatePostMessageText (Markdown editPostState.MessageText) with
            | Some _ -> false, NotEnabled None
            | None -> false, Clickable ((fun _ -> EditPost |> EditPostInput |> dispatch), None)
    let errorText = match editPostState.EditPostStatus with | Some (EditPostFailed errorText) -> errorText |> Some | Some EditPostPending | None -> None
    let (PostId postKey), messageText = editPostState.PostId, editPostState.MessageText
    let helpInfo = [
        str "News posts are persisted and public. You can use "
        [ str "Markdown syntax" ] |> link theme (Internal (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
        str " to format your message. A preview of your message will appear below." ; br; br ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
            yield textArea theme postKey messageText editPostState.MessageErrorText helpInfo true isEditingPost (MessageTextChanged >> EditPostInput >> dispatch)
            if String.IsNullOrWhiteSpace messageText |> not then
                yield notification theme notificationBlack [ Markdown messageText |> notificationContentFromMarkdown theme ] ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Edit post" ] |> button theme { buttonLinkSmall with Interaction = editPostInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderRemovePostModal (useDefaultTheme, postDic:PostDic, removePostState:RemovePostState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Remove post" ] |> para theme paraCentredSmall ]
    let postId = removePostState.PostId
    let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
    let messageText =
        match post with
        | Some post ->
            match post.PostTypeDto with
            | StandardDto messageText -> messageText
            | MatchResultDto (messageText, _) -> messageText
        | None -> Markdown String.Empty
    let confirmInteraction, onDismiss =
        let confirm = (fun _ -> ConfirmRemovePost |> dispatch)
        let cancel = (fun _ -> CancelRemovePost |> dispatch)
        match removePostState.RemovePostStatus with
        | Some RemovePostPending -> Loading, None
        | Some (RemovePostFailed _) | None -> Clickable (confirm, None), cancel |> Some
    let errorText = match removePostState.RemovePostStatus with | Some (RemovePostFailed errorText) -> errorText |> Some | Some RemovePostPending | None -> None
    let warning = [
        [ strong "Are you sure you want to remove this post?" ] |> para theme paraCentredSmaller
        br
        [ str "Please note that this action is irreversible." ] |> para theme paraCentredSmallest ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield br
        yield notification theme notificationLight [ messageText |> notificationContentFromMarkdown theme ]
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Remove post" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

// TODO-2026: Ability to add / edit / remove custom message?...

let private renderAutoFixture theme _authUser userDic fixtureDic squadDic _dispatch (fixtureID, fixture:Fixture) =
    let fixtureStatus = fixtureStatus fixtureDic fixtureID
    let semantic, infoOrWarning, detailsEntered =
        match fixtureStatus with
        | Some NotStarted | Some NotConfirmed | None -> None, None, false
        | Some DetailsPending -> Some Dark, Some RESULT_PENDING, false
        | Some DetailsOverdue -> Some Warning, Some RESULT_OVERDUE, false
        | Some (DetailsMissing _) -> Some Warning, Some RESULT_HAS_MISSING_DETAILS, false
        | Some DetailsEntered -> Some Success, None, true
    let renderChildren () = [
        let kickOffText =
#if TICK
                ago fixture.KickOff.LocalDateTime
#else
                fixture.KickOff.LocalDateTime |> dateAndTimeText
#endif
        yield [ str kickOffText ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned }
        yield! renderAutoFixtureHeader theme squadDic fixture
        match infoOrWarning with
        | Some infoOrWarning -> yield infoOrWarning |> boldItalic |> Markdown |> notificationContentFromMarkdown theme
        | None -> ()
        match fixture.CustomMessage with
        | Some (userId, customMessageText) ->
            let (UserName userName) = userId |> userName userDic
            yield! [
                [ strong userName ; str " wrote" ] |> para theme paraDefaultSmallest
                customMessageText |> notificationContentFromMarkdown theme
            ]
        | None -> ()
        yield! renderAutoFixtureContent theme userDic detailsEntered squadDic fixture
    ]
    match semantic with
    | Some semantic ->
        let children = renderChildren ()
        [
            divVerticalSpace 10
            notification theme { notificationDefault with NotificationSemantic = semantic |> Some } children
        ]
    | None -> []

let private renderPost theme authUser userDic dispatch (postId, post) =
    let editOrRemovePost =
        match post.Removed, authUser with
        | false, Some authUser ->
            match authUser.Permissions.NewsPermissions with
            | Some newsPermissions ->
                match newsPermissions.EditOrRemovePostPermission with
                | Some userId when userId = post.UserId ->
                    let editPost = [ [ str "Edit post" ] |> para theme paraDefaultSmallest ] |> link theme (Internal (fun _ -> postId |> ShowEditPostModal |> dispatch))
                    let removePost = [ [ str "Remove post" ] |> para theme paraDefaultSmallest ] |> link theme (Internal (fun _ -> postId |> ShowRemovePostModal |> dispatch))
                    (editPost, removePost) |> Some
                | Some _ | None -> None
            | None -> None
        | _ -> None
    let renderChildren () = [
        let rightItem =
            let timestampText =
#if TICK
                ago post.Timestamp.LocalDateTime
#else
                post.Timestamp.LocalDateTime |> dateAndTimeText
#endif
            [ str timestampText ] |> para theme paraDefaultSmallest
        let (UserName userName) = post.UserId |> userName userDic
        let messageText =
            if post.Removed then Markdown REMOVED_MARKDOWN
            else
                match post.PostTypeDto with
                | StandardDto messageText -> messageText
                | MatchResultDto (messageText, _) -> messageText
        yield level true [
            levelLeft [ levelItem [ [ strong userName ; str " posted" ] |> para theme paraDefaultSmallest ] ]
            levelRight [ levelItem [ rightItem ] ] ]
        yield messageText |> notificationContentFromMarkdown theme
        match editOrRemovePost with
        | Some (editPost, removePost) ->
            yield level true [ levelLeft [ levelItem [ editPost ] ] ; levelRight [ levelItem [ removePost ] ] ]
        | None -> () ]
    let semantic = if post.Removed then Light else Black
    let children = renderChildren ()
    let onDismissNotification = if post.Removed then (fun _ -> postId |> DismissPost |> dispatch) |> Some else None
    [
        divVerticalSpace 10
        notification theme { notificationDefault with NotificationSemantic = semantic |> Some ; OnDismissNotification = onDismissNotification } children
    ]

let private addPost theme authUser dispatch =
    match authUser with
    | Some authUser ->
        match authUser.Permissions.NewsPermissions with
        | Some newsPermissions ->
            if newsPermissions.CreatePostPermission then
                [ [ str "Add post" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal (fun _ -> ShowAddPostModal |> dispatch)) |> Some
            else None
        | None -> None
    | None -> None

let render (useDefaultTheme, state, authUser:AuthUser option, usersProjection:Projection<_ * UserDic>, fixturesProjection:Projection<_ * FixtureDic>, squadsProjection:Projection<_ * SquadDic>, hasModal, _:int<tick>) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ strong "News" ] |> para theme paraCentredSmall
        yield hr theme false
        match usersProjection, state.NewsProjection with
        | Pending, _ | _, Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed, _ | _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, userDic), Ready (_, postDic, readyState) ->
            let morePosts =
                let paraMore = { paraDefaultSmallest with ParaAlignment = RightAligned }
                if readyState.MorePostsPending then
                    [ br ; [ str "Retrieving more posts... " ; icon iconSpinnerPulseSmall ] |> para theme paraMore ]
                else if readyState.HasMorePosts then
                    [ br ; [ [ str "More posts" ] |> link theme (Internal (fun _ -> MorePosts |> dispatch)) ] |> para theme paraMore ]
                else []
            match hasModal, readyState.AddPostState with
            | false, Some addPostState ->
                yield div divDefault [ lazyViewOrHMR2 renderAddPostModal (useDefaultTheme, addPostState) dispatch ]
            | _ -> ()
            match hasModal, readyState.EditPostState with
            | false, Some editPostState ->
                yield div divDefault [ lazyViewOrHMR2 renderEditPostModal (useDefaultTheme, editPostState) dispatch ]
            | _ -> ()
            match hasModal, readyState.RemovePostState with
            | false, Some removePostState ->
                yield div divDefault [ lazyViewOrHMR2 renderRemovePostModal (useDefaultTheme, postDic, removePostState) (RemovePostInput >> dispatch) ]
            | _ -> ()
            yield RctH.ofOption (addPost theme authUser dispatch)
            let posts = postDic |> List.ofSeq |> List.map (fun (KeyValue (postId, post)) -> post.Timestamp.LocalDateTime, News (postId, post))
            let all =
                match fixturesProjection, squadsProjection with
                | Ready (_, fixtureDic), Ready _ ->
                    let earliestTimeStamp = match posts |> List.sortBy fst with | (timestamp, _) :: _ -> Some timestamp | [] -> None
                    let now = DateTime.Now
                    let autoFixtures =
                        fixtureDic
                        |> List.ofSeq
                        |> List.choose (fun (KeyValue (fixtureId, fixture)) ->
                            let local = fixture.KickOff.LocalDateTime
                            if local <= now then
                                match earliestTimeStamp with
                                | Some earliest -> if local >= earliest then Some (local, AutoFixture (fixtureId, fixture)) else None
                                | None -> None
                            else None)
                    autoFixtures @ posts
                | _ -> posts
            yield! all
                |> List.sortBy fst
                |> List.rev
                |> List.choose (fun (_, newsType) ->
                    match newsType with
                    | News (postId, post) -> Some (renderPost theme authUser userDic dispatch (postId, post))
                    | AutoFixture (fixtureId, fixture) ->
                        match fixturesProjection, squadsProjection with
                        | Ready (_, fixtureDic), Ready (_, squadDic) -> Some (renderAutoFixture theme authUser userDic fixtureDic squadDic dispatch (fixtureId, fixture))
                        | _ -> None)
                |> List.collect id
            yield! morePosts ]
