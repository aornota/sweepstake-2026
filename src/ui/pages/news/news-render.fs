module Aornota.Sweepstake2026.Ui.Pages.News.Render

open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.News
open Aornota.Sweepstake2026.Common.Domain.Squad
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

let private renderAutoFixtureContent theme (userDic:UserDic) detailsEntered (squadDic:SquadDic) (fixture:Fixture) = [
    let plusOrMinus (points:int<point>) =
        if points > 0<point> then sprintf "+%i" points |> bold
        else if points = 0<point> then "0"
        else sprintf "%i" points |> italic
    let cardsText card count =
        let text = match card with | Yellow -> "yellow" | SecondYellow -> "second yellow" | Red -> "red"
        if count = 1 then sprintf "%s card" text else sprintf "%i %s cards" count text
    let teamScoreEventLines (items:(Squad * TeamScoreEvent * int<point>) list) =
        let lines =
            [
                match items |> List.filter (fun (_, teamScoreEvent, _) -> match teamScoreEvent with | MatchWon -> true | _ -> false) with
                | (squad, _, points) :: _ -> yield squad, sprintf "win (%s)" (plusOrMinus points)
                | _ -> ()
                match items |> List.filter (fun (_, teamScoreEvent, _) -> match teamScoreEvent with | MatchDrawn -> true | _ -> false) with
                | (squad, _, points) :: _ -> yield squad, sprintf "draw (%s)" (plusOrMinus points)
                | _ -> ()
                match items |> List.choose (fun (squad, teamScoreEvent, points) -> match teamScoreEvent with | PlayerCard (_, card) -> Some (squad, card, points) | _ -> None) with
                | [] -> ()
                | cardItems ->
                    yield!
                        cardItems
                        |> List.groupBy (fun (squad, card, _) -> squad, card)
                        |> List.map (fun ((squad, card), items) ->
                            let points = items |> List.sumBy (fun (_, _, points) -> points)
                            let text = sprintf "%s (%s)" (cardsText card items.Length) (plusOrMinus points)
                            squad, text)
            ]
        lines
        |> List.groupBy fst
        |> List.map (fun (squad, items) ->
            let (SquadName squadName) = squad.SquadName
            let concatenated = items |> List.map snd |> concatenate
            sprintf "%s: %s" squadName concatenated)
        |> List.sort
    let playerScoreEventLines (items:(Player * (PlayerScoreEvent * int<point>) list) list) =
        let thingText singular (points:int<point> list) =
            let points = points |> List.sum
            sprintf "%s (%s)" singular (plusOrMinus points)
        let thingsText singular plural (points:int<point> list) =
            let count, points = points.Length, points |> List.sum
            if count = 1 then sprintf "%s (%s)" singular (plusOrMinus points) else sprintf "%i %s (%s)" count plural (plusOrMinus points)
        let items =
            items
            |> List.map (fun (player, subItems) -> subItems |> List.map (fun (playerScoreEvent, points) -> player, playerScoreEvent, points))
            |> List.collect id
        let lines =
            [
                match items |> List.choose (fun (player, playerScoreEvent, points) -> match playerScoreEvent with | GoalScored | PenaltyScored -> Some (player, points) | _ -> None) with
                | [] -> ()
                | goalItems ->
                    yield!
                        goalItems
                        |> List.groupBy fst
                        |> List.map (fun (player, points) -> player, points |> List.map snd |> thingsText "goal" "goals")
                match items |> List.choose (fun (player, playerScoreEvent, points) -> match playerScoreEvent with | GoalAssisted -> Some (player, points) | _ -> None) with
                | [] -> ()
                | assistItems ->
                    yield!
                        assistItems
                        |> List.groupBy fst
                        |> List.map (fun (player, points) -> player, points |> List.map snd |> thingsText "assist" "assists")
                match items |> List.choose (fun (player, playerScoreEvent, points) -> match playerScoreEvent with | CleanSheetKept -> Some (player, points) | _ -> None) with
                | [] -> ()
                | cleanSheetItems ->
                    yield!
                        cleanSheetItems
                        |> List.groupBy fst
                        |> List.map (fun (player, points) -> player, points |> List.map snd |> thingText "clean sheet") // should be at most one per PLayer
                match items |> List.choose (fun (player, playerScoreEvent, points) -> match playerScoreEvent with | PenaltySaved -> Some (player, points) | _ -> None) with
                | [] -> ()
                | saveItems ->
                    yield!
                        saveItems
                        |> List.groupBy fst
                        |> List.map (fun (player, points) -> player, points |> List.map snd |> thingsText "penalty saved" "penalties saved")
                match items |> List.choose (fun (player, playerScoreEvent, points) -> match playerScoreEvent with | ManOfTheMatchAwarded -> Some (player, points) | _ -> None) with
                | [] -> ()
                | motmItems ->
                    yield!
                        motmItems
                        |> List.groupBy fst
                        |> List.map (fun (player, points) -> player, points |> List.map snd |> thingText "man-of-the-match") // should be at most one per PLayer
                match items |> List.choose (fun (player, playerScoreEvent, points) -> match playerScoreEvent with | OwnGoalScored -> Some (player, points) | _ -> None) with
                | [] -> ()
                | ownGoalItems ->
                    yield!
                        ownGoalItems
                        |> List.groupBy fst
                        |> List.map (fun (player, points) -> player, points |> List.map snd |> thingsText "own goal" "own goals")
                match items |> List.choose (fun (player, playerScoreEvent, points) -> match playerScoreEvent with | PenaltyMissed -> Some (player, points) | _ -> None) with
                | [] -> ()
                | missedItems ->
                    yield!
                        missedItems
                        |> List.groupBy fst
                        |> List.map (fun (player, points) -> player, points |> List.map snd |> thingsText "penalty missed" "penalties missed")
                match items |> List.choose (fun (player, playerScoreEvent, points) -> match playerScoreEvent with | Card card -> Some (player, card, points) | _ -> None) with
                | [] -> ()
                | cardItems ->
                    yield!
                        cardItems
                        |> List.groupBy (fun (player, card, _) -> player, card)
                        |> List.map (fun ((player, card), items) ->
                            let points = items |> List.sumBy (fun (_, _, points) -> points)
                            let text = sprintf "%s (%s)" (cardsText card items.Length) (plusOrMinus points)
                            player, text)
            ]
        lines
        |> List.groupBy fst
        |> List.map (fun (player, items) ->
            let (PlayerName playerName) = player.PlayerName
            let concatenated = items |> List.map snd |> concatenate
            sprintf "%s: %s" playerName concatenated)
        |> List.sort
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
                            let teamScoreEventLines = items |> List.map (fun (_, squad, teamScoreEvent, points) -> squad, teamScoreEvent, points) |> teamScoreEventLines
                            userId, points, teamScoreEventLines)
                    let userPlayerScores =
                        playerScoreEvents
                        |> List.groupBy (fun (userId, _, _) -> userId)
                        |> List.map (fun (userId, items) ->
                            let points = items |> List.sumBy (fun (_, _, subItems) -> subItems |> List.sumBy snd)
                            let playerScoreEventLines = items |> List.map (fun (_, player, subItems) -> player, subItems) |> playerScoreEventLines
                            userId, points, playerScoreEventLines)
                    yield!
                        userTeamScores @ userPlayerScores
                        |> List.groupBy (fun (userId, _, _) -> userId)
                        |> List.map (fun (userId, items) ->
                            let points = items |> List.map (fun (_, points, _) -> points) |> List.sum
                            let eventLines = items |> List.map (fun (_, _, eventLines) -> eventLines) |> List.collect id
                            userId |> userName userDic, points, eventLines)
                        |> List.sortBy (fun (userName, points, _) -> -points, userName)
                        |> List.map (fun (UserName userName, points, eventLines) ->
                            [
                                yield sprintf "- %s points for %s" (points |> plusOrMinus) (userName |> bold)
                                yield! eventLines |> List.map (fun eventLine -> sprintf "    - %s" eventLine)
                            ])
                        |> List.collect id

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

let private renderAutoFixtureCommon theme fixtureId fixture userDic fixtureDic squadDic (userAndCustomMessage:(UserName option * string) option) addCustomMessage editOrRemoveCustomMessage =
    let fixtureStatus = fixtureStatus fixtureDic fixtureId
    let semantic, infoOrWarning, detailsEntered =
        match fixtureStatus with
        | Some NotStarted | Some NotConfirmed | None -> None, None, false
        | Some DetailsPending -> Some Info, Some RESULT_PENDING, false
        | Some DetailsOverdue -> Some Warning, Some RESULT_OVERDUE, false
        | Some (DetailsMissing _) -> Some Warning, Some RESULT_HAS_MISSING_DETAILS, false
        | Some DetailsEntered -> Some Dark, None, true
    let children = [
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
        match userAndCustomMessage with
        | Some (userName, customMessage) ->
            yield! [
                match userName with
                | Some (UserName userName) ->
                    yield [ strong userName ; str " wrote" ] |> para theme paraDefaultSmallest
                | None -> ()
                if String.IsNullOrWhiteSpace customMessage |> not then yield customMessage |> Markdown |> notificationContentFromMarkdown theme
            ]
            match editOrRemoveCustomMessage with
            | Some (editCustomMessage, removeCustomMessage) ->
                yield level true [ levelLeft [ levelItem [ editCustomMessage ] ] ; levelRight [ levelItem [ removeCustomMessage ] ] ]
            | None -> ()
        | None ->
            match addCustomMessage with
            | Some addCustomMessage -> yield level true [ levelLeft [ levelItem [ addCustomMessage ] ] ]
            | None -> ()
        yield! renderAutoFixtureContent theme userDic detailsEntered squadDic fixture
    ]
    match semantic with
    | Some semantic -> notification theme { notificationDefault with NotificationSemantic = semantic |> Some } children |> Some
    | None -> None

let private renderPostCommon theme semantic (userAndTimestamp:(UserName * DateTimeOffset) option) message editOrRemovePost onDismissNotification =
    let children = [
        let rightItem =
            match userAndTimestamp with
            | Some (_, timestamp) ->
                let timestampText =
#if TICK
                    ago timestamp.LocalDateTime
#else
                    timestamp.LocalDateTime |> dateAndTimeText
#endif
                [ str timestampText ] |> para theme paraDefaultSmallest |> Some
            | None -> None
        yield level true [
            match userAndTimestamp with
            | Some (UserName userName, _) -> yield levelLeft [ levelItem [ [ strong userName ; str " posted" ] |> para theme paraDefaultSmallest ] ]
            | None -> ()
            match rightItem with
            | Some rightItem -> yield levelRight [ levelItem [ rightItem ] ]
            | None -> ()
        ]
        if String.IsNullOrWhiteSpace message |> not then yield message |> Markdown |> notificationContentFromMarkdown theme
        match editOrRemovePost with
        | Some (editPost, removePost) ->
            yield level true [ levelLeft [ levelItem [ editPost ] ] ; levelRight [ levelItem [ removePost ] ] ]
        | None -> ()
    ]
    notification theme { notificationDefault with NotificationSemantic = semantic |> Some ; OnDismissNotification = onDismissNotification } children

let private renderAddPostModal (useDefaultTheme, addPostState:AddPostState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Add post" ] |> para theme paraCentredSmall ]
    let onDismiss = match addPostState.AddPostStatus with | Some AddPostPending -> None | Some _ | None -> (fun _ -> CancelAddPost |> AddPostInput |> dispatch) |> Some
    let isAddingPost, addPostInteraction =
        match addPostState.AddPostStatus with
        | Some AddPostPending -> true, Loading
        | Some (AddPostFailed _) | None ->
            match validatePostMessage (Markdown addPostState.NewMessage) with
            | Some _ -> false, NotEnabled None
            | None -> false, Clickable ((fun _ -> AddPost |> AddPostInput |> dispatch), None)
    let errorText = match addPostState.AddPostStatus with | Some (AddPostFailed errorText) -> errorText |> Some | Some AddPostPending | None -> None
    let (PostId newPostKey), newMessage = addPostState.NewPostId, addPostState.NewMessage
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
            yield textArea theme newPostKey newMessage addPostState.NewMessageErrorText helpInfo true isAddingPost (NewMessageChanged >> AddPostInput >> dispatch)
            yield renderPostCommon theme Black None newMessage None None ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Add post" ] |> button theme { buttonLinkSmall with Interaction = addPostInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderEditPostModal (useDefaultTheme, postDic:PostDic, editPostState:EditPostState, userDic) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Edit post" ] |> para theme paraCentredSmall ]
    let postId = editPostState.PostId
    let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
    let userAndTimestamp =
        match post with
        | Some post -> (post.UserId |> userName userDic, post.Timestamp) |> Some
        | None -> None // should never happen
    let onDismiss = match editPostState.EditPostStatus with | Some EditPostPending -> None | Some _ | None -> (fun _ -> CancelEditPost |> EditPostInput |> dispatch) |> Some
    let isEditingPost, editPostInteraction =
        match editPostState.EditPostStatus with
        | Some EditPostPending -> true, Loading
        | Some (EditPostFailed _) | None ->
            match validatePostMessage (Markdown editPostState.Message) with
            | Some _ -> false, NotEnabled None
            | None -> false, Clickable ((fun _ -> EditPost |> EditPostInput |> dispatch), None)
    let errorText = match editPostState.EditPostStatus with | Some (EditPostFailed errorText) -> errorText |> Some | Some EditPostPending | None -> None
    let (PostId postKey), message = editPostState.PostId, editPostState.Message
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
            yield textArea theme postKey message editPostState.MessageErrorText helpInfo true isEditingPost (MessageChanged >> EditPostInput >> dispatch)
            yield renderPostCommon theme Black userAndTimestamp message None None ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Edit post" ] |> button theme { buttonLinkSmall with Interaction = editPostInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderRemovePostModal (useDefaultTheme, postDic:PostDic, removePostState:RemovePostState, userDic) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Remove post" ] |> para theme paraCentredSmall ]
    let postId = removePostState.PostId
    let post = if postId |> postDic.ContainsKey then postDic.[postId] |> Some else None
    let message, userAndTimestamp =
        match post with
        | Some post ->
            let (Markdown message) = post.Message
            message, (post.UserId |> userName userDic, post.Timestamp) |> Some
        | None -> String.Empty, None // should never happen
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
        yield renderPostCommon theme Light userAndTimestamp message None None
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Remove post" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderAddCustomMessageModal (useDefaultTheme, addCustomMessageState:AddCustomMessageState, userDic, fixtureDic:FixtureDic, squadDic) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Add custom message" ] |> para theme paraCentredSmall ]
    let fixtureId = addCustomMessageState.FixtureId
    let fixture = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId] |> Some else None
    let onDismiss = match addCustomMessageState.AddCustomMessageStatus with | Some AddCustomMessagePending -> None | Some _ | None -> (fun _ -> CancelAddCustomMessage |> AddCustomMessageInput |> dispatch) |> Some
    let isAddingCustomMessage, addCustomMessageInteraction =
        match addCustomMessageState.AddCustomMessageStatus with
        | Some AddCustomMessagePending -> true, Loading
        | Some (AddCustomMessageFailed _) | None ->
            match validateCustomMessage (Markdown addCustomMessageState.NewCustomMessage) with
            | Some _ -> false, NotEnabled None
            | None -> false, Clickable ((fun _ -> AddCustomMessage |> AddCustomMessageInput |> dispatch), None)
    let errorText = match addCustomMessageState.AddCustomMessageStatus with | Some (AddCustomMessageFailed errorText) -> errorText |> Some | Some AddCustomMessagePending | None -> None
    let (FixtureId newCustomMessageKey), newCustomMessage = addCustomMessageState.FixtureId, addCustomMessageState.NewCustomMessage
    let helpInfo = [
        str "Custom messages are persisted and public. You can use "
        [ str "Markdown syntax" ] |> link theme (Internal (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
        str " to format your custom message. A preview of your custom message will appear below." ; br; br ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
            yield textArea theme newCustomMessageKey newCustomMessage addCustomMessageState.NewCustomMessageErrorText helpInfo true isAddingCustomMessage (NewCustomMessageChanged >> AddCustomMessageInput >> dispatch)
            match fixture with
            | Some fixture ->
                match renderAutoFixtureCommon theme fixtureId fixture userDic fixtureDic squadDic ((None, newCustomMessage) |> Some) None None with
                | Some autoFixture -> yield autoFixture
                | None -> ()
            | None -> () ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Add custom message" ] |> button theme { buttonLinkSmall with Interaction = addCustomMessageInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderEditCustomMessageModal (useDefaultTheme, editCustomMessageState:EditCustomMessageState, userDic, fixtureDic:FixtureDic, squadDic) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Edit custom message" ] |> para theme paraCentredSmall ]
    let fixtureId = editCustomMessageState.FixtureId
    let fixture = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId] |> Some else None
    let onDismiss = match editCustomMessageState.EditCustomMessageStatus with | Some EditCustomMessagePending -> None | Some _ | None -> (fun _ -> CancelEditCustomMessage |> EditCustomMessageInput |> dispatch) |> Some
    let isEditingPost, editCustomMessageInteraction =
        match editCustomMessageState.EditCustomMessageStatus with
        | Some EditCustomMessagePending -> true, Loading
        | Some (EditCustomMessageFailed _) | None ->
            match validateCustomMessage (Markdown editCustomMessageState.CustomMessage) with
            | Some _ -> false, NotEnabled None
            | None -> false, Clickable ((fun _ -> EditCustomMessage |> EditCustomMessageInput |> dispatch), None)
    let errorText = match editCustomMessageState.EditCustomMessageStatus with | Some (EditCustomMessageFailed errorText) -> errorText |> Some | Some EditCustomMessagePending | None -> None
    let (FixtureId customMessageKey), customMessage = editCustomMessageState.FixtureId, editCustomMessageState.CustomMessage
    let helpInfo = [
        str "Custom message are persisted and public. You can use "
        [ str "Markdown syntax" ] |> link theme (Internal (fun _ -> ShowMarkdownSyntaxModal |> dispatch))
        str " to format your custom message. A preview of your custom message will appear below." ; br; br ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield field theme { fieldDefault with Grouped = FullWidth |> Some } [
            yield textArea theme customMessageKey customMessage editCustomMessageState.CustomMessageErrorText helpInfo true isEditingPost (CustomMessageChanged >> EditCustomMessageInput >> dispatch)
            match fixture with
            | Some fixture ->
                let userName =
                    match fixture.CustomMessage with
                    | Some (userId, _) -> userId |> userName userDic |> Some
                    | None -> None // should never happen
                match renderAutoFixtureCommon theme fixtureId fixture userDic fixtureDic squadDic ((userName, customMessage) |> Some) None None with
                | Some autoFixture -> yield autoFixture
                | None -> ()
            | None -> () ]
        yield field theme { fieldDefault with Grouped = RightAligned |> Some } [ [ str "Edit custom message" ] |> button theme { buttonLinkSmall with Interaction = editCustomMessageInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderRemoveCustomMessageModal (useDefaultTheme, removeCustomMessageState:RemoveCustomMessageState, userDic, fixtureDic:FixtureDic, squadDic) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Remove custom message" ] |> para theme paraCentredSmall ]
    let fixtureId = removeCustomMessageState.FixtureId
    let fixture = if fixtureId |> fixtureDic.ContainsKey then fixtureDic.[fixtureId] |> Some else None
    let userAndCustomMessage = //(post.UserId |> userName userDic, post.Timestamp) |> Some
        match fixture with
        | Some fixture ->
            match fixture.CustomMessage with
            | Some (userId, Markdown customMessage) -> (userId |> userName userDic |> Some, customMessage) |> Some
            | None -> None
        | None -> None
    let confirmInteraction, onDismiss =
        let confirm = (fun _ -> ConfirmRemoveCustomMessage |> dispatch)
        let cancel = (fun _ -> CancelRemoveCustomMessage |> dispatch)
        match removeCustomMessageState.RemoveCustomMessageStatus with
        | Some RemoveCustomMessagePending -> Loading, None
        | Some (RemoveCustomMessageFailed _) | None -> Clickable (confirm, None), cancel |> Some
    let errorText = match removeCustomMessageState.RemoveCustomMessageStatus with | Some (RemoveCustomMessageFailed errorText) -> errorText |> Some | Some RemoveCustomMessagePending | None -> None
    let warning = [ [ strong "Are you sure you want to remove this custom message?" ] |> para theme paraCentredSmaller ]
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield notification theme notificationWarning warning
        yield br
        match fixture with
        | Some fixture ->
            match renderAutoFixtureCommon theme fixtureId fixture userDic fixtureDic squadDic userAndCustomMessage None None with
            | Some autoFixture -> yield autoFixture
            | None -> ()
        | None -> ()
        yield br
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Remove custom message" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderAutoFixture theme authUser userDic fixtureDic squadDic dispatch (fixtureId, fixture:Fixture) =
    let addCustomMessage =
        match fixture.CustomMessage, authUser with
        | None, Some authUser ->
            match authUser.Permissions.NewsPermissions with
            | Some newsPermissions ->
                if newsPermissions.CreatePostPermission then
                    [ [ str "Add custom message" ] |> para theme paraDefaultSmallest ] |> link theme (Internal (fun _ -> fixtureId |> ShowAddCustomMessageModal |> dispatch)) |> Some
                else None
            | None -> None
        | _ -> None
    let editOrRemoveCustomMessage =
        match fixture.CustomMessage, authUser with
        | Some (userIdForMessage, _), Some authUser ->
            match authUser.Permissions.NewsPermissions with
            | Some newsPermissions ->
                match newsPermissions.EditOrRemovePostPermission with
                | Some userId when userId = userIdForMessage ->
                    let editPost = [ [ str "Edit custom message" ] |> para theme paraDefaultSmallest ] |> link theme (Internal (fun _ -> fixtureId |> ShowEditCustomMessageModal |> dispatch))
                    let removePost = [ [ str "Remove custom message" ] |> para theme paraDefaultSmallest ] |> link theme (Internal (fun _ -> fixtureId |> ShowRemoveCustomMessageModal |> dispatch))
                    (editPost, removePost) |> Some
                | Some _ | None -> None
            | None -> None
        | _ -> None
    let userAndCustomMessage = //(post.UserId |> userName userDic, post.Timestamp) |> Some
        match fixture.CustomMessage with
        | Some (userId, Markdown customMessage) -> (userId |> userName userDic |> Some, customMessage) |> Some
        | None -> None
    match renderAutoFixtureCommon theme fixtureId fixture userDic fixtureDic squadDic userAndCustomMessage addCustomMessage editOrRemoveCustomMessage with
    | Some autoFixture ->
        [
            divVerticalSpace 10
            autoFixture
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
    let semantic = if post.Removed then Light else Black
    let userAndTimestamp = (post.UserId |> userName userDic, post.Timestamp) |> Some
    let message =
        if post.Removed then REMOVED_MARKDOWN
        else
            let (Markdown message) = post.Message
            message
    let onDismissNotification = if post.Removed then (fun _ -> postId |> DismissPost |> dispatch) |> Some else None
    [
        divVerticalSpace 10
        renderPostCommon theme semantic userAndTimestamp message editOrRemovePost onDismissNotification
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
                yield div divDefault [ lazyViewOrHMR2 renderEditPostModal (useDefaultTheme, postDic,editPostState, userDic) dispatch ]
            | _ -> ()
            match hasModal, readyState.RemovePostState with
            | false, Some removePostState ->
                yield div divDefault [ lazyViewOrHMR2 renderRemovePostModal (useDefaultTheme, postDic, removePostState, userDic) (RemovePostInput >> dispatch) ]
            | _ -> ()
            match hasModal, readyState.AddCustomMessageState, fixturesProjection, squadsProjection with
            | false, Some addCustomMessageState, Ready (_, fixtureDic), Ready (_, squadDic) ->
                yield div divDefault [ lazyViewOrHMR2 renderAddCustomMessageModal (useDefaultTheme, addCustomMessageState, userDic, fixtureDic, squadDic) dispatch ]
            | false, Some _, _, _ -> () // should never happen
            | _ -> ()
            match hasModal, readyState.EditCustomMessageState, fixturesProjection, squadsProjection with
            | false, Some editCustomMessageState, Ready (_, fixtureDic), Ready (_, squadDic) ->
                yield div divDefault [ lazyViewOrHMR2 renderEditCustomMessageModal (useDefaultTheme, editCustomMessageState, userDic, fixtureDic, squadDic) dispatch ]
            | false, Some _, _, _ -> () // should never happen
            | _ -> ()
            match hasModal, readyState.RemoveCustomMessageState, fixturesProjection, squadsProjection with
            | false, Some removeCustomMessageState, Ready (_, fixtureDic), Ready (_, squadDic) ->
                yield div divDefault [ lazyViewOrHMR2 renderRemoveCustomMessageModal (useDefaultTheme, removeCustomMessageState, userDic, fixtureDic, squadDic) (RemoveCustomMessageInput >> dispatch) ]
            | false, Some _, _, _ -> () // should never happen
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
