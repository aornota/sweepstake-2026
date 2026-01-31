module Aornota.Sweepstake2026.Ui.Program.Render

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Ui.Common.LazyViewOrHMR
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.Render.Markdown
open Aornota.Sweepstake2026.Ui.Common.TimestampHelper
open Aornota.Sweepstake2026.Ui.Pages
open Aornota.Sweepstake2026.Ui.Program.Common
open Aornota.Sweepstake2026.Ui.Program.Markdown.Literals
open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common
open Aornota.Sweepstake2026.Ui.Shared
open Aornota.Sweepstake2026.Ui.Theme.Common
open Aornota.Sweepstake2026.Ui.Theme.Render.Bulma
open Aornota.Sweepstake2026.Ui.Theme.Shared
open Aornota.Sweepstake2026.Common.Domain.Squad // note: after Aornota.Sweepstake2026.Ui.Render.Bulma to avoid collision with Icon.Forward
open Aornota.Sweepstake2026.Common.Domain.User // note: after Aornota.Sweepstake2026.Ui.Render.Bulma to avoid collision with Icon.Password

open System

module RctH = Fable.React.Helpers

type private HeaderStatus = | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn of authUser : AuthUser

let private headerStatus (appState:AppState) =
    let headerStatus =
        match appState with
        | ReadingPreferences -> ReadingPreferencesHS
        | Connecting _ -> ConnectingHS
        | ServiceUnavailable -> ServiceUnavailableHS
        | AutomaticallySigningIn _ -> SigningIn
        | Unauth unauthState ->
            match unauthState.SignInState with
            | Some signInState -> match signInState.SignInStatus with | Some SignInPending -> SigningIn | Some (SignInFailed _) | None -> NotSignedIn
            | None -> NotSignedIn
        | Auth authState -> match authState.SigningOut with | true -> SigningOut | false -> SignedIn authState.AuthUser
    headerStatus

let private headerPages (appState:AppState) =
    let newsText unseenNewsCount = if unseenNewsCount > 0 then sprintf "News (%i)" unseenNewsCount else "News"
    match appState with
    | Unauth unauthState ->
        let unseenNewsCount = unauthState.UnauthPageStates.NewsState.UnseenCount
        [
            unseenNewsCount |> newsText, unauthState.CurrentUnauthPage = NewsPage, UnauthPage NewsPage, NewsPage |> ShowUnauthPage |> UnauthInput
            "Scores", unauthState.CurrentUnauthPage = ScoresPage, UnauthPage ScoresPage, ScoresPage |> ShowUnauthPage |> UnauthInput
            "Squads", unauthState.CurrentUnauthPage = SquadsPage, UnauthPage SquadsPage, SquadsPage |> ShowUnauthPage |> UnauthInput
            "Fixtures / Results", unauthState.CurrentUnauthPage = FixturesPage, UnauthPage FixturesPage, FixturesPage |> ShowUnauthPage |> UnauthInput
        ]
    | Auth authState ->
        let unseenNewsCount = authState.UnauthPageStates.NewsState.UnseenCount
        let unseenChatCount = authState.AuthPageStates.ChatState.UnseenCount
        let chatText = if unseenChatCount > 0 then sprintf "Chat (%i)" unseenChatCount else "Chat"
        [
            unseenNewsCount |> newsText, authState.CurrentPage = UnauthPage NewsPage, UnauthPage NewsPage, NewsPage |> UnauthPage |> ShowPage |> AuthInput
            "Scores", authState.CurrentPage = UnauthPage ScoresPage, UnauthPage ScoresPage, ScoresPage |> UnauthPage |> ShowPage |> AuthInput
            "Squads", authState.CurrentPage = UnauthPage SquadsPage, UnauthPage SquadsPage, SquadsPage |> UnauthPage |> ShowPage |> AuthInput
            "Fixtures / Results", authState.CurrentPage = UnauthPage FixturesPage, UnauthPage FixturesPage, FixturesPage |> UnauthPage |> ShowPage |> AuthInput
            "User administration", authState.CurrentPage = AuthPage UserAdminPage, AuthPage UserAdminPage, UserAdminPage |> AuthPage |> ShowPage |> AuthInput
            "Draft administration", authState.CurrentPage = AuthPage DraftAdminPage, AuthPage DraftAdminPage, DraftAdminPage |> AuthPage |> ShowPage |> AuthInput
            "Drafts", authState.CurrentPage = AuthPage DraftsPage, AuthPage DraftsPage, DraftsPage |> AuthPage |> ShowPage |> AuthInput
            chatText, authState.CurrentPage = AuthPage ChatPage, AuthPage ChatPage, ChatPage |> AuthPage |> ShowPage |> AuthInput
        ]
    | ReadingPreferences | Connecting _ | ServiceUnavailable | AutomaticallySigningIn _ -> []

let private renderHeader (useDefaultTheme, navbarBurgerIsActive, serverStarted:DateTimeOffset option, headerStatus, headerPages, _:int<tick>) dispatch =
    let isAdminPage page = match page with | AuthPage UserAdminPage | AuthPage DraftAdminPage -> true | _ -> false
    let theme = getTheme useDefaultTheme
    let serverStarted =
        match headerStatus, serverStarted with
        | SignedIn authUser, Some serverStarted when authUser.UserType = SuperUser ->
            let timestampText =
#if TICK
                ago serverStarted.LocalDateTime
#else
                sprintf "on %s" (serverStarted.LocalDateTime |> dateAndTimeText)
#endif
            navbarItem [ [ str (sprintf "Server started %s" timestampText ) ] |> para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDark } ] |> Some
        | _, _ -> None
    let statusInfo =
        let paraStatus = { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker }
        let spinner = icon iconSpinnerPulseSmall
        let separator = [ str "|" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = SemiBold }
        match headerStatus with
        | ReadingPreferencesHS -> [ [ str "Reading preferences... " ; spinner ] |> para theme paraStatus ]
        | ConnectingHS -> [ [ str "Connecting... " ; spinner ] |> para theme paraStatus ]
        | ServiceUnavailableHS -> [ [ str "Service unavailable" ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } ]
        | SigningIn -> [ separator ; [ str "Signing in... " ; spinner ] |> para theme paraStatus ]
        | SigningOut -> [ separator ; [ str "Signing out... " ; spinner ] |> para theme paraStatus ]
        | NotSignedIn ->
            [
                separator ; [ str "Not signed in" ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Primary }
                [ [ str "Sign in" ] |> link theme (Internal (fun _ -> ShowSignInModal |> UnauthInput |> AppInput |> dispatch)) ] |> para theme paraDefaultSmallest
            ]
        | SignedIn authUser ->
            let (UserName userName) = authUser.UserName
            [ separator ; [ str "Signed in as " ; strong userName ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Success } ]
    let authUserDropDown =
        match headerStatus with
        | SignedIn authUser ->
            let changePassword = [ str "Change password" ] |> link theme (Internal (fun _ -> ShowChangePasswordModal |> AuthInput |> AppInput |> dispatch))
            let signOut = [ str "Sign out" ] |> link theme (Internal (fun _ -> SignOut |> AuthInput |> AppInput |> dispatch))
            navbarDropDown theme (icon iconUserSmall) [
                match authUser.Permissions.ChangePasswordPermission with
                | Some userId when userId = authUser.UserId -> yield navbarDropDownItem theme false [ [ changePassword ] |> para theme paraDefaultSmallest ]
                | Some _ | None -> ()
                yield navbarDropDownItem theme false [ [ signOut ] |> para theme paraDefaultSmallest ] ] |> Some
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn -> None
    let pageTabs =
        headerPages
        |> List.filter (fun (_, _, page, _) -> isAdminPage page |> not)
        |> List.map (fun (text, isActive, _, appInput) -> { IsActive = isActive ; TabText = text ; TabLinkType = Internal (fun _ -> appInput |> AppInput |> dispatch) })
    let adminDropDown =
        match headerStatus with
        | SignedIn authUser ->
            let userAdmin =
                match authUser.Permissions.UserAdminPermissions with
                | Some _ ->
                    let text, isActive, appInput =
                        match headerPages |> List.tryFind (fun (_, _, page, _) -> page = AuthPage UserAdminPage) with
                        | Some (text, isActive, _, appInput) -> text, isActive, appInput
                        | None -> "User administration", false, UserAdminPage |> AuthPage |> ShowPage |> AuthInput // note: should never happen
                    let userAdmin = [ str text ] |> link theme (Internal (fun _ -> appInput |> AppInput |> dispatch))
                    navbarDropDownItem theme isActive [ [ userAdmin ] |> para theme paraDefaultSmallest ] |> Some
                | None -> None
            let draftAdmin =
                match authUser.Permissions.DraftAdminPermissions with
                | Some _ ->
                    let text, isActive, appInput =
                        match headerPages |> List.tryFind (fun (_, _, page, _) -> page = AuthPage DraftAdminPage) with
                        | Some (text, isActive, _, appInput) -> text, isActive, appInput
                        | None -> "Draft administration", false, DraftAdminPage |> AuthPage |> ShowPage |> AuthInput // note: should never happen
                    let draftAdmin = [ str text ] |> link theme (Internal (fun _ -> appInput |> AppInput |> dispatch))
                    navbarDropDownItem theme isActive [ [ draftAdmin ] |> para theme paraDefaultSmallest ] |> Some
                | None -> None
            let hasDropDown = match userAdmin, draftAdmin with | None, None -> false | _ -> true
            if hasDropDown then navbarDropDown theme (icon iconAdminSmall) [ RctH.ofOption userAdmin ; RctH.ofOption draftAdmin ] |> Some
            else None
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut | NotSignedIn | SignedIn _ -> None
    let infoDropDown =
        match headerStatus with
        | NotSignedIn | SignedIn _ ->
            let scoringSystem = [ [ str "Scoring system" ] |> link theme (Internal (fun _ -> ScoringSystem |> ShowStaticModal |> dispatch)) ] |> para theme paraDefaultSmallest
            let draftAlgorithm = [ [ str "Draft algorithm" ] |> link theme (Internal (fun _ -> DraftAlgorithm |> ShowStaticModal |> dispatch)) ] |> para theme paraDefaultSmallest
            let payouts = [ [ str "Payouts" ] |> link theme (Internal (fun _ -> Payouts |> ShowStaticModal |> dispatch)) ] |> para theme paraDefaultSmallest
            navbarDropDown theme (icon iconInfoSmall) [
                navbarDropDownItem theme false [ scoringSystem ]
                navbarDropDownItem theme false [ draftAlgorithm ]
                navbarDropDownItem theme false [ payouts ] ] |> Some
        | ReadingPreferencesHS | ConnectingHS | ServiceUnavailableHS | SigningIn | SigningOut -> None
    let toggleThemeTooltipText = match useDefaultTheme with | true -> "Switch to dark theme" | false -> "Switch to light theme"
    let toggleThemeTooltipData = if navbarBurgerIsActive then tooltipDefaultRight else tooltipDefaultLeft
    let toggleThemeInteraction = Clickable ((fun _ -> ToggleTheme |> dispatch), { toggleThemeTooltipData with TooltipText = toggleThemeTooltipText } |> Some)
    let toggleThemeButton = { buttonDarkSmall with Interaction = toggleThemeInteraction ; IconLeft = iconTheme |> Some }
    let navbarData = { navbarDefault with NavbarSemantic = Light |> Some }
    navbar theme navbarData [
        container (Fluid |> Some) [
            navbarBrand [
                yield navbarItem [ image "sweepstake-2026-24x24.png" (FixedSize Square24 |> Some) ]
                yield navbarItem [ [ str SWEEPSTAKE_2026 ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Black ; Weight = Bold } ]
                yield RctH.ofOption serverStarted
                yield! statusInfo |> List.map (fun element -> navbarItem [ element ])
                yield navbarBurger (fun _ -> ToggleNavbarBurger |> dispatch) navbarBurgerIsActive ]
            navbarMenu theme navbarData navbarBurgerIsActive [
                navbarStart [
                    yield RctH.ofOption authUserDropDown
                    yield navbarItem [ tabs theme { tabsDefault with Tabs = pageTabs } ]
                    yield RctH.ofOption adminDropDown
                    yield RctH.ofOption infoDropDown ]
                navbarEnd [
#if TICK
                    navbarItem [ [ str (DateTimeOffset.UtcNow.LocalDateTime.ToString ("HH:mm:ss")) ] |> para theme { paraDefaultSmallest with ParaColour = GreyscalePara GreyDarker } ]
#endif
                    navbarItem [ [] |> button theme toggleThemeButton ] ] ] ] ]

let private renderStaticModal (useDefaultTheme, titleText, markdown) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong titleText ] |> para theme paraCentredSmall ]
    let onDismiss = (fun _ -> HideStaticModal |> dispatch) |> Some
    cardModal theme (Some(title, onDismiss)) [ markdown |> contentFromMarkdown theme ]

let private markdownSyntaxKey = Guid.NewGuid ()

let private renderMarkdownSyntaxModal useDefaultTheme dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Markdown syntax" ] |> para theme paraCentredSmall ]
    let onDismiss = (fun _ -> HideStaticModal |> dispatch) |> Some
    let body = [
        [ str "As a very quick introduction to Markdown syntax, the following:" ] |> para theme paraCentredSmaller ; br
        textArea theme markdownSyntaxKey MARKDOWN_SYNTAX_MARKDOWN None [] false true ignore
        br ; [ str "will appear as:" ] |> para theme paraCentredSmaller ; br
        Markdown MARKDOWN_SYNTAX_MARKDOWN |> contentFromMarkdown theme ]
    cardModal theme (Some(title, onDismiss)) body

let private renderSignInModal (useDefaultTheme, signInState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Sign in" ] |> para theme paraCentredSmall ]
    let isSigningIn, signInInteraction, onEnter, onDismiss =
        let signIn, onDismiss = (fun _ -> SignIn |> dispatch), (fun _ -> CancelSignIn |> dispatch)
        match signInState.SignInStatus with
        | Some SignInPending -> true, Loading, ignore, None
        | Some (SignInFailed _) | None ->
            match validateUserName [] (UserName signInState.UserNameText), validatePassword (Password signInState.PasswordText) with
            | None, None -> false, Clickable (signIn, None), signIn, onDismiss |> Some
            | _ -> false, NotEnabled None, ignore, onDismiss |> Some
    let errorText = match signInState.SignInStatus with | Some (SignInFailed errorText) -> errorText |> Some | Some SignInPending | None -> None
    let body = [
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter your user name and password" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme signInState.UserNameKey signInState.UserNameText (iconUserSmall |> Some) false signInState.UserNameErrorText [] (signInState.FocusPassword |> not) isSigningIn
                (UserNameTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme signInState.PasswordKey signInState.PasswordText (iconPasswordSmall |> Some) true signInState.PasswordErrorText [] signInState.FocusPassword isSigningIn
                (PasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Sign in" ] |> button theme { buttonLinkSmall with Interaction = signInInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderUnauth (useDefaultTheme, unauthState, hasStaticModal, ticks) (dispatch:UnauthInput -> unit) =
    let hasModal = if hasStaticModal then true else match unauthState.SignInState with | Some _ -> true | None -> false
    let usersProjection = unauthState.UnauthProjections.UsersProjection
    let squadsProjection = unauthState.UnauthProjections.SquadsProjection
    let fixturesProjection = unauthState.UnauthProjections.FixturesProjection
    div divDefault [
        match hasStaticModal, unauthState.SignInState with
        | false, Some signInState ->
            yield lazyViewOrHMR2 renderSignInModal (useDefaultTheme, signInState) (SignInInput >> dispatch)
        | _ -> ()
        match unauthState.CurrentUnauthPage with
        | NewsPage ->
            let newsState = unauthState.UnauthPageStates.NewsState
            yield lazyViewOrHMR2 News.Render.render (useDefaultTheme, newsState, None, usersProjection, hasModal, ticks) (NewsInput >> UnauthPageInput >> dispatch)
        | ScoresPage ->
            let scoresState = unauthState.UnauthPageStates.ScoresState
            yield lazyViewOrHMR2 Scores.Render.render (useDefaultTheme, scoresState, None, usersProjection, squadsProjection, fixturesProjection) (ScoresInput >> UnauthPageInput >> dispatch)
        | SquadsPage ->
            let squadsState = unauthState.UnauthPageStates.SquadsState
            yield lazyViewOrHMR2 Squads.Render.render (useDefaultTheme, squadsState, None, squadsProjection, usersProjection, fixturesProjection, None, hasModal)
                (SquadsInput >> UnauthPageInput >> dispatch)
        | FixturesPage ->
            let fixturesState = unauthState.UnauthPageStates.FixturesState
            yield lazyViewOrHMR2 Fixtures.Render.render (useDefaultTheme, fixturesState, None, fixturesProjection, squadsProjection, usersProjection, hasModal, ticks)
                (FixturesInput >> UnauthPageInput >> dispatch) ]

let private renderChangePasswordModal (useDefaultTheme, changePasswordState) dispatch =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Change password" ] |> para theme paraCentredSmall ]
    let onDismiss = match changePasswordState.ChangePasswordStatus with | Some ChangePasswordPending -> None | Some _ | None -> (fun _ -> CancelChangePassword |> dispatch) |> Some
    let isChangingPassword, changePasswordInteraction, onEnter =
        let changePassword = (fun _ -> ChangePassword |> dispatch)
        match changePasswordState.ChangePasswordStatus with
        | Some ChangePasswordPending -> true, Loading, ignore
        | Some (ChangePasswordFailed _) | None ->
            let validPassword = validatePassword (Password changePasswordState.NewPasswordText)
            let validConfirmPassword = validateConfirmPassword (Password changePasswordState.NewPasswordText) (Password changePasswordState.ConfirmPasswordText)
            match validPassword, validConfirmPassword with
            | None, None -> false, Clickable (changePassword, None), changePassword
            | _ -> false, NotEnabled None, ignore
    let errorText = match changePasswordState.ChangePasswordStatus with | Some (ChangePasswordFailed errorText) -> errorText |> Some | Some ChangePasswordPending | None -> None
    let body = [
        match changePasswordState.MustChangePasswordReason with
        | Some mustChangePasswordReason ->
            let because reasonText = sprintf "You must change your password because %s" reasonText
            let reasonText =
                match mustChangePasswordReason with
                | FirstSignIn -> because "this is the first time you have signed in"
                | PasswordReset -> because "it has been reset by a system administrator"
            yield notification theme notificationInfo [ [ str reasonText ] |> para theme paraCentredSmallest ]
            yield br
        | None -> ()
        match errorText with
        | Some errorText ->
            yield notification theme notificationDanger [ [ str errorText ] |> para theme paraDefaultSmallest ]
            yield br
        | None -> ()
        yield [ str "Please enter your new password (twice)" ] |> para theme paraCentredSmaller
        yield br
        // TODO-NMB-MEDIUM: Finesse layout / alignment - and add labels?...
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            textBox theme changePasswordState.NewPasswordKey changePasswordState.NewPasswordText (iconPasswordSmall |> Some) true changePasswordState.NewPasswordErrorText []
                true isChangingPassword (NewPasswordTextChanged >> dispatch) ignore ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
             textBox theme changePasswordState.ConfirmPasswordKey changePasswordState.ConfirmPasswordText (iconPasswordSmall |> Some) true changePasswordState.ConfirmPasswordErrorText []
                false isChangingPassword (ConfirmPasswordTextChanged >> dispatch) onEnter ]
        yield field theme { fieldDefault with Grouped = Centred |> Some } [ [ str "Change password" ] |> button theme { buttonLinkSmall with Interaction = changePasswordInteraction } ]        ]
    cardModal theme (Some(title, onDismiss)) body

let private renderSigningOutModal useDefaultTheme =
    let theme = getTheme useDefaultTheme
    let title = [ [ strong "Signing out" ] |> para theme paraCentredSmall ]
    cardModal theme (Some(title, None)) [ div divCentred [ icon iconSpinnerPulseLarge ] ]

let private userDraftPickSummary theme (squadDic:SquadDic) (userDraftPickDtos:UserDraftPickDto list) =
    let isTeamPick userDraftPick = match userDraftPick with | TeamPick _ -> true | PlayerPick _ -> false
    let isGoalkeeper squadDic userDraftPick =
        match userDraftPick with
        | TeamPick _ -> false
        | PlayerPick (squadId, playerId) -> match (squadId, playerId) |> playerType squadDic with | Some Goalkeeper -> true | _ -> false
    let isOutfieldPlayer squadDic userDraftPick =
        match userDraftPick with
        | TeamPick _ -> false
        | PlayerPick (squadId, playerId) -> match (squadId, playerId) |> playerType squadDic with | Some Defender | Some Midfielder | Some Forward -> true | _ -> false
    let userDraftPicks = userDraftPickDtos |> List.map (fun userDraftPickDto -> userDraftPickDto.UserDraftPick)
    let teamCount = userDraftPicks |> List.filter isTeamPick |> List.length
    let goalkeeperCount = userDraftPicks |> List.filter (isGoalkeeper squadDic) |> List.length
    let outfieldPlayerCount = userDraftPicks |> List.filter (isOutfieldPlayer squadDic) |> List.length
    let counts = [
        if teamCount > 0 then
            let teamPlural, coachPlural = if teamCount > 1 then "s", "es" else String.Empty, String.Empty
            yield sprintf "%i team%s/coach%s" teamCount teamPlural coachPlural
        if goalkeeperCount > 0 then
            let plural = if goalkeeperCount > 1 then "s" else String.Empty
            yield sprintf "%i goalkeeper%s" goalkeeperCount plural
        if outfieldPlayerCount > 0 then
            let plural = if outfieldPlayerCount > 1 then "s" else String.Empty
            yield sprintf "%i outfield player%s" outfieldPlayerCount plural ]
    let items = counts.Length
    let counts = counts |> List.mapi (fun i item -> if i = 0 then item else if i + 1 < items then sprintf ", %s" item else sprintf " and %s" item)
    let counts = counts |> List.fold (fun text item -> sprintf "%s%s" text item) String.Empty
    [ str (sprintf "You have selected %s for this draft." counts) ] |> para theme { paraDefaultSmallest with Weight = SemiBold }

let private currentDraftSummary useDefaultTheme authUser (_, draft:Draft) (currentUserDraftDto:CurrentUserDraftDto option) (squadDic:SquadDic) dispatch =
    let theme = getTheme useDefaultTheme
    let canDraft = match authUser.Permissions.DraftPermission with | Some userId when userId = authUser.UserId -> true | Some _ | None -> false
    if canDraft then
        let squad, players = authUser.UserId |> pickedByUser squadDic
        let pickedCounts = (squad, players) |> pickedCounts
        let stillRequired =
            match pickedCounts |> stillRequired with
            | Some stillRequired -> [ strong (sprintf "%s." stillRequired) ] |> para theme paraDefaultSmallest |> Some
            | None -> None
        let semanticAndContents =
            let draftTextLower = draft.DraftOrdinal |> draftTextLower
            let isFirst = draft.DraftOrdinal = DraftOrdinal 1
            let scoresPageBlurb = [
                br
                [
                    str "You can see your successful picks from previous drafts on the "
                    [ str "Scores" ] |> link theme (Internal (fun _ -> ScoresPage |> UnauthPage |> ShowPage |> dispatch))
                    str " page."
                ] |> para theme paraDefaultSmallest
            ]
            match draft.DraftStatus with
            | PendingOpen (starts, ends) ->
                let starts, ends = starts.LocalDateTime |> dateAndTimeText, ends.LocalDateTime |> dateAndTimeText
                let contents =
                    [
                        yield [ strong (sprintf "The %s will open on %s and will close on %s" draftTextLower starts ends) ] |> para theme paraCentredSmaller
                        if isFirst |> not then yield! scoresPageBlurb
                    ]
                (Info, contents) |> Some
            | Opened ends ->
                let ends = ends.LocalDateTime |> dateAndTimeText
                let semantic, userDraftPickSummary =
                    match stillRequired, currentUserDraftDto with
                    | None, _ ->
                        Success, [ [ strong (sprintf "You do not need to make any selections for the %s." draftTextLower) ] |> para theme paraDefaultSmallest ]
                    | Some stillRequired, Some currentUserDraftDto when currentUserDraftDto.UserDraftPickDtos.Length > 0 ->
                        let contents =
                            [
                                stillRequired
                                br
                                currentUserDraftDto.UserDraftPickDtos |> userDraftPickSummary theme squadDic
                            ]
                        Info, contents
                    | Some stillRequired, _ ->
                        let contents =
                            [
                                stillRequired
                                br
                                [ strong (sprintf "You have not made any selections for the %s." draftTextLower) ] |> para theme { paraDefaultSmallest with ParaColour = SemanticPara Danger }
                            ]
                        Warning, contents
                let recommendation =
                    if isFirst then
                        [
                            br
                            [ em "We recommend making at least 25-30 selections for the first draft." ] |> para theme { paraDefaultSmallest with Weight = SemiBold }
                        ]
                    else []
                let pleaseSelect =
                    match stillRequired with
                    | Some _ ->
                        [
                            str "Please select teams/coaches, goalkeepers and outfield players (as required) on the "
                            [ str "Squads" ] |> link theme (Internal (fun _ -> SquadsPage |> UnauthPage |> ShowPage |> dispatch))
                            str " page. You can prioritize your selections on the "
                            [ str "Drafts" ] |> link theme (Internal (fun _ -> DraftsPage |> AuthPage |> ShowPage |> dispatch))
                            str " page."
                        ] |> para theme paraDefaultSmallest |> Some
                    | None -> None
                let contents = [
                    yield [ strong (sprintf "The %s is now open and will close on %s" draftTextLower ends) ] |> para theme paraCentredSmaller
                    if isFirst |> not then yield! scoresPageBlurb
                    yield br
                    match pleaseSelect with
                    | Some pleaseSelect ->
                        yield pleaseSelect
                        yield br
                    | None -> ()
                    yield! userDraftPickSummary
                    yield! recommendation ]
                (semantic, contents) |> Some
            | PendingProcessing processingStarted ->
                let status = if processingStarted then "is currently being processed" else "will be processed soon"
                let contents = [ [ strong (sprintf "The %s is now closed and %s" draftTextLower status) ] |> para theme paraCentredSmaller ]
                (Info, contents) |> Some
            | FreeSelection ->
                match stillRequired with
                | Some stillRequired ->
                    let required = match pickedCounts |> required with | Some required -> required | None -> "whatever is still required" // note: should never happen
                    let contents = [
                        [ strong "The draft phase is over and \"free pick\" mode has commenced" ] |> para theme paraCentredSmaller
                        br
                        stillRequired
                        br
                        [ str (sprintf "Please pick %s on the Squads page." required) ] |> para theme paraDefaultSmallest
                        br
                        [ em "Note that you will only be credited with points scored by these \"free picks\" in forthcoming fixtures, not with points that they have already scored." ]
                        |> para theme paraDefaultSmallest ]
                    (Warning, contents) |> Some
                | None -> None
            | _ -> None // note: should never happen
        match semanticAndContents with
        | Some (semantic, contents) -> columnContent [ notification theme { notificationDefault with NotificationSemantic = semantic |> Some } contents ] |> Some
        | None -> None
        else None

let private renderAuth (useDefaultTheme, authState, hasStaticModal, ticks) dispatch =
    let hasModal = if hasStaticModal then true else match authState.ChangePasswordState, authState.SigningOut with | Some _, _ -> true | None, true -> true | None, false -> false
    let authUser = authState.AuthUser
    let usersProjection = authState.UnauthProjections.UsersProjection
    let squadsProjection = authState.UnauthProjections.SquadsProjection
    let fixturesProjection = authState.UnauthProjections.FixturesProjection
    let draftsProjection = authState.AuthProjections.DraftsProjection
    div divDefault [
        match hasStaticModal, authState.ChangePasswordState with
        | false, Some changePasswordState ->
            yield lazyViewOrHMR2 renderChangePasswordModal (useDefaultTheme, changePasswordState) (ChangePasswordInput >> dispatch)
        | _ -> ()
        match hasStaticModal, authState.SigningOut with
        | false, true ->
            yield lazyViewOrHMR renderSigningOutModal useDefaultTheme
        | _ -> ()
        match squadsProjection, draftsProjection with
        | Pending, _ | _, Pending | Failed, _ | _, Failed -> ()
        | Ready (_, squadDic), Ready (_, draftDic, currentUserDraftDto) ->
            match draftDic |> currentDraft with
            | Some currentDraft ->
                yield RctH.ofOption (currentDraftSummary useDefaultTheme authState.AuthUser currentDraft currentUserDraftDto squadDic dispatch)
            | None -> ()
        match authState.CurrentPage with
        | UnauthPage NewsPage ->
            let newsState = authState.UnauthPageStates.NewsState
            yield lazyViewOrHMR2 News.Render.render (useDefaultTheme, newsState, authState.AuthUser |> Some, usersProjection, hasModal, ticks) (NewsInput >> UPageInput >> PageInput >> dispatch)
        | UnauthPage ScoresPage ->
            let scoresState = authState.UnauthPageStates.ScoresState
            yield lazyViewOrHMR2 Scores.Render.render (useDefaultTheme, scoresState, authUser |> Some, usersProjection, squadsProjection, fixturesProjection)
                (ScoresInput >> UPageInput >> PageInput >> dispatch)
        | UnauthPage SquadsPage ->
            let squadsState = authState.UnauthPageStates.SquadsState
            yield lazyViewOrHMR2 Squads.Render.render (useDefaultTheme, squadsState, authUser |> Some, squadsProjection, usersProjection, fixturesProjection, draftsProjection |> Some, hasModal)
                (SquadsInput >> UPageInput >> PageInput >> dispatch)
        | UnauthPage FixturesPage ->
            let fixturesState = authState.UnauthPageStates.FixturesState
            yield lazyViewOrHMR2 Fixtures.Render.render (useDefaultTheme, fixturesState, authUser |> Some, fixturesProjection, squadsProjection, usersProjection, hasModal, ticks)
                (FixturesInput >> UPageInput >> PageInput >> dispatch)
        | AuthPage UserAdminPage ->
            match authState.AuthPageStates.UserAdminState with
            | Some userAdminState ->
                yield lazyViewOrHMR2 UserAdmin.Render.render (useDefaultTheme, userAdminState, authState.AuthUser, usersProjection, hasModal) (UserAdminInput >> APageInput >> PageInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is AuthPage UserAdminPage when AuthPageStates.UserAdminState is None" false
                yield lazyViewOrHMR renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2026, message, ticks)
        | AuthPage DraftAdminPage ->
            match authState.AuthPageStates.DraftAdminState with
            | Some draftAdminState ->
                yield lazyViewOrHMR2 DraftAdmin.Render.render (useDefaultTheme, draftAdminState, authUser, draftsProjection, usersProjection, hasModal)
                    (DraftAdminInput >> APageInput >> PageInput >> dispatch)
            | None ->
                let message = debugMessage "CurrentPage is AuthPage DraftAdminPage when AuthPageStates.DraftAdminState is None" false
                yield lazyViewOrHMR renderSpecialNotificationMessage (useDefaultTheme, SWEEPSTAKE_2026, message, ticks)
        | AuthPage DraftsPage ->
            let draftsState = authState.AuthPageStates.DraftsState
            yield lazyViewOrHMR2 Drafts.Render.render (useDefaultTheme, draftsState, authUser, draftsProjection, usersProjection, squadsProjection)
                (DraftsInput >> APageInput >> PageInput >> dispatch)
        | AuthPage ChatPage ->
            let chatState = authState.AuthPageStates.ChatState
            yield lazyViewOrHMR2 Chat.Render.render (useDefaultTheme, chatState, usersProjection, hasModal, ticks) (ChatInput >> APageInput >> PageInput >> dispatch) ]

let private renderContent state dispatch =
    let renderSpinner () = div divDefault [ divVerticalSpace 10 ; div divCentred [ icon iconSpinnerPulseMedium ] ]
    let renderServiceUnavailable useDefaultTheme =
        let theme = getTheme useDefaultTheme
        columnContent [ [ str "Service unavailable" ] |> para theme paraCentredSmall ; hr theme false ; [ str "Please try again later" ] |> para theme paraCentredSmaller ]
    let hasStaticModal = match state.StaticModal with | Some _ -> true | None -> false
    div divDefault [
        yield lazyViewOrHMR divVerticalSpace 20
        match state.AppState with
        | ReadingPreferences | Connecting _ | AutomaticallySigningIn _ ->
            yield lazyViewOrHMR renderSpinner ()
        | ServiceUnavailable ->
            yield lazyViewOrHMR renderServiceUnavailable state.UseDefaultTheme
        | Unauth unauthState ->
            yield renderUnauth (state.UseDefaultTheme, unauthState, hasStaticModal, state.Ticks) (UnauthInput >> AppInput >> dispatch) // note: renderUnauth has its own lazyViewOrHMR[n] handling
        | Auth authState ->
            yield renderAuth (state.UseDefaultTheme, authState, hasStaticModal, state.Ticks) (AuthInput >> AppInput >> dispatch) // note: renderAuth has its own lazyViewOrHMR[n] handling
        yield lazyViewOrHMR divVerticalSpace 20 ]

let private renderFooter useDefaultTheme =
    let theme = getTheme useDefaultTheme
    footer theme true [
        container (Fluid |> Some) [
            [
                [ str "Written" ] |> link theme (NewWindow "https://github.com/aornota/sweepstake-2026") ; str " in "
                [ str "F#" ] |> link theme (NewWindow "http://fsharp.org/") ; str " using "
                [ str "Fable" ] |> link theme (NewWindow "http://fable.io/") ; str ", "
                [ str "Elmish" ] |> link theme (NewWindow "https://elmish.github.io/") ; str ", and "
                [ str "Fulma" ] |> link theme (NewWindow "https://fulma.github.io/Fulma/") ; str " / "
                [ str "Bulma" ] |> link theme (NewWindow "https://bulma.io/") ; str ". Developed in "
                [ str "Visual Studio Code" ] |> link theme (NewWindow "https://code.visualstudio.com/") ; str " using "
                [ str "Ionide-fsharp" ] |> link theme (NewWindow "http://ionide.io/") ; str ". Best viewed with "
                [ str "Chrome" ] |> link theme (NewWindow "https://www.google.com/chrome/") ; str ". Not especially mobile-friendly." ] |> para theme paraCentredSmallest ] ]

let render state dispatch =
    div divDefault [
        let serverStarted = match state.ConnectionState with | Connected connectionState -> connectionState.ServerStarted |> Some | NotConnected | InitializingConnection _ -> None
        yield lazyViewOrHMR2 renderHeader (state.UseDefaultTheme, state.NavbarBurgerIsActive, serverStarted, headerStatus state.AppState, headerPages state.AppState, state.Ticks) dispatch
        match state.StaticModal with
        | Some ScoringSystem ->
            yield lazyViewOrHMR2 renderStaticModal (state.UseDefaultTheme, "Scoring system", (Markdown SCORING_SYSTEM_MARKDOWN)) dispatch
        | Some DraftAlgorithm ->
            yield lazyViewOrHMR2 renderStaticModal (state.UseDefaultTheme, "Draft algorithm", (Markdown DRAFT_ALGORITHM_MARKDOWN)) dispatch
        | Some Payouts ->
            yield lazyViewOrHMR2 renderStaticModal (state.UseDefaultTheme, "Payouts", (Markdown PAYOUTS_MARKDOWN)) dispatch
        | Some MarkdownSyntax ->
            yield lazyViewOrHMR2 renderMarkdownSyntaxModal state.UseDefaultTheme dispatch
        | None -> ()
        yield lazyViewOrHMR2 renderNotificationMessages (state.UseDefaultTheme, SWEEPSTAKE_2026, state.NotificationMessages, state.Ticks) (DismissNotificationMessage >> dispatch)
        yield renderContent state dispatch // note: renderContent has its own lazyViewOrHMR[n] handling
        yield lazyViewOrHMR renderFooter state.UseDefaultTheme ]
