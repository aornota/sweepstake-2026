module Aornota.Sweepstake2026.Ui.Pages.DraftAdmin.Render

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Ui.Common.LazyViewOrHMR
open Aornota.Sweepstake2026.Ui.Common.TimestampHelper
open Aornota.Sweepstake2026.Ui.Pages.DraftAdmin.Common
open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common
open Aornota.Sweepstake2026.Ui.Shared
open Aornota.Sweepstake2026.Ui.Theme.Common
open Aornota.Sweepstake2026.Ui.Theme.Render.Bulma
open Aornota.Sweepstake2026.Ui.Theme.Shared

module RctH = Fable.React.Helpers

let private renderProcessDraftModal (useDefaultTheme, draftDic:DraftDic, processDraftState:ProcessDraftState) dispatch =
    let theme = getTheme useDefaultTheme
    let draftId = processDraftState.DraftId
    let draft = if draftId |> draftDic.ContainsKey then draftDic.[draftId] |> Some else None
    let titleText =
        match draft with
        | Some draft -> sprintf "Process %s" (draft.DraftOrdinal |> draftTextLower)
        | None -> "Process draft" // note: should never happen
    let title = [ [ strong titleText ] |> para theme paraCentredSmall ]
    let confirmInteraction, onDismiss =
        let confirm = (fun _ -> ConfirmProcessDraft |> dispatch)
        let cancel = (fun _ -> CancelProcessDraft |> dispatch)
        match processDraftState.ProcessDraftStatus with
        | Some ProcessDraftPending -> Loading, None
        | Some (ProcessDraftFailed _) | None -> Clickable (confirm, None), cancel |> Some
    let errorText = match processDraftState.ProcessDraftStatus with | Some (ProcessDraftFailed errorText) -> errorText |> Some | Some ProcessDraftPending | None -> None
    let warning = [
        [ strong "Are you sure you want to process this draft?" ] |> para theme paraCentredSmaller
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
        yield field theme { fieldDefault with Grouped = Centred |> Some } [
            [ str "Process draft" ] |> button theme { buttonLinkSmall with Interaction = confirmInteraction } ] ]
    cardModal theme (Some(title, onDismiss)) body

let private renderDrafts (useDefaultTheme, draftDic:DraftDic, authUser) dispatch =
    let theme = getTheme useDefaultTheme
    let canProcessDraft = match authUser.Permissions.DraftAdminPermissions with | Some draftAdminPermissions -> draftAdminPermissions.ProcessDraftPermission | None -> false
    let processDraft (draftId, draft:Draft) =
        let isPendingProcessing = match draft.DraftStatus with | PendingProcessing false -> true | _ -> false
        if canProcessDraft && isPendingProcessing then
            let onClick = (fun _ -> draftId |> ShowProcessDraftModal |> dispatch)
            [ [ str "Process" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] |> link theme (Internal onClick) |> Some
        else None
    let draftRow (draftId, draft:Draft) =
        let (DraftOrdinal draftOrdinal) = draft.DraftOrdinal
        let draftOrdinal = [ str (sprintf "#%i" draftOrdinal) ] |> para theme paraCentredSmallest
        let draftOrdinal, status, starts, ends =
            match draft.DraftStatus with
            | PendingOpen (starts, ends) ->
                let status = [ em "Pending" ] |> para theme paraDefaultSmallest
                let starts, ends = starts.LocalDateTime |> dateAndTimeText, ends.LocalDateTime |> dateAndTimeText
                let starts = [ str starts ] |> para theme paraDefaultSmallest
                let ends = [ str ends ] |> para theme paraDefaultSmallest
                draftOrdinal |> Some, status |> Some, starts |> Some, ends |> Some
            | Opened ends ->
                let status = [ str "Open" ] |> para theme paraDefaultSmallest
                let ends = ends.LocalDateTime |> dateAndTimeText
                let ends = [ str ends ] |> para theme paraDefaultSmallest
                draftOrdinal |> Some, status |> Some, None, ends |> Some
            | PendingProcessing false ->
                let status = [ em "Pending processing" ] |> para theme paraDefaultSmallest
                draftOrdinal |> Some, status |> Some, None, None
            | PendingProcessing true ->
                let status = [ strong "Processing in progress" ] |> para theme paraDefaultSmallest
                draftOrdinal |> Some, status |> Some, None, None
            | Processed ->
                let status = [ str "Processed" ] |> para theme paraDefaultSmallest
                draftOrdinal |> Some, status |> Some, None, None
            | FreeSelection ->
                let status = [ str "No more drafts" ] |> para theme paraDefaultSmallest
                None, status |> Some, None, None
            | _ -> None, None, None, None
        tr false [
            td [ RctH.ofOption draftOrdinal ]
            td [ RctH.ofOption status ]
            td [ RctH.ofOption starts ]
            td [ RctH.ofOption ends ]
            td [ RctH.ofOption ((draftId, draft) |> processDraft) ] ]
    let drafts = draftDic |> List.ofSeq |> List.map (fun (KeyValue (draftId, draft)) -> draftId, draft) |> List.sortBy (fun (_, draft) -> draft.DraftOrdinal)
    let draftRows = drafts |> List.map draftRow
    div divCentred [
        if draftDic.Count > 0 then
            yield table theme false { tableDefault with IsNarrow = true ; IsFullWidth = true } [
                thead [
                    tr false [
                        th [ [ strong "Draft" ] |> para theme paraCentredSmallest ]
                        th [ [ strong "Status" ] |> para theme paraDefaultSmallest ]
                        th [ [ strong "Starts" ] |> para theme paraDefaultSmallest ]
                        th [ [ strong "Ends" ] |> para theme paraDefaultSmallest ]
                        th [] ] ]
                tbody [ yield! draftRows ] ]
        else yield [ str "There are no drafts" ] |> para theme paraCentredSmaller ] // note: should never happen

let private activeDraftSummary useDefaultTheme (userDraftProjection:Projection<_ * UserDraftSummaryDic>) (draftDic:DraftDic) (userDic:UserDic) =
    let theme = getTheme useDefaultTheme
    match userDraftProjection with
    | Pending ->
        [ div divCentred [ icon iconSpinnerPulseMedium ] ]
    | Failed -> // note: should never happen
        [ [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold } ]
    | Ready (_, userDraftSummaryDic) ->
        match draftDic |> activeDraft with
        | Some (draftId, draft) ->
            let userDraftSummaryRow userDraftSummaryDto =
                let (UserName userName) = fst userDraftSummaryDto.UserDraftKey |> userName userDic
                tr false [
                    td [ [ str userName ] |> para theme paraDefaultSmallest ]
                    td [ [ str (sprintf "%i" userDraftSummaryDto.PickCount) ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ]
            let userDraftSummaries =
                userDraftSummaryDic |> List.ofSeq |> List.filter (fun (KeyValue (userDraftKey, _)) -> snd userDraftKey = draftId)
                |> List.sortBy (fun (KeyValue (userDraftKey, _)) -> fst userDraftKey |> userName userDic)
                |> List.map (fun (KeyValue (_, userDraftSummaryDto)) -> userDraftSummaryDto)
            let userDraftSummaryRows = userDraftSummaries |> List.map userDraftSummaryRow
            [
                yield [ strong (sprintf "Summary for %s" (draft.DraftOrdinal |> draftTextLower)) ] |> para theme paraCentredSmaller
                yield hr theme true
                if userDraftSummaries.Length > 0 then
                    yield div divCentred [
                        table theme false { tableDefault with IsNarrow = true } [
                            thead [
                                tr false [
                                    th [ [ strong "User" ] |> para theme paraDefaultSmallest ]
                                    th [ [ strong "Selections" ] |> para theme { paraDefaultSmallest with ParaAlignment = RightAligned } ] ] ]
                            tbody [ yield! userDraftSummaryRows ] ] ]
                else yield [ str "There are no user draft picks" ] |> para theme paraCentredSmallest
            ]
        | None -> []

let render (useDefaultTheme, state, authUser:AuthUser, draftProjection:Projection<_ * DraftDic * _>, usersProjection:Projection<_ * UserDic>, hasModal) dispatch =
    let theme = getTheme useDefaultTheme
    columnContent [
        yield [ strong "Draft administration" ] |> para theme paraCentredSmall
        yield hr theme false
        match usersProjection, draftProjection with
        | Pending, _ | _, Pending ->
            yield div divCentred [ icon iconSpinnerPulseLarge ]
        | Failed, _ | _, Failed -> // note: should never happen
            yield [ str "This functionality is not currently available" ] |> para theme { paraCentredSmallest with ParaColour = SemanticPara Danger ; Weight = Bold }
        | Ready (_, userDic), Ready (_, draftDic, _) ->
            match hasModal, state.ProcessDraftState with
            | false, Some processDraftState ->
                yield div divDefault [ lazyViewOrHMR2 renderProcessDraftModal (useDefaultTheme, draftDic, processDraftState) (ProcessDraftInput >> dispatch) ]
            | _ -> ()
            yield lazyViewOrHMR2 renderDrafts (useDefaultTheme, draftDic, authUser) dispatch
            match activeDraftSummary useDefaultTheme state.UserDraftSummaryProjection draftDic userDic with
            | h :: t ->
                yield br
                yield columnContent (h :: t)
            | [] -> () ]
