module Aornota.Sweepstake2026.Ui.Pages.DraftAdmin.State

open Aornota.Sweepstake2026.Common.Delta
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Common.Toasts
open Aornota.Sweepstake2026.Ui.Pages.DraftAdmin.Common
open Aornota.Sweepstake2026.Ui.Shared

open Elmish

let initialize (authUser:AuthUser) =
    let userAdminSummaryProjectionCmd =
        match authUser.Permissions.DraftAdminPermissions with | Some _ -> InitializeUserDraftSummaryProjectionQry |> UiAuthDraftAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg | None -> Cmd.none
    { UserDraftSummaryProjection = Pending ; ProcessDraftState = None }, userAdminSummaryProjectionCmd

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private userDraftSummaryDic (userDraftSummaryDtos:UserDraftSummaryDto list) =
    let userDraftSummaryDic = UserDraftSummaryDic ()
    userDraftSummaryDtos |> List.iter (fun userDraftSummaryDto ->
        if userDraftSummaryDto.UserDraftKey |> userDraftSummaryDic.ContainsKey |> not then // note: silently ignore duplicate keys (should never happer)
            (userDraftSummaryDto.UserDraftKey, userDraftSummaryDto) |> userDraftSummaryDic.Add)
    userDraftSummaryDic

let private applyUserDraftSummariesDelta currentRvn deltaRvn (delta:Delta<UserDraftKey, UserDraftSummaryDto>) (userDraftSummaryDic:UserDraftSummaryDic) =
    let userDraftSummaryDic = UserDraftSummaryDic userDraftSummaryDic // note: copy to ensure that passed-in dictionary *not* modified if error
    if deltaRvn |> validateNextRvn (currentRvn |> Some) then () |> Ok else (currentRvn, deltaRvn) |> MissedDelta |> Error
    |> Result.bind (fun _ ->
        let alreadyExist = delta.Added |> List.choose (fun (userDraftKey, userDraftSummaryDto) -> if userDraftKey |> userDraftSummaryDic.ContainsKey then (userDraftKey, userDraftSummaryDto) |> Some else None)
        if alreadyExist.Length = 0 then delta.Added |> List.iter (fun (userDraftKey, userDraftSummaryDto) -> (userDraftKey, userDraftSummaryDto) |> userDraftSummaryDic.Add) |> Ok
        else alreadyExist |> AddedAlreadyExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Changed |> List.choose (fun (userDraftKey, userDraftSummaryDto) -> if userDraftKey |> userDraftSummaryDic.ContainsKey |> not then (userDraftKey, userDraftSummaryDto) |> Some else None)
        if doNotExist.Length = 0 then delta.Changed |> List.iter (fun (userDraftKey, userDraftSummaryDto) -> userDraftSummaryDic.[userDraftKey] <- userDraftSummaryDto) |> Ok
        else doNotExist |> ChangedDoNotExist |> Error)
    |> Result.bind (fun _ ->
        let doNotExist = delta.Removed |> List.choose (fun userDraftKey -> if userDraftKey |> userDraftSummaryDic.ContainsKey |> not then userDraftKey |> Some else None)
        if doNotExist.Length = 0 then delta.Removed |> List.iter (userDraftSummaryDic.Remove >> ignore) |> Ok
        else doNotExist |> RemovedDoNotExist |> Error)
    |> Result.bind (fun _ -> userDraftSummaryDic |> Ok)

let private handleProcessDraftCmdResult (result:Result<unit, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.ProcessDraftState with
    | Some processDraftState ->
        match processDraftState.ProcessDraftStatus with
        | Some ProcessDraftPending ->
            match result with
            | Ok _ ->
                { state with ProcessDraftState = None }, "Draft has been processed" |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ProcessDraftCmdResult error -> %A" error) (error |> cmdErrorText)
                let processDraftState = { processDraftState with ProcessDraftStatus = errorText |> ProcessDraftFailed |> Some }
                { state with ProcessDraftState = processDraftState |> Some }, "Unable to process draft" |> errorToastCmd
        | Some (ProcessDraftFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ProcessDraftCmdResult when ProcessDraftStatus is not ProcessDraftPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ProcessDraftCmdResult when ProcessDraftState is None -> %A" result)

let private handleServerDraftAdminMsg serverDraftAdminMsg authUser state : State * Cmd<Input> =
    match serverDraftAdminMsg, state.UserDraftSummaryProjection with
    | InitializeUserDraftSummaryProjectionQryResult (Ok userDraftSummaryDtos), Pending ->
        { state with UserDraftSummaryProjection = (initialRvn, userDraftSummaryDtos |> userDraftSummaryDic) |> Ready }, Cmd.none
    | InitializeUserDraftSummaryProjectionQryResult (Error error), Pending ->
        { state with UserDraftSummaryProjection = Failed }, error |> qryErrorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
    | ProcessDraftCmdResult result, _ ->
        state |> handleProcessDraftCmdResult result
    | UserDraftSummaryProjectionMsg (UserDraftSummariesDeltaMsg (deltaRvn, userDraftSummaryDtoDelta)), Ready (rvn, userDraftSummaryDic) ->
        match userDraftSummaryDic |> applyUserDraftSummariesDelta rvn deltaRvn userDraftSummaryDtoDelta with
        | Ok userDraftSummaryDic ->
            { state with UserDraftSummaryProjection = (deltaRvn, userDraftSummaryDic) |> Ready }, Cmd.none
        | Error error ->
            let shouldNeverHappenCmd = shouldNeverHappenCmd (sprintf "Unable to apply %A to %A -> %A" userDraftSummaryDtoDelta userDraftSummaryDic error)
            let state, cmd = initialize authUser
            state, Cmd.batch [ cmd ; shouldNeverHappenCmd ; UNEXPECTED_ERROR |> errorToastCmd ]
    | UserDraftSummaryProjectionMsg _, _ -> // note: silently ignore UserDraftSummaryProjectionMsg if not Ready
        state, Cmd.none
    | _, _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ServerDraftAdminMsg when %A -> %A" state.UserDraftSummaryProjection serverDraftAdminMsg)

let private handleProcessDraftInput processDraftInput (draftDic:DraftDic) state : State * Cmd<Input> * bool =
    match processDraftInput, state.ProcessDraftState with
    | ConfirmProcessDraft, Some processDraftState ->
        let processDraftState = { processDraftState with ProcessDraftStatus = ProcessDraftPending |> Some }
        let draftId = processDraftState.DraftId
        let currentRvn = if draftId |> draftDic.ContainsKey then draftDic.[draftId].Rvn else initialRvn
        let cmd = (draftId, currentRvn) |> ProcessDraftCmd |> UiAuthDraftAdminMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ProcessDraftState = processDraftState |> Some }, cmd, true
    | CancelProcessDraft, Some processDraftState ->
        match processDraftState.ProcessDraftStatus with
        | Some ProcessDraftPending ->
            state, shouldNeverHappenCmd "Unexpected CancelProcessDraft when ProcessDraftPending", false
        | Some (ProcessDraftFailed _) | None ->
            { state with ProcessDraftState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ProcessDraftInput when ProcessDraftState is None -> %A" processDraftInput), false

let transition input authUser (draftsProjection:Projection<_ * DraftDic * _>) state =
    let state, cmd, isUserNonApiActivity =
        match input, draftsProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerDraftAdminMsg serverDraftAdminMsg, Ready _ ->
            let state, cmd = state |> handleServerDraftAdminMsg serverDraftAdminMsg authUser
            state, cmd, false
        | ShowProcessDraftModal draftId, Ready _ -> // note: no need to check for unknown draftId (should never happen)
            let processDraftState = { DraftId = draftId ; ProcessDraftStatus = None }
            { state with ProcessDraftState = processDraftState |> Some }, Cmd.none, true
        | ProcessDraftInput processDraftInput, Ready (_, draftDic, _) ->
            state |> handleProcessDraftInput processDraftInput draftDic
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" draftsProjection input), false
    state, cmd, isUserNonApiActivity
