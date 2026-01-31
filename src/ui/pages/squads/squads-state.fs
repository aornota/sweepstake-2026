module Aornota.Sweepstake2026.Ui.Pages.Squads.State

open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Revision
open Aornota.Sweepstake2026.Common.UnexpectedError
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Ui.Common.Notifications
open Aornota.Sweepstake2026.Ui.Common.ShouldNeverHappen
open Aornota.Sweepstake2026.Ui.Common.Toasts
open Aornota.Sweepstake2026.Ui.Pages.Squads.Common
open Aornota.Sweepstake2026.Ui.Shared

open System

open Elmish

let initialize () : State * Cmd<Input> =
    let pendingPicksState = { PendingPicks = [] ; PendingRvn = None }
    { CurrentGroup = None ; CurrentSquadId = None ; LastSquads = LastSquads () ; PendingPicksState = pendingPicksState ; AddPlayersState = None ; ChangePlayerNameState = None
      ChangePlayerTypeState = None ; WithdrawPlayerState = None ; EliminateSquadState = None ; FreePickState = None }, Cmd.none

let private squadRvn (squadDic:SquadDic) squadId = if squadId |> squadDic.ContainsKey then squadDic.[squadId].Rvn |> Some else None

let private defaultAddPlayersState squadId playerType addPlayerStatus resultRvn = {
    SquadId = squadId
    NewPlayerId = PlayerId.Create ()
    NewPlayerNameText = String.Empty
    NewPlayerNameErrorText = None
    NewPlayerType = playerType
    AddPlayerStatus = addPlayerStatus
    ResultRvn = resultRvn }

let private shouldNeverHappenCmd debugText = debugText |> shouldNeverHappenText |> debugDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg

let private handleAddPlayerCmdResult (result:Result<Rvn * PlayerName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.AddPlayersState with
    | Some addPlayersState ->
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending ->
            match result with
            | Ok (rvn, playerName) ->
                let (PlayerName playerName) = playerName
                let addPlayersState = defaultAddPlayersState addPlayersState.SquadId addPlayersState.NewPlayerType None (rvn |> Some)
                { state with AddPlayersState = addPlayersState |> Some }, sprintf "<strong>%s</strong> has been added" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "AddPlayerCmdResult error -> %A" error) (error |> cmdErrorText)
                let addPlayersState = { addPlayersState with AddPlayerStatus = errorText |> AddPlayerFailed |> Some }
                { state with AddPlayersState = addPlayersState |> Some }, "Unable to add player" |> errorToastCmd
        | Some (AddPlayerFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayerCmdResult when AddPlayersStatus is not AddPlayerPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayerCmdResult when AddPlayersState is None -> %A" result)

let private handleChangePlayerNameCmdResult (result:Result<PlayerName * PlayerName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.ChangePlayerNameState with
    | Some changePlayerNameState ->
        match changePlayerNameState.ChangePlayerNameStatus with
        | Some ChangePlayerNamePending ->
            match result with
            | Ok (previousPlayerName, playerName) ->
                let (PlayerName previousPlayerName), (PlayerName playerName) = previousPlayerName, playerName
                { state with ChangePlayerNameState = None }, sprintf "<strong>%s</strong> is now <strong>%s</strong>" previousPlayerName playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangePlayerNameCmdResult error -> %A" error) (error |> cmdErrorText)
                let changePlayerNameState = { changePlayerNameState with ChangePlayerNameStatus = errorText |> ChangePlayerNameFailed |> Some }
                { state with ChangePlayerNameState = changePlayerNameState |> Some }, "Unable to edit player name" |> errorToastCmd
        | Some (ChangePlayerNameFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameCmdResult when ChangePlayerNameStatus is not ChangePlayerNamePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameCmdResult when ChangePlayerNameState is None -> %A" result)

let private handleChangePlayerTypeCmdResult (result:Result<PlayerName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.ChangePlayerTypeState with
    | Some changePlayerTypeState ->
        match changePlayerTypeState.ChangePlayerTypeStatus with
        | Some ChangePlayerTypePending ->
            match result with
            | Ok playerName ->
                let (PlayerName playerName) = playerName
                { state with ChangePlayerTypeState = None }, sprintf "Position has been changed for <strong>%s</strong>" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "ChangePlayerTypeCmdResult error -> %A" error) (error |> cmdErrorText)
                let changePlayerTypeState = { changePlayerTypeState with ChangePlayerTypeStatus = errorText |> ChangePlayerTypeFailed |> Some }
                { state with ChangePlayerTypeState = changePlayerTypeState |> Some }, "Unable to change player position" |> errorToastCmd
        | Some (ChangePlayerTypeFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeCmdResult when ChangePlayerTypeStatus is not ChangePlayerTypePending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeCmdResult when ChangePlayerTypeState is None -> %A" result)

let private handleWithdrawPlayerCmdResult (result:Result<PlayerName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.WithdrawPlayerState with
    | Some withdrawPlayerState ->
        match withdrawPlayerState.WithdrawPlayerStatus with
        | Some WithdrawPlayerPending ->
            match result with
            | Ok playerName ->
                let (PlayerName playerName) = playerName
                { state with WithdrawPlayerState = None }, sprintf "<strong>%s</strong> has been withdrawn" playerName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "WithdrawPlayerCmdResult error -> %A" error) (error |> cmdErrorText)
                let withdrawPlayerState = { withdrawPlayerState with WithdrawPlayerStatus = errorText |> WithdrawPlayerFailed |> Some }
                { state with WithdrawPlayerState = withdrawPlayerState |> Some }, "Unable to withdraw player" |> errorToastCmd
        | Some (WithdrawPlayerFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerCmdResult when WithdrawPlayerStatus is not WithdrawPlayerPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerCmdResult when WithdrawPlayerState is None -> %A" result)

let private handleEliminateSquadCmdResult (result:Result<SquadName, AuthCmdError<string>>) state : State * Cmd<Input> =
    match state.EliminateSquadState with
    | Some eliminateSquadState ->
        match eliminateSquadState.EliminateSquadStatus with
        | Some EliminateSquadPending ->
            match result with
            | Ok squadName ->
                let (SquadName squadName) = squadName
                { state with EliminateSquadState = None }, sprintf "<strong>%s</strong> has been eliminated" squadName |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "EliminateSquadCmdResult error -> %A" error) (error |> cmdErrorText)
                let eliminateSquadState = { eliminateSquadState with EliminateSquadStatus = errorText |> EliminateSquadFailed |> Some }
                { state with EliminateSquadState = eliminateSquadState |> Some }, "Unable to eliminate team" |> errorToastCmd
        | Some (EliminateSquadFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadCmdResult when EliminateSquadStatus is not EliminateSquadPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadCmdResult when EliminateSquadState is None -> %A" result)

let private handleFreePickCmdResult (result:Result<DraftPick, AuthCmdError<string>>) (squadDic:SquadDic) state : State * Cmd<Input> =
    match state.FreePickState with
    | Some freePickState ->
        match freePickState.FreePickStatus with
        | Some FreePickPending ->
            match result with
            | Ok draftPick ->
                { state with FreePickState = None }, sprintf "<strong>%s</strong> has been picked" (draftPick |> draftPickText squadDic) |> successToastCmd
            | Error error ->
                let errorText = ifDebug (sprintf "FreePickCmdResult error -> %A" error) (error |> cmdErrorText)
                let freePickState = { freePickState with FreePickStatus = errorText |> FreePickFailed |> Some }
                { state with FreePickState = freePickState |> Some }, "Unable to eliminate team" |> errorToastCmd
        | Some (FreePickFailed _) | None ->
            state, shouldNeverHappenCmd (sprintf "Unexpected FreePickCmdResult when FreePickStatus is not FreePickPending -> %A" result)
    | _ ->
        state, shouldNeverHappenCmd (sprintf "Unexpected FreePickCmdResult when FreePickState is None -> %A" result)

let private handleServerSquadsMsg serverSquadsMsg (squadDic:SquadDic) state : State * Cmd<Input> =
    match serverSquadsMsg with
    | AddPlayerCmdResult result ->
        state |> handleAddPlayerCmdResult result
    | ChangePlayerNameCmdResult result ->
        state |> handleChangePlayerNameCmdResult result
    | ChangePlayerTypeCmdResult result ->
        state |> handleChangePlayerTypeCmdResult result
    | WithdrawPlayerCmdResult result ->
        state |> handleWithdrawPlayerCmdResult result
    | EliminateSquadCmdResult result ->
        state |> handleEliminateSquadCmdResult result
    | AddToDraftCmdResult (Ok userDraftPick) ->
        state, sprintf "<strong>%s</strong> has been added to draft" (userDraftPick |> userDraftPickText squadDic) |> successToastCmd
    | AddToDraftCmdResult (Error (userDraftPick, error)) ->
        let pendingPicksState = state.PendingPicksState
        let pendingPicks = pendingPicksState.PendingPicks
        if pendingPicks |> List.exists (fun pendingPick -> pendingPick.UserDraftPick = userDraftPick && pendingPick |> isAdding) then
            let errorText = ifDebug (sprintf "AddToDraftCmdResult error -> %A" error) (error |> cmdErrorText)
            let errorCmd = errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
            let errorToastCmd = sprintf "Unable to add <strong>%s</strong> to draft" (userDraftPick |> userDraftPickText squadDic) |> errorToastCmd
            let pendingPicks = pendingPicks |> List.filter (fun pendingPick -> pendingPick.UserDraftPick <> userDraftPick || pendingPick |> isAdding |> not)
            let pendingPicksState = { pendingPicksState with PendingPicks = pendingPicks }
            { state with PendingPicksState = pendingPicksState }, Cmd.batch [ errorCmd ; errorToastCmd ]
        else state, Cmd.none
    | ServerSquadsMsg.RemoveFromDraftCmdResult (Ok userDraftPick) ->
        state, sprintf "<strong>%s</strong> has been removed from draft" (userDraftPick |> userDraftPickText squadDic) |> successToastCmd
    | ServerSquadsMsg.RemoveFromDraftCmdResult (Error (userDraftPick, error)) ->
        let pendingPicksState = state.PendingPicksState
        let pendingPicks = pendingPicksState.PendingPicks
        if pendingPicks |> List.exists (fun pendingPick -> pendingPick.UserDraftPick = userDraftPick && pendingPick |> isRemoving) then
            let errorText = ifDebug (sprintf "ServerSquadsMsg.RemoveFromDraftCmdResult error -> %A" error) (error |> cmdErrorText)
            let errorCmd = errorText |> dangerDismissableMessage |> AddNotificationMessage |> Cmd.ofMsg
            let errorToastCmd = sprintf "Unable to remove <strong>%s</strong> from draft" (userDraftPick |> userDraftPickText squadDic) |> errorToastCmd
            let pendingPicks = pendingPicks |> List.filter (fun pendingPick -> pendingPick.UserDraftPick <> userDraftPick || pendingPick |> isRemoving |> not)
            let pendingPicksState = { pendingPicksState with PendingPicks = pendingPicks }
            { state with PendingPicksState = pendingPicksState }, Cmd.batch [ errorCmd ; errorToastCmd ]
        else state, Cmd.none
    | FreePickCmdResult result ->
        state |> handleFreePickCmdResult result squadDic

let private handleAddPlayersInput addPlayersInput (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match addPlayersInput, state.AddPlayersState with
    | NewPlayerNameTextChanged newPlayerNameText, Some addPlayersState ->
        let squadId = addPlayersState.SquadId
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let playerNames = match squad with | Some squad -> squad.PlayerDic |> playerNames | None -> []
        let newPlayerNameErrorText = validatePlayerName playerNames (PlayerName newPlayerNameText)
        let addPlayersState = { addPlayersState with NewPlayerNameText = newPlayerNameText ; NewPlayerNameErrorText = newPlayerNameErrorText }
        { state with AddPlayersState = addPlayersState |> Some }, Cmd.none, true
    | NewPlayerTypeChanged newPlayerType, Some addPlayersState ->
        let addPlayersState = { addPlayersState with NewPlayerType = newPlayerType }
        { state with AddPlayersState = addPlayersState |> Some }, Cmd.none, true
    | AddPlayer, Some addPlayersState -> // note: assume no need to validate NewPlayerNameText (i.e. because Squads.Render.renderAddPlayersModal will ensure that AddPlayer can only be dispatched when valid)
        let addPlayersState = { addPlayersState with AddPlayerStatus = AddPlayerPending |> Some }
        let squadId, resultRvn = addPlayersState.SquadId, addPlayersState.ResultRvn
        let currentRvn =
            match squadId |> squadRvn squadDic with
            | Some (Rvn squadRvn) -> match resultRvn with | Some (Rvn resultRvn) when resultRvn > squadRvn -> Rvn resultRvn | Some _ | None -> Rvn squadRvn
            | None -> match resultRvn with | Some rvn -> rvn | None -> initialRvn
        let addPlayerCmdParams = squadId, currentRvn, addPlayersState.NewPlayerId, PlayerName (addPlayersState.NewPlayerNameText.Trim ()), addPlayersState.NewPlayerType
        let cmd = addPlayerCmdParams |> AddPlayerCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with AddPlayersState = addPlayersState |> Some }, cmd, true
    | CancelAddPlayers, Some addPlayersState ->
        match addPlayersState.AddPlayerStatus with
        | Some AddPlayerPending ->
            state, shouldNeverHappenCmd "Unexpected CancelAddPlayers when AddPlayerPending", false
        | Some (AddPlayerFailed _) | None ->
            { state with AddPlayersState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected AddPlayersInput when AddPlayersState is None -> %A" addPlayersInput), false

let private handleChangePlayerNameInput changePlayerNameInput (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match changePlayerNameInput, state.ChangePlayerNameState with
    | PlayerNameTextChanged playerNameText, Some changePlayerNameState ->
        let squadId = changePlayerNameState.SquadId
        let squad = if squadId |> squadDic.ContainsKey then squadDic.[squadId] |> Some else None
        let playerNames = match squad with | Some squad -> squad.PlayerDic |> playerNames | None -> []
        let playerNameErrorText = validatePlayerName playerNames (PlayerName playerNameText)
        let changePlayerNameState = { changePlayerNameState with PlayerNameText = playerNameText ; PlayerNameErrorText = playerNameErrorText }
        { state with ChangePlayerNameState = changePlayerNameState |> Some }, Cmd.none, true
    | ChangePlayerName, Some changePlayerNameState -> // note: assume no need to validate PlayerNameText (i.e. because Squads.Render.renderChangePlayerNameModal will ensure that ChangePlayerName can only be dispatched when valid)
        let changePlayerNameState = { changePlayerNameState with ChangePlayerNameStatus = ChangePlayerNamePending |> Some }
        let squadId = changePlayerNameState.SquadId
        let currentRvn = match squadId |> squadRvn squadDic with | Some squadRvn -> squadRvn | None -> initialRvn
        let changePlayerNameCmdParams = squadId, currentRvn, changePlayerNameState.PlayerId, PlayerName (changePlayerNameState.PlayerNameText.Trim ())
        let cmd = changePlayerNameCmdParams |> ChangePlayerNameCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with ChangePlayerNameState = changePlayerNameState |> Some }, cmd, true
    | CancelChangePlayerName, Some changePlayerNameState ->
        match changePlayerNameState.ChangePlayerNameStatus with
        | Some ChangePlayerNamePending ->
            state, shouldNeverHappenCmd "Unexpected CancelChangePlayerName when ChangePlayerNamePending", false
        | Some (ChangePlayerNameFailed _) | None ->
            { state with ChangePlayerNameState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerNameInput when ChangePlayerNameState is None -> %A" changePlayerNameInput), false

let private handleChangePlayerTypeInput changePlayerTypeInput (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match changePlayerTypeInput, state.ChangePlayerTypeState with
    | PlayerTypeChanged playerType, Some changePlayerTypeState ->
        let changePlayerTypeState = { changePlayerTypeState with PlayerType = playerType |> Some }
        { state with ChangePlayerTypeState = changePlayerTypeState |> Some }, Cmd.none, true
    | ChangePlayerType, Some changePlayerTypeState -> // note: assume no need to validate PlayerType (i.e. because Squads.Render.renderChangePlayerTypeModal will ensure that ChangePlayerType can only be dispatched when valid)
        match changePlayerTypeState.PlayerType with
        | Some playerType ->
            let changePlayerTypeState = { changePlayerTypeState with ChangePlayerTypeStatus = ChangePlayerTypePending |> Some }
            let squadId = changePlayerTypeState.SquadId
            let currentRvn = match squadId |> squadRvn squadDic with | Some squadRvn -> squadRvn | None -> initialRvn
            let changePlayerTypeCmdParams = squadId, currentRvn, changePlayerTypeState.PlayerId, playerType
            let cmd = changePlayerTypeCmdParams |> ChangePlayerTypeCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
            { state with ChangePlayerTypeState = changePlayerTypeState |> Some }, cmd, true
        | None -> // note: should never happen
            state, Cmd.none, false
    | CancelChangePlayerType, Some changePlayerTypeState ->
        match changePlayerTypeState.ChangePlayerTypeStatus with
        | Some ChangePlayerTypePending ->
            state, shouldNeverHappenCmd "Unexpected CancelChangePlayerType when ChangePlayerTypePending", false
        | Some (ChangePlayerTypeFailed _) | None ->
            { state with ChangePlayerTypeState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected ChangePlayerTypeInput when ChangePlayerTypeState is None -> %A" changePlayerTypeInput), false

let private handleWithdrawPlayerInput withdrawPlayer (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match withdrawPlayer, state.WithdrawPlayerState with
    | ConfirmWithdrawPlayer, Some withdrawPlayerState ->
        let withdrawPlayerState = { withdrawPlayerState with WithdrawPlayerStatus = WithdrawPlayerPending |> Some }
        let squadId = withdrawPlayerState.SquadId
        let currentRvn = match squadId |> squadRvn squadDic with | Some squadRvn -> squadRvn | None -> initialRvn
        let cmd = (squadId, currentRvn, withdrawPlayerState.PlayerId) |> WithdrawPlayerCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with WithdrawPlayerState = withdrawPlayerState |> Some }, cmd, true
    | CancelWithdrawPlayer, Some withdrawPlayerState ->
        match withdrawPlayerState.WithdrawPlayerStatus with
        | Some WithdrawPlayerPending ->
            state, shouldNeverHappenCmd "Unexpected CancelWithdrawPlayer when WithdrawPlayerPending", false
        | Some (WithdrawPlayerFailed _) | None ->
            { state with WithdrawPlayerState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected WithdrawPlayerInput when WithdrawPlayerState is None -> %A" withdrawPlayer), false

let private handleEliminateSquadInput eliminateSquadInput (squadDic:SquadDic) state : State * Cmd<Input> * bool =
    match eliminateSquadInput, state.EliminateSquadState with
    | ConfirmEliminateSquad, Some eliminateSquadState ->
        let eliminateSquadState = { eliminateSquadState with EliminateSquadStatus = EliminateSquadPending |> Some }
        let squadId = eliminateSquadState.SquadId
        let currentRvn = match squadId |> squadRvn squadDic with | Some squadRvn -> squadRvn | None -> initialRvn
        let cmd = (squadId, currentRvn) |> EliminateSquadCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with EliminateSquadState = eliminateSquadState |> Some }, cmd, true
    | CancelEliminateSquad, Some eliminateSquadState ->
        match eliminateSquadState.EliminateSquadStatus with
        | Some EliminateSquadPending ->
            state, shouldNeverHappenCmd "Unexpected CancelEliminateSquad when EliminateSquadPending", false
        | Some (EliminateSquadFailed _) | None ->
            { state with EliminateSquadState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected EliminateSquadInput when EliminateSquadState is None -> %A" eliminateSquadInput), false

let private handleFreePickInput freePickInput (draftDic:DraftDic) state : State * Cmd<Input> * bool =
    match freePickInput, state.FreePickState with
    | ConfirmFreePick, Some freePickState ->
        let freePickState = { freePickState with FreePickStatus = FreePickPending |> Some }
        let draftId, draftPick = freePickState.DraftId, freePickState.DraftPick
        let currentRvn = if draftId |> draftDic.ContainsKey then draftDic.[draftId].Rvn else initialRvn
        let cmd = (draftId, currentRvn, draftPick) |> FreePickCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
        { state with FreePickState = freePickState |> Some }, cmd, true
    | CancelFreePick, Some freePickState ->
        match freePickState.FreePickStatus with
        | Some FreePickPending ->
            state, shouldNeverHappenCmd "Unexpected CancelFreePick when FreePickPending", false
        | Some (FreePickFailed _) | None ->
            { state with FreePickState = None }, Cmd.none, false
    | _, None ->
        state, shouldNeverHappenCmd (sprintf "Unexpected FreePickInput when FreePickState is None -> %A" freePickInput), false

let private updateLast (squadDic:SquadDic) state =
    match state.CurrentSquadId with
    | Some squadId ->
        if squadId |> squadDic.ContainsKey then
            let squad = squadDic.[squadId]
            let group = squad.Group
            let lastSquads = state.LastSquads
            if group |> lastSquads.ContainsKey then lastSquads.[group] <- squadId else (group, squadId) |> lastSquads.Add
        else () // note: should never happen
    | None -> ()

let transition input (authUser:AuthUser option) (squadsProjection:Projection<_ * SquadDic>) (draftDic:DraftDic option) (currentUserDraftDto:CurrentUserDraftDto option) state =
    let draftDic = match draftDic with | Some draftDic -> draftDic | None -> DraftDic ()
    let state, cmd, isUserNonApiActivity =
        match input, squadsProjection with
        | AddNotificationMessage _, _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | SendUiAuthMsg _, Ready _ -> // note: expected to be handled by Program.State.transition
            state, Cmd.none, false
        | ReceiveServerSquadsMsg serverSquadsMsg, Ready (_, squadDic) ->
            let state, cmd = state |> handleServerSquadsMsg serverSquadsMsg squadDic
            state, cmd, false
        | ShowGroup group, Ready (_, squadDic) ->
            state |> updateLast squadDic
            let squad =
                match state.CurrentSquadId with
                | Some currentSquadId -> if currentSquadId |> squadDic.ContainsKey then squadDic.[currentSquadId] |> Some else None
                | None -> None
            let state =
                match squad with
                | Some squad when squad.Group = group -> state
                | _ ->
                    let lastSquads = state.LastSquads
                    let currentGroup, currentSquadId = if group |> lastSquads.ContainsKey then None, lastSquads.[group] |> Some else group |> Some, None
                    { state with CurrentGroup = currentGroup ; CurrentSquadId = currentSquadId }
            state, Cmd.none, true
        | ShowSquad squadId, Ready (_, squadDic) -> // note: no need to check for unknown squadId (should never happen)
            state |> updateLast squadDic
            { state with CurrentSquadId = squadId |> Some }, Cmd.none, true
        | AddToDraft (draftId, userDraftPick), Ready _ ->
            match authUser with
            | Some _ ->
                let isPicked =
                    match currentUserDraftDto with
                    | Some currentUserDraftDto -> currentUserDraftDto.UserDraftPickDtos |> List.exists (fun userDraftPickDto -> userDraftPickDto.UserDraftPick = userDraftPick)
                    | None -> false
                if isPicked |> not then
                    let pendingPicksState = state.PendingPicksState
                    let pendingPicks = pendingPicksState.PendingPicks
                    if pendingPicks |> List.exists (fun pendingPick -> pendingPick.UserDraftPick = userDraftPick) |> not then
                        let pendingPick = { UserDraftPick = userDraftPick ; PendingPickStatus = Adding }
                        let currentRvn =
                            match currentUserDraftDto with
                            | Some currentUserDraftDto ->
                                let (Rvn currentRvn) = currentUserDraftDto.Rvn
                                match pendingPicksState.PendingRvn with | Some (Rvn rvn) when rvn > currentRvn -> Rvn rvn | Some _ | None -> Rvn currentRvn
                            | None -> match pendingPicksState.PendingRvn with | Some rvn -> rvn | None -> initialRvn
                        let cmd = (draftId, currentRvn, userDraftPick) |> AddToDraftCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
                        let pendingPicksState = { pendingPicksState with PendingPicks = pendingPick :: pendingPicks ; PendingRvn = currentRvn |> incrementRvn |> Some }
                        { state with PendingPicksState = pendingPicksState }, cmd, true
                    else state, UNEXPECTED_ERROR |> errorToastCmd, true
                else state, UNEXPECTED_ERROR |> errorToastCmd, true
            | None -> // note: should never happen
                state, Cmd.none, false
        | RemoveFromDraft (draftId, userDraftPick), Ready _ ->
            match authUser with
            | Some _ ->
                let isPicked =
                    match currentUserDraftDto with
                    | Some currentUserDraftDto -> currentUserDraftDto.UserDraftPickDtos |> List.exists (fun userDraftPickDto -> userDraftPickDto.UserDraftPick = userDraftPick)
                    | None -> false
                if isPicked then
                    let pendingPicksState = state.PendingPicksState
                    let pendingPicks = pendingPicksState.PendingPicks
                    if pendingPicks |> List.exists (fun pendingPick -> pendingPick.UserDraftPick = userDraftPick) |> not then
                        let pendingPick = { UserDraftPick = userDraftPick ; PendingPickStatus = Removing }
                        let currentRvn =
                            match currentUserDraftDto with
                            | Some currentUserDraftDto ->
                                let (Rvn currentRvn) = currentUserDraftDto.Rvn
                                match pendingPicksState.PendingRvn with | Some (Rvn rvn) when rvn > currentRvn -> Rvn rvn | Some _ | None -> Rvn currentRvn
                            | None -> match pendingPicksState.PendingRvn with | Some rvn -> rvn | None -> initialRvn
                        let cmd = (draftId, currentRvn, userDraftPick) |> UiAuthSquadsMsg.RemoveFromDraftCmd |> UiAuthSquadsMsg |> SendUiAuthMsg |> Cmd.ofMsg
                        let pendingPicksState = { pendingPicksState with PendingPicks = pendingPick :: pendingPicks ; PendingRvn = currentRvn |> incrementRvn |> Some }
                        { state with PendingPicksState = pendingPicksState }, cmd, true
                    else state, UNEXPECTED_ERROR |> errorToastCmd, true
                else state, UNEXPECTED_ERROR |> errorToastCmd, true
            | None -> // note: should never happen
                state, Cmd.none, false
        | ShowAddPlayersModal squadId, Ready _ -> // note: no need to check for unknown squadId (should never happen)
            let addPlayersState = defaultAddPlayersState squadId Goalkeeper None None
            { state with AddPlayersState = addPlayersState |> Some }, Cmd.none, true
        | AddPlayersInput addPlayersInput, Ready (_, squadDic) ->
            state |> handleAddPlayersInput addPlayersInput squadDic
        | ShowChangePlayerNameModal (squadId, playerId), Ready _ -> // note: no need to check for unknown squadId / playerId (should never happen)
            let changePlayerNameState = { SquadId = squadId ; PlayerId = playerId ; PlayerNameText = String.Empty ; PlayerNameErrorText = None ; ChangePlayerNameStatus = None }
            { state with ChangePlayerNameState = changePlayerNameState |> Some }, Cmd.none, true
        | ChangePlayerNameInput changePlayerNameInput, Ready (_, squadDic) ->
            state |> handleChangePlayerNameInput changePlayerNameInput squadDic
        | ShowChangePlayerTypeModal (squadId, playerId), Ready _ -> // note: no need to check for unknown squadId / playerId (should never happen)
            let changePlayerTypeState = { SquadId = squadId ; PlayerId = playerId ; PlayerType = None ; ChangePlayerTypeStatus = None }
            { state with ChangePlayerTypeState = changePlayerTypeState |> Some }, Cmd.none, true
        | ChangePlayerTypeInput changePlayerTypeInput, Ready (_, squadDic) ->
            state |> handleChangePlayerTypeInput changePlayerTypeInput squadDic
        | ShowWithdrawPlayerModal (squadId, playerId), Ready _ -> // note: no need to check for unknown squadId / playerId (should never happen)
            let withdrawPlayerState = { SquadId = squadId ; PlayerId = playerId ; WithdrawPlayerStatus = None }
            { state with WithdrawPlayerState = withdrawPlayerState |> Some }, Cmd.none, true
        | WithdrawPlayerInput withdrawPlayerInput, Ready (_, squadDic) ->
            state |> handleWithdrawPlayerInput withdrawPlayerInput squadDic
        | ShowEliminateSquadModal squadId, Ready _ -> // note: no need to check for unknown squadId (should never happen)
            let eliminateSquadState = { SquadId = squadId ; EliminateSquadStatus = None }
            { state with EliminateSquadState = eliminateSquadState |> Some }, Cmd.none, true
        | EliminateSquadInput eliminateSquadInput, Ready (_, squadDic) ->
            state |> handleEliminateSquadInput eliminateSquadInput squadDic
        | ShowFreePickModal (draftId, draftPick), Ready _ -> // note: no need to check for unknown draftId (should never happen)
            let freePickState = { DraftId = draftId ; DraftPick = draftPick ; FreePickStatus = None }
            { state with FreePickState = freePickState |> Some }, Cmd.none, true
        | FreePickInput freePickInput, Ready _ ->
            state |> handleFreePickInput freePickInput draftDic
        | _, _ ->
            state, shouldNeverHappenCmd (sprintf "Unexpected Input when %A -> %A" squadsProjection input), false
    state, cmd, isUserNonApiActivity
