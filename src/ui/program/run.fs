module Aornota.Sweepstake2026.Ui.Program.Run

open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Literals
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Ui.Common.JsonConverter
open Aornota.Sweepstake2026.Ui.Common.Marked
open Aornota.Sweepstake2026.Ui.Program.Common
open Aornota.Sweepstake2026.Ui.Program.Render
open Aornota.Sweepstake2026.Ui.Program.State

open System

open Browser
open Browser.Types

open Elmish
#if DEBUG
open Elmish.Debug
#endif
open Elmish.React
#if HMR
open Elmish.HMR // note: needs to be last open Elmish.Xyz (see https://elmish.github.io/hmr/)
#endif

open Fable.Core
open Fable.Core.JsInterop

let [<Literal>] private SECONDS_PER_TICK = 1<second> // note: "ignored" if less than 1<second>

let private secondsPerTick = max SECONDS_PER_TICK 1<second>

let private ticker () =
    let millisecondsPerTick = int (secondsToMilliseconds (float secondsPerTick * 1.<second>))
    let start dispatch =
        let intervalToken =
            JS.setInterval
                (fun _ -> dispatch Tick)
                millisecondsPerTick
        { new IDisposable with member _.Dispose() = JS.clearInterval intervalToken }
    start

let wsSub initialize =
    let start dispatch =
        if initialize then
            let receiveServerMsg (wsMessage:MessageEvent) =
                try // note: expect wsMessage.data to be deserializable to ServerMsg
                    let serverMsg = wsMessage.data |> unbox |> fromJson<ServerMsg>
                    ifDebugFakeErrorFailWith (sprintf "Fake error deserializing %A" serverMsg)
                    serverMsg |> HandleServerMsg |> dispatch
                with exn -> exn.Message |> DeserializeServerMsgError |> WsError |> dispatch
            let wsUrl =
#if AZURE
                "wss://sweepstake-2026.azurewebsites.net:443" // note: WS_PORT irrelevant for Azure (since effectively "internal")
#else
                sprintf "ws://localhost:%i" WS_PORT
#endif
            let wsApiUrl = sprintf "%s%s" wsUrl WS_API_PATH
            try
                let ws = WebSocket.Create wsApiUrl
                ws.onopen <- (fun _ -> ws |> ConnectingInput |> AppInput |> dispatch)
                ws.onerror <- (fun _ -> wsApiUrl |> WsOnError |> WsError |> dispatch)
                ws.onmessage <- receiveServerMsg
                ()
            with _ -> wsApiUrl |> WsOnError |> WsError |> dispatch
        { new IDisposable with member _.Dispose() = () }
    start

let private subscribe (state:State) =
    [
        yield ["ticker"], ticker ()

        match state.AppState with
        | Connecting _ -> yield ["wsSub"], wsSub true
        | _ ->
            match state.ConnectionState with
            | NotConnected -> ()
            | _ ->  yield ["wsSub"], wsSub false
    ]

Globals.marked.setOptions (unbox (createObj [ "sanitize" ==> true ])) |> ignore // note: "sanitize" ensures Html rendered as text

Program.mkProgram initialize transition render
|> Program.withSubscription subscribe
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app" // i.e. <div id="elmish-app"> in index.html
#if DEBUG
// TEMP-NMB: Commented-out - else get Cannot generate auto encoder for Browser.Types.WebSocket errors...|> Program.withDebugger
#endif
|> Program.run
