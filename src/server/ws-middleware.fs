module Aornota.Sweepstake2026.Server.WsMiddleware

open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Json
open Aornota.Sweepstake2026.Common.Literals
open Aornota.Sweepstake2026.Common.WsApi.UiMsg
open Aornota.Sweepstake2026.Server.Agents.Connections
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Common.JsonConverter
open Aornota.Sweepstake2026.Server.Connection

open System
open System.IO
open System.Net.WebSockets
open System.Text
open System.Threading
open System.Threading.Tasks

open Microsoft.AspNetCore.Http

let private log category = (WsMiddleware, category) |> consoleLogger.Log

let private encoding = Encoding.UTF8

type WsMiddleware (next:RequestDelegate) =
    let rec receive (ws:WebSocket) (buffer:ArraySegment<byte>) (ms:MemoryStream) = async {
        let! result = ws.ReceiveAsync (buffer, CancellationToken.None) |> Async.AwaitTask
        ms.Write (buffer.Array, buffer.Offset, result.Count)
        if result.EndOfMessage |> not then return! ms |> receive ws buffer
        else
            ms.Seek (0L, SeekOrigin.Begin) |> ignore
            use reader = new StreamReader (ms, encoding)
            return (result, reader.ReadToEnd ()) }
    let rec receiving (connectionId, ws:WebSocket) consecutiveReceiveFailureCount = async {
        (* Note: Although buffer size should be adequate for serialized UiMsg data (even though Jwt can be fairly large), it seems that on Azure, "messages" are sometimes split into
                 multiple chunks - so we need to cater for this [by checking EndOfMessage &c.]. *)
        let buffer : byte [] = Array.zeroCreate 4096
        let buffer = ArraySegment<byte> buffer
        try
            use ms = new MemoryStream ()
            let! (receiveResult, receivedText) = ms |> receive ws buffer
            sprintf "receiving message for %A" connectionId |> Verbose |> log
            ifDebugFakeErrorFailWith (sprintf "Fake error receiving message for %A" connectionId)
            if receiveResult.CloseStatus.HasValue then return receiveResult |> Some
            else
                try // note: expect buffer to be deserializable to UiMsg
                    sprintf "deserializing message for %A" connectionId |> Verbose |> log
                    let uiMsg = receivedText |> Json |> fromJson<UiMsg>
                    ifDebugFakeErrorFailWith (sprintf "Fake error deserializing %A for %A" uiMsg connectionId)
                    sprintf "message deserialized for %A -> %A" connectionId uiMsg |> Verbose |> log
                    (connectionId, uiMsg) |> connections.HandleUiMsg
                    return! receiving (connectionId, ws) 0u
                with exn ->
                    sprintf "deserializing message failed for %A (%s) -> %A" connectionId receivedText exn.Message |> Danger |> log
                    (connectionId, exn) |> connections.OnDeserializeUiMsgError
                    return! receiving (connectionId, ws) 0u
        with exn ->
            let consecutiveReceiveFailureCount = consecutiveReceiveFailureCount + 1u
            sprintf "receiving message failed for %A -> receive failure count %i -> %A" connectionId consecutiveReceiveFailureCount exn.Message |> Danger |> log
            if ws.State = WebSocketState.Open then (connectionId, exn) |> connections.OnReceiveUiMsgError // note: attempt to send message
            // Note: Try to avoid infinite loop, e.g. of exceptions from ws.ReceiveAsync (...) calls.
            if ws.State = WebSocketState.Open && consecutiveReceiveFailureCount < 3u then
                do! Async.Sleep 1000 // note: just in case it helps
                return! receiving (connectionId, ws) consecutiveReceiveFailureCount
            else return None }
    member __.Invoke (ctx:HttpContext) =
        async {
            if ctx.Request.Path = PathString WS_API_PATH then
                match ctx.WebSockets.IsWebSocketRequest with
                | true ->
                    "new web socket request" |> Verbose |> log
                    (* TEMP-NMB...
                    do! ifDebugSleepAsync 25 125 *)
                    let! ws = ctx.WebSockets.AcceptWebSocketAsync () |> Async.AwaitTask
                    let connectionId = ConnectionId.Create ()
                    sprintf "new web socket accepted -> %A" connectionId |> Verbose |> log
                    (connectionId, ws) |> connections.AddConnection
                    let! receiveResult = receiving (connectionId, ws) 0u
                    sprintf "web socket closing -> %A" connectionId |> Verbose |> log
                    match receiveResult with
                    | Some receiveResult -> ws.CloseAsync (receiveResult.CloseStatus.Value, receiveResult.CloseStatusDescription, CancellationToken.None) |> Async.AwaitTask |> ignore
                    | None -> ()
                    connectionId |> connections.RemoveConnection
                    sprintf "web socket closed -> %A" connectionId |> Verbose |> log
                | false -> ctx.Response.StatusCode <- 400
            else ctx |> next.Invoke |> ignore
        } |> Async.StartAsTask :> Task
