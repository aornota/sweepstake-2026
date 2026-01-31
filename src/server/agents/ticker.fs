module Aornota.Sweepstake2026.Server.Agents.Ticker

(* Broadcasts: Tick
   Subscribes: N/A *)

open Aornota.Sweepstake2026.Common.UnitsOfMeasure
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Signal

type private TickerInput =
    | Start of secondsPerTick : int<second/tick> * reply : AsyncReplyChannel<unit>
    | Stop of reply : AsyncReplyChannel<unit>

let private log category = (Ticker, category) |> consoleLogger.Log

type Ticker () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start (secondsPerTick, reply) ->
                let secondsPerTick = if secondsPerTick > 0<second/tick> then secondsPerTick else 1<second/tick>
                sprintf "Start when awaitingStart -> ticking (%i<second/tick>)" (int secondsPerTick) |> Info |> log
                () |> reply.Reply
                return! ticking (secondsPerTick, None)
            | Stop _ -> "Stop when awaitingStart" |> IgnoredInput |> Agent |> log ; return! awaitingStart () }
        and ticking (secondsPerTick, ticks) = async {
            let millisecondsPerTick = ((float secondsPerTick) * 1.<second/tick>) * MILLISECONDS_PER_SECOND
            do! Async.Sleep (int millisecondsPerTick)
            let ticks = match ticks with | Some ticks -> ticks + 1<tick> | None -> 1<tick>
            (ticks, secondsPerTick) |> Tick |> broadcaster.Broadcast
            // Note: Only call inbox.Receive () if queue is non-empty (since would otherwise block until non-empty, thus preventing further Tick broadcasts).
            if inbox.CurrentQueueLength > 0 then
                let! input = inbox.Receive ()
                match input with
                | Start _ -> "Start when ticking" |> IgnoredInput |> Agent |> log ; return! ticking (secondsPerTick, Some ticks)
                | Stop reply ->
                    sprintf "Stop when ticking (%i<tick>) -> awaitingStart" (int ticks) |> Info |> log
                    () |> reply.Reply
                    return! awaitingStart ()
            else return! ticking (secondsPerTick, Some ticks) }
        "agent instantiated -> awaitingStart" |> Info |> log
        awaitingStart ())
    do Source.Ticker |> logAgentException |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start secondsPerTick = (fun reply -> (secondsPerTick, reply) |> Start) |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.Stop () = Stop |> agent.PostAndReply

let isEveryNSeconds (everyN:int<second>) (ticks, secondsPerTick) =
    let seconds = ticks * secondsPerTick
    if seconds < everyN then false
    else seconds % everyN < secondsPerTick * 1<tick>

let ticker = Ticker ()
