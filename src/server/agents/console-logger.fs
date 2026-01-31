module Aornota.Sweepstake2026.Server.Agents.ConsoleLogger

(* Broadcasts: N/A
   Subscribes: N/A *)

open System

type Entity =
    | Users
    | News
    | Squads
    | Fixtures
    | Drafts

type Projection =
    | Users
    | News
    | Squads
    | Fixtures
    | Drafts
    | UserDraftSummary
    | Chat

type Source =
    | ConsoleLogger
    | Broadcaster
    | Ticker
    | Persistence
    | Entity of entity : Entity
    | Projection of projection : Projection
    | Connections
    | WsMiddleware
    | Host

type Agent =
    | IgnoredInput of text : string
    | SkippedInput of text : string
    | Exception of exn : exn

type Category =
    | Verbose of text : string
    | Info of text : string
    | Warning of text : string
    | Danger of text : string
    | Agent of agent : Agent

type CategoryFilter = Category -> bool
type SourceFilter = Source -> CategoryFilter
type LogFilter = string * SourceFilter

type private ConsoleLoggerInput =
    | Start of logFilter : LogFilter * reply : AsyncReplyChannel<unit>
    | Log of source : Source * category : Category
    | CurrentLogFilter of reply : AsyncReplyChannel<LogFilter>
    | ChangeLogFilter of logFilter : LogFilter * reply : AsyncReplyChannel<unit>

let [<Literal>] private IGNORED_INPUT = "ignored input"

let private sourceTextAndColour source =
    match source with
    | ConsoleLogger -> "ConsoleLogger", ConsoleColor.White
    | Broadcaster -> "Broadcaster", ConsoleColor.Yellow
    | Ticker -> "Ticker", ConsoleColor.DarkYellow
    | Persistence -> "Persistence", ConsoleColor.Cyan
    | Entity entity ->
        let text =
            match entity with
            | Entity.Users -> "Users"
            | Entity.News -> "News"
            | Entity.Squads -> "Squads"
            | Entity.Fixtures -> "Fixtures"
            | Entity.Drafts -> "Drafts"
        sprintf "%s [entity]" text, ConsoleColor.Blue
    | Projection projection ->
        let text =
            match projection with
            | Projection.Users -> "Users"
            | Projection.News -> "News"
            | Projection.Squads -> "Squads"
            | Projection.Fixtures -> "Fixtures"
            | Projection.Drafts -> "Drafts"
            | UserDraftSummary -> "UserDraftSummary"
            | Chat -> "Chat"
        sprintf "%s [projection]" text, ConsoleColor.DarkBlue
    | Connections -> "Connections", ConsoleColor.Magenta
    | WsMiddleware -> "WsMiddleware", ConsoleColor.DarkMagenta
    | Host -> "Host", ConsoleColor.Green

let allCategories : CategoryFilter = function | _ -> true
let allExceptVerbose : CategoryFilter = function | Verbose _ -> false | _ -> true
let onlyWarningsAndWorse : CategoryFilter = function | Warning _ | Danger _ | Agent (Exception _) -> true | _ -> false
let noCategories : CategoryFilter = function | _ -> false

let private everything : SourceFilter = function | _ -> allCategories
let private everythingExceptVerbose : SourceFilter = function | _ -> allExceptVerbose
let private everythingExceptTicker : SourceFilter = function | Ticker -> noCategories | _ -> allCategories
let private everythingExceptVerboseAndTicker : SourceFilter = function | Ticker -> noCategories | _ -> allExceptVerbose
let private warningsAndWorseOnly : SourceFilter = function | _ -> onlyWarningsAndWorse
let private nothing : SourceFilter = function | _ -> noCategories

let logEverything : LogFilter = "everything", everything
let logEverythingExceptVerbose : LogFilter = "everything except Verbose", everythingExceptVerbose
let logEverythingExceptTicker : LogFilter = "everything except Ticker", everythingExceptTicker
let logEverythingExceptVerboseAndTicker : LogFilter = "everything except Verbose and Ticker", everythingExceptVerboseAndTicker
let logWarningsAndWorseOnly : LogFilter = "only warnings and worse", warningsAndWorseOnly
let logNothing : LogFilter = "nothing", nothing

let formatIgnoredInput text = sprintf "%s -> %s" IGNORED_INPUT text
let formatSkippedInput text = sprintf "skipped input -> %s" text

// TODO-NMB-HIGH: Should logging to Console be restricted based on #if DEBUG?...
let private log sourceFilter source category =
    let warningColours = ConsoleColor.DarkRed, ConsoleColor.White
    let dangerColours = ConsoleColor.White, ConsoleColor.Red
    if sourceFilter source category then
        let prefix, text, colour =
            let sourceText, sourceColour = sourceTextAndColour source
            let formatText text = sprintf "%s => %s" sourceText text
            match category with
            | Verbose text -> None, text |> formatText, sourceColour
            | Info text -> None, text |> formatText, sourceColour
            | Warning text -> (" Warning ", warningColours) |> Some, text |> formatText, ConsoleColor.DarkRed
            | Danger text -> (" Danger ", dangerColours) |> Some, text |> formatText, ConsoleColor.Red
            | Agent (IgnoredInput text) -> (sprintf " %s " IGNORED_INPUT, warningColours) |> Some, text |> formatText, ConsoleColor.DarkGray
            | Agent (SkippedInput text) -> None, text |> formatSkippedInput |> formatText, ConsoleColor.Gray
            | Agent (Exception exn) -> (" CRITICAL ", dangerColours) |> Some, sprintf "agent terminated -> %s" exn.Message |> formatText, ConsoleColor.Red
        // Note: No need for lock since only called from ConsoleLogger agent (though can still get mixed up with ASP.Net Core logging output, i.e. since Console not thread-safe).
        let timestampText = sprintf "%s " (DateTimeOffset.UtcNow.ToString ("HH:mm:ss.fff"))
        let previousForegroundColour = Console.ForegroundColor
        Console.Write timestampText
        match prefix with
        | Some (prefixText, (foregroundColour, backgroundColor)) ->
            let previousBackgroundColour = Console.BackgroundColor
            Console.ForegroundColor <- foregroundColour
            Console.BackgroundColor <- backgroundColor
            Console.Write prefixText
            Console.BackgroundColor <- previousBackgroundColour
            Console.Write " "
        | None -> ()
        Console.ForegroundColor <- colour
        Console.WriteLine text
        Console.ForegroundColor <- previousForegroundColour

type ConsoleLogger () =
    let agent = MailboxProcessor.Start (fun inbox ->
        let rec awaitingStart () = async {
            let! input = inbox.Receive ()
            match input with
            | Start ((filterName, sourceFilter), reply) ->
                sprintf "Start when awaitingStart -> logging (filter: '%s')" filterName |> Info |> log everything Source.ConsoleLogger // note: always log (irrespective of sourceFilter)
                () |> reply.Reply
                return! logging (filterName, sourceFilter)
            | Log _ -> "Log when awaitingStart" |> IgnoredInput |> Agent |> log everything Source.ConsoleLogger ; return! awaitingStart ()
            | CurrentLogFilter _ -> "CurrentLogFilter when awaitingStart" |> IgnoredInput |> Agent |> log everything Source.ConsoleLogger ; return! awaitingStart ()
            | ChangeLogFilter _ -> "ChangeLogFilter when awaitingStart" |> IgnoredInput |> Agent |> log everything Source.ConsoleLogger ; return! awaitingStart () }
        and logging (filterName, sourceFilter) = async {
            let! input = inbox.Receive ()
            match input with
            | Start _ -> "Start when logging" |> IgnoredInput |> Agent |> log everything Source.ConsoleLogger ; return! logging (filterName, sourceFilter) // note: always log (irrespective of sourceFilter)
            | Log (source, category) ->
                log sourceFilter source category
                return! logging (filterName, sourceFilter)
            | CurrentLogFilter reply ->
                (filterName, sourceFilter) |> reply.Reply
                return! logging (filterName, sourceFilter)
            | ChangeLogFilter ((filterName, sourceFilter), reply) ->
                sprintf "ChangeLogFilter when logging -> logging (filter: '%s')" filterName |> Info |> log everything Source.ConsoleLogger // note: always log (irrespective of sourceFilter)
                () |> reply.Reply
                return! logging (filterName, sourceFilter) }
        "agent instantiated -> awaitingStart" |> Info |> log everything Source.ConsoleLogger
        awaitingStart ())
    do (fun exn -> exn |> Exception |> Agent |> log everything Source.ConsoleLogger) |> agent.Error.Add // note: an unhandled exception will "kill" the agent - but at least we can log the exception
    member __.Start logFilter = (fun reply -> (logFilter, reply) |> Start) |> agent.PostAndReply // note: not async (since need to start agents deterministically)
    member __.Log (source, category) = (source, category) |> Log |> agent.Post
    member __.CurrentLogFilter () = CurrentLogFilter |> agent.PostAndReply
    member __.ChangeLogFilter logFilter = (fun reply -> (logFilter, reply) |> ChangeLogFilter) |> agent.PostAndReply

let consoleLogger = ConsoleLogger ()

let logAgentException source exn = (source, Agent (Exception exn)) |> consoleLogger.Log
