module Aornota.Sweepstake2026.Server.Host

open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.Literals
open Aornota.Sweepstake2026.Common.UnitsOfMeasure

open Aornota.Sweepstake2026.Server.Agents
open Aornota.Sweepstake2026.Server.Agents.Broadcaster
open Aornota.Sweepstake2026.Server.Agents.Connections
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Agents.Persistence
open Aornota.Sweepstake2026.Server.Agents.Ticker
open Aornota.Sweepstake2026.Server.DefaultData
open Aornota.Sweepstake2026.Server.Startup

open System
open System.IO

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Hosting

let [<Literal>] private SECONDS_PER_TICK = 1<second/tick>

let private log category = (Host, category) |> consoleLogger.Log

let private publicPath =
    let publicPath = Path.Combine ("..", "ui/public") |> Path.GetFullPath // e.g. when served via webpack-dev-server
    if Directory.Exists publicPath then publicPath else "public" |> Path.GetFullPath // e.g. when published/deployed

let private serverStarted = DateTimeOffset.UtcNow

"starting ConsoleLogger agent" |> Info |> log // note: will be logged as IgnoredInput (since ConsoleLogger agent not yet started) - but this is fine since consoleLogger.Log is not blocking
ifDebug logEverythingExceptVerboseAndTicker logWarningsAndWorseOnly |> consoleLogger.Start
"starting core agents" |> Info |> log
logNoSignals |> broadcaster.Start
SECONDS_PER_TICK |> ticker.Start
() |> persistence.Start

createInitialPersistedEventsIfNecessary |> Async.RunSynchronously

// Note: If entity agents were started by createInitialPersistedEventsIfNecessary [then "reset"], they will "bypass" subsequent Start calls (i.e. no new subscription) and *not* block the caller.
"starting Entities agents" |> Info |> log
() |> Entities.Users.users.Start
() |> Entities.News.news.Start
() |> Entities.Squads.squads.Start
() |> Entities.Fixtures.fixtures.Start
() |> Entities.Drafts.drafts.Start

"starting Projections agents" |> Info |> log
() |> Projections.Users.users.Start
() |> Projections.News.news.Start
() |> Projections.Squads.squads.Start
() |> Projections.Fixtures.fixtures.Start
() |> Projections.Drafts.drafts.Start
() |> Projections.UserDraftSummary.userDraftSummary.Start
() |> Projections.Chat.chat.Start

"reading persisted events" |> Info |> log
readPersistedEvents ()

"requesting immediate housekeeping" |> Info |> log
() |> Entities.Drafts.drafts.Housekeeping

"starting Connections agent" |> Info |> log
serverStarted |> connections.Start

(* TEMP-NMB: Finesse logging for development/debugging purposes... *)
("development/debugging", function | Entity Entity.Fixtures | Projection Projection.Fixtures -> allCategories | Persistence -> allExceptVerbose | _ -> onlyWarningsAndWorse) |> consoleLogger.ChangeLogFilter

"ready" |> Info |> log

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .UseUrls(sprintf "http://0.0.0.0:%i/" WS_PORT)
    (* TODO-NMB-LOW: Suppress ASP.Net Core logging (since can get mixed up with ConsoleLogger output, i.e. since Console not thread-safe)?...
    .ConfigureLogging(...) *)
    .UseStartup<Startup>()
    .Build()
    .Run()
