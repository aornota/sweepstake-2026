#r "paket: groupref build //"
#if !FAKE
// See https://github.com/ionide/ionide-vscode-fsharp/issues/839#issuecomment-396296095.
#r "netstandard"
#r "Facades/netstandard"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

open System

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators

open Farmer
open Farmer.Builders

[<Literal>]
let private APPLICATION_NAME = "sweepstake-2026"

let private serverDir = Path.getFullName "./src/server"
let private uiDir = Path.getFullName "./src/ui"
let private uiPublishDir = uiDir </> "publish"
let private publishDir = Path.getFullName "./publish"
let private publishPublicDir = publishDir </> "public"

let private webapp = webApp {
    name APPLICATION_NAME
    sku WebApp.Sku.F1
    zip_deploy publishDir }

let private deployment = arm {
    location Location.NorthEurope
    add_resource webapp }

let private platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | None -> failwithf "%s not found in path. Please install it and make sure it's available from your path. See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info." tool

let private yarnTool = platformTool "yarn" "yarn.cmd"

let private runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand(cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let private runDotNet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd String.Empty
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s." cmd workingDir

let private openBrowser url =
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCodeWithMessage "Unable to open browser."
    |> Proc.run
    |> ignore

let private buildUi forAzure =
    if forAzure then runDotNet (sprintf "fable %s --define AZURE --run webpack" uiDir) __SOURCE_DIRECTORY__
    else runDotNet (sprintf "fable %s --run webpack" uiDir) __SOURCE_DIRECTORY__

let private publishUi () = Shell.copyDir publishPublicDir uiPublishDir FileFilter.allFiles

Target.create "clean-ui-publish" (fun _ -> Shell.cleanDir uiPublishDir)
Target.create "clean-publish" (fun _ -> Shell.cleanDir publishDir) // note: this will delete any .\persisted and .\secret folders in publishDir (though should not be running server from publishDir!)

Target.create "restore-ui" (fun _ ->
    printfn "Yarn version:"
    runTool yarnTool "--version" __SOURCE_DIRECTORY__
    runTool yarnTool "install --frozen-lockfile" __SOURCE_DIRECTORY__
    runDotNet "restore" uiDir)

Target.create "run" (fun _ ->
    let server = async { runDotNet "watch run" serverDir }
    let client = async { runDotNet (sprintf "fable %s --define DEBUG --run webpack serve" uiDir) __SOURCE_DIRECTORY__ }
    let browser = async {
        do! Async.Sleep 2500
        openBrowser "http://localhost:8080" }
    Async.Parallel [ server ; client ; browser ] |> Async.RunSynchronously |> ignore)

Target.create "build-server" (fun _ -> runDotNet "build -c Release" serverDir)
Target.create "build-ui" (fun _ -> buildUi false)
Target.create "build" ignore

Target.create "publish-server" (fun _ -> runDotNet (sprintf "publish -c Release -o \"%s\"" publishDir) serverDir)
Target.create "publish-ui-local" (fun _ ->
    buildUi false
    publishUi ())
Target.create "publish-ui-azure" (fun _ ->
    buildUi true
    publishUi ())
Target.create "publish" ignore
Target.create "publish-azure" ignore

Target.create "deploy-azure" (fun _ -> deployment |> Deploy.execute APPLICATION_NAME Deploy.NoParameters |> ignore)

Target.create "help" (fun _ ->
    printfn "\nThese useful build targets can be run via 'fake build -t {target}':"
    printfn "\n\trun -> builds, runs and watches [Debug] server and [non-production] ui (served via webpack-dev-server)"
    printfn "\n\tbuild -> builds [Release] server and [production] ui (which writes output to .\\src\\ui\\publish)"
    printfn "\tpublish -> builds [Release] server and [production] ui and copies output to .\\publish"
    printfn "\n\tdeploy-azure -> builds [Release] server and [production] ui, copies output to .\\publish and deploys to Azure"
    printfn "\n\thelp -> shows this list of build targets\n")

"restore-ui" ==> "run"
"restore-ui" ==> "clean-ui-publish"

"build-server" ==> "build"
"clean-ui-publish" ==> "build-ui" ==> "build"

"clean-publish" ==> "publish-server" ==> "publish"
"clean-ui-publish" ==> "publish-ui-local"
"clean-publish" ==> "publish-ui-local" ==> "publish"

"clean-publish" ==> "publish-server" ==> "publish-azure"
"clean-ui-publish" ==> "publish-ui-azure"
"clean-publish" ==> "publish-ui-azure" ==> "publish-azure"

"publish-azure" ==> "deploy-azure"

Target.runOrDefaultWithArguments "help"
