module Aornota.Sweepstake2026.Server.DefaultData

open Aornota.Sweepstake2026.Common.Domain.Core
open Aornota.Sweepstake2026.Common.Domain.Draft
open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.Squad
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.IfDebug
open Aornota.Sweepstake2026.Common.WsApi.ServerMsg
open Aornota.Sweepstake2026.Server.Agents.ConsoleLogger
open Aornota.Sweepstake2026.Server.Agents.Entities.Drafts
open Aornota.Sweepstake2026.Server.Agents.Entities.Fixtures
open Aornota.Sweepstake2026.Server.Agents.Entities.Squads
open Aornota.Sweepstake2026.Server.Agents.Entities.Users
open Aornota.Sweepstake2026.Server.Agents.Persistence
open Aornota.Sweepstake2026.Server.Authorization
open Aornota.Sweepstake2026.Server.Common.Helpers

open System
open System.IO

let private deleteExistingUsersEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingSquadsEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingFixturesEvents = ifDebug true false // note: should *not* generally set to true for Release (and only with caution for Debug!)
let private deleteExistingDraftsEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)

let private log category = (Host, category) |> consoleLogger.Log

let private logResult shouldSucceed scenario result =
    match shouldSucceed, result with
    | true, Ok _ -> sprintf "%s -> succeeded (as expected)" scenario |> Verbose |> log
    | true, Error error -> sprintf "%s -> unexpectedly failed -> %A" scenario error |> Danger |> log
    | false, Ok _ -> sprintf "%s -> unexpectedly succeeded" scenario |> Danger |> log
    | false, Error error -> sprintf "%s -> failed (as expected) -> %A" scenario error |> Verbose |> log
let private logShouldSucceed scenario result = result |> logResult true scenario
let private logShouldFail scenario result = result |> logResult false scenario

let private delete dir =
    Directory.GetFiles dir |> Array.iter File.Delete
    Directory.Delete dir

let private ifToken fCmdAsync token = async { return! match token with | Some token -> token |> fCmdAsync | None -> NotAuthorized |> AuthCmdAuthznError |> Error |> thingAsync }

let private superUser = SuperUser
let private nephId = Guid.Empty |> UserId
let private nephTokens = permissions nephId superUser |> UserTokens

let private mexicoId = Guid "00000011-0000-0000-0000-000000000000" |> SquadId
let private southAfricaId = Guid "00000012-0000-0000-0000-000000000000" |> SquadId
let private southKoreaId = Guid "00000013-0000-0000-0000-000000000000" |> SquadId
let private czechRepublicId = Guid "00000014-0000-0000-0000-000000000000" |> SquadId

let private canadaId = Guid "00000021-0000-0000-0000-000000000000" |> SquadId
let private bosniaId = Guid "00000022-0000-0000-0000-000000000000" |> SquadId
let private qatarId = Guid "00000023-0000-0000-0000-000000000000" |> SquadId
let private switzerlandId = Guid "00000024-0000-0000-0000-000000000000" |> SquadId

let private brazilId = Guid "00000031-0000-0000-0000-000000000000" |> SquadId
let private moroccoId = Guid "00000032-0000-0000-0000-000000000000" |> SquadId
let private haitiId = Guid "00000033-0000-0000-0000-000000000000" |> SquadId
let private scotlandId = Guid "00000034-0000-0000-0000-000000000000" |> SquadId

let private unitedStatesId = Guid "00000041-0000-0000-0000-000000000000" |> SquadId
let private paraguayId = Guid "00000042-0000-0000-0000-000000000000" |> SquadId
let private australiaId = Guid "00000043-0000-0000-0000-000000000000" |> SquadId
let private turkeyId = Guid "00000044-0000-0000-0000-000000000000" |> SquadId

let private germanyId = Guid "00000051-0000-0000-0000-000000000000" |> SquadId
let private curaçaoId = Guid "00000052-0000-0000-0000-000000000000" |> SquadId
let private ivoryCoastId = Guid "00000053-0000-0000-0000-000000000000" |> SquadId
let private ecuadorId = Guid "00000054-0000-0000-0000-000000000000" |> SquadId

let private netherlandsId = Guid "00000061-0000-0000-0000-000000000000" |> SquadId
let private japanId = Guid "00000062-0000-0000-0000-000000000000" |> SquadId
let private swedenId = Guid "00000063-0000-0000-0000-000000000000" |> SquadId
let private tunisiaId = Guid "00000064-0000-0000-0000-000000000000" |> SquadId

let private belgiumId = Guid "00000071-0000-0000-0000-000000000000" |> SquadId
let private egyptId = Guid "00000072-0000-0000-0000-000000000000" |> SquadId
let private iranId = Guid "00000073-0000-0000-0000-000000000000" |> SquadId
let private newZealandId = Guid "00000074-0000-0000-0000-000000000000" |> SquadId

let private spainId = Guid "00000081-0000-0000-0000-000000000000" |> SquadId
let private capeVerdeId = Guid "00000082-0000-0000-0000-000000000000" |> SquadId
let private saudiArabiaId = Guid "00000083-0000-0000-0000-000000000000" |> SquadId
let private uruguayId = Guid "00000084-0000-0000-0000-000000000000" |> SquadId

let private franceId = Guid "00000091-0000-0000-0000-000000000000" |> SquadId
let private senegalId = Guid "00000092-0000-0000-0000-000000000000" |> SquadId
let private iraqId = Guid "00000093-0000-0000-0000-000000000000" |> SquadId
let private norwayId = Guid "00000094-0000-0000-0000-000000000000" |> SquadId

let private argentinaId = Guid "000000A1-0000-0000-0000-000000000000" |> SquadId
let private algeriaId = Guid "000000A2-0000-0000-0000-000000000000" |> SquadId
let private austriaId = Guid "000000A3-0000-0000-0000-000000000000" |> SquadId
let private jordanId = Guid "000000A4-0000-0000-0000-000000000000" |> SquadId

let private portugalId = Guid "000000B1-0000-0000-0000-000000000000" |> SquadId
let private drCongoId = Guid "000000B2-0000-0000-0000-000000000000" |> SquadId
let private uzbekistanId = Guid "000000B3-0000-0000-0000-000000000000" |> SquadId
let private colombiaId = Guid "000000B4-0000-0000-0000-000000000000" |> SquadId

let private englandId = Guid "000000C1-0000-0000-0000-000000000000" |> SquadId
let private croatiaId = Guid "000000C2-0000-0000-0000-000000000000" |> SquadId
let private ghanaId = Guid "000000C3-0000-0000-0000-000000000000" |> SquadId
let private panamaId = Guid "000000C4-0000-0000-0000-000000000000" |> SquadId

let private createInitialUsersEventsIfNecessary = async {
    let usersDir = directory EntityType.Users

    // Force re-creation of initial User/s events if directory already exists (if requested).
    if deleteExistingUsersEvents && Directory.Exists usersDir then
        sprintf "deleting existing User/s events -> %s" usersDir |> Info |> log
        delete usersDir

    if Directory.Exists usersDir then sprintf "preserving existing User/s events -> %s" usersDir |> Info |> log
    else
        sprintf "creating initial User/s events -> %s" usersDir |> Info |> log
        "starting Users agent" |> Info |> log
        () |> users.Start
        // Note: Send dummy OnUsersEventsRead to Users agent to ensure that it transitions [from pendingOnUsersEventsRead] to managingUsers; otherwise HandleCreateUserCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnUsersEventsRead to Users agent" |> Info |> log
        [] |> users.OnUsersEventsRead

        // #region: Create initial SuperUser | Administators - and various Plebs
        let neph = UserName "neph"
        let dummyPassword = Password "password"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, nephId, neph, dummyPassword, superUser) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" neph)
        let administrator = Administrator
        let rosieId, rosie = Guid "ffffffff-0001-0000-0000-000000000000" |> UserId, UserName "rosie"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, rosieId, rosie, dummyPassword, administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rosie)
        let hughId, hugh = Guid "ffffffff-0002-0000-0000-000000000000" |> UserId, UserName "hugh"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, hughId, hugh, dummyPassword, administrator) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" hugh)
        (*
        let pleb = Pleb
        let chrisId, chris = Guid.NewGuid() |> UserId, UserName "chris"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, chrisId, chris, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" chris)
        let damianId, damian = Guid.NewGuid() |> UserId, UserName "damian"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, damianId, damian, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" damian)
        let denisId, denis = Guid.NewGuid() |> UserId, UserName "denis"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, denisId, denis, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" denis)
        let jemId, jem = Guid.NewGuid() |> UserId, UserName "jem"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, jemId, jem, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" jem)
        let robId, rob = Guid.NewGuid() |> UserId, UserName "rob"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, robId, rob, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" rob)
        let steveMId, steveM = Guid.NewGuid() |> UserId, UserName "steve m"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, steveMId, steveM, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" steveM)
        let steveSId, steveS = Guid.NewGuid() |> UserId, UserName "steve s"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, steveSId, steveS, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" steveS)
        let susieId, susie = Guid.NewGuid() |> UserId, UserName "susie"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, susieId, susie, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" susie)
        let tomId, tom = Guid.NewGuid() |> UserId, UserName "tom"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, tomId, tom, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" tom)
        let willId, will = Guid.NewGuid() |> UserId, UserName "will"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, willId, will, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" will)
        let callumId, callum = Guid.NewGuid() |> UserId, UserName "callum"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, callumId, callum, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" callum)
        let jackId, jack = Guid.NewGuid() |> UserId, UserName "jack"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, jackId, jack, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" jack)
        let martynId, martyn = Guid.NewGuid() |> UserId, UserName "martyn"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, martynId, martyn, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" martyn)
        let mikeId, mike = Guid.NewGuid() |> UserId, UserName "mike"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, mikeId, mike, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" mike)
        let sueId, sue = Guid.NewGuid() |> UserId, UserName "sue"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, sueId, sue, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" sue)
        let highnamId, highnam = Guid.NewGuid() |> UserId, UserName "highnam"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, highnamId, highnam, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" highnam)
        let mollyId, molly = Guid.NewGuid() |> UserId, UserName "molly"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, mollyId, molly, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" molly)
        let nourdineId, nourdine = Guid.NewGuid() |> UserId, UserName "nourdine"
        let! result = nephTokens.CreateUserToken |> ifToken (fun token -> (token, nephId, nourdineId, nourdine, dummyPassword, pleb) |> users.HandleCreateUserCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUserCmdAsync (%A)" nourdine)
        *)
        // #endregion

        // Note: Reset Users agent [to pendingOnUsersEventsRead] so that it handles subsequent UsersEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Users agent" |> Info |> log
        () |> users.Reset
    return () }

let private createInitialSquadsEventsIfNecessary = async {
    let squadsDir = directory EntityType.Squads

    // Force re-creation of initial Squad/s events if directory already exists (if requested).
    if deleteExistingSquadsEvents && Directory.Exists squadsDir then
        sprintf "deleting existing Squad/s events -> %s" squadsDir |> Info |> log
        delete squadsDir

    if Directory.Exists squadsDir then sprintf "preserving existing Squad/s events -> %s" squadsDir |> Info |> log
    else
        sprintf "creating initial Squad/s events -> %s" squadsDir |> Info |> log
        "starting Squads agent" |> Info |> log
        () |> squads.Start
        // Note: Send dummy OnSquadsEventsRead to Squads agent to ensure that it transitions [from pendingOnSquadsEventsRead] to managingSquads; otherwise HandleCreateSquadCmdAsync (&c.) would be ignored (and block).
        "sending dummy OnSquadsEventsRead to Squads agent" |> Info |> log
        [] |> squads.OnSquadsEventsRead

        // Create initial Squads.

        // Group A
        let mexico = SquadName "Mexico"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, mexicoId, mexico, GroupA, Some (Seeding 15), CoachName "Javier Aguirre") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" mexico)
        let southAfrice = SquadName "South Africa"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, southAfricaId, southAfrice, GroupA, None, CoachName "Hugo Broos") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" southAfrice)
        let southKorea = SquadName "South Korea"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, southKoreaId, southKorea, GroupA, Some (Seeding 22), CoachName "Hong Myung-bo") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" southKorea)
        let czechRepublic = SquadName "Czech Republic"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, czechRepublicId, czechRepublic, GroupA, None, CoachName "Miroslav Koubek") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" czechRepublic)

        // #Group B
        let canada = SquadName "Canada"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, canadaId, canada, GroupB, Some (Seeding 27), CoachName "Jesse Marsch") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" canada)
        let bosnia = SquadName "Bosnia and Herzegovina"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, bosniaId, bosnia, GroupB, None, CoachName "Sergej Barbarez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" bosnia)
        let qatar = SquadName "Qatar"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, qatarId, qatar, GroupB, None, CoachName "Julen Lopetegui") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" qatar)
        let switzerland = SquadName "Switzerland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, switzerlandId, switzerland, GroupB, Some (Seeding 17), CoachName "Murat Yakin") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" switzerland)

        // Group C
        let brazil = SquadName "Brazil"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, brazilId, brazil, GroupC, Some (Seeding 5), CoachName "Carlo Ancelotti") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" brazil)
        let morocco = SquadName "Morocco"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, moroccoId, morocco, GroupC, Some (Seeding 11), CoachName "Mohamed Ouahbi") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUseHandleCreateSquadCmdAsyncrCmdAsync (%A)" morocco)
        let haiti = SquadName "Haiti"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, haitiId, haiti, GroupC, None, CoachName "Sébastien Migné") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" haiti)
        let scotland = SquadName "Scotland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, scotlandId, scotland, GroupC, None, CoachName "Steve Clarke") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" scotland)

        // Group D
        let unitedStates = SquadName "United States"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, unitedStatesId, unitedStates, GroupD, Some (Seeding 14), CoachName "Mauricio Pochettino") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" unitedStates)
        let paraguay = SquadName "Paraguay"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, paraguayId, paraguay, GroupD, None, CoachName "Gustavo Alfaro") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" paraguay)
        let australia = SquadName "Australia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, australiaId, australia, GroupD, Some (Seeding 26), CoachName "Tony Popovic") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" australia)
        let turkey = SquadName "Turkey"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, turkeyId, turkey, GroupD, None, CoachName "Vincenzo Montella") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" turkey)

        // Group E
        let germany = SquadName "Germany"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, germanyId, germany, GroupE, Some (Seeding 9), CoachName "Julian Nagelsmann") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" germany)
        let curaçao = SquadName "Curaçao"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, curaçaoId, curaçao, GroupE, None, CoachName "Fred Rutten") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" curaçao)
        let ivoryCoast = SquadName "Ivory Coast"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, ivoryCoastId, ivoryCoast, GroupE, None, CoachName "Emerse Faé") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" ivoryCoast)
        let ecuador = SquadName "Ecuador"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, ecuadorId, ecuador, GroupE, Some (Seeding 23), CoachName "Sebastián Beccacece") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" ecuador)

        // Group F
        let netherlands = SquadName "Netherlands"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, netherlandsId, netherlands, GroupF, Some (Seeding 7), CoachName "Ronald Koeman") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" netherlands)
        let japan = SquadName "Japan"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, japanId, japan, GroupF, Some (Seeding 18), CoachName "Hajime Moriyasu") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" japan)
        let sweden = SquadName "Sweden"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, swedenId, sweden, GroupF, None, CoachName "Graham Potter") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" sweden)
        let tunisia = SquadName "Tunisia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, tunisiaId, tunisia, GroupF, None, CoachName "Sabri Lamouchi") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" tunisia)

        // Group G
        let belgium = SquadName "Belgium"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, belgiumId, belgium, GroupG, Some (Seeding 8), CoachName "Rudi Garcia") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" belgium)
        let egypt = SquadName "Egypt"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, egyptId, egypt, GroupG, None, CoachName "Hossam Hassan") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" egypt)
        let iran = SquadName "Iran"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, iranId, iran, GroupG, Some (Seeding 20), CoachName "Amir Ghalenoei") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" iran)
        let newZealand = SquadName "New Zealand"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, newZealandId, newZealand, GroupG, None, CoachName "Darren Bazeley") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" newZealand)

        // Group H
        let spain = SquadName "Spain"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, spainId, spain, GroupH, Some (Seeding 1), CoachName "Luis de la Fuente") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" spain)
        let capeVerde = SquadName "Cape Verde"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, capeVerdeId, capeVerde, GroupH, None, CoachName "Bubista") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" capeVerde)
        let saudiArabia = SquadName "Saudi Arabia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, saudiArabiaId, saudiArabia, GroupH, None, CoachName "Georgios Donis") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" saudiArabia)
        let uruguay = SquadName "Uruguay"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, uruguayId, uruguay, GroupH, Some (Seeding 16), CoachName "Marcelo Bielsa") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" uruguay)

        // Group I
        let france = SquadName "Rrance"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, franceId, france, GroupI, Some (Seeding 2), CoachName "Didier Deschamps") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" france)
        let senegal = SquadName "Senegal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, senegalId, senegal, GroupI, Some (Seeding 19), CoachName "Pape Thiaw") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" senegal)
        let iraq = SquadName "Iraq"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, iraqId, iraq, GroupI, None, CoachName "Graham Arnold") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" iraq)
        let norway = SquadName "Norway"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, norwayId, norway, GroupI, None, CoachName "Ståle Solbakken") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" norway)

        // Group J
        let argentina = SquadName "Argentina"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, argentinaId, argentina, GroupJ, Some (Seeding 2), CoachName "Lionel Scaloni") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" argentina)
        let algeria = SquadName "Algeria"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, algeriaId, algeria, GroupJ, None, CoachName "Vladimir Petković") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" algeria)
        let austria = SquadName "Austria"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, austriaId, austria, GroupJ, Some (Seeding 24), CoachName "Ralf Rangnick") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" austria)
        let jordan = SquadName "Jordan"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, jordanId, jordan, GroupJ, None, CoachName "Jamal Sellami") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" jordan)

        // Group K
        let portugal = SquadName "Portugal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, portugalId, portugal, GroupK, Some (Seeding 6), CoachName "Roberto Martínez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" portugal)
        let drCongo = SquadName "DR Congo"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, drCongoId, drCongo, GroupK, None, CoachName "Sébastien Desabre") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" drCongo)
        let uzbekistan = SquadName "Uzbekistan"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, uzbekistanId, uzbekistan, GroupK, None, CoachName "Fabio Cannavaro") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" uzbekistan)
        let colombia = SquadName "Colombia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, colombiaId, colombia, GroupK, Some (Seeding 13), CoachName "Néstor Lorenzo") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" colombia)

        // Group L
        let england = SquadName "England"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, englandId, england, GroupL, Some (Seeding 4), CoachName "Thomas Tuchel") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" england)
        let croatia = SquadName "Croatia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, croatiaId, croatia, GroupL, Some (Seeding 10), CoachName "Zlatko Dalić") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" croatia)
        let ghana = SquadName "Ghana"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, ghanaId, ghana, GroupL, None, CoachName "Carlos Queiroz") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" ghana)
        let panama = SquadName "Panama"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, panamaId, panama, GroupL, None, CoachName "Thomas Christiansen") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" panama)

        // Note: Reset Squads agent [to pendingOnSquadsEventsRead] so that it handles subsequent SquadsEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Squads agent" |> Info |> log
        () |> squads.Reset
    return () }

let private createInitialFixturesEventsIfNecessary = async {
    let fixtureId matchNumber =
        if matchNumber < 10u then sprintf "00000000-0000-0000-0000-00000000000%i" matchNumber |> Guid |> FixtureId
        else if matchNumber < 100u then sprintf "00000000-0000-0000-0000-0000000000%i" matchNumber |> Guid |> FixtureId
        else FixtureId.Create ()

    let fixturesDir = directory EntityType.Fixtures

    // Force re-creation of initial Fixture/s events if directory already exists (if requested).
    if deleteExistingFixturesEvents && Directory.Exists fixturesDir then
        sprintf "deleting existing Fixture/s events -> %s" fixturesDir |> Info |> log
        delete fixturesDir

    if Directory.Exists fixturesDir then sprintf "preserving existing Fixture/s events -> %s" fixturesDir |> Info |> log
    else
        sprintf "creating initial Fixture/s events -> %s" fixturesDir |> Info |> log
        "starting Fixtures agent" |> Info |> log
        () |> fixtures.Start
        // Note: Send dummy OnFixturesEventsRead to Users agent to ensure that it transitions [from pendingOnFixturesEventsRead] to managingFixtures; otherwise HandleCreateFixtureCmdAsync would be ignored (and block).
        "sending dummy OnFixturesEventsRead to Fixtures agent" |> Info |> log
        [] |> fixtures.OnFixturesEventsRead

        // Group A
        let mexicoVsSouthAFrica = (2026, 06, 11, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 1u, Group GroupA, Confirmed mexicoId, Confirmed southAfricaId, mexicoVsSouthAFrica) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 1u)

        let southKoreaVsCzechRepublic = (2026, 06, 12, 02, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 2u, Group GroupA, Confirmed southKoreaId, Confirmed czechRepublicId, southKoreaVsCzechRepublic) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 2u)

        let czechRepublicVsSouthAfrica = (2026, 06, 18, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 25u, Group GroupA, Confirmed czechRepublicId, Confirmed southAfricaId, czechRepublicVsSouthAfrica) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 25u)

        let mexicoVsSouthKorea = (2026, 06, 19, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 28u, Group GroupA, Confirmed mexicoId, Confirmed southKoreaId, mexicoVsSouthKorea) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 28u)

        let czechRespublicVsMexico = (2026, 06, 25, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 53u, Group GroupA, Confirmed czechRepublicId, Confirmed mexicoId, czechRespublicVsMexico) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 53u)

        let southKoreaVsSouthAfrica = (2026, 06, 25, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 54u, Group GroupA, Confirmed southKoreaId, Confirmed southAfricaId, southKoreaVsSouthAfrica) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 54u)

        // Group B
        let canadaVsBosnia = (2026, 06, 12, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 3u, Group GroupB, Confirmed canadaId, Confirmed bosniaId, canadaVsBosnia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 3u)

        let qatarVsSwitzerland = (2026, 06, 13, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 8u, Group GroupB, Confirmed qatarId, Confirmed switzerlandId, qatarVsSwitzerland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 8u)

        let switzerlandVsBosnia = (2026, 06, 18, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 26u, Group GroupB, Confirmed switzerlandId, Confirmed bosniaId, switzerlandVsBosnia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 26u)

        let canadaVsQatar = (2026, 06, 18, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 27u, Group GroupB, Confirmed canadaId, Confirmed qatarId, canadaVsQatar) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 27u)

        let switzerlandVsCanada = (2026, 06, 24, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 51u, Group GroupB, Confirmed switzerlandId, Confirmed canadaId, switzerlandVsCanada) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 51u)

        let bosniaVsQatar = (2026, 06, 24, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 52u, Group GroupB, Confirmed bosniaId, Confirmed qatarId, bosniaVsQatar) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 52u)

        // Group C
        let brazilVsMorocco = (2026, 06, 13, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 7u, Group GroupC, Confirmed brazilId, Confirmed moroccoId, brazilVsMorocco) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 7u)

        let haitiVsScotland = (2026, 06, 14, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 5u, Group GroupC, Confirmed haitiId, Confirmed scotlandId, haitiVsScotland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 5u)

        let scotlandVsMorocoo = (2026, 06, 19, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 30u, Group GroupC, Confirmed scotlandId, Confirmed moroccoId, scotlandVsMorocoo) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 30u)

        let brazilVsHaiti = (2026, 06, 20, 00, 30) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 29u, Group GroupC, Confirmed brazilId, Confirmed haitiId, brazilVsHaiti) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 29u)

        let scotlandVsBrazil = (2026, 06, 24, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 49u, Group GroupC, Confirmed scotlandId, Confirmed brazilId, scotlandVsBrazil) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 49u)

        let morocooVsHaiti = (2026, 06, 24, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 50u, Group GroupC, Confirmed moroccoId, Confirmed haitiId, morocooVsHaiti) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 50u)

        // Group D
        let unitedStatesVsParaguay = (2026, 06, 13, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 4u, Group GroupD, Confirmed unitedStatesId, Confirmed paraguayId, unitedStatesVsParaguay) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 4u)

        let australiaVsTurkey = (2026, 06, 14, 04, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 6u, Group GroupD, Confirmed australiaId, Confirmed turkeyId, australiaVsTurkey) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 6u)

        let unitedStatesVsAustralia = (2026, 06, 19, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 32u, Group GroupD, Confirmed unitedStatesId, Confirmed australiaId, unitedStatesVsAustralia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 32u)

        let turkeyVsParaguay = (2026, 06, 20, 03, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 31u, Group GroupD, Confirmed turkeyId, Confirmed paraguayId, turkeyVsParaguay) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 31u)

        let turkeyVsUnitedStates = (2026, 06, 26, 02, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 59u, Group GroupD, Confirmed turkeyId, Confirmed unitedStatesId, turkeyVsUnitedStates) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 59u)

        let paraguayVsAustralia = (2026, 06, 26, 02, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 60u, Group GroupD, Confirmed paraguayId, Confirmed australiaId, paraguayVsAustralia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 60u)

        // Group E
        let germnayVsCuraçao = (2026, 06, 14, 17, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 10u, Group GroupE, Confirmed germanyId, Confirmed curaçaoId, germnayVsCuraçao) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 10u)

        let ivoryCoastVsEcuador = (2026, 06, 14, 23, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 9u, Group GroupE, Confirmed ivoryCoastId, Confirmed ecuadorId, ivoryCoastVsEcuador) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 9u)

        let germanyVsIvoryCoast = (2026, 06, 20, 20, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 33u, Group GroupE, Confirmed germanyId, Confirmed ivoryCoastId, germanyVsIvoryCoast) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 33u)

        let ecuadorVsCuraçao = (2026, 06, 21, 00, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 34u, Group GroupE, Confirmed ecuadorId, Confirmed curaçaoId, ecuadorVsCuraçao) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 34u)

        let curaçaoVsIvoryCoast = (2026, 06, 25, 20, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 55u, Group GroupE, Confirmed curaçaoId, Confirmed ivoryCoastId, curaçaoVsIvoryCoast) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 55u)

        let ecuadorVsGermany = (2026, 06, 25, 20, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 56u, Group GroupE, Confirmed ecuadorId, Confirmed germanyId, ecuadorVsGermany) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 56u)

        // Group F
        let netherlandVsJapan = (2026, 06, 14, 20, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 11u, Group GroupF, Confirmed netherlandsId, Confirmed japanId, netherlandVsJapan) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 11u)

        let swedenVsTunisia = (2026, 06, 15, 02, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 12u, Group GroupF, Confirmed swedenId, Confirmed tunisiaId, swedenVsTunisia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 12u)

        let netherlandsVsSweden = (2026, 06, 20, 17, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 35u, Group GroupF, Confirmed netherlandsId, Confirmed swedenId, netherlandsVsSweden) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 35u)

        let tunisiaVsJapan = (2026, 06, 21, 04, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 36u, Group GroupF, Confirmed tunisiaId, Confirmed japanId, tunisiaVsJapan) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 36u)

        let japanVsSewden = (2026, 06, 25, 23, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 57u, Group GroupF, Confirmed japanId, Confirmed swedenId, japanVsSewden) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 57u)

        let tunisiaVsNetherlands = (2026, 06, 25, 23, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 58u, Group GroupF, Confirmed tunisiaId, Confirmed netherlandsId, tunisiaVsNetherlands) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 58u)

        // Group G
        let belgiumVsEgypt = (2026, 06, 15, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 16u, Group GroupG, Confirmed belgiumId, Confirmed egyptId, belgiumVsEgypt) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 16u)

        let iranVsNewZealand = (2026, 06, 16, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 15u, Group GroupG, Confirmed iranId, Confirmed newZealandId, iranVsNewZealand) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 15u)

        let belgiumVsIran = (2026, 06, 21, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 39u, Group GroupG, Confirmed belgiumId, Confirmed iranId, belgiumVsIran) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 39u)

        let newZealandVsEgypt = (2026, 06, 22, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 40u, Group GroupG, Confirmed newZealandId, Confirmed egyptId, newZealandVsEgypt) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 40u)

        let egyptVsIran = (2026, 06, 27, 03, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 63u, Group GroupG, Confirmed egyptId, Confirmed iranId, egyptVsIran) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 63u)

        let newZealandVsBelgium = (2026, 06, 27, 03, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 64u, Group GroupG, Confirmed newZealandId, Confirmed belgiumId, newZealandVsBelgium) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 64u)

        // Group H
        let spainVsCapeVerde = (2026, 06, 15, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 14u, Group GroupH, Confirmed spainId, Confirmed capeVerdeId, spainVsCapeVerde) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 14u)

        let saudiArabiaVsUruguay = (2026, 06, 15, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 13u, Group GroupH, Confirmed saudiArabiaId, Confirmed uruguayId, saudiArabiaVsUruguay) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 13u)

        let spainVsSaudiArabia = (2026, 06, 21, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 38u, Group GroupH, Confirmed spainId, Confirmed saudiArabiaId, spainVsSaudiArabia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 38u)

        let uruguayVsCapeVerde = (2026, 06, 21, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 37u, Group GroupH, Confirmed uruguayId, Confirmed capeVerdeId, uruguayVsCapeVerde) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 37u)

        let capeVerdeVsSaudiArabia = (2026, 06, 27, 00, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 65u, Group GroupH, Confirmed capeVerdeId, Confirmed saudiArabiaId, capeVerdeVsSaudiArabia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 65u)

        let uruguayVsSpain = (2026, 06, 27, 00, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 66u, Group GroupH, Confirmed uruguayId, Confirmed spainId, uruguayVsSpain) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 66u)

        // Group I
        let franceVsSenegal = (2026, 06, 16, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 17u, Group GroupI, Confirmed franceId, Confirmed senegalId, franceVsSenegal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 17u)

        let iraqVsNorway = (2026, 06, 16, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 18u, Group GroupI, Confirmed iraqId, Confirmed norwayId, iraqVsNorway) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 18u)

        let franceVsIraq = (2026, 06, 22, 21, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 42u, Group GroupI, Confirmed franceId, Confirmed iraqId, franceVsIraq) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 42u)

        let norwayVsSenegal = (2026, 06, 23, 00, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 41u, Group GroupI, Confirmed norwayId, Confirmed senegalId, norwayVsSenegal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 41u)

        let norwayVsFrance = (2026, 06, 26, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 61u, Group GroupI, Confirmed norwayId, Confirmed franceId, norwayVsFrance) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 61u)

        let senegalVsIraq = (2026, 06, 26, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 62u, Group GroupI, Confirmed senegalId, Confirmed iraqId, senegalVsIraq) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 62u)

        // Group J
        let argentinaVsAlgeria = (2026, 06, 17, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 19u, Group GroupJ, Confirmed argentinaId, Confirmed algeriaId, argentinaVsAlgeria) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 19u)

        let austriaVsJordan = (2026, 06, 17, 04, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 20u, Group GroupJ, Confirmed austriaId, Confirmed jordanId, austriaVsJordan) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 20u)

        let argentinaVsAustria = (2026, 06, 22, 17, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 43u, Group GroupJ, Confirmed argentinaId, Confirmed austriaId, argentinaVsAustria) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 43u)

        let jordanVsAlgeria = (2026, 06, 23, 03, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 44u, Group GroupJ, Confirmed jordanId, Confirmed algeriaId, jordanVsAlgeria) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 44u)

        let algeriaVsAustria = (2026, 06, 28, 02, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 69u, Group GroupJ, Confirmed algeriaId, Confirmed austriaId, algeriaVsAustria) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 69u)

        let jordanVsArgentina = (2026, 06, 28, 02, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 70u, Group GroupJ, Confirmed jordanId, Confirmed argentinaId, jordanVsArgentina) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 70u)

        // Group K
        let portugalVsDRCongo = (2026, 06, 17, 17, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 23u, Group GroupK, Confirmed portugalId, Confirmed drCongoId, portugalVsDRCongo) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 23u)

        let uzbekistanVsColombia = (2026, 06, 18, 02, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 24u, Group GroupK, Confirmed uzbekistanId, Confirmed colombiaId, uzbekistanVsColombia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 24u)

        let portugalVsUzbekistan = (2026, 06, 23, 17, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 47u, Group GroupK, Confirmed portugalId, Confirmed uzbekistanId, portugalVsUzbekistan) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 47u)

        let colombiaVsDRCongo = (2026, 06, 24, 02, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 48u, Group GroupK, Confirmed colombiaId, Confirmed drCongoId, colombiaVsDRCongo) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 48u)

        let colombiaVsPortugal = (2026, 06, 27, 23, 30) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 71u, Group GroupK, Confirmed colombiaId, Confirmed portugalId, colombiaVsPortugal) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 71u)

        let drCongoVsUzbekistan = (2026, 06, 27, 23, 30) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 72u, Group GroupK, Confirmed drCongoId, Confirmed uzbekistanId, drCongoVsUzbekistan) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 72u)

        // Group L
        let englandVsCroatia = (2026, 06, 17, 20, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 22u, Group GroupL, Confirmed englandId, Confirmed croatiaId, englandVsCroatia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 22u)

        let ghanaVsPanama = (2026, 06, 17, 23, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 21u, Group GroupL, Confirmed ghanaId, Confirmed panamaId, ghanaVsPanama) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 21u)

        let englandVsGhana = (2026, 06, 23, 20, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 45u, Group GroupL, Confirmed englandId, Confirmed ghanaId, englandVsGhana) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 45u)

        let panamaVsCroatia = (2026, 06, 23, 23, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 46u, Group GroupL, Confirmed panamaId, Confirmed croatiaId, panamaVsCroatia) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 46u)

        let panamaVsEngland = (2026, 06, 27, 21, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 67u, Group GroupL, Confirmed panamaId, Confirmed englandId, panamaVsEngland) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 67u)

        let croatiaVsGhana = (2026, 06, 27, 21, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 68u, Group GroupL, Confirmed croatiaId, Confirmed ghanaId, croatiaVsGhana) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 68u)

        // Round-of-32
        let runnerUpAVsRunnerUpBKO = (2026, 06, 28, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 73u, RoundOf32 73u, Unconfirmed (RunnerUp GroupA), Unconfirmed (RunnerUp GroupB), runnerUpAVsRunnerUpBKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 73u)

        let winnerCVsRunnerUpF = (2026, 06, 29, 17, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 76u, RoundOf32 76u, Unconfirmed (Winner (Group GroupC)), Unconfirmed (RunnerUp GroupF), winnerCVsRunnerUpF) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 76u)

        let winnerEVsThirdABCDF = (2026, 06, 29, 20, 30) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 74u, RoundOf32 74u, Unconfirmed (Winner (Group GroupE)), Unconfirmed (ThirdPlace [ GroupA ; GroupB ; GroupC ; GroupD ; GroupF ]), winnerEVsThirdABCDF) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 74u)

        let winnerFVsRunnerUpC = (2026, 06, 30, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 75u, RoundOf32 75u, Unconfirmed (Winner (Group GroupF)), Unconfirmed (RunnerUp GroupC), winnerFVsRunnerUpC) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 75u)

        let runnderUpEVsRunnerUpI = (2026, 06, 30, 17, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 78u, RoundOf32 78u, Unconfirmed (RunnerUp GroupE), Unconfirmed (RunnerUp GroupI), runnderUpEVsRunnerUpI) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 78u)

        let winnerIVsThirdCDFGH = (2026, 06, 30, 21, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 77u, RoundOf32 77u, Unconfirmed (Winner (Group GroupI)), Unconfirmed (ThirdPlace [ GroupC ; GroupD ; GroupF ; GroupG ; GroupH ]), winnerIVsThirdCDFGH) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 77u)

        let winnerAVsThirdCEFHI = (2026, 07, 01, 01, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 79u, RoundOf32 79u, Unconfirmed (Winner (Group GroupA)), Unconfirmed (ThirdPlace [ GroupC ; GroupE ; GroupF ; GroupH ; GroupI ]), winnerAVsThirdCEFHI) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 79u)

        let winnerLVsThirdEHIJK = (2026, 07, 01, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 80u, RoundOf32 80u, Unconfirmed (Winner (Group GroupL)), Unconfirmed (ThirdPlace [ GroupE ; GroupH ; GroupI ; GroupJ ; GroupK ]), winnerLVsThirdEHIJK) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 80u)

        let winnerGVsThirdAEHIJ = (2026, 07, 01, 20, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 82u, RoundOf32 82u, Unconfirmed (Winner (Group GroupG)), Unconfirmed (ThirdPlace [ GroupA ; GroupE ; GroupH ; GroupI ; GroupJ ]), winnerGVsThirdAEHIJ) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 82u)

        let winnerDVsThirdBEFIJ = (2026, 07, 02, 00, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 81u, RoundOf32 81u, Unconfirmed (Winner (Group GroupD)), Unconfirmed (ThirdPlace [ GroupB ; GroupE ; GroupF ; GroupI ; GroupJ ]), winnerDVsThirdBEFIJ) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 81u)

        let winnerHVsRunnerUpJ = (2026, 07, 02, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 84u, RoundOf32 84u, Unconfirmed (Winner (Group GroupH)), Unconfirmed (RunnerUp GroupJ), winnerHVsRunnerUpJ) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 84u)

        let runnerUpKVsRunnerUpL = (2026, 07, 02, 23, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 83u, RoundOf32 83u, Unconfirmed (RunnerUp GroupK), Unconfirmed (RunnerUp GroupL), runnerUpKVsRunnerUpL) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 83u)

        let winnerBVsThirdEFGIJ = (2026, 07, 03, 03, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 85u, RoundOf32 85u, Unconfirmed (Winner (Group GroupB)), Unconfirmed (ThirdPlace [ GroupE ; GroupF ; GroupG ; GroupI ; GroupJ ]), winnerBVsThirdEFGIJ) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 85u)

        let runnerUpDVsRunnerUpG = (2026, 07, 03, 18, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 88u, RoundOf32 88u, Unconfirmed (RunnerUp GroupD), Unconfirmed (RunnerUp GroupG), runnerUpDVsRunnerUpG) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 88u)

        let winnerJVsRunnerUpH = (2026, 07, 03, 22, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 86u, RoundOf32 86u, Unconfirmed (Winner (Group GroupJ)), Unconfirmed (RunnerUp GroupH), winnerJVsRunnerUpH) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 86u)

        let winnerKVsThirdDEIJL = (2026, 07, 04, 01, 30) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 87u, RoundOf32 87u, Unconfirmed (Winner (Group GroupK)), Unconfirmed (ThirdPlace [ GroupD ; GroupE ; GroupI ; GroupJ ; GroupL ]), winnerKVsThirdDEIJL) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 87u)

        (* TODO-2026...
        // Round-of-16
        let runnerUpAVsRunnerUpBKO = (2026, 07, 09, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 38u, RoundOf16 38u, Unconfirmed (RunnerUp GroupA), Unconfirmed (RunnerUp GroupB), runnerUpAVsRunnerUpBKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 38u)

        // Quarter-finals
        let winner39VsWinner37KO = (2026, 07, 05, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 46u, QuarterFinal 2u, Unconfirmed (Winner (RoundOf16 39u)), Unconfirmed (Winner (RoundOf16 37u)), winner39VsWinner37KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 45u)

        // Semi-finals
        let winnerQF1VsWinnerQF2KO = (2026, 07, 09, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 49u, SemiFinal 1u, Unconfirmed (Winner (QuarterFinal 1u)), Unconfirmed (Winner (QuarterFinal 2u)), winnerQF1VsWinnerQF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 49u)

        // Third place play-off
        let...

        // Final
        let winnerSF1VsWinnerSF2KO = (2026, 07, 14, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 51u, Final, Unconfirmed (Winner (SemiFinal 1u)), Unconfirmed (Winner (SemiFinal 2u)), winnerSF1VsWinnerSF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 51u)
        *)

        // Note: Reset Fixtures agent [to pendingOnFixturesEventsRead] so that it handles subsequent FixturesEventsRead event appropriately (i.e. from readPersistedEvents).
        "resetting Fixtures agent" |> Info |> log
        () |> fixtures.Reset
    return () }

let private createInitialDraftsEventsIfNecessary = async {
    let draftsDir = directory EntityType.Drafts

    // Force re-creation of initial Draft/s events if directory already exists (if requested).
    if deleteExistingDraftsEvents && Directory.Exists draftsDir then
        sprintf "deleting existing Draft/s events -> %s" draftsDir |> Info |> log
        delete draftsDir

    if Directory.Exists draftsDir then sprintf "preserving existing Draft/s events -> %s" draftsDir |> Info |> log
    else
        sprintf "creating initial Draft/s events -> %s" draftsDir |> Info |> log
        "starting Drafts agent" |> Info |> log
        () |> drafts.Start
        // Note: Send dummy OnSquadsRead | OnDraftsEventsRead | OnUserDraftsEventsRead to Drafts agent to ensure that it transitions [from pendingAllRead] to managingDrafts; otherwise HandleCreateDraftCmdAsync would be ignored (and block).
        "sending dummy OnSquadsRead | OnDraftsEventsRead | OnUserDraftsEventsRead to Drafts agent" |> Info |> log
        [] |> drafts.OnSquadsRead
        [] |> drafts.OnDraftsEventsRead
        [] |> drafts.OnUserDraftsEventsRead

        let draft1Id, draft1Ordinal = Guid "00000000-0000-0000-0000-000000000001" |> DraftId, DraftOrdinal 1
        let draft1Starts, draft1Ends = (2026, 05, 30, 07, 00) |> dateTimeOffsetUtc, (2026, 06, 06, 19, 00) |> dateTimeOffsetUtc
        let draft1Type = (draft1Starts, draft1Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft1Id, draft1Ordinal, draft1Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft1Id draft1Ordinal)

        let draft2Id, draft2Ordinal = Guid "00000000-0000-0000-0000-000000000002" |> DraftId, DraftOrdinal 2
        let draft2Starts, draft2Ends = (2026, 06, 06, 21, 00) |> dateTimeOffsetUtc, (2026, 06, 10, 19, 00) |> dateTimeOffsetUtc
        let draft2Type = (draft2Starts, draft2Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft2Id, draft2Ordinal, draft2Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft2Id draft2Ordinal)

        let draft3Id, draft3Ordinal = Guid "00000000-0000-0000-0000-000000000003" |> DraftId, DraftOrdinal 3
        let draft3Type = Unconstrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft3Id, draft3Ordinal, draft3Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft3Id draft3Ordinal)

        // Note: Reset Drafts agent [to pendingAllRead] so that it handles subsequent DraftsEventsRead event (&c.) appropriately (i.e. from readPersistedEvents).
        "resetting Drafts agent" |> Info |> log
        () |> drafts.Reset
    return () }

let createInitialPersistedEventsIfNecessary = async {
    "creating initial persisted events (if necessary)" |> Info |> log
    let previousLogFilter = () |> consoleLogger.CurrentLogFilter
    let customLogFilter = "createInitialPersistedEventsIfNecessary", function | Host -> allCategories | Entity _ -> allExceptVerbose | _ -> onlyWarningsAndWorse
    customLogFilter |> consoleLogger.ChangeLogFilter
    do! createInitialUsersEventsIfNecessary // note: although this can cause various events to be broadcast (UsersRead | UserEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialSquadsEventsIfNecessary // note: although this can cause various events to be broadcast (SquadsRead | SquadEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialFixturesEventsIfNecessary // note: although this can cause various events to be broadcast (FixturesRead | FixtureEventWritten | &c.), no agents should yet be subscribed to these
    do! createInitialDraftsEventsIfNecessary // note: although this can cause various events to be broadcast (DraftsRead | DraftEventWritten | &c.), no agents should yet be subscribed to these
    previousLogFilter |> consoleLogger.ChangeLogFilter }
