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
let private deleteExistingFixturesEvents = ifDebug false false // note: should *not* generally set to true for Release (and only with caution for Debug!)
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

let private germanyId = Guid "00000011-0000-0000-0000-000000000000" |> SquadId
let private hungaryId = Guid "00000012-0000-0000-0000-000000000000" |> SquadId
let private scotlandId = Guid "00000013-0000-0000-0000-000000000000" |> SquadId
let private switzerlandId = Guid "00000014-0000-0000-0000-000000000000" |> SquadId

let private albaniaId = Guid "00000021-0000-0000-0000-000000000000" |> SquadId
let private croatiaId = Guid "00000022-0000-0000-0000-000000000000" |> SquadId
let private italyId = Guid "00000023-0000-0000-0000-000000000000" |> SquadId
let private spainId = Guid "00000024-0000-0000-0000-000000000000" |> SquadId

let private denmarkId = Guid "00000031-0000-0000-0000-000000000000" |> SquadId
let private englandId = Guid "00000032-0000-0000-0000-000000000000" |> SquadId
let private serbiaId = Guid "00000033-0000-0000-0000-000000000000" |> SquadId
let private sloveniaId = Guid "00000034-0000-0000-0000-000000000000" |> SquadId

let private austriaId = Guid "00000041-0000-0000-0000-000000000000" |> SquadId
let private franceId = Guid "00000042-0000-0000-0000-000000000000" |> SquadId
let private netherlandsId = Guid "00000043-0000-0000-0000-000000000000" |> SquadId
let private polandId = Guid "00000044-0000-0000-0000-000000000000" |> SquadId

let private belgiumId = Guid "00000051-0000-0000-0000-000000000000" |> SquadId
let private romaniaId = Guid "00000052-0000-0000-0000-000000000000" |> SquadId
let private slovakiaId = Guid "00000053-0000-0000-0000-000000000000" |> SquadId
let private ukraineId = Guid "00000054-0000-0000-0000-000000000000" |> SquadId

let private czechRepublicId = Guid "00000061-0000-0000-0000-000000000000" |> SquadId
let private georgiaId = Guid "00000062-0000-0000-0000-000000000000" |> SquadId
let private portugalId = Guid "00000063-0000-0000-0000-000000000000" |> SquadId
let private turkeyId = Guid "00000064-0000-0000-0000-000000000000" |> SquadId

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
        let germany = SquadName "Germany"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, germanyId, germany, GroupA, Some (Seeding 1), CoachName "Julian Nagelsmann") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" germany)
        let hungary = SquadName "Hungary"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, hungaryId, hungary, GroupA, Some (Seeding 7), CoachName "Marco Rossi") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" hungary)
        let scotland = SquadName "Scotland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, scotlandId, scotland, GroupA, None, CoachName "Steve Clarke") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" scotland)
        let switzerland = SquadName "Switzerland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, switzerlandId, switzerland, GroupA, None, CoachName "Murat Yakin") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" switzerland)

        // #Group B
        let albania = SquadName "Albania"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, albaniaId, albania, GroupB, Some (Seeding 11), CoachName "Sylvinho") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" albania)
        let croatia = SquadName "Croatia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, croatiaId, croatia, GroupB, None, CoachName "Zlatko Dalić") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" croatia)
        let italy = SquadName "Italy"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, italyId, italy, GroupB, None, CoachName "Luciano Spalletti") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" italy)
        let spain = SquadName "Spain"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, spainId, spain, GroupB, Some (Seeding 4), CoachName "Luis de la Fuente") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" spain)

        // Group C
        let denmark = SquadName "Denmark"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, denmarkId, denmark, GroupC, Some (Seeding 10), CoachName "Kasper Hjulmand") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" denmark)
        let england = SquadName "England"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, englandId, england, GroupC, Some (Seeding 6), CoachName "Gareth Southgate") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateUseHandleCreateSquadCmdAsyncrCmdAsync (%A)" england)
        let serbia = SquadName "Serbia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, serbiaId, serbia, GroupC, None, CoachName "Dragan Stojković") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" serbia)
        let slovenia = SquadName "Slovenia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, sloveniaId, slovenia, GroupC, None, CoachName "Matjaž Kek") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" slovenia)

        // Group D
        let austria = SquadName "Austria"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, austriaId, austria, GroupD, Some (Seeding 12), CoachName "Ralf Rangnick") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" austria)
        let france = SquadName "France"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, franceId, france, GroupD, Some (Seeding 3), CoachName "Didier Deschamps") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" france)
        let netherlands = SquadName "Netherlands"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, netherlandsId, netherlands, GroupD, None, CoachName "Ronald Koeman") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" netherlands)
        let poland = SquadName "Poland"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, polandId, poland, GroupD, None, CoachName "Michał Probierz") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" poland)

        // Group E
        let belgium = SquadName "Belgium"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, belgiumId, belgium, GroupE, Some (Seeding 5), CoachName "Domenico Tedesco") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" belgium)
        let romania = SquadName "Romania"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, romaniaId, romania, GroupE, Some (Seeding 9), CoachName "Edward Iordănescu") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" romania)
        let slovakia = SquadName "Slovakia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, slovakiaId, slovakia, GroupE, None, CoachName "Francesco Calzona") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" slovakia)
        let ukraine = SquadName "Ukraine"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, ukraineId, ukraine, GroupE, None, CoachName "Serhiy Rebrov") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" ukraine)

        // Group F
        let czechRepublic = SquadName "Czech Republic"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, czechRepublicId, czechRepublic, GroupF, None, CoachName "Ivan Hašek") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" czechRepublic)
        let georgia = SquadName "Georgia"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, georgiaId, georgia, GroupF, None, CoachName "Willy Sagnol") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" georgia)
        let portugal = SquadName "Portugal"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, portugalId, portugal, GroupF, Some (Seeding 2), CoachName "Roberto Martínez") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" portugal)
        let turkey = SquadName "Turkey"
        let! result = nephTokens.CreateSquadToken |> ifToken (fun token -> (token, nephId, turkeyId, turkey, GroupF, Some (Seeding 8), CoachName "Vincenzo Montella") |> squads.HandleCreateSquadCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateSquadCmdAsync (%A)" turkey)

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
        let germnayVsScotlandKO = (2024, 06, 14, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 1u, Group GroupA, Confirmed germanyId, Confirmed scotlandId, germnayVsScotlandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 1u)

        let hungaryVsSwitzerlandKO = (2024, 06, 15, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 2u, Group GroupA, Confirmed hungaryId, Confirmed switzerlandId, hungaryVsSwitzerlandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 2u)

        let germanyVsHungaryKO = (2024, 06, 19, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 13u, Group GroupA, Confirmed germanyId, Confirmed hungaryId, germanyVsHungaryKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 14u)

        let scotlandVsSwitzerlandKO = (2024, 06, 19, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 14u, Group GroupA, Confirmed scotlandId, Confirmed switzerlandId, scotlandVsSwitzerlandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 13u)

        let switzerlandVsGermanyKO = (2024, 06, 23, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 25u, Group GroupA, Confirmed switzerlandId, Confirmed germanyId, switzerlandVsGermanyKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 25u)

        let scotlandVsHungaryKO = (2024, 06, 23, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 26u, Group GroupA, Confirmed scotlandId, Confirmed hungaryId, scotlandVsHungaryKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 26u)

        // Group B
        let spainVsCroatiaKO = (2024, 06, 15, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 3u, Group GroupB, Confirmed spainId, Confirmed croatiaId, spainVsCroatiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 3u)

        let italyVsAlbaniaKO = (2024, 06, 15, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 4u, Group GroupB, Confirmed italyId, Confirmed albaniaId, italyVsAlbaniaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 4u)

        let croatiaVsAlbaniaKO = (2024, 06, 19, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 15u, Group GroupB, Confirmed croatiaId, Confirmed albaniaId, croatiaVsAlbaniaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 15u)

        let spainVsItalyKO = (2024, 06, 20, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 16u, Group GroupB, Confirmed spainId, Confirmed italyId, spainVsItalyKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 16u)

        let albaniaVsSpainKO = (2024, 06, 24, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 27u, Group GroupB, Confirmed albaniaId, Confirmed spainId, albaniaVsSpainKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 27u)

        let croatiaVsItalyKO = (2024, 06, 24, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 28u, Group GroupB, Confirmed croatiaId, Confirmed italyId, croatiaVsItalyKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 28u)

        // Group C
        let sloveniaVsDenmarkKO = (2024, 06, 16, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 6u, Group GroupC, Confirmed sloveniaId, Confirmed denmarkId, sloveniaVsDenmarkKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 6u)

        let serbiaVsEnglandKO = (2024, 06, 16, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 5u, Group GroupC, Confirmed serbiaId, Confirmed englandId, serbiaVsEnglandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 5u)

        let sloveniaVsSerbiaKO = (2024, 06, 20, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 18u, Group GroupC, Confirmed sloveniaId, Confirmed serbiaId, sloveniaVsSerbiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 18u)

        let denmarkVsEnglandKO = (2024, 06, 20, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 17u, Group GroupC, Confirmed denmarkId, Confirmed englandId, denmarkVsEnglandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 17u)

        let englandVsSloveniaKO = (2024, 06, 25, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 29u, Group GroupC, Confirmed englandId, Confirmed sloveniaId, englandVsSloveniaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 29u)

        let denmarkVsSerbiaKO = (2024, 06, 25, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 30u, Group GroupC, Confirmed denmarkId, Confirmed serbiaId, denmarkVsSerbiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 30u)

        // Group D
        let polandVsNetherlandsKO = (2024, 06, 16, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 7u, Group GroupD, Confirmed polandId, Confirmed netherlandsId, polandVsNetherlandsKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 7u)

        let austriaVsFranceKO = (2024, 06, 17, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 8u, Group GroupD, Confirmed austriaId, Confirmed franceId, austriaVsFranceKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 8u)

        let polandVsAustriaKO = (2024, 06, 21, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 19u, Group GroupD, Confirmed polandId, Confirmed austriaId, polandVsAustriaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 19u)

        let netherlandsVsFranceKO = (2024, 06, 21, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 20u, Group GroupD, Confirmed netherlandsId, Confirmed franceId, netherlandsVsFranceKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 20u)

        let netherlandsVsAustriaKO = (2024, 06, 25, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 31u, Group GroupD, Confirmed netherlandsId, Confirmed austriaId, netherlandsVsAustriaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 31u)

        let franceVsPolandKO = (2024, 06, 25, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 32u, Group GroupD, Confirmed franceId, Confirmed polandId, franceVsPolandKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 32u)

        // Group E
        let romaniaVsUkraineKO = (2024, 06, 17, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 10u, Group GroupE, Confirmed romaniaId, Confirmed ukraineId, romaniaVsUkraineKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 10u)

        let belgiumVsSlovakiaKO = (2024, 06, 17, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 9u, Group GroupE, Confirmed belgiumId, Confirmed slovakiaId, belgiumVsSlovakiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 9u)

        let slovakieVsUkraineKO = (2024, 06, 21, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 21u, Group GroupE, Confirmed slovakiaId, Confirmed ukraineId, slovakieVsUkraineKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 21u)

        let belgiumVsRomaniaKO = (2024, 06, 22, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 22u, Group GroupE, Confirmed belgiumId, Confirmed romaniaId, belgiumVsRomaniaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 22u)

        let slovakiaVsRomaniaKO = (2024, 06, 26, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 33u, Group GroupE, Confirmed slovakiaId, Confirmed romaniaId, slovakiaVsRomaniaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 33u)

        let ukraineVsBelgiumKO = (2024, 06, 26, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 34u, Group GroupE, Confirmed ukraineId, Confirmed belgiumId, ukraineVsBelgiumKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 34u)

        // Group F
        let turkeyVsGeorgiaKO = (2024, 06, 18, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 11u, Group GroupF, Confirmed turkeyId, Confirmed georgiaId, turkeyVsGeorgiaKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 11u)

        let portugalVsCzechRepublicKO = (2024, 06, 18, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 12u, Group GroupF, Confirmed portugalId, Confirmed czechRepublicId, portugalVsCzechRepublicKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 12u)

        let georgiaVsCzechRepublicKO = (2024, 06, 22, 13, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 23u, Group GroupF, Confirmed georgiaId, Confirmed czechRepublicId, georgiaVsCzechRepublicKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 24u)

        let turkeyVsPortugalKO = (2024, 06, 22, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 24u, Group GroupF, Confirmed turkeyId, Confirmed portugalId, turkeyVsPortugalKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 23u)

        let georgiaVsPortugalKO = (2024, 06, 26, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 35u, Group GroupF, Confirmed georgiaId, Confirmed portugalId, georgiaVsPortugalKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 35u)

        let czexhRepublicVsTurkeyKO = (2024, 06, 26, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 36u, Group GroupF, Confirmed czechRepublicId, Confirmed turkeyId, czexhRepublicVsTurkeyKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 36u)

        // Round-of-16
        let runnerUpAVsRunnerUpBKO = (2024, 06, 29, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 38u, RoundOf16 38u, Unconfirmed (RunnerUp GroupA), Unconfirmed (RunnerUp GroupB), runnerUpAVsRunnerUpBKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 38u)

        let winnerAVsRunnerUpCKO = (2024, 06, 29, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 37u, RoundOf16 37u, Unconfirmed (Winner (Group GroupA)), Unconfirmed (RunnerUp GroupC), winnerAVsRunnerUpCKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 37u)

        let winnerCVsThirdPlaceDEFKO = (2024, 06, 30, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 40u, RoundOf16 40u, Unconfirmed (Winner (Group GroupC)), Unconfirmed (ThirdPlace [ GroupD ; GroupE ; GroupF ]), winnerCVsThirdPlaceDEFKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 40u)

        let winnerBVsThirdPlaceADEFKO = (2024, 06, 30, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 39u, RoundOf16 39u, Unconfirmed (Winner (Group GroupB)), Unconfirmed (ThirdPlace [ GroupA ; GroupD ; GroupE ; GroupF ]), winnerBVsThirdPlaceADEFKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 39u)

        let runnerUpDVsRunnerUpEKO = (2024, 07, 01, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 42u, RoundOf16 42u, Unconfirmed (RunnerUp GroupD), Unconfirmed (RunnerUp GroupE), runnerUpDVsRunnerUpEKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 42u)

        let winnerFVsThirdPlaceABCKO = (2024, 07, 01, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 41u, RoundOf16 41u, Unconfirmed (Winner (Group GroupF)), Unconfirmed (ThirdPlace [ GroupA ; GroupB ; GroupC ]), winnerFVsThirdPlaceABCKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 41u)

        let winnerEVsThirdPlaceABCDKO = (2024, 07, 02, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 43u, RoundOf16 43u, Unconfirmed (Winner (Group GroupE)), Unconfirmed (ThirdPlace [ GroupA ; GroupB ; GroupC ; GroupD ]), winnerEVsThirdPlaceABCDKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 44u)

        let winnerDVsRunnerUpFKO = (2024, 07, 02, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 44u, RoundOf16 44u, Unconfirmed (Winner (Group GroupD)), Unconfirmed (RunnerUp GroupF), winnerDVsRunnerUpFKO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 43u)

        // Quarter-finals
        let winner39VsWinner37KO = (2024, 07, 05, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 46u, QuarterFinal 2u, Unconfirmed (Winner (RoundOf16 39u)), Unconfirmed (Winner (RoundOf16 37u)), winner39VsWinner37KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 45u)

        let winner41VsWinner42KO = (2024, 07, 05, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 45u, QuarterFinal 1u, Unconfirmed (Winner (RoundOf16 41u)), Unconfirmed (Winner (RoundOf16 42u)), winner41VsWinner42KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 46u)

        let winner40VsWinner38KO = (2024, 07, 06, 16, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 47u, QuarterFinal 3u, Unconfirmed (Winner (RoundOf16 40u)), Unconfirmed (Winner (RoundOf16 38u)), winner40VsWinner38KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 48u)

        let winner43VsWinner44KO = (2024, 07, 06, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 48u, QuarterFinal 4u, Unconfirmed (Winner (RoundOf16 43u)), Unconfirmed (Winner (RoundOf16 44u)), winner43VsWinner44KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 47u)

        // Semi-finals
        let winnerQF1VsWinnerQF2KO = (2024, 07, 09, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 49u, SemiFinal 1u, Unconfirmed (Winner (QuarterFinal 1u)), Unconfirmed (Winner (QuarterFinal 2u)), winnerQF1VsWinnerQF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 49u)

        let winnerQF3VsWinnerQF4KO = (2024, 07, 10, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 50u, SemiFinal 2u, Unconfirmed (Winner (QuarterFinal 3u)), Unconfirmed (Winner (QuarterFinal 4u)), winnerQF3VsWinnerQF4KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 50u)

        // Final
        let winnerSF1VsWinnerSF2KO = (2024, 07, 14, 19, 00) |> dateTimeOffsetUtc
        let! result = nephTokens.CreateFixtureToken |> ifToken (fun token -> (token, nephId, fixtureId 51u, Final, Unconfirmed (Winner (SemiFinal 1u)), Unconfirmed (Winner (SemiFinal 2u)), winnerSF1VsWinnerSF2KO) |> fixtures.HandleCreateFixtureCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateFixtureCmdAsync (match %i)" 51u)

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
        let draft1Starts, draft1Ends = (2024, 06, 05, 07, 00) |> dateTimeOffsetUtc, (2024, 06, 09, 19, 00) |> dateTimeOffsetUtc
        let draft1Type = (draft1Starts, draft1Ends) |> Constrained
        let! result = nephTokens.ProcessDraftToken |> ifToken (fun token -> (token, nephId, draft1Id, draft1Ordinal, draft1Type) |> drafts.HandleCreateDraftCmdAsync)
        result |> logShouldSucceed (sprintf "HandleCreateDraftCmdAsync (%A %A)" draft1Id draft1Ordinal)

        let draft2Id, draft2Ordinal = Guid "00000000-0000-0000-0000-000000000002" |> DraftId, DraftOrdinal 2
        let draft2Starts, draft2Ends = (2024, 06, 09, 21, 00) |> dateTimeOffsetUtc, (2024, 06, 12, 19, 00) |> dateTimeOffsetUtc
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
