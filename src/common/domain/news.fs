module Aornota.Sweepstake2026.Common.Domain.News

open Aornota.Sweepstake2026.Common.Domain.Fixture
open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision

open System

type PostId = | PostId of guid : Guid with static member Create () = Guid.NewGuid () |> PostId

type PostType =
    | Standard
    | MatchResult of fixtureId : FixtureId

type PostTypeDto =
    | StandardDto of messageText : Markdown
    | MatchResultDto of messageText : Markdown * fixtureId : FixtureId

type PostDto = { PostId : PostId ; Rvn : Rvn ; UserId : UserId ; PostTypeDto : PostTypeDto ; Timestamp : DateTimeOffset }

let [<Literal>] private MAX_NEWS_POST_LENGTH = 2000

let validatePostMessageText (Markdown messageText) =
    if String.IsNullOrWhiteSpace messageText then "News post must not be blank" |> Some
    else if (messageText.Trim ()).Length > MAX_NEWS_POST_LENGTH then "News post is too long" |> Some
    else None
