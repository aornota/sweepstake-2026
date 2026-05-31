module Aornota.Sweepstake2026.Common.Domain.News

open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Common.Revision

open System

type PostId = | PostId of guid : Guid with static member Create () = Guid.NewGuid () |> PostId

type PostDto = { PostId : PostId ; Rvn : Rvn ; UserId : UserId ; Message : Markdown ; Timestamp : DateTimeOffset }

let [<Literal>] private MAX_NEWS_POST_LENGTH = 2000

let validatePostMessage (Markdown messageText) =
    if String.IsNullOrWhiteSpace messageText then "Message must not be blank" |> Some
    else if (messageText.Trim ()).Length > MAX_NEWS_POST_LENGTH then "Message is too long" |> Some
    else None
