module Aornota.Sweepstake2026.Ui.Common.Render.Markdown

open Aornota.Sweepstake2026.Common.Markdown
open Aornota.Sweepstake2026.Ui.Common.Marked
open Aornota.Sweepstake2026.Ui.Render.Bulma
open Aornota.Sweepstake2026.Ui.Render.Common
open Aornota.Sweepstake2026.Ui.Theme.Common

open Fable.React
open Fable.React.Props

type private DangerousInnerHtml = { __html : string }

let [<Literal>] private MARKDOWN_CLASS = "markdown"

let contentFromMarkdown' theme inNotification (Markdown markdown) =
    let (ThemeClass className) = theme.ThemeClass
    let customClasses = [
        yield MARKDOWN_CLASS
        if inNotification then yield sprintf "%s-in-notification" className else yield className ]
    let customClass = match customClasses with | _ :: _ -> ClassName (String.concat SPACE customClasses) |> Some | [] -> None
    content [
        div [
            match customClass with | Some customClass -> yield customClass :> IHTMLProp | None -> ()
            yield DangerouslySetInnerHTML { __html = Globals.marked.parse markdown } :> IHTMLProp ] [] ]

let contentFromMarkdown theme markdown = markdown |> contentFromMarkdown' theme false

let notificationContentFromMarkdown theme markdown = markdown |> contentFromMarkdown' theme true
