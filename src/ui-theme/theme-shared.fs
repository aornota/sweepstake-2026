module Aornota.Sweepstake2026.Ui.Theme.Shared

open Aornota.Sweepstake2026.Ui.Theme.Light
open Aornota.Sweepstake2026.Ui.Theme.Dark

let getTheme useDefaultTheme = if useDefaultTheme then themeLight else themeDark
