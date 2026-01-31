module Aornota.Sweepstake2026.Common.UnexpectedError

let [<Literal>] UNEXPECTED_ERROR = "An unexpected error has occurred"

let unexpectedErrorWhen text = sprintf "An unexpected error has occurred when %s" text
