module Aornota.Sweepstake2026.Common.Json

type Json = Json of json : string

let [<Literal>] SPACE_COUNT = 0 // note: need compact serialization because persistence requires that each serialized event is a single line
