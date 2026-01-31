module Aornota.Sweepstake2026.Server.Jwt

open Aornota.Sweepstake2026.Common.Domain.User
open Aornota.Sweepstake2026.Common.Json
open Aornota.Sweepstake2026.Server.Common.JsonConverter

open System.IO
open System.Security.Cryptography

open Jose

let [<Literal>] private JWT_KEY_FILE = "./secret/jwt.txt"

let private jwtKey =
    let file = FileInfo JWT_KEY_FILE
    if file.Exists |> not then
        if file.Directory.Exists |> not then file.Directory.Create ()
        let bytes : byte [] = Array.zeroCreate 32
        (RandomNumberGenerator.Create ()).GetBytes bytes
        File.WriteAllBytes (file.FullName, bytes)
    File.ReadAllBytes file.FullName

let private encode (Json json) = JWT.Encode (json, jwtKey, JweAlgorithm.A256KW, JweEncryption.A256CBC_HS512)
let private decode text = JWT.Decode (text, jwtKey, JweAlgorithm.A256KW, JweEncryption.A256CBC_HS512) |> Json

let toJwt (userId:UserId, permissions:Permissions) =
    try
        (userId, permissions) |> toJson |> encode |> Jwt |> Ok
    with | exn -> exn.Message |> Error

let fromJwt (Jwt jwt) =
    try
        let userId, permissions = jwt |> decode |> fromJson<UserId * Permissions>
        (userId, permissions) |> Ok
    with | exn -> exn.Message |> Error
