module Aornota.Sweepstake2026.Server.Events.UserEvents

open Aornota.Sweepstake2026.Common.Domain.User

type Salt = | Salt of string
type Hash = | Hash of string

type UserEvent =
    | UserCreated of userId : UserId * userName : UserName * passwordSalt : Salt * passwordHash : Hash * userType : UserType
    | PasswordChanged of userId : UserId * passwordSalt : Salt * passwordHash : Hash
    | PasswordReset of userId : UserId * passwordSalt : Salt * passwordHash : Hash
    | UserTypeChanged of userId : UserId * userType : UserType
    with
        member self.UserId =
            match self with
            | UserCreated (userId, _, _, _, _) -> userId
            | PasswordChanged (userId, _, _) -> userId
            | PasswordReset (userId, _, _) -> userId
            | UserTypeChanged (userId, _) -> userId
