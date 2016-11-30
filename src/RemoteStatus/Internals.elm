module RemoteStatus.Internals exposing (..)

{-| This is an internal module not meant for general use. The internal types
and functions are more likely to change compared to the base `RemoteStatus`
module.

@docs Status

-}


{-| Store the current status as a union type, with queued/completed statuses
stored as Ints.
-}
type Status
    = NotStarted
    | InProgress Int Int
    | Finished
