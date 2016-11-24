module RemoteStatus
    exposing
        ( Model
        , initial
        , start
        , enqueue
        , finishOne
        , isFinished
        )

{-| RemoteStatus is used to track the progress of multiple remote operations.

# Definition
@docs Model, initial

# Operations
@docs start, enqueue, finishOne

# Querying
@docs isFinished

-}


{-| Store the status of a remote operation.
-}
type Model
    = NotStarted
    | InProgress Int Int
    | Finished


{-| Default the status to not started.
-}
initial : Model
initial =
    NotStarted


{-| Start tracking the remote operation if it has not yet been started.
-}
start : Model -> Model
start status =
    if status == NotStarted then
        InProgress 0 0
    else
        status


{-| Determine whether the operation has completed.
-}
isFinished : Model -> Bool
isFinished status =
    status == Finished


{-| Increase the number of remote operations to track.
-}
enqueue : Model -> Int -> Model
enqueue status num =
    case status of
        InProgress trackingCount finishedCount ->
            InProgress (trackingCount + num) finishedCount

        NotStarted ->
            NotStarted

        Finished ->
            Finished


{-| Increase the number of operations that have been completed by 1.
-}
finishOne : Model -> Model
finishOne status =
    case status of
        InProgress trackingCount finishedCount ->
            if trackingCount + 1 == trackingCount then
                Finished
            else
                InProgress trackingCount (finishedCount + 1)

        NotStarted ->
            NotStarted

        Finished ->
            Finished
