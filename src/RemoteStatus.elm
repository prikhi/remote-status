module RemoteStatus
    exposing
        ( Status
        , initial
        , start
        , enqueue
        , finishOne
        , isInProgress
        , isFinished
        , percentageCompleted
        )

{-| RemoteStatus is used to track the progress of multiple remote operations.

# Definition
@docs Status, initial

# Operations
@docs start, enqueue, finishOne

# Querying
@docs isInProgress, isFinished, percentageCompleted

-}


{-| Store the status of a remote operation, differentiating between unstarted,
in progress and completed states.
-}
type Status
    = NotStarted
    | InProgress Int Int
    | Finished


{-| Default the status to not started.
-}
initial : Status
initial =
    NotStarted


{-| Start tracking the remote operation if the status is in it's initial state.
-}
start : Status -> Status
start status =
    if status == NotStarted then
        InProgress 0 0
    else
        status


{-| Increase the number of remote operations to track.
-}
enqueue : Status -> Int -> Status
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
finishOne : Status -> Status
finishOne status =
    case status of
        InProgress trackingCount finishedCount ->
            if finishedCount + 1 == trackingCount then
                Finished
            else
                InProgress trackingCount (finishedCount + 1)

        NotStarted ->
            NotStarted

        Finished ->
            Finished


{-| Determine whether the operations are still in progress.
-}
isInProgress : Status -> Bool
isInProgress status =
    case status of
        InProgress _ _ ->
            True

        _ ->
            False


{-| Determine whether the operations have completed.
-}
isFinished : Status -> Bool
isFinished status =
    status == Finished


{-| Determine the percentage of completed operations if operations are in progress.
-}
percentageCompleted : Status -> Maybe Int
percentageCompleted status =
    case status of
        InProgress totalCount finishedCount ->
            Just << floor <| (toFloat finishedCount / toFloat totalCount) * 100

        _ ->
            Nothing
