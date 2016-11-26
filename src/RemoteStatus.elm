module RemoteStatus
    exposing
        ( Model
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
@docs Model, initial

# Operations
@docs start, enqueue, finishOne

# Querying
@docs isInProgress, isFinished, percentageCompleted

-}


{-| Store the status of a remote operation, differentiating between unstarted,
in progress and completed states.
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


{-| Start tracking the remote operation if the status is in it's initial state.
-}
start : Model -> Model
start status =
    if status == NotStarted then
        InProgress 0 0
    else
        status


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
isInProgress : Model -> Bool
isInProgress status =
    case status of
        InProgress _ _ ->
            True

        _ ->
            False


{-| Determine whether the operations have completed.
-}
isFinished : Model -> Bool
isFinished status =
    status == Finished


{-| Determine the percentage of completed operations if operations are in progress.
-}
percentageCompleted : Model -> Maybe Int
percentageCompleted status =
    case status of
        InProgress totalCount finishedCount ->
            Just << floor <| (toFloat finishedCount / toFloat totalCount) * 100

        _ ->
            Nothing
