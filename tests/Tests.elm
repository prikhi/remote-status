module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (int)
import RemoteStatus exposing (..)
import RemoteStatus.Internals exposing (..)


all : Test
all =
    describe "RemoteStatus"
        [ describe "initial"
            [ test "is equal to NotStarted" <|
                \() ->
                    Expect.equal initial NotStarted
            ]
        , describe "start"
            [ test "progresses NotStarted statuses correctly" <|
                \() ->
                    Expect.equal (start NotStarted) (InProgress 0 0)
            , fuzz2 int int "does nothing to InProgress statuses" <|
                \total saved ->
                    Expect.equal (start <| InProgress total saved)
                        (InProgress total saved)
            , test "does nothing to Finished statuses" <|
                \() ->
                    Expect.equal (start Finished) Finished
            ]
        , describe "enqueue"
            [ fuzz3 int int int "increases InProgress statuses tracking count" <|
                \total saved extra ->
                    Expect.equal (enqueue (InProgress total saved) extra)
                        (InProgress (total + extra) saved)
            , fuzz int "does nothing to NotStarted statuses" <|
                \extra ->
                    Expect.equal (enqueue NotStarted extra) NotStarted
            , fuzz int "does nothing to Finished statuses" <|
                \extra ->
                    Expect.equal (enqueue Finished extra) Finished
            ]
        , describe "finishOne"
            [ fuzz int "increases InProgress statuses by one" <|
                \saved ->
                    Expect.equal (finishOne <| InProgress (saved + 2) saved) <|
                        InProgress (saved + 2) (saved + 1)
            , fuzz int "completes finished InProgress statuses" <|
                \total ->
                    Expect.equal (finishOne <| InProgress total (total - 1))
                        Finished
            , test "does nothing to NotStarted statuses" <|
                \() ->
                    Expect.equal (finishOne NotStarted) NotStarted
            , test "does nothing to Finished statuses" <|
                \() ->
                    Expect.equal (finishOne Finished) Finished
            ]
        , describe "isInProgress"
            [ fuzz2 int int "is True for InProgress statuses" <|
                \total saved ->
                    Expect.true "Expected in InProgress to return True."
                        (isInProgress <| InProgress total saved)
            , test "is False for NotStarted statuses" <|
                \() ->
                    Expect.false "Expected NotStarted to return False."
                        (isInProgress NotStarted)
            , test "is False for Finished statuses" <|
                \() ->
                    Expect.false "Expected Finished to return False."
                        (isInProgress Finished)
            ]
        , describe "isFinished"
            [ test "is True for Finished statuses" <|
                \() ->
                    Expect.true "Expected Finished to return True."
                        (isFinished Finished)
            , test "is False for NotStarted statuses" <|
                \() ->
                    Expect.false "Expected NotStarted to return False."
                        (isFinished NotStarted)
            , fuzz2 int int "is False for InProgress statuses" <|
                \total saved ->
                    Expect.false "Expected InProgress to return False."
                        (isFinished <| InProgress total saved)
            ]
        , describe "percentageCompleted"
            [ fuzz2 int int "returns the correct percentage for InProgress statuses" <|
                \extra saved ->
                    Expect.equal (percentageCompleted <| InProgress (saved + extra) saved)
                        (Just << floor <| toFloat saved / toFloat (saved + extra) * 100)
            , test "returns Nothing for NotStarted statuses" <|
                \() -> Expect.equal (percentageCompleted NotStarted) Nothing
            , test "returns Nothing for Finished statuses" <|
                \() -> Expect.equal (percentageCompleted Finished) Nothing
            ]
        ]
