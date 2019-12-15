module Aoc2 exposing (..)

import Array exposing (Array)


type alias State =
    Array Int


type Operation
    = Add
    | Multiply
    | Halt


fromOpCode : Int -> Maybe Operation
fromOpCode opCode =
    case opCode of
        1 ->
            Just Add

        2 ->
            Just Multiply

        99 ->
            Just Halt

        _ ->
            Nothing


findInputs : Int -> State -> Maybe Int
findInputs target state =
    findInputsTailRec 0 0 target state


findInputsTailRec : Int -> Int -> Int -> State -> Maybe Int
findInputsTailRec noun verb target state =
    if solve noun verb state == Just target then
        Just (100 * noun + verb)

    else
        case ( noun, verb ) of
            ( 99, 99 ) ->
                Nothing

            ( _, 99 ) ->
                findInputsTailRec (noun + 1) 0 target state

            _ ->
                findInputsTailRec noun (verb + 1) target state


solve : Int -> Int -> State -> Maybe Int
solve noun verb state =
    Array.set 2 verb (Array.set 1 noun state)
        |> execute
        |> Maybe.andThen (Array.get 0)


execute : State -> Maybe State
execute =
    step 0


step : Int -> State -> Maybe State
step cursor state =
    case Maybe.andThen fromOpCode (Array.get cursor state) of
        Nothing ->
            Nothing

        Just Add ->
            stepBinaryOp (+) cursor state
                |> Maybe.andThen (step (cursor + 4))

        Just Multiply ->
            stepBinaryOp (*) cursor state
                |> Maybe.andThen (step (cursor + 4))

        Just Halt ->
            Just state


stepBinaryOp : (Int -> Int -> Int) -> Int -> State -> Maybe State
stepBinaryOp op cursor state =
    Maybe.map3 (\a b posReturn -> Array.set posReturn (op a b) state)
        (getOperand (cursor + 1) state)
        (getOperand (cursor + 2) state)
        (Array.get (cursor + 3) state)


getOperand : Int -> State -> Maybe Int
getOperand pos state =
    Array.get pos state
        |> Maybe.andThen (\newPos -> Array.get newPos state)



-- Example programs ############################################################


program1 =
    Array.fromList [ 1, 0, 0, 0, 99 ]


program2 =
    Array.fromList [ 2, 3, 0, 3, 99 ]


program3 =
    Array.fromList [ 2, 4, 4, 5, 99, 0 ]


program4 =
    Array.fromList [ 1, 1, 1, 4, 99, 5, 6, 0, 99 ]


input =
    Array.fromList [ 1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 1, 6, 19, 1, 19, 6, 23, 2, 23, 6, 27, 2, 6, 27, 31, 2, 13, 31, 35, 1, 9, 35, 39, 2, 10, 39, 43, 1, 6, 43, 47, 1, 13, 47, 51, 2, 6, 51, 55, 2, 55, 6, 59, 1, 59, 5, 63, 2, 9, 63, 67, 1, 5, 67, 71, 2, 10, 71, 75, 1, 6, 75, 79, 1, 79, 5, 83, 2, 83, 10, 87, 1, 9, 87, 91, 1, 5, 91, 95, 1, 95, 6, 99, 2, 10, 99, 103, 1, 5, 103, 107, 1, 107, 6, 111, 1, 5, 111, 115, 2, 115, 6, 119, 1, 119, 6, 123, 1, 123, 10, 127, 1, 127, 13, 131, 1, 131, 2, 135, 1, 135, 5, 0, 99, 2, 14, 0, 0 ]
