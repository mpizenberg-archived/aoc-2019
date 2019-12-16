module Aoc5 exposing (..)

import Array exposing (Array)


type alias Computer =
    { input : Int
    , output : Int
    , memory : Memory
    , pointer : Int
    }


type alias Memory =
    Array Int


type Operation
    = Add Mode Mode
    | Multiply Mode Mode
    | Input
    | Output Mode
    | JumpIf Bool Mode Mode
    | LessThan Mode Mode
    | Equals Mode Mode
    | Halt


type Mode
    = Position
    | Immediate


fromOpCode : Int -> Maybe Operation
fromOpCode opCode =
    case opCodeDecomposition opCode of
        1 :: m1 :: m2 :: _ ->
            Just (Add (opMode m1) (opMode m2))

        2 :: m1 :: m2 :: _ ->
            Just (Multiply (opMode m1) (opMode m2))

        3 :: _ ->
            Just Input

        4 :: m1 :: _ ->
            Just (Output (opMode m1))

        5 :: m1 :: m2 :: _ ->
            Just (JumpIf True (opMode m1) (opMode m2))

        6 :: m1 :: m2 :: _ ->
            Just (JumpIf False (opMode m1) (opMode m2))

        7 :: m1 :: m2 :: _ ->
            Just (LessThan (opMode m1) (opMode m2))

        8 :: m1 :: m2 :: _ ->
            Just (Equals (opMode m1) (opMode m2))

        99 :: _ ->
            Just Halt

        _ ->
            Nothing


opMode : Int -> Mode
opMode m =
    case m of
        0 ->
            Position

        _ ->
            Immediate


opCodeDecomposition : Int -> List Int
opCodeDecomposition code =
    let
        operation =
            modBy 100 code

        quotient =
            code // 100
    in
    operation :: List.reverse (opCodeDecompositionAcc quotient 3 [])


opCodeDecompositionAcc : Int -> Int -> List Int -> List Int
opCodeDecompositionAcc code nbArg acc =
    case nbArg of
        0 ->
            acc

        _ ->
            let
                paramMode =
                    modBy 10 code

                quotient =
                    code // 10
            in
            opCodeDecompositionAcc quotient (nbArg - 1) (paramMode :: acc)


execute : Int -> Memory -> Maybe Computer
execute input0 program =
    step
        { input = input0
        , output = 0
        , memory = program
        , pointer = 0
        }


step : Computer -> Maybe Computer
step computer =
    case Maybe.andThen fromOpCode (Array.get computer.pointer computer.memory) of
        Nothing ->
            Nothing

        Just Input ->
            -- a little verbose but I want tail call optimization
            -- which would not happen with Maybe.andThen
            case stepInput computer of
                Nothing ->
                    Nothing

                Just newComputer ->
                    step newComputer

        Just (Output m1) ->
            case stepOutput m1 computer of
                Nothing ->
                    Nothing

                Just newComputer ->
                    step newComputer

        Just (JumpIf bool m1 m2) ->
            case stepJumpIf bool m1 m2 computer of
                Nothing ->
                    Nothing

                Just newComputer ->
                    step newComputer

        Just (LessThan m1 m2) ->
            case stepBinaryOp (\a b -> boolToInt (a < b)) m1 m2 computer of
                Nothing ->
                    Nothing

                Just newComputer ->
                    step newComputer

        Just (Equals m1 m2) ->
            case stepBinaryOp (\a b -> boolToInt (a == b)) m1 m2 computer of
                Nothing ->
                    Nothing

                Just newComputer ->
                    step newComputer

        Just (Add m1 m2) ->
            case stepBinaryOp (+) m1 m2 computer of
                Nothing ->
                    Nothing

                Just newComputer ->
                    step newComputer

        Just (Multiply m1 m2) ->
            case stepBinaryOp (*) m1 m2 computer of
                Nothing ->
                    Nothing

                Just newComputer ->
                    step newComputer

        Just Halt ->
            Just computer


boolToInt : Bool -> Int
boolToInt bool =
    if bool then
        1

    else
        0


stepInput : Computer -> Maybe Computer
stepInput computer =
    case Array.get (computer.pointer + 1) computer.memory of
        Just address ->
            Just
                { input = computer.input
                , output = computer.output
                , memory = Array.set address computer.input computer.memory
                , pointer = computer.pointer + 2
                }

        Nothing ->
            Nothing


stepOutput : Mode -> Computer -> Maybe Computer
stepOutput mode computer =
    case getOperand mode (computer.pointer + 1) computer.memory of
        Just value ->
            Just
                { input = computer.input
                , output = value
                , memory = computer.memory
                , pointer = computer.pointer + 2
                }

        Nothing ->
            Nothing


stepJumpIf : Bool -> Mode -> Mode -> Computer -> Maybe Computer
stepJumpIf bool m1 m2 computer =
    let
        param1 =
            getOperand m1 (computer.pointer + 1) computer.memory

        param2 =
            getOperand m2 (computer.pointer + 2) computer.memory
    in
    case ( param1, param2 ) of
        ( Just value, Just address ) ->
            Just
                { input = computer.input
                , output = computer.output
                , memory = computer.memory
                , pointer =
                    if bool == (value /= 0) then
                        address

                    else
                        computer.pointer + 3
                }

        _ ->
            Nothing


stepBinaryOp : (Int -> Int -> Int) -> Mode -> Mode -> Computer -> Maybe Computer
stepBinaryOp op paramMode1 paramMode2 computer =
    let
        newMemory =
            Maybe.map3 (\a b returnPointer -> Array.set returnPointer (op a b) computer.memory)
                (getOperand paramMode1 (computer.pointer + 1) computer.memory)
                (getOperand paramMode2 (computer.pointer + 2) computer.memory)
                (Array.get (computer.pointer + 3) computer.memory)
    in
    case newMemory of
        Just memory ->
            Just
                { input = computer.input
                , output = computer.output
                , memory = memory
                , pointer = computer.pointer + 4
                }

        _ ->
            Nothing


getOperand : Mode -> Int -> Memory -> Maybe Int
getOperand mode pos memory =
    case mode of
        Immediate ->
            Array.get pos memory

        Position ->
            Maybe.andThen
                (\newPos -> Array.get newPos memory)
                (Array.get pos memory)



-- Example programs ############################################################


program1 =
    Array.fromList [ 1002, 4, 3, 4, 33 ]


program2 =
    Array.fromList [ 3, 0, 4, 0, 99 ]



-- Comparison opcodes


program3 =
    Array.fromList [ 3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8 ]


program4 =
    Array.fromList [ 3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8 ]


program5 =
    Array.fromList [ 3, 3, 1108, -1, 8, 3, 4, 3, 99 ]


program6 =
    Array.fromList [ 3, 3, 1107, -1, 8, 3, 4, 3, 99 ]



-- Jumps


program7 =
    Array.fromList [ 3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9 ]


program8 =
    Array.fromList [ 3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1 ]



-- Compare to 8


program9 =
    Array.fromList [ 3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31, 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104, 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99 ]


input =
    Array.fromList [ 3, 225, 1, 225, 6, 6, 1100, 1, 238, 225, 104, 0, 1101, 90, 60, 224, 1001, 224, -150, 224, 4, 224, 1002, 223, 8, 223, 1001, 224, 7, 224, 1, 224, 223, 223, 1, 57, 83, 224, 1001, 224, -99, 224, 4, 224, 1002, 223, 8, 223, 1001, 224, 5, 224, 1, 223, 224, 223, 1102, 92, 88, 225, 101, 41, 187, 224, 1001, 224, -82, 224, 4, 224, 1002, 223, 8, 223, 101, 7, 224, 224, 1, 224, 223, 223, 1101, 7, 20, 225, 1101, 82, 64, 225, 1002, 183, 42, 224, 101, -1554, 224, 224, 4, 224, 102, 8, 223, 223, 1001, 224, 1, 224, 1, 224, 223, 223, 1102, 70, 30, 224, 101, -2100, 224, 224, 4, 224, 102, 8, 223, 223, 101, 1, 224, 224, 1, 224, 223, 223, 2, 87, 214, 224, 1001, 224, -2460, 224, 4, 224, 1002, 223, 8, 223, 101, 7, 224, 224, 1, 223, 224, 223, 102, 36, 180, 224, 1001, 224, -1368, 224, 4, 224, 1002, 223, 8, 223, 1001, 224, 5, 224, 1, 223, 224, 223, 1102, 50, 38, 225, 1102, 37, 14, 225, 1101, 41, 20, 225, 1001, 217, 7, 224, 101, -25, 224, 224, 4, 224, 1002, 223, 8, 223, 101, 2, 224, 224, 1, 224, 223, 223, 1101, 7, 30, 225, 1102, 18, 16, 225, 4, 223, 99, 0, 0, 0, 677, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1105, 0, 99999, 1105, 227, 247, 1105, 1, 99999, 1005, 227, 99999, 1005, 0, 256, 1105, 1, 99999, 1106, 227, 99999, 1106, 0, 265, 1105, 1, 99999, 1006, 0, 99999, 1006, 227, 274, 1105, 1, 99999, 1105, 1, 280, 1105, 1, 99999, 1, 225, 225, 225, 1101, 294, 0, 0, 105, 1, 0, 1105, 1, 99999, 1106, 0, 300, 1105, 1, 99999, 1, 225, 225, 225, 1101, 314, 0, 0, 106, 0, 0, 1105, 1, 99999, 7, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 329, 101, 1, 223, 223, 1107, 677, 226, 224, 102, 2, 223, 223, 1006, 224, 344, 1001, 223, 1, 223, 8, 677, 226, 224, 1002, 223, 2, 223, 1005, 224, 359, 101, 1, 223, 223, 107, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 374, 101, 1, 223, 223, 7, 677, 226, 224, 1002, 223, 2, 223, 1006, 224, 389, 101, 1, 223, 223, 108, 677, 226, 224, 1002, 223, 2, 223, 1005, 224, 404, 101, 1, 223, 223, 1108, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 419, 101, 1, 223, 223, 8, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 434, 1001, 223, 1, 223, 1008, 677, 677, 224, 1002, 223, 2, 223, 1005, 224, 449, 1001, 223, 1, 223, 1107, 226, 677, 224, 102, 2, 223, 223, 1006, 224, 464, 101, 1, 223, 223, 107, 226, 677, 224, 1002, 223, 2, 223, 1006, 224, 479, 1001, 223, 1, 223, 7, 226, 677, 224, 102, 2, 223, 223, 1005, 224, 494, 1001, 223, 1, 223, 8, 677, 677, 224, 102, 2, 223, 223, 1006, 224, 509, 1001, 223, 1, 223, 1108, 677, 677, 224, 102, 2, 223, 223, 1005, 224, 524, 1001, 223, 1, 223, 1108, 226, 677, 224, 1002, 223, 2, 223, 1005, 224, 539, 101, 1, 223, 223, 107, 226, 226, 224, 102, 2, 223, 223, 1006, 224, 554, 1001, 223, 1, 223, 1007, 226, 226, 224, 102, 2, 223, 223, 1005, 224, 569, 1001, 223, 1, 223, 1008, 226, 226, 224, 102, 2, 223, 223, 1005, 224, 584, 101, 1, 223, 223, 1007, 677, 677, 224, 1002, 223, 2, 223, 1005, 224, 599, 1001, 223, 1, 223, 108, 677, 677, 224, 1002, 223, 2, 223, 1006, 224, 614, 1001, 223, 1, 223, 1007, 226, 677, 224, 1002, 223, 2, 223, 1006, 224, 629, 101, 1, 223, 223, 1008, 677, 226, 224, 102, 2, 223, 223, 1005, 224, 644, 101, 1, 223, 223, 1107, 226, 226, 224, 1002, 223, 2, 223, 1005, 224, 659, 1001, 223, 1, 223, 108, 226, 226, 224, 1002, 223, 2, 223, 1005, 224, 674, 101, 1, 223, 223, 4, 223, 99, 226 ]
