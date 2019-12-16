module Aoc4 exposing (..)

import Dict exposing (Dict)


validCodes : ( Int, Int ) -> List Int
validCodes ( start, end ) =
    validCodesAcc start end []


validCodesAcc : Int -> Int -> List Int -> List Int
validCodesAcc start end acc =
    if start > end then
        acc

    else if isValid start then
        validCodesAcc (start + 1) end (start :: acc)

    else
        validCodesAcc (start + 1) end acc


isValid : Int -> Bool
isValid n =
    let
        code =
            digits n
    in
    hasIncreasingDigits 0 code
        && List.member 2 (Dict.values (adjacentGroups code))


digits : Int -> List Int
digits n =
    String.fromInt n
        |> String.toList
        |> List.map (\c -> Char.toCode c - 48)


hasIncreasingDigits : Int -> List Int -> Bool
hasIncreasingDigits d code =
    case code of
        [] ->
            True

        d1 :: rest ->
            if d1 < d then
                False

            else
                hasIncreasingDigits d1 rest


adjacentGroups : List Int -> Dict Int Int
adjacentGroups code =
    List.foldl (\digit -> Dict.update digit incrementCount) Dict.empty code


incrementCount : Maybe Int -> Maybe Int
incrementCount count =
    case count of
        Nothing ->
            Just 1

        Just n ->
            Just (n + 1)



-- INPUTS ############################################################


input =
    ( 130254, 678275 )
