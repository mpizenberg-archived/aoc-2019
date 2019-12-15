module Aoc3 exposing (..)


type alias Wire =
    { orientation : Orientation
    , start : ( Int, Int )
    , displacement : Int
    , accumDistance : Int
    }


type Orientation
    = Horizontal
    | Vertical


solution : String -> String -> Maybe Int
solution pathStr1 pathStr2 =
    let
        cable1 =
            wiresFromPath (parse pathStr1)

        cable2 =
            wiresFromPath (parse pathStr2)

        cableCrossings =
            crossings cable1 cable2
    in
    List.map (\( ( x, y ), steps ) -> steps) cableCrossings
        |> List.minimum


crossings : List Wire -> List Wire -> List ( ( Int, Int ), Int )
crossings cable1 cable2 =
    case cable2 of
        _ :: rest2 ->
            crossingsAccum cable1 rest2 cable2 []

        _ ->
            []


crossingsAccum : List Wire -> List Wire -> List Wire -> List ( ( Int, Int ), Int ) -> List ( ( Int, Int ), Int )
crossingsAccum cable1 cable2 origCable2 acc =
    case ( cable1, cable2 ) of
        ( [], _ ) ->
            acc

        ( _ :: rest1, [] ) ->
            crossingsAccum rest1 origCable2 origCable2 acc

        ( wire1 :: _, wire2 :: rest2 ) ->
            case cross wire1 wire2 of
                Nothing ->
                    crossingsAccum cable1 rest2 origCable2 acc

                Just position ->
                    crossingsAccum cable1 rest2 origCable2 (position :: acc)


endOfWire : Wire -> ( Int, Int )
endOfWire { orientation, start, displacement } =
    let
        ( x, y ) =
            start
    in
    case orientation of
        Horizontal ->
            ( x + displacement, y )

        Vertical ->
            ( x, y + displacement )


cross : Wire -> Wire -> Maybe ( ( Int, Int ), Int )
cross wire1 wire2 =
    if wire1.orientation == wire2.orientation then
        Nothing

    else
        let
            ( x1, y1 ) =
                wire1.start

            ( x2, y2 ) =
                wire2.start

            ( theoreticalCross, d1, d2 ) =
                if wire1.orientation == Horizontal then
                    ( ( x2, y1 ), x2 - x1, y1 - y2 )

                else
                    ( ( x1, y2 ), y2 - y1, x1 - x2 )
        in
        if within d1 wire1.displacement && within d2 wire2.displacement then
            let
                steps =
                    wire1.accumDistance + abs d1 + wire2.accumDistance + abs d2
            in
            Just ( theoreticalCross, steps )

        else
            Nothing


within : Int -> Int -> Bool
within d wireDisplacement =
    if d == 0 then
        True

    else if d * wireDisplacement <= 0 then
        False

    else
        abs d <= abs wireDisplacement


type alias Path =
    List ( Orientation, Int )


wiresFromPath : Path -> List Wire
wiresFromPath path =
    wiresFromPathAccum path []


wiresFromPathAccum : Path -> List Wire -> List Wire
wiresFromPathAccum path acc =
    case ( path, acc ) of
        ( [], _ ) ->
            List.reverse acc

        ( ( orientation, displacement ) :: nextPath, [] ) ->
            let
                firstWire =
                    { orientation = orientation
                    , displacement = displacement
                    , start = ( 0, 0 )
                    , accumDistance = 0
                    }
            in
            wiresFromPathAccum nextPath [ firstWire ]

        ( ( orientation, displacement ) :: nextPath, lastWire :: _ ) ->
            let
                nextWire =
                    { orientation = orientation
                    , displacement = displacement
                    , start = endOfWire lastWire
                    , accumDistance = lastWire.accumDistance + abs lastWire.displacement
                    }
            in
            wiresFromPathAccum nextPath (nextWire :: acc)


parse : String -> Path
parse str =
    String.split "," str
        |> List.map parseWire


parseWire : String -> ( Orientation, Int )
parseWire str =
    case String.uncons str of
        Just ( 'R', strDisplacement ) ->
            ( Horizontal, Maybe.withDefault 0 (String.toInt strDisplacement) )

        Just ( 'L', strDisplacement ) ->
            ( Horizontal, -(Maybe.withDefault 0 (String.toInt strDisplacement)) )

        Just ( 'U', strDisplacement ) ->
            ( Vertical, Maybe.withDefault 0 (String.toInt strDisplacement) )

        Just ( 'D', strDisplacement ) ->
            ( Vertical, -(Maybe.withDefault 0 (String.toInt strDisplacement)) )

        _ ->
            ( Horizontal, 0 )



-- INPUTS ############################################################


pathA1 =
    "R8,U5,L5,D3"


pathA2 =
    "U7,R6,D4,L4"


pathB1 =
    "R75,D30,R83,U83,L12,D49,R71,U7,L72"


pathB2 =
    "U62,R66,U55,R34,D71,R55,D58,R83"


pathC1 =
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"


pathC2 =
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"


input1 =
    "R1009,U34,L600,U800,R387,D247,R76,U797,R79,D582,L325,D236,R287,U799,R760,U2,L261,D965,R854,D901,R527,D998,R247,U835,L29,U525,L10,D351,L599,D653,L39,D112,R579,D650,L539,D974,R290,U729,L117,D112,L926,U270,L158,D800,L291,U710,L28,D211,R700,U691,L488,D307,R448,U527,L9,D950,L535,D281,L683,U576,L372,U849,R485,D237,L691,U453,L667,U856,R832,U956,L47,D951,R171,U484,R651,D731,L768,D44,R292,U107,L237,U731,L795,D460,R781,U77,L316,U873,L994,D322,L479,U121,R754,U68,L454,D162,L308,D986,L893,D808,R929,D328,L591,D718,R616,U139,R221,U124,R477,U614,L439,D329,R217,D157,L65,D460,R523,U955,R512,D458,L823,D975,R506,D870,R176,U558,R935,U319,L281,D470,L285,U639,L974,U186,L874,U487,L979,D95,R988,U398,R776,D637,R75,U331,R746,D603,R102,U978,R702,U89,L48,D757,L173,D422,L394,U800,R955,U644,R911,D327,R471,D313,L982,D93,R998,U549,R210,D640,R332,U566,R736,U302,L69,U677,L137,U674,R204,D720,R868,U143,L635,D177,L277,D749,R180,D432,R451,D426,R559,U964,L35,U452,L848,D707,R758,D41,R889,D966,R460,U11,R819,D30,L953,U150,L621,U915,R400,D723,R299,D93,L987,D790,L541,U864,R711,D968,L2,D963,L996,D260,L824,D765,L617,U257,R175,U786,L873,D118,L433,U246,R821,D308,L468,U53,R859,U806,L197,D663,R540,D84,L398,D945,L999,U114,L731,D676,L538,U680,R519,U313,R699,U746,R471,D393,L902,U697,R542,D385,R183,U463,R276,U990,R111,U709,R726,D996,L728,D215,R726,D911,L199,D484,R282,U129,L329,U309,L270,U990,L813,U242,L353,D741,R447,D253,L556,U487,L102,D747,L965,D743,R768,U589,R657,D910,L760,D981,L982,D292,R730,U236,L831"


input2 =
    "L1000,U720,R111,D390,L400,U931,R961,D366,L172,D434,R514,D185,L555,D91,R644,U693,L902,U833,L28,U136,R204,D897,L18,D601,R855,U409,R567,U57,L561,D598,R399,D238,R37,U478,R792,D587,R740,D647,L593,U576,L662,U389,R540,U359,R547,D449,R518,D747,L887,U421,R153,D365,L357,U495,L374,D27,L338,D57,R431,U796,L487,D480,L273,U662,R874,D29,R596,D166,R167,D788,R175,D395,L739,U180,R145,U824,L156,D387,R427,U167,R268,D653,L371,D467,L216,U23,L930,D494,L76,U338,R813,U373,R237,D1,R706,U37,R202,D187,L905,D431,R787,D391,R576,D370,R320,U225,L901,D921,R656,U517,R782,D965,L849,U241,L160,U792,L587,U408,L750,U21,R317,U919,L449,D691,L895,D853,L547,D178,R793,D921,L873,D962,L232,U690,L815,U309,R455,U156,L200,U34,L761,U402,R278,U952,L294,D183,R475,U770,L375,D117,R58,D905,L580,U240,R263,U549,R771,U512,L20,D996,L265,U619,L742,U754,L68,D824,R694,D678,R412,D321,R611,U325,L874,U776,L907,U39,R568,D485,R528,D197,R487,D920,R879,D935,R107,U897,L263,D979,L420,U498,L757,D348,L279,U266,R699,D729,R65,U672,L945,U780,L339,U324,R927,U357,R324,U435,R602,D245,L456,D161,L537,U740,R454,U211,L952,D356,L317,U456,L6,D718,L389,D554,L366,D141,R543,U756,R334,U209,L207,U726,R375,U59,L238,D118,L514,D390,R212,U272,L350,U898,L105,U514,L591,U839,L767,U651,R298,U726,L429,U350,L53,U789,R9,D295,L558,U9,L515,D177,L430,U158,L959,U601,L994,U635,L252,D159,R155,U601,L809,D5,R47,U567,R328,U559,R149,U43,L612,U428,R694,D568,L80,U80,R983,D143,R612,U735,L10,D697,L640,D788,R714,U555,L139,U396,L830,D825,R928,D25,L889,U973,L343"
