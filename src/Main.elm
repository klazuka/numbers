module Main exposing (..)

import Html exposing (Html, text, div, h1, img, p, button)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)


---- MODEL ----


type alias Model =
    { mode : Mode
    , maxN : Int
    }


type Mode
    = IsEven
    | IsOdd
    | Has2And3AsFactors


init : ( Model, Cmd Msg )
init =
    ( { mode = IsEven
      , maxN = 20
      }
    , Cmd.none
    )


cellSize =
    40


divisibleBy q n =
    (q % n) == 0


type alias Product =
    { a : Int
    , b : Int
    , p : Int
    }


multiplicationTable : Int -> List Product
multiplicationTable high =
    let
        range =
            List.range 1 high

        pairs =
            List.concatMap
                (\n -> List.map (\x -> ( n, x )) range)
                range

        makeProduct ( x, y ) =
            { a = x, b = y, p = x * y }
    in
        List.map makeProduct pairs


pred : Mode -> (Int -> Bool)
pred mode =
    case mode of
        IsEven ->
            (\n -> divisibleBy n 2)

        IsOdd ->
            (\n -> not (divisibleBy n 2))

        Has2And3AsFactors ->
            (\n -> divisibleBy n 2 && divisibleBy n 3)



---- UPDATE ----


type Msg
    = SetMode Mode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMode mode ->
            ( { model | mode = mode }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        data =
            multiplicationTable model.maxN
    in
        div []
            ([ button [ onClick (SetMode IsEven) ] [ text "evens" ]
             , button [ onClick (SetMode IsOdd) ] [ text "odds" ]
             , button [ onClick (SetMode Has2And3AsFactors) ] [ text "divisible by 2 and 3" ]
             , viewSummary model.mode data
             ]
                ++ (List.map (viewBox (pred model.mode)) data)
            )


viewBox : (Int -> Bool) -> Product -> Html Msg
viewBox pred { a, b, p } =
    let
        bgColor =
            if pred p then
                "orange"
            else
                "lightgray"
    in
        div
            [ style
                [ ( "width", toString cellSize ++ "px" )
                , ( "height", toString cellSize ++ "px" )
                , ( "background-color", bgColor )
                , ( "position", "absolute" )
                , ( "top", (toString (a * cellSize + 100)) ++ "px" )
                , ( "left", (toString (b * cellSize + 100)) ++ "px" )
                ]
            ]
            [ text (toString p) ]


viewSummary : Mode -> List Product -> Html Msg
viewSummary mode products =
    let
        highlighted =
            List.map .p products
                |> List.filter (pred mode)

        numHighlighted =
            List.length highlighted

        numTotal =
            List.length products
    in
        div []
            [ p [] [ text ("# highlighted: " ++ toString numHighlighted) ]
            , p [] [ text ("# total: " ++ toString numTotal) ]
            , p []
                [ text
                    ("% highlighted = "
                        ++ (toString
                                (100 * toFloat numHighlighted / toFloat numTotal)
                           )
                    )
                ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
