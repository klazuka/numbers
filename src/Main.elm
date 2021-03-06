module Main exposing (..)

import Html exposing (Html, text, div, h1, img, p, button, input)
import Html.Attributes exposing (src, style, type_, value)
import Html.Events exposing (onClick, onInput)


---- MODEL ----


type alias Model =
    { mode : Mode
    , maxN : Int
    , custom1 : Int
    , custom2 : Int
    }


type Mode
    = IsEven
    | IsOdd
    | Custom


init : ( Model, Cmd Msg )
init =
    ( { mode = IsEven
      , maxN = 20
      , custom1 = 2
      , custom2 = 3
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


pred : Model -> (Int -> Bool)
pred { mode, custom1, custom2 } =
    case mode of
        IsEven ->
            (\n -> divisibleBy n 2)

        IsOdd ->
            (\n -> not (divisibleBy n 2))

        Custom ->
            (\n -> divisibleBy n custom1 && divisibleBy n custom2)



---- UPDATE ----


type Msg
    = SetMode Mode
    | SetMax String
    | SetCustom1 String
    | SetCustom2 String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMode mode ->
            ( { model | mode = mode }
            , Cmd.none
            )

        SetMax maxStr ->
            ( { model
                | maxN =
                    String.toInt maxStr |> Result.withDefault 12
              }
            , Cmd.none
            )

        SetCustom1 n ->
            ( { model | custom1 = String.toInt n |> Result.withDefault model.custom1 }
            , Cmd.none
            )

        SetCustom2 n ->
            ( { model | custom2 = String.toInt n |> Result.withDefault 0 }
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
             , button [ onClick (SetMode Custom) ] [ text "custom" ]
             , input [ type_ "range", Html.Attributes.min "1", onInput SetMax ] []
             , input [ type_ "text", value (toString model.maxN) ] []
             , input [ type_ "range", value (toString model.custom1), Html.Attributes.min "1", Html.Attributes.max "20", onInput SetCustom1 ] []
             , input [ type_ "range", value (toString model.custom2), Html.Attributes.min "1", Html.Attributes.max "20", onInput SetCustom2 ] []
             , input [ type_ "text", value (toString model.custom1) ] []
             , input [ type_ "text", value (toString model.custom2) ] []
             , viewSummary model data
             ]
                ++ (List.map (viewBox (pred model)) data)
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


viewSummary : Model -> List Product -> Html Msg
viewSummary model products =
    let
        highlighted =
            List.map .p products
                |> List.filter (pred model)

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
