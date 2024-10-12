module Pyramid exposing (main)

import Browser
import Html exposing (Html, button, div, pre, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { count : Int, invert : Bool }


init : Model
init =
    { count = 0
    , invert = False
    }


type Msg
    = Increment
    | Decrement
    | Invert


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = max 0 (model.count - 1) }

        Invert ->
            { model | invert = not model.invert }


view : Model -> Html Msg
view model =
    div []
        ([ button [ onClick Increment ] [ text "+" ]
         , button [ onClick Decrement ] [ text "-" ]
         , button [ onClick Invert ] [ text "invert" ]
         ]
            ++ makePyramid model
        )


makePyramid : Model -> List (Html Msg)
makePyramid model =
    List.range 1 model.count
        |> (if model.invert then
                List.reverse

            else
                \n -> n
           )
        |> List.map (pyramidRow model)


pyramidRow : Model -> Int -> Html Msg
pyramidRow model rowNumber =
    pre []
        [ text
            (String.repeat (model.count - rowNumber) " "
                ++ String.repeat (rowNumber * 2 - 1) "#"
                ++ String.repeat (model.count - rowNumber) " "
            )
        ]
