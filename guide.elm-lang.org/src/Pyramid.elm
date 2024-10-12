module Pyramid exposing (Model, Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, pre, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = { count = 0, invert = False }, update = update, view = view }


type alias Model =
    { count : Int, invert : Bool }


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
            ++ (List.range 1 model.count
                    |> (if model.invert then
                            List.reverse

                        else
                            \n -> n
                       )
                    |> List.map (\n -> pre [] [ text (String.repeat (model.count - n) " " ++ String.repeat (n * 2 - 1) "#" ++ String.repeat (model.count - n) " ") ])
               )
        )
