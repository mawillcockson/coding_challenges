module Pyramid exposing (main)

import Browser
import Html exposing (Html, button, div, input, pre, text)
import Html.Attributes exposing (placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { count : Int
    , invert : Bool
    , padding : String
    , character : String
    }


init : Model
init =
    { count = 0
    , invert = False
    , padding = " "
    , character = "#"
    }


type Msg
    = Increment
    | Decrement
    | Invert
    | Padding String
    | Character String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = max 0 (model.count - 1) }

        Invert ->
            { model | invert = not model.invert }

        Padding character ->
            { model | padding = character }

        Character character ->
            { model | character = character }


view : Model -> Html Msg
view model =
    div []
        ([ button [ onClick Increment ] [ text "+" ]
         , button [ onClick Decrement ] [ text "-" ]
         , button [ onClick Invert ] [ text "invert" ]
         , input [ type_ "text", placeholder "Padding", value model.padding, onInput Padding ] []
         , input [ type_ "text", placeholder "Character", value model.character, onInput Character ] []
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
            (String.repeat (model.count - rowNumber) model.padding
                ++ String.repeat (rowNumber * 2 - 1) model.character
                ++ String.repeat (model.count - rowNumber) model.padding
            )
        ]
