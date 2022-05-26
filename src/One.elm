module One exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Int


init : Model
init =
    0


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1


view : Model -> Html Msg
view model =
    div []
        [ div [ style "float" "left" ] [ input [ style "width" "60px", value (String.fromInt model) ] [] ]
        , button [ style "margin-left" "10px", onClick Increment ] [ text "Count" ]
        ]
