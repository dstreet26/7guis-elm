module Two exposing (..)

import Browser
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { celcius : Float
    , fahrenheit : Float
    }


init : Model
init =
    { celcius = 0
    , fahrenheit = 0
    }


type Msg
    = ChangeCelcius String
    | ChangeFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeCelcius x ->
            let
                newCelcius =
                    case String.toFloat x of
                        Nothing ->
                            model.celcius

                        Just f ->
                            f

                newFahrenheit =
                    newCelcius * (9 / 5) + 32
            in
            { model | celcius = newCelcius, fahrenheit = newFahrenheit }

        ChangeFahrenheit x ->
            let
                newFahrenheit =
                    case String.toFloat x of
                        Nothing ->
                            model.fahrenheit

                        Just f ->
                            f

                newCelcius =
                    (newFahrenheit - 32) * (5 / 9)
            in
            { model | celcius = newCelcius, fahrenheit = newFahrenheit }


view : Model -> Html Msg
view model =
    div []
        [ div [ style "float" "left" ] [ input [ style "width" "80px", value (String.fromFloat model.celcius), onInput ChangeCelcius ] [] ]
        , div [ style "float" "left", style "margin-left" "10px" ] [ text "Celcius =" ]
        , div [ style "float" "left", style "margin-left" "10px" ] [ input [ style "width" "60px", value (String.fromFloat model.fahrenheit), onInput ChangeFahrenheit ] [] ]
        , div [ style "margin-left" "10px", style "margin-left" "10px" ] [ text "Fahrenheit" ]
        ]
