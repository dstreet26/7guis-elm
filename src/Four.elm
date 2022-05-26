module Four exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, button, div, input, meter, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Time exposing (Posix)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    -- This also works and could be used for smoother animation
    -- onAnimationFrame Tick
    Time.every 100 Tick


type alias Model =
    { sliderValue : String
    , elapsedTime : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { sliderValue = "10"
    , elapsedTime = 0
    }


type Msg
    = ChangeSlider String
    | ResetTimer
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSlider s ->
            ( { model | sliderValue = s }, Cmd.none )

        ResetTimer ->
            ( { model | elapsedTime = 0 }, Cmd.none )

        Tick p ->
            ( { model | elapsedTime = model.elapsedTime + 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ style "display" "inline" ]
            [ text "Elapsed Time: "
            , meter
                [ style "width" "160px"
                , Html.Attributes.min "0"
                , value (String.fromInt model.elapsedTime)
                , Html.Attributes.max model.sliderValue
                ]
                []
            ]
        , div []
            [ div [] [ text ((((model.elapsedTime |> Basics.toFloat) / 10) |> String.fromFloat) ++ "s") ]
            ]
        , div []
            [ div [ style "display" "inline" ] [ text "Duration:" ]
            , div [ style "display" "inline" ]
                [ input
                    [ type_ "range"
                    , Html.Attributes.min "0"
                    , value model.sliderValue
                    , Html.Attributes.max "300"
                    , onInput ChangeSlider
                    ]
                    []
                ]
            ]
        , div []
            [ button [ style "width" "260px", onClick ResetTimer ] [ text "Reset Timer" ]
            ]
        ]
