module Six exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode exposing (Decoder, at, float, int, map4)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Circles =
    List Circle


type alias Model =
    { past : List Circles
    , present : Circles
    , future : List Circles
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { past = []
    , present = []
    , future = []
    }


decoder1 : Decoder Coordinates
decoder1 =
    map4 Coordinates
        (at [ "offsetX" ] int)
        (at [ "offsetY" ] int)
        (at [ "target", "offsetHeight" ] float)
        (at [ "target", "offsetWidth" ] float)


type alias Coordinates =
    { offsetX : Int
    , offsetY : Int
    , offsetHeight : Float
    , offsetWidth : Float
    }


type Msg
    = Undo
    | Redo
    | AdjustDiameterClicked
    | AdjustedDiameter String
    | MouseClicked Coordinates


type alias Circle =
    { x : Int
    , y : Int
    , radius : Float
    }


undoStateNew : Circles -> Model -> Model
undoStateNew circles model =
    { past = circles :: model.past
    , present = circles
    , future = []
    }



-- There is a bug in this implementation or something, the first click doesn't do antyhing.


undoStateUndo : Model -> Model
undoStateUndo model =
    let
        ( newPresent, newPast ) =
            case model.past of
                x :: xs ->
                    ( x, xs )

                _ ->
                    ( [], [] )

        newFuture =
            model.present :: model.future
    in
    { past = newPast
    , present = newPresent
    , future = newFuture
    }


undoStateRedo : Model -> Model
undoStateRedo model =
    let
        ( newPresent, newFuture ) =
            case model.future of
                x :: xs ->
                    ( x, xs )

                _ ->
                    ( [], [] )

        newPast =
            model.present :: model.past
    in
    { past = newPast
    , present = newPresent
    , future = newFuture
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseClicked coordinates ->
            ( undoStateNew
                ({ x = coordinates.offsetX
                 , y = coordinates.offsetY
                 , radius = 10
                 }
                    :: model.present
                )
                model
            , Cmd.none
            )

        Undo ->
            ( undoStateUndo model, Cmd.none )

        Redo ->
            ( undoStateRedo model, Cmd.none )

        AdjustDiameterClicked ->
            ( model, Cmd.none )

        AdjustedDiameter newDiameter ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ viewButtons model
        , viewCanvas model

        -- , viewCanvasOverlay model
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    div []
        [ button [ onClick Undo ] [ text "Undo" ]
        , button [ onClick Redo ] [ text "Redo" ]
        ]


viewCanvas : Model -> Html Msg
viewCanvas model =
    div []
        [ div [] []
        , div
            [ style "width" "300px"
            , style "height" "200px"
            , style "background-color" "gray"
            , on "click" (Decode.map MouseClicked decoder1)
            ]
            [ div [ style "position" "relative" ] (viewCircles model)
            ]
        ]


viewCircles : Model -> List (Html Msg)
viewCircles model =
    List.map viewCircle model.present


viewCircle : Circle -> Html Msg
viewCircle circle =
    div
        [ style "position" "absolute"
        , style "left" (String.fromInt circle.x ++ "px")
        , style "top" (String.fromInt circle.y ++ "px")
        , style "width" (String.fromFloat (circle.radius * 2) ++ "px")
        , style "height" (String.fromFloat (circle.radius * 2) ++ "px")
        , style "background-color" "red"
        ]
        []


viewCanvasOverlay : Model -> Html Msg
viewCanvasOverlay model =
    div []
        [ div [ on "click" (Decode.map MouseClicked decoder1) ] [ text "Canvas Overlay" ] ]
