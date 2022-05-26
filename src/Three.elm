module Three exposing (..)

import Browser
import Html exposing (Html, button, div, input, main_, option, select, text)
import Html.Attributes exposing (disabled, style, value)
import Html.Events exposing (onClick, onInput)
import Parser exposing (..)
import Ports exposing (sendAlert)
import Time exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none


type FlightType
    = OneWay
    | Return


type alias Model =
    { flightType : FlightType
    , inputOne : String
    , inputTwo : String
    , parsedOne : Maybe Int
    , parsedTwo : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { flightType = OneWay
    , inputOne = "26.05.2022"
    , inputTwo = "26.05.2022"
    , parsedOne = parseDate "26.05.2022"
    , parsedTwo = parseDate "26.5.2022"
    }


type Msg
    = Book
    | ChangeOne String
    | ChangeTwo String
    | ChangeType String


type alias DaysMonthsYears =
    { days : Int
    , months : Int
    , years : Int
    }


parser1 : Parser DaysMonthsYears
parser1 =
    succeed DaysMonthsYears
        |= parseDay
        |. seperator
        |= parseMonth
        |. seperator
        |= parseYear
        |. end


parseDay : Parser Int
parseDay =
    getChompedString (chompWhile Char.isDigit)
        |> andThen checkDay


checkDay : String -> Parser Int
checkDay input =
    case String.toInt input of
        Nothing ->
            problem "Not a number"

        Just day ->
            if day > 31 then
                problem "Day is too big"

            else
                succeed day


parseMonth : Parser Int
parseMonth =
    getChompedString (chompWhile Char.isDigit)
        |> andThen checkMonth


checkMonth : String -> Parser Int
checkMonth input =
    case String.toInt input of
        Nothing ->
            problem "Not a number"

        Just month ->
            if month > 12 then
                problem "Month is too big"

            else
                succeed month


parseYear : Parser Int
parseYear =
    getChompedString (chompWhile Char.isDigit)
        |> andThen checkYear


checkYear : String -> Parser Int
checkYear input =
    case String.toInt input of
        Nothing ->
            problem "Not a number"

        Just year ->
            if year < 0 then
                problem "Year is too small"

            else if year < 1970 then
                problem "Year would normally be too small"

            else
                succeed year


seperator =
    oneOf [ symbol ".", symbol "-", symbol "/" ]


parseDate : String -> Maybe Int
parseDate s =
    case run parser1 s of
        Err _ ->
            Nothing

        Ok x ->
            Just ((x.years * 365) + (x.months * 30) + x.days)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Book ->
            if model.flightType == OneWay then
                if model.parsedOne == Nothing then
                    ( model, Cmd.none )

                else
                    ( model, sendAlert "Booking" )

            else if model.parsedOne == Nothing || model.parsedTwo == Nothing then
                ( model, Cmd.none )

            else
                ( model, sendAlert "Booking" )

        ChangeOne s ->
            ( { model | inputOne = s, parsedOne = parseDate s }, Cmd.none )

        ChangeTwo s ->
            ( { model | inputTwo = s, parsedTwo = parseDate s }, Cmd.none )

        ChangeType s ->
            ( { model
                | flightType =
                    case s of
                        "one-way flight" ->
                            OneWay

                        _ ->
                            Return
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ select [ style "width" "100px", onInput ChangeType ]
                [ option [] [ text "one-way flight" ]
                , option [] [ text "return flight" ]
                ]
            ]
        , div []
            [ input
                [ style "width" "160px"
                , value model.inputOne
                , onInput ChangeOne
                ]
                []
            ]
        , div []
            [ input
                [ style "width" "160px"
                , value model.inputTwo
                , onInput ChangeTwo
                , disabled (model.flightType == OneWay)
                ]
                []
            ]
        , div []
            [ button
                [ style "width" "160px"
                , onClick Book
                , disabled
                    (isButtonDisabled model)
                ]
                [ text "Book" ]
            ]
        ]


isButtonDisabled model =
    case model.flightType of
        OneWay ->
            case model.parsedOne of
                Nothing ->
                    True

                Just _ ->
                    False

        Return ->
            case model.parsedOne of
                Nothing ->
                    True

                Just one ->
                    case model.parsedTwo of
                        Nothing ->
                            True

                        Just two ->
                            if two < one then
                                True

                            else
                                False
