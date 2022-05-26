module Five exposing (..)

import Browser
import Html exposing (Html, button, div, input, option, select, text)
import Html.Attributes exposing (multiple, style, value)
import Html.Events exposing (onClick, onInput)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Person =
    { name : String
    , surname : String
    }


type alias Model =
    { people : List Person
    , filterPrefix : String
    , updateName : String
    , updateSurname : String
    , selectedPerson : Maybe Person
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { people = initialPeople
    , filterPrefix = ""
    , updateName = ""
    , updateSurname = ""
    , selectedPerson = Nothing
    }


initialPeople : List Person
initialPeople =
    [ { name = "Hans"
      , surname = "Emil"
      }
    , { name = "Max"
      , surname = "Mustermann"
      }
    , { name = "Roman"
      , surname = "Tisch"
      }
    ]


type Msg
    = ChangeSelection String
    | ChangeFilter String
    | ChangeUpdateName String
    | ChangeUpdateSurName String
    | CreateClicked
    | UpdateClicked
    | DeleteClicked


formatPerson : Person -> String
formatPerson person =
    person.surname ++ ", " ++ person.name


unformatPerson : String -> Person
unformatPerson str =
    case String.split ", " str of
        [ x, y ] ->
            { name = y
            , surname = x
            }

        _ ->
            { name = ""
            , surname = ""
            }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSelection name ->
            let
                unformatted =
                    unformatPerson name
            in
            ( { model
                | selectedPerson = Just unformatted
                , updateName = unformatted.name
                , updateSurname = unformatted.surname
              }
            , Cmd.none
            )

        ChangeFilter prefix ->
            ( { model | filterPrefix = prefix }
            , Cmd.none
            )

        ChangeUpdateName name ->
            ( { model | updateName = name }
            , Cmd.none
            )

        ChangeUpdateSurName surname ->
            ( { model | updateSurname = surname }
            , Cmd.none
            )

        CreateClicked ->
            ( { model
                | people = model.people ++ [ { name = model.updateName, surname = model.updateSurname } ]
                , updateName = ""
                , updateSurname = ""
              }
            , Cmd.none
            )

        UpdateClicked ->
            ( { model
                | people =
                    List.map
                        (\person ->
                            if person.name == model.updateName then
                                { name = model.updateName, surname = model.updateSurname }

                            else
                                person
                        )
                        model.people
                , updateName = ""
                , updateSurname = ""
              }
            , Cmd.none
            )

        DeleteClicked ->
            case model.selectedPerson of
                Nothing ->
                    ( model, Cmd.none )

                Just person ->
                    ( { model | people = List.filter (\p -> not (peopleEqual p person)) model.people }
                    , Cmd.none
                    )


peopleEqual : Person -> Person -> Bool
peopleEqual p1 p2 =
    p1.name == p2.name && p1.surname == p2.surname


view : Model -> Html Msg
view model =
    div []
        [ viewFilter model
        , viewPeopleList model
        , viewupdateForms model
        , viewButtons model
        ]


viewFilter : Model -> Html Msg
viewFilter model =
    div [ style "display" "inline" ]
        [ div [] [ text "Filter prefix: " ]
        , input [ onInput ChangeFilter ] []
        ]


viewPeopleList : Model -> Html Msg
viewPeopleList model =
    div []
        [ select [ style "width" "100px", onInput ChangeSelection, multiple True ]
            (model.people
                |> List.filter
                    (\p ->
                        let
                            name =
                                String.toLower p.name

                            surname =
                                String.toLower p.surname

                            filter =
                                String.toLower model.filterPrefix
                        in
                        String.startsWith filter name || String.startsWith filter surname
                    )
                |> List.map viewPerson
            )
        ]


viewPerson : Person -> Html Msg
viewPerson person =
    option [] [ text (person.surname ++ ", " ++ person.name) ]


viewupdateForms : Model -> Html Msg
viewupdateForms model =
    div []
        [ div [] [ div [] [ text "Name: " ], input [ style "width" "100px", value model.updateName, onInput ChangeUpdateName ] [] ]
        , div [] [ div [] [ text "Surname: " ], input [ style "width" "100px", value model.updateSurname, onInput ChangeUpdateSurName ] [] ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    div []
        [ button [ onClick CreateClicked ] [ text "Create" ]
        , button [ onClick UpdateClicked ] [ text "Update" ]
        , button [ onClick DeleteClicked ] [ text "Delete" ]
        ]
