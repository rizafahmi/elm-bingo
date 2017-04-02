module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


type alias Model =
    { name : String, gameNumber : Int, entries : List Entry }



-- UPDATE


type Msg
    = NewGame
    | Mark Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            { model | gameNumber = model.gameNumber + 1 }

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                { model | entries = List.map markEntry model.entries }



-- MODEL


initialModel : Model
initialModel =
    { name = "Riza"
    , gameNumber = 1
    , entries = initialEntries
    }


initialEntries : List Entry
initialEntries =
    [ Entry 1 "Future-Proof" 100 False
    , Entry 2 "Doing Agile" 200 False
    , Entry 3 "Scrum" 150 False
    ]


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> Html.text
    in
        h2 [ id "info", class "classy" ] [ playerInfoText ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org/" ] [ text "Powered by Elm" ] ]


viewEntryList : Entry -> Html Msg
viewEntryList entry =
    li [ onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntries : List Entry -> Html Msg
viewEntries entries =
    entries
        |> List.map viewEntryList
        |> ul []


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "Buzzword Bingo"
        , viewPlayer model.name model.gameNumber
        , viewEntries model.entries
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "NewGame" ] ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
