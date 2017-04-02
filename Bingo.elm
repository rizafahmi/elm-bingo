module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


type alias Model =
    { name : String, gameNumber : Int, entries : List Entry }


type Msg
    = NewGame
    | Mark Int
    | SortPoint



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            { model
                | gameNumber = model.gameNumber + 1
                , entries = initialEntries
            }

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                { model | entries = List.map markEntry model.entries }

        SortPoint ->
            { model
                | entries =
                    List.sortBy .points model.entries
                        |> List.reverse
            }



-- MODEL


initialModel : Model
initialModel =
    { name = "Riza"
    , gameNumber = 1
    , entries = initialEntries
    }


initialEntries : List Entry
initialEntries =
    [ Entry 3 "In The Cloud" 300 False
    , Entry 1 "Future-Proof" 100 False
    , Entry 4 "Rock-Star Ninja" 400 False
    , Entry 2 "Doing Agile" 200 False
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
    li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
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
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "NewGame" ]
            , button [ onClick SortPoint ] [ text "Sort" ]
            ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum


viewScore : Int -> Html Msg
viewScore sum =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , update = update
        , view = view
        }
