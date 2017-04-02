module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import Http


-- TYPES


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


type alias Model =
    { name : String, gameNumber : Int, entries : List Entry }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | SortPoint
    | NewRandom Int
    | NewEntries (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            { model | gameNumber = randomNumber } ! [ Cmd.none ]

        NewEntries (Ok jsonString) ->
            let
                _ =
                    Debug.log "It worked!" jsonString
            in
                ( model, Cmd.none )

        NewEntries (Err error) ->
            let
                _ =
                    Debug.log "Oooopss!" error
            in
                ( model, Cmd.none )

        NewGame ->
            { model
                | gameNumber = model.gameNumber + 1
            }
                ! [ getEntries
                  ]

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                { model | entries = List.map markEntry model.entries } ! [ Cmd.none ]

        SortPoint ->
            { model
                | entries =
                    List.sortBy .points model.entries
                        |> List.reverse
            }
                ! [ Cmd.none ]



-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


entriesUrl =
    "http://localhost:3000/random-entries"


getEntries : Cmd Msg
getEntries =
    -- Http.send (\result -> NewEntries result) (Http.getString entriesUrl)
    -- Http.send NewEntries (Http.getString entriesUrl)
    entriesUrl
        |> Http.getString
        |> Http.send NewEntries



-- MODEL


initialModel : Model
initialModel =
    { name = "Riza"
    , gameNumber = 1
    , entries = []
    }



-- VIEWS


allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
    List.all .marked entries


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
        |> List.foldl (\e sum -> e.points + sum) 0


viewScore : Int -> Html Msg
viewScore sum =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
