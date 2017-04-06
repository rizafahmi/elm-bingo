module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, value, autofocus, placeholder, type_, classList, href, id, disabled)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)


-- TYPES


type GameState
    = Playing
    | EnteringName


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


type alias Entry =
    { id : Int, phrase : String, points : Int, marked : Bool }


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    , inputName : String
    , gameState : GameState
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | SortPoint
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            { model | gameState = state, inputName = model.name } ! [ Cmd.none ]

        CancelName ->
            { model | inputName = "", gameState = Playing } ! [ Cmd.none ]

        SaveName ->
            { model
                | name = model.inputName
                , inputName = ""
                , gameState = Playing
            }
                ! [ Cmd.none ]

        SetNameInput value ->
            { model | inputName = value } ! [ Cmd.none ]

        CloseAlert ->
            { model | alertMessage = Nothing } ! [ Cmd.none ]

        ShareScore ->
            model ! [ postScore model ]

        NewScore (Ok score) ->
            let
                message =
                    "Your "
                        ++ (toString score.score)
                        ++ " was successfully shared!"
            in
                { model | alertMessage = Just message }
                    ! [ Cmd.none
                      ]

        NewScore (Err error) ->
            let
                message =
                    "Cannot share your score. There was some error: "
                        ++ (httpError error)
            in
                { model | alertMessage = Just message }
                    ! [ Cmd.none
                      ]

        NewRandom randomNumber ->
            { model | gameNumber = randomNumber } ! [ Cmd.none ]

        NewEntries (Ok randomEntries) ->
            { model
                | entries =
                    randomEntries
                        |> List.sortBy .points
                        |> List.reverse
            }
                ! [ Cmd.none ]

        NewEntries (Err error) ->
            { model | alertMessage = Just (httpError error) } ! [ Cmd.none ]

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


httpError : Http.Error -> String
httpError error =
    case error of
        Http.NetworkError ->
            "Is the server running?"

        Http.Timeout ->
            "Request timeout"

        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    "Unauthorized"

                404 ->
                    "Not found"

                code ->
                    (toString code)

        _ ->
            toString error



-- DECODERS/ENCODERS


entryDecoder : Decoder Entry
entryDecoder =
    decode Entry
        |> required "id" Decode.int
        |> required "phrase" Decode.string
        |> optional "points" Decode.int 100
        |> hardcoded False


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)


encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (sumMarkedPoints model.entries) )
        ]



-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


apiUrl : String
apiUrl =
    "http://localhost:3000/"


entriesUrl : String
entriesUrl =
    apiUrl ++ "random-entries"


getEntries : Cmd Msg
getEntries =
    -- Http.send (\result -> NewEntries result) (Http.getString entriesUrl)
    -- Http.send NewEntries (Http.getString entriesUrl)
    (Decode.list entryDecoder)
        |> Http.get entriesUrl
        |> Http.send NewEntries


postScore : Model -> Cmd Msg
postScore model =
    let
        url =
            apiUrl ++ "scores"

        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send NewScore request



-- MODEL


initialModel : Model
initialModel =
    { name = "Riza"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , inputName = ""
    , gameState = EnteringName
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
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ]
            [ text name ]
        , text (" - Game #" ++ (toString gameNumber))
        ]


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


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage message =
    case message of
        Just textMessage ->
            div [ class "alert" ]
                [ text textMessage
                , span [ class "close", onClick CloseAlert ] [ text "X" ]
                ]

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "Buzzword Bingo"
        , viewPlayer model.name model.gameNumber
        , viewAlertMessage model.alertMessage
        , viewInputName model
        , viewEntries model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "NewGame" ]
            , button [ onClick SortPoint ] [ text "Sort" ]
            , button [ onClick ShareScore, disabled (isPointZero model) ] [ text "Share" ]
            ]
        , div [ class "debug" ] [ text (toString model) ]
        , viewFooter
        ]


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\e sum -> e.points + sum) 0


isPointZero : Model -> Bool
isPointZero model =
    (sumMarkedPoints model.entries) == 0


viewScore : Int -> Html Msg
viewScore sum =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


viewInputName : Model -> Html Msg
viewInputName model =
    case model.gameState of
        EnteringName ->
            div [ class "" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , onInput SetNameInput
                    , value model.inputName
                    ]
                    []
                , button [ onClick SaveName ] [ text "Save" ]
                , button [ onClick CancelName ] [ text "Cancel" ]
                ]

        Playing ->
            text ""


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Just _ ->
            False

        Nothing ->
            True


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, getEntries )
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
