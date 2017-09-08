module Debug.Main exposing (..)

import Debug.Doc as Debug
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events
import Text exposing ((<$>), (<+>), Doc)


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = OnSubmit Operation


type Operation
    = Hang
    | Align
    | Nest


init : ( Model, Cmd Msg )
init =
    ( Model "", Cmd.none )


type alias Model =
    { results : String
    }


view : Model -> Html Msg
view model =
    div
        []
        [ button
            [ Events.onClick (OnSubmit Hang) ]
            [ text "Hang" ]
        , button
            [ Events.onClick (OnSubmit Align) ]
            [ text "Align" ]
        , button
            [ Events.onClick (OnSubmit Nest) ]
            [ text "Nest" ]
        , hr [] []
        , div
            []
            (docStringToHtml model.results)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update (OnSubmit op) model =
    case op of
        Hang ->
            ( { model | results = Debug.showTheDoc 0 0 hangOperation }, Cmd.none )

        Align ->
            ( { model | results = Debug.showTheDoc 0 0 alignOperation }, Cmd.none )

        Nest ->
            ( { model | results = Debug.showTheDoc 0 0 nestOperation }, Cmd.none )


docStringToHtml : String -> List (Html Msg)
docStringToHtml str =
    str
        |> String.split "\n"
        |> List.map indent


indent : String -> Html Msg
indent str =
    let
        margin =
            String.length str - String.length (String.trimLeft str)
    in
    div
        [ style [ ( "margin-left", toString (margin * 10) ++ "px" ) ] ]
        [ text str ]


hangOperation : Doc
hangOperation =
    let
        words =
            "the hang combinator indents these words !"
    in
    words
        |> String.words
        |> List.map Text.text
        |> Text.fillSep
        |> Text.hang 4


alignOperation : Doc
alignOperation =
    let
        aligning left right =
            Text.align (left <$> right)
    in
    Text.text "hi" <+> aligning (Text.text "nice") (Text.text "world")


nestOperation : Doc
nestOperation =
    Text.nest 2 (Text.text "hello" <$> Text.text "world") <$> Text.text "!"
