module Main exposing (..)

import Html exposing (Html, program)
import Html.Events exposing (onInput)


type alias Model =
    { name : String
    , formatted : String
    }


type Msg
    = UpdateName String
    | FormatName


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName name ->
            ( { model | name = name }, Debounce.debounce FormatName )

        FormatName ->
            ( { model | formatted = model.name }, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.input [ onInput UpdateName ] [ Html.text model.name ]
        , Html.text <| "Formatted: " ++ model.formatted
        ]


main : Program Never Model Msg
main =
    program
        { init = (( { name = "", formatted = "" }, Cmd.none ))
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }
