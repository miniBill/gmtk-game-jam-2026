module Main exposing (main)

import Browser
import Html exposing (Html)


type alias Model =
    {}


type Msg
    = Noop


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( {}, Cmd.none )


view : Model -> Html Msg
view model =
    Html.main_
        []
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
