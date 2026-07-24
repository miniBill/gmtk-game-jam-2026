port module Main exposing (main)

import Audio exposing (Audio, AudioCmd, AudioData)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Json.Encode
import Quantity
import Random
import Task
import Time


type alias Model =
    { page : Page
    , sound : Maybe ( Audio.Source, Time.Posix )
    }


type Page
    = Menu
    | Playing PlayingModel


type alias PlayingModel =
    { initialSeed : Int
    , currentSeed : Random.Seed
    }


type Msg
    = Play
    | TimeResult Audio.Source Time.Posix
    | InitialSeed Int
    | PlaySound
    | AudioLoadResult (Result Audio.LoadError Audio.Source)


main : Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , audio = audio
        , audioPort =
            { fromJS = audioPortFromJS
            , toJS = audioPortToJS
            }
        }


port audioPortToJS : Json.Encode.Value -> Cmd msg


port audioPortFromJS : (Json.Decode.Value -> msg) -> Sub msg


audio : AudioData -> Model -> Audio
audio audioData model =
    case model.sound of
        Nothing ->
            Audio.silence

        Just ( source, at ) ->
            Audio.audioWithConfig
                { loop = Nothing
                , playbackRate = 1
                , startAt = Quantity.zero
                }
                source
                at


init : () -> ( Model, Cmd Msg, AudioCmd Msg )
init () =
    ( { page = Menu
      , sound = Nothing
      }
    , Cmd.none
    , Audio.cmdNone
    )


view : AudioData -> Model -> Html Msg
view audioData model =
    Html.main_
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "8px"
        ]
        (case model.page of
            Menu ->
                [ Html.button
                    [ Html.Events.onClick Play
                    ]
                    [ Html.text "Play game" ]

                -- , Html.button
                --     [ Html.Events.onClick PlaySound
                --     ]
                --     [ Html.text "Play sound" ]
                ]

            Playing playingModel ->
                viewPlaying audioData model playingModel
        )


viewPlaying : AudioData -> Model -> PlayingModel -> List (Html Msg)
viewPlaying audioData model playingModel =
    [ Html.text "TODO" ]


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update audioData msg model =
    case
        -- Debug.log "msg"
        msg
    of
        PlaySound ->
            case model.sound of
                Nothing ->
                    ( model
                    , Cmd.none
                    , Audio.loadAudio AudioLoadResult "/media/Interface_Bleeps_OGG/Bleep_05.ogg"
                    )

                Just ( sound, _ ) ->
                    ( model
                    , Time.now |> Task.perform (TimeResult sound)
                    , Audio.cmdNone
                    )

        Play ->
            ( model
            , Random.int 0 Random.maxInt |> Random.generate InitialSeed
            , Audio.cmdNone
            )

        InitialSeed initialSeed ->
            ( { model
                | page =
                    Playing
                        { initialSeed = initialSeed
                        , currentSeed = Random.initialSeed initialSeed
                        }
              }
            , Cmd.none
            , Audio.cmdNone
            )

        AudioLoadResult (Err e) ->
            let
                _ =
                    Debug.log "AudioLoadResult (Err e)" e
            in
            ( model, Cmd.none, Audio.cmdNone )

        AudioLoadResult (Ok sound) ->
            ( model, Time.now |> Task.perform (TimeResult sound), Audio.cmdNone )

        TimeResult sound now ->
            ( { model | sound = Just ( sound, now ) }, Cmd.none, Audio.cmdNone )


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
