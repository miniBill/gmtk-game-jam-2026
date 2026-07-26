port module Main exposing (main)

import Audio exposing (Audio, AudioData)
import Browser.Events
import Json.Decode
import Json.Encode
import Quantity
import Types exposing (Model, Msg(..), Page(..))
import Update
import View


main : Program () (Audio.Model Msg Model) (Audio.Msg Msg)
main =
    Audio.elementWithAudio
        { init = Update.init
        , view = View.view
        , update = Update.update
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
audio _ model =
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


subscriptions : AudioData -> Model -> Sub Msg
subscriptions _ _ =
    Browser.Events.onResize Resized
