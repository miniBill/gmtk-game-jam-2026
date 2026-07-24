module View exposing (..)

import Audio exposing (AudioData)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Types exposing (Model, Msg(..), Page(..), PlayingModel)


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
