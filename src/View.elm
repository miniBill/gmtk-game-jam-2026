module View exposing (view)

import Audio exposing (AudioData)
import BoundingBox2d exposing (BoundingBox2d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Id exposing (Id, LinkId, PlanetId)
import IdDict
import Length exposing (Length, Meters)
import Point2d
import Quantity
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Types exposing (Link, Model, Msg(..), Page(..), Planet, PlayingModel, PlayingMsg(..), Selected(..))


view : AudioData -> Model -> Html Msg
view audioData model =
    Html.main_
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100dvh"
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
                    |> List.map (Html.map PlayingMsg)
        )


viewPlaying : AudioData -> Model -> PlayingModel -> List (Html PlayingMsg)
viewPlaying audioData model playingModel =
    let
        boundingBox : BoundingBox2d Meters ()
        boundingBox =
            playingModel.planets
                |> IdDict.values
                |> List.map .position
                |> BoundingBox2d.hull Point2d.origin

        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema boundingBox

        viewBox : String
        viewBox =
            let
                padding : Length
                padding =
                    Length.lightYear
            in
            [ minX |> Quantity.minus padding
            , minY |> Quantity.minus padding
            , Quantity.difference maxX minX |> Quantity.plus (Quantity.multiplyBy 2 padding)
            , Quantity.difference maxY minY |> Quantity.plus (Quantity.multiplyBy 2 padding)
            ]
                |> List.map (\l -> String.fromFloat (Length.inLightYears l))
                |> String.join " "
    in
    [ Svg.svg
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "auto"
        , Svg.Attributes.viewBox viewBox
        ]
        [ viewEarth playingModel
        , Svg.g
            [ Svg.Attributes.id "planets" ]
            (viewPlanets playingModel)
        , Svg.g [ Svg.Attributes.id "links" ]
            (viewLinks playingModel)
        ]
    ]


planetRadius : Float
planetRadius =
    0.1


viewEarth : PlayingModel -> Svg PlayingMsg
viewEarth model =
    Svg.circle
        [ Svg.Attributes.r (String.fromFloat (1.1 * planetRadius))
        , Svg.Attributes.fill "#55f"
        , Svg.Events.onClick SelectEarth
        , if model.selected == SelectedEarth then
            Svg.Attributes.stroke "red"

          else
            Svg.Attributes.stroke "green"
        , Svg.Attributes.strokeWidth "0.025"
        , Svg.Attributes.cursor "pointer"
        ]
        []


viewPlanets : PlayingModel -> List (Svg PlayingMsg)
viewPlanets model =
    IdDict.fold (\k v acc -> viewPlanet model.selected k v :: acc) [] model.planets


viewPlanet : Selected -> Id PlanetId -> Planet -> Svg PlayingMsg
viewPlanet selected id planet =
    Svg.circle
        [ Svg.Attributes.r (String.fromFloat planetRadius)
        , cx (Point2d.xCoordinate planet.position)
        , cy (Point2d.yCoordinate planet.position)
        , Svg.Attributes.fill "#5f5"
        , Svg.Events.onClick (SelectPlanet id)
        , if selected == SelectedPlanet id then
            Svg.Attributes.stroke "red"

          else
            Svg.Attributes.stroke "green"
        , Svg.Attributes.strokeWidth "0.025"
        , Svg.Attributes.cursor "pointer"
        ]
        []


cx : Length -> Svg.Attribute msg
cx x =
    Svg.Attributes.cx (String.fromFloat (Length.inLightYears x))


cy : Length -> Svg.Attribute msg
cy y =
    Svg.Attributes.cy (String.fromFloat (Length.inLightYears y))


viewLinks : PlayingModel -> List (Svg PlayingMsg)
viewLinks model =
    IdDict.fold (\k v acc -> viewLink k v :: acc) [] model.links


viewLink : Id LinkId -> Link -> Svg PlayingMsg
viewLink id link =
    Svg.text_ [] [ Svg.text "TODO: viewPlanet" ]
