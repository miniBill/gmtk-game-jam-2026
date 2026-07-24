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
import Round
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Theme
import Types exposing (Link, Model, Msg(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))


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
                |> List.map (\l -> Round.round 2 (Length.inLightYears l))
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


viewEarth : PlayingModel -> Svg PlayingMsg
viewEarth model =
    let
        x : Float
        x =
            -Theme.planetRadius

        y : Float
        y =
            -Theme.planetRadius
    in
    Svg.g [ Svg.Attributes.id "earth" ]
        [ Svg.image
            [ Svg.Attributes.x (Round.round 2 x)
            , Svg.Attributes.y (Round.round 2 y)
            , Svg.Attributes.width (Round.round 2 (Theme.planetRadius * 2))
            , Svg.Attributes.height (Round.round 2 (Theme.planetRadius * 2))
            , Svg.Attributes.xlinkHref Theme.planetTerran
            , Svg.Events.onClick SelectEarth
            , Svg.Attributes.cursor "pointer"
            ]
            []
        , if model.selected == SelectedEarth then
            Svg.circle
                [ Svg.Attributes.r (Round.round 2 (Theme.planetRadius * 1.1))
                , Svg.Attributes.fill "transparent"
                , Svg.Attributes.strokeWidth "0.024"
                , Svg.Attributes.stroke "red"
                ]
                []

          else
            Svg.text ""
        ]


viewPlanets : PlayingModel -> List (Svg PlayingMsg)
viewPlanets model =
    IdDict.fold (\k v acc -> viewPlanet model.selected k v :: acc) [] model.planets


viewPlanet : Selected -> Id PlanetId -> Planet -> Svg PlayingMsg
viewPlanet selected id planet =
    let
        ( cx, cy ) =
            Point2d.coordinates planet.position

        x : Float
        x =
            Length.inLightYears cx - Theme.planetRadius

        y : Float
        y =
            Length.inLightYears cy - Theme.planetRadius
    in
    Svg.g []
        [ Svg.image
            [ Svg.Attributes.x (Round.round 2 x)
            , Svg.Attributes.y (Round.round 2 y)
            , Svg.Attributes.width (Round.round 2 (Theme.planetRadius * 2))
            , Svg.Attributes.height (Round.round 2 (Theme.planetRadius * 2))
            , case planet.kind of
                VirginPlanet _ ->
                    Svg.Attributes.xlinkHref Theme.planetIce

                OccupiedPlanet _ ->
                    Svg.Attributes.xlinkHref Theme.planetLava
            , Svg.Events.onClick (SelectPlanet id)
            , Svg.Attributes.cursor "pointer"
            ]
            []
        , if selected == SelectedPlanet id then
            Svg.circle
                [ Svg.Attributes.cx (Round.round 2 (Length.inLightYears cx))
                , Svg.Attributes.cy (Round.round 2 (Length.inLightYears cy))
                , Svg.Attributes.r (Round.round 2 (Theme.planetRadius * 1.1))
                , Svg.Attributes.fill "transparent"
                , Svg.Attributes.strokeWidth "0.024"
                , Svg.Attributes.stroke "red"
                ]
                []

          else
            Svg.text ""
        ]


viewLinks : PlayingModel -> List (Svg PlayingMsg)
viewLinks model =
    IdDict.fold (\k v acc -> viewLink k v :: acc) [] model.links


viewLink : Id LinkId -> Link -> Svg PlayingMsg
viewLink id link =
    Svg.text_ [] [ Svg.text "TODO: viewPlanet" ]
