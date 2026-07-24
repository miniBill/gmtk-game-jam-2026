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
import SvgAttributes
import Theme
import Types exposing (Link, Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))


view : AudioData -> Model -> Html Msg
view audioData model =
    Html.main_
        [ Html.Attributes.style "display" "flex"
        , Html.Attributes.style "gap" "8px"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "justify-content" "center"
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
                    Length.lightYears 1.5
            in
            SvgAttributes.viewBoxWithPadding padding
                minX
                minY
                (Quantity.difference maxX minX)
                (Quantity.difference maxY minY)
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
    Svg.g [ Svg.Attributes.id "earth" ]
        [ Svg.image
            [ SvgAttributes.x -Theme.planetRadius
            , SvgAttributes.y -Theme.planetRadius
            , SvgAttributes.width (Theme.planetRadius * 2)
            , SvgAttributes.height (Theme.planetRadius * 2)
            , Svg.Attributes.xlinkHref Theme.planetTerran
            , Svg.Events.onClick SelectEarth
            , Svg.Attributes.cursor "pointer"
            ]
            []
        , if model.selected == SelectedEarth then
            Svg.circle
                [ SvgAttributes.r (Theme.planetRadius * 1.1)
                , Svg.Attributes.fill "transparent"
                , SvgAttributes.strokeWidth 0.024
                , Svg.Attributes.stroke "green"
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

        src : String
        src =
            case planet.kind of
                VirginPlanet _ ->
                    Theme.planetBarren

                OccupiedPlanet (FarmPlanet { turnsLeft }) ->
                    if turnsLeft == 0 then
                        Theme.planetBlackHole

                    else
                        Theme.planetTerran

                OccupiedPlanet (FactoryPlanet _) ->
                    Theme.planetLava

                OccupiedPlanet (DepositPlanet _) ->
                    Theme.planetIce

        img : Svg PlayingMsg
        img =
            Svg.image
                [ SvgAttributes.x x
                , SvgAttributes.y y
                , SvgAttributes.width (Theme.planetRadius * 2)
                , SvgAttributes.height (Theme.planetRadius * 2)
                , Svg.Attributes.xlinkHref src
                , Svg.Events.onClick (SelectPlanet id)
                , Svg.Attributes.cursor "pointer"
                ]
                []

        selectionView : List (Svg msg)
        selectionView =
            if selected == SelectedPlanet id then
                [ Svg.circle
                    [ SvgAttributes.cx (Length.inLightYears cx)
                    , SvgAttributes.cy (Length.inLightYears cy)
                    , SvgAttributes.r (Theme.planetRadius * 1.1)
                    , Svg.Attributes.fill "transparent"
                    , SvgAttributes.strokeWidth 0.024
                    , Svg.Attributes.stroke "green"
                    ]
                    []
                ]

            else
                []
    in
    Svg.g
        [ Svg.Attributes.id (Id.toString id) ]
        (img :: selectionView)


viewLinks : PlayingModel -> List (Svg PlayingMsg)
viewLinks model =
    IdDict.fold (\k v acc -> viewLink k v :: acc) [] model.links


viewLink : Id LinkId -> Link -> Svg PlayingMsg
viewLink id link =
    Svg.text_ [] [ Svg.text "TODO: viewPlanet" ]
