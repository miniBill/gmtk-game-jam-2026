module View exposing (view)

import Audio exposing (AudioData)
import BoundingBox2d exposing (BoundingBox2d)
import Data
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events
import Id exposing (Id, LinkId, PlanetId)
import IdDict
import Length exposing (Length, Meters)
import Phosphor
import Point2d
import Quantity
import String.Extra
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import SvgAttributes
import Theme
import Types exposing (Link, Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))


view : AudioData -> Model -> Html Msg
view _ model =
    Html.main_
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "8px"
        , style "align-items" "center"
        , style "justify-content" "center"
        , style "padding" "8px"
        , style "width" "100vw"
        , style "height" "100dvh"
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
                viewPlaying playingModel
                    |> List.map (Html.map PlayingMsg)
        )


viewPlaying : PlayingModel -> List (Html PlayingMsg)
viewPlaying model =
    let
        boundingBox : BoundingBox2d Meters ()
        boundingBox =
            model.planets
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
    [ Html.div
        [ style "width" "100%"
        , style "display" "block"
        , style "max-height" "calc(100dvh - 200px)"
        , style "anchor-name" playingFieldAnchor
        ]
        [ Svg.svg
            [ style "width" "100%"
            , style "height" "auto"
            , style "max-height" "100%"
            , Svg.Attributes.viewBox viewBox
            ]
            [ viewEarth model
            , Svg.g
                [ Svg.Attributes.id "planets" ]
                (viewPlanets model)
            , Svg.g [ Svg.Attributes.id "links" ]
                (viewLinks model)
            ]
        ]
    , case model.selected of
        SelectedNone ->
            Html.text ""

        SelectedPlanet planetId ->
            case IdDict.get planetId model.planets of
                Nothing ->
                    Html.text ""

                Just planet ->
                    bottomBox []
                        (case planet.kind of
                            VirginPlanet options ->
                                [ Html.p
                                    [ style "display" "block"
                                    , style "color" "white"
                                    , style "text-align" "center"
                                    , style "font-weight" "bold"
                                    ]
                                    [ Html.text
                                        ("Colonize planet "
                                            ++ String.Extra.toSentenceCase planet.name
                                        )
                                    ]
                                , selectionRow []
                                    (List.map (viewVirginPlanetOption planetId) options)
                                ]

                            OccupiedPlanet _ ->
                                [ Html.div
                                    [ style "background" "white"
                                    , style "padding" "8px"
                                    ]
                                    [ Html.text "branch 'OccupiedPlanet _' not implemented" ]
                                ]
                        )

        SelectedLink _ ->
            bottomBox []
                [ Html.div
                    [ style "background" "white"
                    , style "padding" "8px"
                    ]
                    [ Html.text "branch 'SelectedLink _' not implemented" ]
                ]

        SelectedEarth ->
            bottomBox []
                [ Html.div
                    [ style "background" "white"
                    , style "padding" "8px"
                    ]
                    [ Html.text "branch 'SelectedEarth' not implemented" ]
                ]
    ]


playingFieldAnchor : String
playingFieldAnchor =
    "--playing-field"


viewVirginPlanetOption : Id PlanetId -> OccupiedPlanet -> Html PlayingMsg
viewVirginPlanetOption planetId option =
    Html.div
        [ style "border-radius" "4px"
        , style "gap" "4px"
        , style "padding" "4px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "background-color" "lightgray"
        , style "width" "60px"
        , Html.Attributes.class "on-hover-highlight"
        , Html.Events.onClick (OccupyPlanet planetId option)
        ]
        (case option of
            FarmPlanet { product, turnsLeft, perTurn } ->
                [ icon [ style "height" "30px" ]
                    { icon = Phosphor.tractor
                    , title = "Farm"
                    }
                , Html.div
                    [ style "display" "grid"
                    , style "gap" "2px"
                    , style "align-items" "center"
                    , style "grid-template-columns" "auto auto"
                    , style "text-align" "center"
                    ]
                    [ Html.div [] [ Html.text (String.fromInt perTurn) ]
                    , icon []
                        { icon = Data.productToIcon product
                        , title = Data.productToString product
                        }
                    , Html.div [] [ Html.text (String.fromInt turnsLeft) ]
                    , icon []
                        { title = "Turns"
                        , icon = Phosphor.hourglass
                        }
                    ]
                ]

            FactoryPlanet _ ->
                [ Phosphor.factory Phosphor.Duotone
                    |> Phosphor.withSize 100
                    |> Phosphor.withSizeUnit "%"
                    |> Phosphor.toHtml []
                    |> List.singleton
                    |> Html.div
                        [ style "width" "50%"
                        , Html.Attributes.title "Factory"
                        ]
                ]

            DepositPlanet _ ->
                [ Phosphor.warehouse Phosphor.Duotone
                    |> Phosphor.withSize 100
                    |> Phosphor.withSizeUnit "%"
                    |> Phosphor.toHtml []
                    |> List.singleton
                    |> Html.div
                        [ style "width" "50%"
                        , Html.Attributes.title "Deposit"
                        ]
                ]
        )


icon :
    List (Attribute msg)
    ->
        { icon : Phosphor.IconWeight -> Phosphor.IconVariant
        , title : String
        }
    -> Html msg
icon attrs config =
    config.icon Phosphor.Duotone
        |> Phosphor.withSize 100
        |> Phosphor.withSizeUnit "%"
        |> Phosphor.toHtml []
        |> List.singleton
        |> Html.div
            (style "height" "16px"
                :: Html.Attributes.title config.title
                :: attrs
            )


bottomBox : List (Attribute msg) -> List (Html msg) -> Html msg
bottomBox attrs children =
    Html.div
        ([ style "display" "flex"
         , style "gap" "16px"
         , style "flex-direction" "column"
         , style "max-width" "90vw"
         , style "position" "absolute"
         , style "position-anchor" playingFieldAnchor
         , style "position-area" "bottom"
         ]
            ++ attrs
        )
        children


selectionRow : List (Attribute msg) -> List (Html msg) -> Html msg
selectionRow attrs children =
    Html.div
        ([ style "display" "flex"
         , style "flex-wrap" "wrap"
         , style "gap" "16px"
         , style "justify-content" "center"
         , style "align-items" "stretch"
         ]
            ++ attrs
        )
        children


viewEarth : PlayingModel -> Svg PlayingMsg
viewEarth model =
    Svg.g
        [ Svg.Attributes.id "earth"
        , Svg.Events.onClick SelectEarth
        , Svg.Attributes.cursor "pointer"
        ]
        [ if model.selected == SelectedEarth then
            Svg.circle
                [ SvgAttributes.r (Theme.planetRadius * 1.3)
                , Svg.Attributes.fill "green"
                ]
                []

          else
            Svg.text ""
        , Svg.image
            [ SvgAttributes.x -Theme.planetRadius
            , SvgAttributes.y -Theme.planetRadius
            , SvgAttributes.width (Theme.planetRadius * 2)
            , SvgAttributes.height (Theme.planetRadius * 2)
            , Svg.Attributes.xlinkHref Theme.planetTerran
            ]
            []
        , Phosphor.hourglass Phosphor.Duotone
            |> Phosphor.withSize Theme.planetRadius
            |> Phosphor.withSizeUnit ""
            |> Phosphor.toHtml [ SvgAttributes.y Theme.planetRadius ]
        , Svg.text_
            [ SvgAttributes.fontSize Theme.planetRadius ]
            [ Svg.text (String.fromInt model.earthNeed.timeout) ]
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
                ]
                []

        selectionView : List (Svg msg)
        selectionView =
            if selected == SelectedPlanet id then
                [ Svg.circle
                    [ SvgAttributes.cx (Length.inLightYears cx)
                    , SvgAttributes.cy (Length.inLightYears cy)
                    , SvgAttributes.r (Theme.planetRadius * 1.3)
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
        [ Svg.Attributes.id (Id.toString id)
        , Svg.Events.onClick (SelectPlanet id)
        , Svg.Attributes.cursor "pointer"
        ]
        (img :: selectionView)


viewLinks : PlayingModel -> List (Svg PlayingMsg)
viewLinks model =
    IdDict.fold (\k v acc -> viewLink k v :: acc) [] model.links


viewLink : Id LinkId -> Link -> Svg PlayingMsg
viewLink id link =
    Svg.text_ [] [ Svg.text "TODO: viewPlanet" ]
