module View exposing (view)

import Audio exposing (AudioData)
import BoundingBox2d exposing (BoundingBox2d)
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events
import Id exposing (Id, PlanetId)
import IdDict
import Length exposing (Length, Meters)
import Phosphor
import Point2d
import Product exposing (Product)
import Quantity
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import SvgAttributes
import Theme
import Types exposing (FactoryData, Link, Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))


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
            [ Svg.g [ Svg.Attributes.id "links" ]
                (viewLinks model)
            , Svg.g
                [ Svg.Attributes.id "planets" ]
                (viewPlanets model)
            ]
        ]
    , Html.div
        [ style "display" "flex"
        , style "gap" "16px"
        , style "flex-direction" "row"
        , style "max-width" "90vw"
        , style "position" "absolute"
        , style "position-anchor" playingFieldAnchor
        , style "position-area" "top"
        ]
        [ Html.div
            [ style "color" "white" ]
            [ Html.text ("Score: " ++ String.fromInt model.score) ]
        , Html.div [ style "flex" "1 0" ] []
        , Html.button [ Html.Events.onClick EndTurn ] [ Html.text "End turn" ]
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
                        (viewSelectedPlanet planetId planet
                            ++ (case planet.kind of
                                    VirginPlanet _ ->
                                        []

                                    ColonyPlanet _ ->
                                        []

                                    OccupiedPlanet (FarmPlanet _) ->
                                        viewLinkPossibilities model planetId planet

                                    OccupiedPlanet (FactoryPlanet _) ->
                                        viewLinkPossibilities model planetId planet

                                    OccupiedPlanet (DepositPlanet _) ->
                                        viewLinkPossibilities model planetId planet
                               )
                        )
    ]


maximumLinkLength : Length
maximumLinkLength =
    Length.lightYears 1.5


viewLinkPossibilities : PlayingModel -> Id PlanetId -> Planet -> List (Html PlayingMsg)
viewLinkPossibilities model from fromPlanet =
    let
        addLinks to toPlanet acc =
            viewLinkPossibility model
                { from = from
                , fromPlanet = fromPlanet
                , to = to
                , toPlanet = toPlanet
                }
                (IdDict.get to fromPlanet.links |> Maybe.withDefault [])
                ++ acc

        children : List (Html PlayingMsg)
        children =
            IdDict.fold
                (\to toPlanet acc ->
                    if Point2d.distanceFrom fromPlanet.position toPlanet.position |> Quantity.lessThan maximumLinkLength then
                        case toPlanet.kind of
                            VirginPlanet _ ->
                                acc

                            ColonyPlanet _ ->
                                addLinks to toPlanet acc

                            OccupiedPlanet (FarmPlanet _) ->
                                acc

                            OccupiedPlanet (FactoryPlanet _) ->
                                addLinks to toPlanet acc

                            OccupiedPlanet (DepositPlanet _) ->
                                addLinks to toPlanet acc

                    else
                        acc
                )
                []
                model.planets

        products =
            []

        columns : String
        columns =
            "auto"
                |> List.repeat (List.length products + 1)
                |> String.join " "
    in
    [ Html.p [ style "color" "white" ] [ Html.text "and is sending" ]
    , Html.div
        [ style "display" "grid"
        , style "grid-template-columns" columns
        , style "color" "white"
        , style "gap" "8px"
        ]
        children
    ]


viewLinkPossibility :
    PlayingModel
    ->
        { from : Id PlanetId
        , fromPlanet : Planet
        , to : Id PlanetId
        , toPlanet : { links : IdDict.IdDict PlanetId Link, name : String, kind : PlanetKind, position : Point2d.Point2d Meters () }
        }
    -> List { product : Product, quantity : Int }
    -> List (Html PlayingMsg)
viewLinkPossibility model endpoints link =
    [ Html.div
        [ style "display" "flex"
        , style "gap" "8px"
        ]
        [ Html.text "To"
        , Html.img
            [ Html.Attributes.src (planetImage endpoints.toPlanet)
            , style "width" "4vw"
            ]
            []
        , Html.text endpoints.toPlanet.name
        ]
    ]


viewSelectedPlanet : Id PlanetId -> Planet -> List (Html PlayingMsg)
viewSelectedPlanet planetId planet =
    case planet.kind of
        VirginPlanet options ->
            [ Html.p
                [ style "display" "block"
                , style "color" "white"
                , style "text-align" "center"
                , style "font-weight" "bold"
                ]
                [ Html.text ("Colonize planet " ++ planet.name)
                ]
            , selectionRow []
                (List.map (viewVirginPlanetOption planetId) options)
            ]

        ColonyPlanet colony ->
            [ Html.p
                [ style "color" "white" ]
                [ Html.text (planet.name ++ " ")
                , Html.span
                    [ style "font-weight" "bold" ]
                    [ Html.text "needs" ]
                ]
            , htmlTwoColumnGrid
                [ style "color" "white"
                ]
                [ Product colony.quantity colony.product
                , Timeout colony.timeout
                ]
            ]

        OccupiedPlanet (FarmPlanet farm) ->
            [ Html.p
                [ style "display" "block"
                , style "color" "white"
                , style "text-align" "center"
                , style "font-weight" "bold"
                ]
                [ Html.text ("The planet " ++ planet.name ++ " is producing")
                ]
            , htmlTwoColumnGrid
                [ style "align-self" "center"
                , style "color" "white"
                ]
                [ Product farm.perTurn farm.product
                , Timeout farm.timeout
                ]
            ]

        OccupiedPlanet (FactoryPlanet factory) ->
            if factory.efficiency == 0 then
                [ Html.p
                    [ style "display" "block"
                    , style "color" "white"
                    , style "text-align" "center"
                    , style "font-weight" "bold"
                    ]
                    [ Html.text "This factory is broken" ]
                ]

            else
                [ Html.p
                    [ style "display" "block"
                    , style "color" "white"
                    , style "text-align" "center"
                    , style "font-weight" "bold"
                    ]
                    [ case factory.order of
                        Just _ ->
                            Html.text ("The planet " ++ planet.name ++ " is producing")

                        Nothing ->
                            Html.text ("The planet " ++ planet.name ++ " can produce")
                    ]
                , selectionRow []
                    (List.filterMap (viewFactoryOption factory) Product.all)
                    |> Html.map (SetFactoryProduction planetId)
                ]

        OccupiedPlanet (DepositPlanet deposit) ->
            [ Html.p
                [ style "display" "block"
                , style "color" "white"
                , style "text-align" "center"
                , style "font-weight" "bold"
                ]
                [ if List.isEmpty deposit.content then
                    Html.text ("The planet " ++ planet.name ++ " is empty")

                  else
                    Html.text ("The planet " ++ planet.name ++ " contains")
                ]
            , htmlTwoColumnGrid []
                (List.map (\item -> Product item.quantity item.product) deposit.content)
            ]


viewFactoryOption : FactoryData -> Product -> Maybe (Html (Maybe Product))
viewFactoryOption factory product =
    Product.toRecipe product
        |> Maybe.map
            (\recipe ->
                let
                    selected : Bool
                    selected =
                        factory.order == Just product
                in
                Html.div
                    [ if selected then
                        style "background" "#f44"

                      else
                        style "background" "gray"
                    , style "padding" "8px"
                    , Html.Attributes.class "on-hover-highlight"
                    , if selected then
                        Html.Events.onClick Nothing

                      else
                        Html.Events.onClick (Just product)
                    , style "cursor" "pointer"
                    ]
                    [ htmlTwoColumnGrid []
                        [ Product factory.efficiency product
                        ]
                    , Html.text "from"
                    , htmlTwoColumnGrid []
                        (List.map
                            (\ingredient -> Product (factory.efficiency * ingredient.quantity) ingredient.product)
                            recipe
                        )
                    ]
            )


playingFieldAnchor : String
playingFieldAnchor =
    "--playing-field"


viewVirginPlanetOption : Id PlanetId -> OccupiedPlanet -> Html PlayingMsg
viewVirginPlanetOption planetId option =
    let
        ( background, children ) =
            case option of
                FarmPlanet { product, timeout, perTurn } ->
                    ( "#cfc"
                    , [ icon [ style "height" "30px" ]
                            { icon = Phosphor.tractor
                            , title = "Farm"
                            , color = Nothing
                            }
                      , htmlTwoColumnGrid []
                            [ Product perTurn product
                            , Timeout timeout
                            ]
                      ]
                    )

                FactoryPlanet { efficiency } ->
                    ( "#fcc"
                    , [ icon [ style "height" "30px" ]
                            { title = "Factory"
                            , icon = Phosphor.factory
                            , color = Nothing
                            }
                      , htmlTwoColumnGrid []
                            [ Efficiency efficiency
                            ]
                      ]
                    )

                DepositPlanet { capacity } ->
                    ( "#ccc"
                    , [ icon [ style "height" "30px" ]
                            { title = "Deposit"
                            , icon = Phosphor.warehouse
                            , color = Nothing
                            }
                      , htmlTwoColumnGrid []
                            [ Capacity capacity
                            ]
                      ]
                    )
    in
    Html.div
        [ style "border-radius" "4px"
        , style "gap" "4px"
        , style "padding" "4px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "background-color" background
        , style "width" "60px"
        , Html.Attributes.class "on-hover-highlight"
        , Html.Events.onClick (OccupyPlanet planetId option)
        ]
        children


htmlTwoColumnGrid : List (Attribute msg) -> List GridRow -> Html msg
htmlTwoColumnGrid attrs children =
    Html.div
        ([ style "display" "grid"
         , style "gap" "2px"
         , style "align-items" "center"
         , style "grid-template-columns" "auto auto"
         , style "text-align" "center"
         ]
            ++ attrs
        )
        (List.concatMap gridRowToHtml children)


type GridRow
    = Product Int Product
    | Timeout Int
    | Efficiency Int
    | Capacity Int


gridRowToHtml : GridRow -> List (Html msg)
gridRowToHtml gridRow =
    let
        ( text, rowIcon ) =
            gridRowToTuple gridRow
    in
    [ Html.div [] [ Html.text text ]
    , icon [] rowIcon
    ]


gridRowToTuple :
    GridRow
    ->
        ( String
        , { icon : Phosphor.IconWeight -> Phosphor.IconVariant
          , title : String
          , color : Maybe String
          }
        )
gridRowToTuple gridRow =
    case gridRow of
        Product quantity product ->
            ( String.fromInt quantity
            , { icon = Product.toIcon product
              , title = Product.toString product
              , color = Just (Product.toColor product)
              }
            )

        Timeout turns ->
            ( String.fromInt turns
            , { icon = Phosphor.hourglass
              , title = "Turns"
              , color = Nothing
              }
            )

        Efficiency efficiency ->
            ( String.fromInt efficiency
            , { icon = Phosphor.speedometer
              , title = "Production per turn"
              , color = Just "gray"
              }
            )

        Capacity capacity ->
            ( String.fromInt capacity
            , { icon = Phosphor.package
              , title = "Capacity"
              , color = Just "brown"
              }
            )


icon :
    List (Attribute msg)
    ->
        { icon : Phosphor.IconWeight -> Phosphor.IconVariant
        , title : String
        , color : Maybe String
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
                :: (case config.color of
                        Just color ->
                            style "color" color
                                :: attrs

                        Nothing ->
                            attrs
                   )
            )


bottomBox : List (Attribute msg) -> List (Html msg) -> Html msg
bottomBox attrs children =
    Html.div
        ([ style "display" "flex"
         , style "gap" "8px"
         , style "flex-direction" "column"
         , style "max-width" "90vw"
         , style "position" "absolute"
         , style "position-anchor" playingFieldAnchor
         , style "position-area" "bottom"
         , style "align-items" "center"
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


svgTwoColumnGrid :
    { x : Length
    , y : Length
    }
    -> List GridRow
    -> Svg msg
svgTwoColumnGrid config rows =
    rows
        |> List.indexedMap gridRowToSvg
        |> Svg.g
            [ SvgAttributes.transformTranslate
                { x = Length.inLightYears config.x
                , y = Length.inLightYears config.y
                }
            ]


gridRowToSvg : Int -> GridRow -> Svg msg
gridRowToSvg i row =
    let
        ( text, rowIcon ) =
            gridRowToTuple row
    in
    Svg.g
        [ SvgAttributes.transformTranslate
            { x = 0
            , y = (toFloat i + 1.1) * Theme.planetRadius * 1.3
            }
        ]
        [ rowIcon.icon Phosphor.Duotone
            |> Phosphor.withSize (1.1 * Theme.planetRadius)
            |> Phosphor.withSizeUnit ""
            |> Phosphor.toHtml [ Svg.Attributes.fill (Maybe.withDefault "white" rowIcon.color) ]
        , Svg.text_
            [ SvgAttributes.x (-Theme.planetRadius * 0.25)
            , Svg.Attributes.dominantBaseline "hanging"
            , Svg.Attributes.textAnchor "end"
            , SvgAttributes.fontSize (1.1 * Theme.planetRadius)
            , Svg.Attributes.fill "white"
            ]
            [ Svg.text text ]
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

        img : Svg PlayingMsg
        img =
            Svg.image
                [ SvgAttributes.x x
                , SvgAttributes.y y
                , SvgAttributes.width (Theme.planetRadius * 2)
                , SvgAttributes.height (Theme.planetRadius * 2)
                , Svg.Attributes.xlinkHref (planetImage planet)
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

        bottomView : Svg msg
        bottomView =
            case planet.kind of
                VirginPlanet options ->
                    options
                        |> List.filterMap
                            (\option ->
                                case option of
                                    FarmPlanet farm ->
                                        let
                                            quantity : Int
                                            quantity =
                                                farm.perTurn * farm.timeout
                                        in
                                        ( -quantity, Product quantity farm.product )
                                            |> Just

                                    FactoryPlanet _ ->
                                        Nothing

                                    DepositPlanet _ ->
                                        Nothing
                            )
                        |> List.sortBy Tuple.first
                        |> List.map Tuple.second
                        |> svgTwoColumnGrid
                            { x = cx
                            , y = cy
                            }

                ColonyPlanet colony ->
                    svgTwoColumnGrid
                        { x = cx
                        , y = cy
                        }
                        [ Product colony.quantity colony.product
                        , Timeout colony.timeout
                        ]

                OccupiedPlanet (FarmPlanet farm) ->
                    svgTwoColumnGrid
                        { x = cx
                        , y = cy
                        }
                        [ Product farm.perTurn farm.product
                        , Timeout farm.timeout
                        ]

                OccupiedPlanet (FactoryPlanet factory) ->
                    svgTwoColumnGrid
                        { x = cx
                        , y = cy
                        }
                        (case factory.order of
                            Just product ->
                                [ Product factory.efficiency product
                                ]

                            Nothing ->
                                [ Efficiency factory.efficiency
                                ]
                        )

                OccupiedPlanet (DepositPlanet deposit) ->
                    svgTwoColumnGrid { x = cx, y = cy }
                        (Capacity deposit.capacity
                            :: List.map
                                (\{ product, quantity } -> Product quantity product)
                                deposit.content
                        )
    in
    Svg.g
        [ Svg.Attributes.id (Id.toString id)
        , Svg.Events.onClick (SelectPlanet id)
        , Svg.Events.onMouseOver (HighlightPlanet id)
        , Svg.Events.onMouseOut HighlightNone
        , Svg.Attributes.cursor "pointer"
        ]
        (img :: bottomView :: selectionView)


planetImage : Planet -> String
planetImage planet =
    case planet.kind of
        VirginPlanet _ ->
            Theme.planetIce

        ColonyPlanet _ ->
            Theme.planetTerran

        OccupiedPlanet (FarmPlanet { timeout }) ->
            if timeout == 0 then
                Theme.planetBlackHole

            else
                Theme.planetTerran

        OccupiedPlanet (FactoryPlanet _) ->
            Theme.planetLava

        OccupiedPlanet (DepositPlanet _) ->
            Theme.planetBarren


viewLinks : PlayingModel -> List (Svg PlayingMsg)
viewLinks model =
    IdDict.fold
        (\from fromPlanet acc ->
            IdDict.fold
                (\to link iacc ->
                    case viewLink model from fromPlanet to link of
                        Nothing ->
                            iacc

                        Just linkView ->
                            linkView :: iacc
                )
                acc
                fromPlanet.links
        )
        []
        model.planets


viewLink : PlayingModel -> Id PlanetId -> Planet -> Id PlanetId -> Link -> Maybe (Svg PlayingMsg)
viewLink model from fromPlanet to _ =
    IdDict.get to model.planets
        |> Maybe.map
            (\toPlanet ->
                let
                    ( x1, y1 ) =
                        Point2d.coordinates fromPlanet.position

                    ( x2, y2 ) =
                        Point2d.coordinates toPlanet.position
                in
                Svg.line
                    [ SvgAttributes.x1 (Length.inLightYears x1)
                    , SvgAttributes.y1 (Length.inLightYears y1)
                    , SvgAttributes.x2 (Length.inLightYears x2)
                    , SvgAttributes.y2 (Length.inLightYears y2)
                    , Svg.Events.onClick (SelectPlanet from)
                    , Svg.Attributes.stroke "white"
                    , Svg.Events.onMouseOver (HighlightLink from to)
                    , Svg.Events.onMouseOut HighlightNone
                    ]
                    []
            )
