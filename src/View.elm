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
import Product.Dict
import Quantity
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import SvgAttributes
import Theme
import Types exposing (FactoryData, Highlighted(..), Link, Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))


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

            Lost lostModel ->
                [ Html.p
                    [ style "color" "white"
                    ]
                    [ Html.text ("Final score: " ++ String.fromInt lostModel.score) ]
                , Html.button
                    [ Html.Events.onClick Play
                    ]
                    [ Html.text "Play game" ]

                -- , Html.button
                --     [ Html.Events.onClick PlaySound
                --     ]
                --     [ Html.text "Play sound" ]
                ]
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
                    maximumLinkLength
            in
            SvgAttributes.viewBoxWithPadding padding
                minX
                minY
                (Quantity.difference maxX minX)
                (Quantity.difference maxY minY |> Quantity.plus Length.lightYear)
    in
    [ Html.div
        [ style "display" "flex"
        , style "gap" "16px"
        , style "flex-direction" "row"
        , style "max-width" "90vw"
        ]
        [ Html.div
            [ style "color" "white" ]
            [ Html.text ("Score: " ++ String.fromInt model.score) ]
        , Html.div [ style "flex" "1 0" ] []
        , Html.button [ Html.Events.onClick EndTurn ] [ Html.text "End turn" ]
        ]
    , let
        ( planetsViews, background ) =
            viewPlanets model
      in
      Svg.svg
        [ style "display" "block"
        , style "max-width" "90vmin"
        , Svg.Attributes.viewBox viewBox
        ]
        [ Svg.defs []
            [ selectionGradient.def
            , linkGradient.def
            , fadeFilter.def
            ]
        , Svg.g [ Svg.Attributes.id "background" ] background
        , Svg.g [ Svg.Attributes.id "links" ]
            (viewLinks model)
        , Svg.g
            [ Svg.Attributes.id "planets" ]
            planetsViews
        ]
    , Html.div [ style "flex" "1 0" ] []
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


selectionGradient : { ref : String, def : Svg msg }
selectionGradient =
    let
        id : String
        id =
            "selection-gradient"
    in
    { ref = "url(#" ++ id ++ ")"
    , def =
        Svg.radialGradient
            [ Svg.Attributes.id id
            ]
            [ Svg.stop
                [ Svg.Attributes.offset "0%"
                , Svg.Attributes.stopColor "#4f45"
                ]
                []
            , Svg.stop
                [ Svg.Attributes.offset "100%"
                , Svg.Attributes.stopColor "#4f43"
                ]
                []
            ]
    }


linkGradient : { ref : String, def : Svg msg }
linkGradient =
    let
        id : String
        id =
            "link-gradient"
    in
    { ref = "url(#" ++ id ++ ")"
    , def =
        Svg.linearGradient
            [ Svg.Attributes.id id
            ]
            [ Svg.stop
                [ Svg.Attributes.offset "0%"
                , Svg.Attributes.stopColor "blue"
                ]
                []
            , Svg.stop
                [ Svg.Attributes.offset "100%"
                , Svg.Attributes.stopColor "red"
                ]
                []
            ]
    }


fadeFilter : { ref : String, def : Svg msg }
fadeFilter =
    let
        id : String
        id =
            "fade-filter"
    in
    { ref = "url(#" ++ id ++ ")"
    , def =
        Svg.filter [ Svg.Attributes.id id ]
            [ Svg.feColorMatrix
                [ Svg.Attributes.type_ "saturate"
                , Svg.Attributes.in_ "SourceGraphic"
                , Svg.Attributes.values "0.2"
                ]
                []
            ]
    }


maximumLinkLength : Length
maximumLinkLength =
    Length.lightYears 2


viewLinkPossibilities : PlayingModel -> Id PlanetId -> Planet -> List (Html PlayingMsg)
viewLinkPossibilities model from fromPlanet =
    let
        addLinks :
            Id PlanetId
            -> { links : IdDict.IdDict PlanetId Link, name : String, kind : PlanetKind, position : Point2d.Point2d Meters () }
            -> List (Html PlayingMsg)
            -> List (Html PlayingMsg)
        addLinks to toPlanet acc =
            viewLinkPossibility model
                { from = from
                , fromPlanet = fromPlanet
                , to = to
                , toPlanet = toPlanet
                , available = products
                , link =
                    IdDict.get to fromPlanet.links
                        |> Maybe.withDefault Product.Dict.empty
                }
                ++ acc

        children : List (Html PlayingMsg)
        children =
            IdDict.fold
                (\to toPlanet acc ->
                    if
                        (Point2d.distanceFrom fromPlanet.position toPlanet.position
                            |> Quantity.lessThan maximumLinkLength
                        )
                            && (to /= from)
                    then
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

        products : List { product : Product, quantity : Int }
        products =
            case fromPlanet.kind of
                VirginPlanet _ ->
                    []

                ColonyPlanet _ ->
                    []

                OccupiedPlanet (FarmPlanet { product, perTurn }) ->
                    [ { product = product, quantity = perTurn } ]

                OccupiedPlanet (FactoryPlanet { order, efficiency }) ->
                    case order of
                        Just product ->
                            [ { product = product
                              , quantity = efficiency
                              }
                            ]

                        Nothing ->
                            []

                OccupiedPlanet (DepositPlanet { content }) ->
                    Product.Dict.toList content
                        |> List.map
                            (\( product, quantity ) ->
                                { product = product
                                , quantity = quantity
                                }
                            )

        columns : String
        columns =
            "auto"
                |> List.repeat (List.length products + 1)
                |> String.join " "

        header : List (Html msg)
        header =
            Html.div [] []
                :: List.map
                    (\{ product } ->
                        icon
                            [ style "padding" "4px"
                            , style "height" "28px"
                            ]
                            { icon = Product.toIcon product
                            , title = Product.toString product
                            , color = Just (Product.toColor product)
                            }
                    )
                    products
    in
    [ Html.p [ style "color" "white" ] [ Html.text "and is sending" ]
    , Html.div
        [ style "display" "grid"
        , style "grid-template-columns" columns
        , style "color" "white"
        ]
        (header ++ children)
    ]


viewLinkPossibility :
    PlayingModel
    ->
        { from : Id PlanetId
        , fromPlanet : Planet
        , to : Id PlanetId
        , toPlanet : { links : IdDict.IdDict PlanetId Link, name : String, kind : PlanetKind, position : Point2d.Point2d Meters () }
        , available : List { product : Product, quantity : Int }
        , link : Link
        }
    -> List (Html PlayingMsg)
viewLinkPossibility model config =
    let
        nameView : Html PlayingMsg
        nameView =
            Html.div
                [ style "display" "flex"
                , style "gap" "8px"
                , style "padding" "4px"
                , if model.highlighted == HighlightedPlanet config.to then
                    style "background" "#fff8"

                  else
                    Html.Attributes.classList []
                , Html.Events.onMouseEnter (HighlightPlanet config.to)
                , Html.Events.onMouseLeave HighlightNone
                ]
                [ Html.text "To"
                , let
                    ( src, { fade } ) =
                        planetImage config.toPlanet
                  in
                  Html.img
                    [ Html.Attributes.src src
                    , style "width" "32px"
                    , style "transform" "translate(0, -4px)"
                    , if fade then
                        style "filter" "saturate(10%)"

                      else
                        Html.Attributes.classList []
                    ]
                    []
                , Html.text config.toPlanet.name
                ]

        viewProductInput : { product : Product, quantity : Int } -> Html PlayingMsg
        viewProductInput { product, quantity } =
            let
                value : Int
                value =
                    Product.Dict.get product config.link
                        |> Maybe.withDefault 0
            in
            Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.min "0"
                , value
                    |> String.fromInt
                    |> Html.Attributes.value
                , Html.Attributes.max (String.fromInt quantity)
                , Html.Events.onInput
                    (\v ->
                        v
                            |> String.toInt
                            |> Maybe.withDefault value
                            |> SetLink config.from config.to product
                    )
                ]
                []
    in
    nameView :: List.map viewProductInput config.available


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
                , Countdown colony.countdown
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
                , Countdown farm.countdown
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
                [ if Product.Dict.isEmpty deposit.content then
                    Html.text ("The planet " ++ planet.name ++ " is empty")

                  else
                    Html.text ("The planet " ++ planet.name ++ " contains")
                ]
            , deposit.content
                |> Product.Dict.toList
                |> List.map (\( product, quantity ) -> Product quantity product)
                |> htmlTwoColumnGrid []
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


viewVirginPlanetOption : Id PlanetId -> OccupiedPlanet -> Html PlayingMsg
viewVirginPlanetOption planetId option =
    let
        ( background, children ) =
            case option of
                FarmPlanet { product, countdown, perTurn } ->
                    ( "#cfc"
                    , [ icon [ style "height" "30px" ]
                            { icon = Phosphor.tractor
                            , title = "Farm"
                            , color = Nothing
                            }
                      , htmlTwoColumnGrid []
                            [ Product perTurn product
                            , Countdown countdown
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
    | Countdown Int
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

        Countdown turns ->
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


viewPlanets : PlayingModel -> ( List (Svg PlayingMsg), List (Svg msg) )
viewPlanets model =
    IdDict.fold
        (\k v ( acc, bgAcc ) ->
            let
                ( e, bg ) =
                    viewPlanet model k v
            in
            ( e :: acc, bg ++ bgAcc )
        )
        ( [], [] )
        model.planets


viewPlanet :
    PlayingModel
    -> Id PlanetId
    -> Planet
    -> ( Svg PlayingMsg, List (Svg msg) )
viewPlanet { selected, highlighted } id planet =
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
            let
                ( src, { fade } ) =
                    planetImage planet
            in
            Svg.image
                [ SvgAttributes.x x
                , SvgAttributes.y y
                , SvgAttributes.width (Theme.planetRadius * 2)
                , SvgAttributes.height (Theme.planetRadius * 2)
                , Svg.Attributes.xlinkHref src
                , if fade then
                    Svg.Attributes.filter fadeFilter.ref

                  else
                    Svg.Attributes.style ""
                ]
                []

        selectionView : List (Svg msg)
        selectionView =
            if selected == SelectedPlanet id then
                [ Svg.circle
                    [ SvgAttributes.cx (Length.inLightYears cx)
                    , SvgAttributes.cy (Length.inLightYears cy)
                    , SvgAttributes.r (Length.inLightYears maximumLinkLength)
                    , SvgAttributes.strokeWidth 0.01
                    , Svg.Attributes.stroke "green"
                    , Svg.Attributes.fill selectionGradient.ref
                    ]
                    []
                ]

            else if highlighted == HighlightedPlanet id then
                [ Svg.circle
                    [ SvgAttributes.cx (Length.inLightYears cx)
                    , SvgAttributes.cy (Length.inLightYears cy)
                    , SvgAttributes.r (Theme.planetRadius * 1.2)
                    , SvgAttributes.strokeWidth 0.01
                    , Svg.Attributes.fill "white"
                    ]
                    []
                ]

            else
                []

        bottomView : Svg PlayingMsg
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
                                                farm.perTurn * farm.countdown
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
                        , Countdown colony.countdown
                        ]

                OccupiedPlanet (FarmPlanet farm) ->
                    svgTwoColumnGrid
                        { x = cx
                        , y = cy
                        }
                        [ Product farm.perTurn farm.product
                        , Countdown farm.countdown
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
                                (\( product, quantity ) -> Product quantity product)
                                (Product.Dict.toList deposit.content)
                        )
    in
    ( Svg.g
        [ Svg.Attributes.id (Id.toString id)
        , Svg.Events.onClick (SelectPlanet id)
        , Svg.Events.onMouseOver (HighlightPlanet id)
        , Svg.Events.onMouseOut HighlightNone
        , Svg.Attributes.cursor "pointer"
        ]
        [ img, bottomView ]
    , selectionView
    )


planetImage : Planet -> ( String, { fade : Bool } )
planetImage planet =
    case planet.kind of
        VirginPlanet _ ->
            ( Theme.planetIce, { fade = False } )

        ColonyPlanet { countdown } ->
            ( Theme.planetBlackHole, { fade = countdown <= 0 } )

        OccupiedPlanet (FarmPlanet { countdown }) ->
            ( Theme.planetTerran, { fade = countdown <= 0 } )

        OccupiedPlanet (FactoryPlanet _) ->
            ( Theme.planetLava, { fade = False } )

        OccupiedPlanet (DepositPlanet _) ->
            ( Theme.planetBarren, { fade = False } )


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
viewLink model from fromPlanet to link =
    if Product.Dict.all (\_ v -> v <= 0) link then
        Nothing

    else
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
                        , SvgAttributes.strokeWidth 0.01
                        , Svg.Attributes.stroke linkGradient.ref
                        ]
                        []
                )
