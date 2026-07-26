module View exposing (svgContainerId, view)

import Angle
import Audio exposing (AudioData)
import Color
import Color.Oklch as Oklch exposing (Oklch)
import Food exposing (Food, Ingredient, Product)
import Food.Dict
import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Html.Events
import Id exposing (Id, PlanetId)
import IdDict exposing (IdDict)
import Json.Decode
import Length exposing (Length, Meters)
import List.Extra
import Maybe.Extra
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Round
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import SvgAttributes
import Theme
import Types exposing (FactoryData, GameMode(..), GamePhase(..), Highlighted(..), Link, Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))
import Vector2d


view : AudioData -> Model -> Html Msg
view _ model =
    let
        container : List (Attribute msg) -> List (Html msg) -> Html msg
        container attrs children =
            Html.main_
                ([ style "display" "flex"
                 , style "gap" "8px"
                 , style "align-items" "center"
                 , style "justify-content" "center"
                 , style "padding" "8px"
                 , style "width" "100vw"
                 , style "height" "100dvh"
                 ]
                    ++ attrs
                )
                children
    in
    case model.page of
        Menu seed ->
            container
                [ style "flex-direction" "column" ]
                [ startGameButtons
                , Html.label [ style "color" "white" ]
                    [ Html.text "Game seed (leave blank for random) "
                    , Html.input
                        [ Html.Attributes.value seed
                        , Html.Events.onInput SetSeed
                        ]
                        []
                    ]
                ]

        Playing playingModel ->
            viewPlaying playingModel
                |> List.map (Html.map PlayingMsg)
                |> container []

        Lost lostModel ->
            container
                [ style "flex-direction" "column" ]
                [ Html.p
                    [ style "color" "white"
                    ]
                    [ Html.text
                        ("Final score: "
                            ++ String.fromInt lostModel.score
                            ++ " points in "
                            ++ String.fromInt lostModel.turns
                            ++ " turns"
                        )
                    ]
                , Html.p
                    [ style "color" "white"
                    ]
                    [ Html.text ("Seed: " ++ String.fromInt lostModel.initialSeed)
                    ]
                , startGameButtons
                ]


startGameButtons : Html Msg
startGameButtons =
    Html.div
        [ style "display" "flex"
        , style "gap" "16px"
        ]
        [ Html.button
            [ Html.Events.onClick (Play Easy)
            ]
            [ Html.text "Play game (easy mode - no complex recipes)" ]
        , Html.button
            [ Html.Events.onClick (Play Normal)
            ]
            [ Html.text "Play game (normal mode)" ]
        , Html.button
            [ Html.Events.onClick (Play Hard)
            ]
            [ Html.text "Play game (hard mode - deposit size limit)" ]

        -- , Html.button
        --     [ Html.Events.onClick PlaySound
        --     ]
        --     [ Html.text "Play sound" ]
        ]


viewPlaying : PlayingModel -> List (Html PlayingMsg)
viewPlaying model =
    [ let
        ( containerWidth, containerHeight ) =
            model.svgContainerSize

        maxWidth : Length
        maxWidth =
            Quantity.at model.zoom containerWidth

        maxHeight : Length
        maxHeight =
            Quantity.at model.zoom containerHeight

        minX : Length
        minX =
            Point2d.xCoordinate model.center
                |> Quantity.minus (Quantity.multiplyBy 0.5 maxWidth)

        minY : Length
        minY =
            Point2d.yCoordinate model.center
                |> Quantity.minus (Quantity.multiplyBy 0.5 maxHeight)

        viewBox : String
        viewBox =
            SvgAttributes.viewBox minX minY maxWidth maxHeight

        ( planetsViews, background ) =
            viewPlanets model
      in
      Svg.svg
        [ Html.Attributes.id svgContainerId
        , style "flex" "9"
        , style "align-self" "stretch"
        , style "width" "100%"
        , style "max-height" "100dvh"
        , Svg.Attributes.viewBox viewBox
        , Svg.Events.on "wheel"
            (Json.Decode.map3
                (calculateZoom
                    model
                )
                (Json.Decode.field "deltaY" Json.Decode.float)
                (Json.Decode.map Pixels.pixels
                    (Json.Decode.field "offsetX" Json.Decode.float)
                )
                (Json.Decode.map Pixels.pixels
                    (Json.Decode.field "offsetY" Json.Decode.float)
                )
            )
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
    , Html.div
        [ style "flex" "1 0 200px"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-self" "stretch"
        , style "gap" "8px"
        ]
        [ case model.selected of
            SelectedPlanet planetId ->
                case IdDict.get planetId model.planets of
                    Just planet ->
                        linksBox model planetId planet

                    Nothing ->
                        Html.text ""

            SelectedNone ->
                Html.text ""
        , Html.div [ style "flex" "1 0" ] []
        , case Types.gamePhase model of
            EarlyGame ->
                Html.text ""

            MidGame ->
                Html.p
                    [ style "color" "white"
                    , style "font-weight" "bold"
                    , style "text-align" "center"
                    ]
                    [ Html.text "Recipes" ]

            LateGame ->
                Html.p
                    [ style "color" "white"
                    , style "font-weight" "bold"
                    , style "text-align" "center"
                    ]
                    [ Html.text "Recipes" ]
        , case Types.gamePhase model of
            EarlyGame ->
                Html.text ""

            MidGame ->
                Food.allProducts
                    |> List.Extra.removeWhen Food.isDuneProduct
                    |> List.map viewRecipe
                    |> List.sortBy Tuple.first
                    |> List.map Tuple.second
                    |> Html.div
                        [ style "display" "flex"
                        , style "flex-wrap" "wrap"
                        , style "gap" "8px"
                        , style "max-height" "40dvh"
                        , style "overflow-y" "scroll"
                        ]

            LateGame ->
                Food.allProducts
                    |> List.map viewRecipe
                    |> List.sortBy Tuple.first
                    |> List.map Tuple.second
                    |> Html.div
                        [ style "display" "flex"
                        , style "flex-wrap" "wrap"
                        , style "gap" "8px"
                        , style "max-height" "40dvh"
                        , style "overflow-y" "scroll"
                        ]
        , Html.div
            [ style "display" "flex"
            , style "gap" "8px"
            , style "max-width" "90vw"
            ]
            [ Html.div
                [ style "color" "white" ]
                [ Html.text ("Score: " ++ String.fromInt model.score) ]
            , Html.div [ style "flex" "1 0" ] []
            , Html.div
                [ style "color" "white" ]
                [ Html.text ("Turns: " ++ String.fromInt model.turns) ]
            , Html.div [ style "flex" "1 0" ] []
            , Html.button [ Html.Events.onClick EndTurn ] [ Html.text "End turn" ]
            ]
        ]
    ]


calculateZoom :
    PlayingModel
    -> Float
    -> Quantity Float Pixels
    -> Quantity Float Pixels
    -> PlayingMsg
calculateZoom model deltaY offsetX offsetY =
    let
        ( containerWidth, containerHeight ) =
            model.svgContainerSize

        minX : Length
        minX =
            Point2d.xCoordinate model.center
                |> Quantity.minus (Quantity.multiplyBy 0.5 maxWidth)

        minY : Length
        minY =
            Point2d.yCoordinate model.center
                |> Quantity.minus (Quantity.multiplyBy 0.5 maxHeight)

        maxWidth : Length
        maxWidth =
            Quantity.at model.zoom containerWidth

        maxHeight : Length
        maxHeight =
            Quantity.at model.zoom containerHeight

        mousePosition : Point2d Meters ()
        mousePosition =
            Point2d.xy
                (minX
                    |> Quantity.plus
                        (Quantity.multiplyBy
                            (Quantity.ratio
                                offsetX
                                containerWidth
                            )
                            maxWidth
                        )
                )
                (minY
                    |> Quantity.plus
                        (Quantity.multiplyBy
                            (Quantity.ratio
                                offsetY
                                containerHeight
                            )
                            maxHeight
                        )
                )

        newZoom : Quantity Float (Quantity.Rate Meters Pixels)
        newZoom =
            Quantity.multiplyBy (1.003 ^ deltaY) model.zoom

        newCenter : Point2d Meters ()
        newCenter =
            -- (mouse - oldCenter) * oldZoon = (mouse - newCenter) * newZoom
            -- (oldCenter - mouse) * oldZoon = (newCenter - mouse) * newZoom
            -- (newCenter - mouse) = (oldCenter - mouse) * oldZoon / newZoon
            -- newCenter = (oldCenter - mouse) * oldZoon / newZoon + mouse
            Point2d.translateBy
                (Vector2d.from mousePosition model.center
                    |> Vector2d.at_ model.zoom
                    |> Vector2d.at newZoom
                )
                mousePosition
    in
    MouseWheel newZoom newCenter


svgContainerId : String
svgContainerId =
    "svg-container"


linksBox : PlayingModel -> Id PlanetId -> Planet -> Html PlayingMsg
linksBox model planetId planet =
    Html.div
        [ style "display" "flex"
        , style "gap" "8px"
        , style "flex-direction" "column"
        , style "max-width" "90vw"
        , style "align-items" "center"
        ]
        (viewSelectedPlanet model planetId planet
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
            -> { links : IdDict PlanetId Link, name : String, kind : PlanetKind, position : Point2d Meters () }
            -> List (Html PlayingMsg)
            -> List (Html PlayingMsg)
        addLinks to toPlanet acc =
            viewLinkPossibility model
                { from = from
                , fromPlanet = fromPlanet
                , to = to
                , toPlanet = toPlanet
                , available = foods
                , link =
                    IdDict.get to fromPlanet.links
                        |> Maybe.withDefault Food.Dict.empty
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

        foods : List { food : Food, quantity : Int }
        foods =
            case fromPlanet.kind of
                VirginPlanet _ ->
                    []

                ColonyPlanet _ ->
                    []

                OccupiedPlanet (FarmPlanet { ingredient, perTurn }) ->
                    [ { food = Food.Ingredient ingredient, quantity = perTurn } ]

                OccupiedPlanet (FactoryPlanet { product, efficiency }) ->
                    case product of
                        Just p ->
                            [ { food = Food.Product p
                              , quantity = efficiency
                              }
                            ]

                        Nothing ->
                            []

                OccupiedPlanet (DepositPlanet { content }) ->
                    Food.Dict.toList content
                        |> List.map
                            (\( food, quantity ) ->
                                { food = food
                                , quantity = quantity
                                }
                            )

        columns : String
        columns =
            "auto"
                |> List.repeat (List.length foods + 1)
                |> String.join " "

        header : List (Html msg)
        header =
            Html.div [] []
                :: List.map
                    (\{ food } ->
                        htmlIcon
                            [ style "padding" "4px"
                            , style "height" "28px"
                            ]
                            { icon = Food.toIcon food
                            , title = Food.toString food
                            , colors = Food.toColors food
                            }
                    )
                    foods
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
        , toPlanet : { links : IdDict PlanetId Link, name : String, kind : PlanetKind, position : Point2d Meters () }
        , available : List { food : Food, quantity : Int }
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

        viewProductInput : { food : Food, quantity : Int } -> Html PlayingMsg
        viewProductInput { food, quantity } =
            let
                value : Int
                value =
                    Food.Dict.get food config.link
                        |> Maybe.withDefault 0
            in
            Html.div
                [ style "padding" "4px"
                ]
                [ Html.input
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
                                |> SetLink config.from config.to food
                        )
                    ]
                    []
                ]
    in
    nameView :: List.map viewProductInput config.available


viewSelectedPlanet : PlayingModel -> Id PlanetId -> Planet -> List (Html PlayingMsg)
viewSelectedPlanet model planetId planet =
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
                (List.filterMap (viewVirginPlanetOption (Types.gamePhase model) planetId) options)
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
                [ Food colony.quantity colony.product
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
                [ if farm.countdown > 0 then
                    Html.text ("The planet " ++ planet.name ++ " is producing")

                  else
                    Html.text ("The planet " ++ planet.name ++ " was producing")
                ]
            , htmlTwoColumnGrid
                [ style "align-self" "center"
                , style "color" "white"
                ]
                [ Ingredient farm.perTurn farm.ingredient
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
                let
                    gamePhase : GamePhase
                    gamePhase =
                        Types.gamePhase model
                in
                [ Html.p
                    [ style "display" "block"
                    , style "color" "white"
                    , style "text-align" "center"
                    , style "font-weight" "bold"
                    ]
                    [ case factory.product of
                        Just _ ->
                            Html.text ("The planet " ++ planet.name ++ " is producing")

                        Nothing ->
                            Html.text ("The planet " ++ planet.name ++ " can produce")
                    ]
                , Food.allProducts
                    |> List.Extra.removeWhen
                        (\product ->
                            Food.isDuneProduct product
                                && (gamePhase /= LateGame)
                        )
                    |> List.map (viewFactoryOption factory)
                    |> selectionRow
                        [ style "overflow-y" "scroll"
                        , style "max-height" "40vh"
                        ]
                    |> Html.map (SetFactoryProduction planetId)
                ]

        OccupiedPlanet (DepositPlanet deposit) ->
            [ Html.p
                [ style "display" "block"
                , style "color" "white"
                , style "text-align" "center"
                , style "font-weight" "bold"
                ]
                [ if Food.Dict.isEmpty deposit.content then
                    Html.text ("The planet " ++ planet.name ++ " is empty")

                  else
                    Html.text ("The planet " ++ planet.name ++ " contains")
                ]
            , deposit.content
                |> Food.Dict.toList
                |> List.map (\( product, quantity ) -> Food quantity product)
                |> htmlTwoColumnGrid []
            ]


viewFactoryOption : FactoryData -> Product -> Html (Maybe Product)
viewFactoryOption factory product =
    let
        selected : Bool
        selected =
            factory.product == Just product
    in
    htmlTwoColumnGrid
        [ style "flex-direction" "column"
        , if selected then
            style "background" "#f44"

          else
            style "background" "gray"
        , style "padding" "8px"
        , style "align-items" "center"
        , Html.Attributes.class "on-hover-highlight"
        , if selected then
            Html.Events.onClick Nothing

          else
            Html.Events.onClick (Just product)
        , style "cursor" "pointer"
        ]
        [ Product_ 1 product
        ]


viewRecipe : Product -> ( Int, Html msg )
viewRecipe product =
    let
        recipe : Food.Recipe
        recipe =
            Food.toRecipe product
    in
    ( List.length recipe
    , Html.div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "background" "gray"
        , style "padding" "8px"
        , style "gap" "4px"
        , style "align-items" "center"
        , Html.Attributes.class "on-hover-highlight"
        , style "cursor" "pointer"
        ]
        [ htmlTwoColumnGrid []
            [ Product_ 1 product
            ]
        , Html.text "from"
        , recipe
            |> List.sortBy (\ingredient -> Food.toString ingredient.food)
            |> List.map (\ingredient -> Food_ ingredient.quantity ingredient.food)
            |> htmlTwoColumnGrid []
        ]
    )


viewVirginPlanetOption : GamePhase -> Id PlanetId -> OccupiedPlanet -> Maybe (Html PlayingMsg)
viewVirginPlanetOption gamePhase planetId option =
    let
        ( background, children, isFactory ) =
            case option of
                FarmPlanet { ingredient, countdown, perTurn } ->
                    ( "#cfc"
                    , [ htmlIcon [ style "height" "30px" ]
                            { icon = Theme.iconTractor
                            , title = "Farm"
                            , colors = []
                            }
                      , Html.text "Build farm"
                      , htmlTwoColumnGrid []
                            [ Ingredient perTurn ingredient
                            , Countdown countdown
                            ]
                      ]
                    , False
                    )

                FactoryPlanet { efficiency } ->
                    ( "#fcc"
                    , [ htmlIcon [ style "height" "30px" ]
                            { icon = Theme.iconFactory
                            , title = "Factory"
                            , colors = []
                            }
                      , Html.text "Build factory"
                      , htmlTwoColumnGrid []
                            [ Efficiency efficiency
                            ]
                      ]
                    , True
                    )

                DepositPlanet { capacity } ->
                    ( "#ccc"
                    , [ htmlIcon [ style "height" "30px" ]
                            { icon = Theme.iconWarehouse
                            , title = "Deposit"
                            , colors = []
                            }
                      , Html.text "Build deposit"
                      , htmlTwoColumnGrid []
                            [ Capacity capacity
                            ]
                      ]
                    , False
                    )
    in
    if gamePhase == EarlyGame && isFactory then
        Nothing

    else
        Html.div
            [ style "border-radius" "4px"
            , style "gap" "4px"
            , style "padding" "4px"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "background-color" background
            , style "flex" "1 0"
            , Html.Attributes.class "on-hover-highlight"
            , Html.Events.onClick (OccupyPlanet planetId option)
            ]
            children
            |> Just


htmlTwoColumnGrid : List (Attribute msg) -> List GridRow -> Html msg
htmlTwoColumnGrid attrs children =
    Html.div
        ([ style "display" "grid"
         , style "gap" "4px"
         , style "align-items" "center"
         , style "grid-template-columns" "auto 28px auto"
         , style "text-align" "center"
         ]
            ++ attrs
        )
        (List.concatMap gridRowToHtml children)


type GridRow
    = Ingredient Int Ingredient
    | Product Int Product
    | Product_ Int Product
    | Food Int Food
    | Food_ Int Food
    | Countdown Int
    | Efficiency Int
    | Capacity (Maybe Int)


gridRowToHtml : GridRow -> List (Html msg)
gridRowToHtml gridRow =
    case gridRowToTuple gridRow of
        Just ( text, rowIcon ) ->
            [ Html.div [] [ Html.text text ]
            , htmlIcon [ style "height" "28px" ] rowIcon
            , Html.div [] [ Html.text rowIcon.title ]
            ]

        Nothing ->
            []


gridRowToTuple :
    GridRow
    ->
        Maybe
            ( String
            , { icon : String
              , title : String
              , colors : List Oklch
              }
            )
gridRowToTuple gridRow =
    case gridRow of
        Food quantity food ->
            ( String.fromInt quantity
            , { icon = Food.toIcon food
              , title = Food.toString food
              , colors = Food.toColors food
              }
            )
                |> Just

        Food_ 1 food ->
            ( ""
            , { icon = Food.toIcon food
              , title = Food.toString food
              , colors = Food.toColors food
              }
            )
                |> Just

        Food_ quantity food ->
            ( String.fromInt quantity
            , { icon = Food.toIcon food
              , title = Food.toString food
              , colors = Food.toColors food
              }
            )
                |> Just

        Product_ 1 product ->
            ( ""
            , { icon = Food.productToIcon product
              , title = Food.productToString product
              , colors = Food.productToColors product
              }
            )
                |> Just

        Product_ quantity product ->
            ( String.fromInt quantity
            , { icon = Food.productToIcon product
              , title = Food.productToString product
              , colors = Food.productToColors product
              }
            )
                |> Just

        Product quantity product ->
            ( String.fromInt quantity
            , { icon = Food.productToIcon product
              , title = Food.productToString product
              , colors = Food.productToColors product
              }
            )
                |> Just

        Ingredient quantity ingredient ->
            ( String.fromInt quantity
            , { icon = Food.ingredientToIcon ingredient
              , title = Food.ingredientToString ingredient
              , colors = [ Food.ingredientToColor ingredient ]
              }
            )
                |> Just

        Countdown turns ->
            ( String.fromInt turns
            , { icon = Theme.iconHourglass
              , title = "Turns"
              , colors =
                    if turns > 3 then
                        [ Color.green |> Oklch.fromColor ]

                    else if turns > 0 then
                        [ Color.yellow |> Oklch.fromColor ]

                    else
                        [ Color.red |> Oklch.fromColor ]
              }
            )
                |> Just

        Efficiency efficiency ->
            ( String.fromInt efficiency
            , { icon = Theme.iconSpeed
              , title = "Production per turn"
              , colors = [ Color.gray |> Oklch.fromColor ]
              }
            )
                |> Just

        Capacity Nothing ->
            Nothing

        Capacity (Just capacity) ->
            ( String.fromInt capacity
            , { icon = Theme.iconPackage
              , title = "Capacity"
              , colors = [ Color.brown |> Oklch.fromColor ]
              }
            )
                |> Just


htmlIcon :
    List (Attribute msg)
    ->
        { icon : String
        , title : String
        , colors : List Oklch
        }
    -> Html msg
htmlIcon attrs config =
    Html.img
        (style "height" "18px"
            :: style "padding" "2px"
            :: style "border-radius"
                (if List.isEmpty config.colors then
                    "0"

                 else
                    "999px"
                )
            :: Html.Attributes.title config.title
            :: Html.Attributes.src config.icon
            :: style "background"
                (if List.isEmpty config.colors then
                    "transparent"

                 else
                    let
                        sectorDegrees : Int
                        sectorDegrees =
                            360 // List.length config.colors

                        stops : String
                        stops =
                            config.colors
                                |> List.indexedMap
                                    (\i color ->
                                        [ Oklch.toCssString color
                                            ++ " "
                                            ++ String.fromInt (sectorDegrees * i)
                                            ++ "deg"
                                        , Oklch.toCssString color
                                            ++ " "
                                            ++ String.fromInt (sectorDegrees * (i + 1))
                                            ++ "deg"
                                        ]
                                    )
                                |> List.concat
                                |> String.join ","
                    in
                    "conic-gradient(" ++ stops ++ ")"
                )
            :: attrs
        )
        []


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


svgTwoColumnsGrid :
    List (Svg.Attribute msg)
    ->
        { x : Length
        , y : Length
        }
    -> List GridRow
    -> Svg msg
svgTwoColumnsGrid attrs config rows =
    rows
        |> List.indexedMap (gridRowToSvg attrs)
        |> Svg.g
            [ SvgAttributes.transformTranslate
                { x = Length.inLightYears config.x
                , y = Length.inLightYears config.y
                }
            ]


svgThreeColumnsGrid :
    { x : Length
    , y : Length
    }
    -> List (List Ingredient)
    -> Svg msg
svgThreeColumnsGrid config rows =
    rows
        |> List.indexedMap gridIngredientsRowToSvg
        |> Svg.g
            [ SvgAttributes.transformTranslate
                { x = Length.inLightYears config.x
                , y = Length.inLightYears config.y
                }
            ]


gridIngredientsRowToSvg : Int -> List Ingredient -> Svg msg
gridIngredientsRowToSvg y row =
    let
        rhythm : Float
        rhythm =
            Theme.planetRadius * 1.3
    in
    row
        |> List.indexedMap
            (\x ingredient ->
                Svg.g
                    [ SvgAttributes.transformTranslate
                        { x = rhythm * (toFloat x - 1.5)
                        , y = rhythm * (toFloat y + 1.1)
                        }
                    ]
                    [ svgRadialBackground [ Food.ingredientToColor ingredient ]
                    , Svg.image
                        [ Svg.Attributes.xlinkHref (Food.ingredientToIcon ingredient)
                        , SvgAttributes.x (Theme.planetRadius * 0.1)
                        , SvgAttributes.y (Theme.planetRadius * 0.1)
                        , SvgAttributes.width (Theme.planetRadius * 0.8)
                        , SvgAttributes.height (Theme.planetRadius * 0.8)
                        ]
                        []
                    ]
            )
        |> Svg.g []


gridRowToSvg : List (Svg.Attribute msg) -> Int -> GridRow -> Svg msg
gridRowToSvg attrs i row =
    case gridRowToTuple row of
        Just ( text, rowIcon ) ->
            Svg.g
                [ SvgAttributes.transformTranslate
                    { x = 0
                    , y = (toFloat i + 1.1) * Theme.planetRadius * 1.3
                    }
                ]
                [ Svg.g attrs
                    [ Svg.g
                        [ SvgAttributes.transformTranslate
                            { x = 0
                            , y = Theme.planetRadius * 0.1
                            }
                        ]
                        [ svgRadialBackground rowIcon.colors
                        , Svg.image
                            [ Svg.Attributes.xlinkHref rowIcon.icon
                            , SvgAttributes.x (Theme.planetRadius * 0.1)
                            , SvgAttributes.y (Theme.planetRadius * 0.1)
                            , SvgAttributes.width (Theme.planetRadius * 0.8)
                            , SvgAttributes.height (Theme.planetRadius * 0.8)
                            ]
                            []
                        ]
                    , Svg.text_
                        [ SvgAttributes.x (-Theme.planetRadius * 0.25)
                        , Svg.Attributes.dominantBaseline "hanging"
                        , Svg.Attributes.textAnchor "end"
                        , SvgAttributes.fontSize (1.1 * Theme.planetRadius)
                        , Svg.Attributes.fill "white"
                        ]
                        [ Svg.text text ]
                    ]
                ]

        Nothing ->
            Svg.text ""


svgRadialBackground :
    List Oklch
    -> Svg msg
svgRadialBackground colors =
    let
        radius : Float
        radius =
            Theme.planetRadius * 1.1 / 2
    in
    case colors of
        [] ->
            Svg.text ""

        [ color ] ->
            Svg.circle
                [ SvgAttributes.cx (Theme.planetRadius / 2)
                , SvgAttributes.cy (Theme.planetRadius / 2)
                , SvgAttributes.r radius
                , Svg.Attributes.fill (Oklch.toCssString color)
                ]
                []

        _ :: _ :: _ ->
            let
                sectorDegrees : Int
                sectorDegrees =
                    360 // List.length colors

                sector : Int -> Oklch -> Svg msg
                sector i color =
                    let
                        at : Float -> { x : String, y : String }
                        at angle =
                            { x = Round.round 4 (radius * Angle.cos (Angle.degrees (angle - 90)))
                            , y = Round.round 4 (radius * Angle.sin (Angle.degrees (angle - 90)))
                            }

                        start : { x : String, y : String }
                        start =
                            at (toFloat (i * sectorDegrees))

                        end : { x : String, y : String }
                        end =
                            at (toFloat ((i + 1) * sectorDegrees))

                        largeArcFlag : String
                        largeArcFlag =
                            if sectorDegrees <= 180 then
                                "0"

                            else
                                "1"
                    in
                    Svg.path
                        [ Svg.Attributes.fill (Oklch.toCssString color)
                        , [ "M"
                          , start.x
                          , start.y
                          , "A"
                          , Round.round 4 radius
                          , Round.round 4 radius
                          , "0" -- x axis rotation
                          , largeArcFlag
                          , "1" -- sweep flag
                          , end.x
                          , end.y
                          , "L 0 0 L"
                          , start.x
                          , start.y
                          ]
                            |> String.join " "
                            |> Svg.Attributes.d
                        ]
                        []
            in
            colors
                |> List.indexedMap sector
                |> Svg.g
                    [ SvgAttributes.transformTranslate
                        { x = Theme.planetRadius * 0.5
                        , y = Theme.planetRadius * 0.5
                        }
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

        img : Svg PlayingMsg
        img =
            let
                x : Float
                x =
                    Length.inLightYears cx - Theme.planetRadius

                y : Float
                y =
                    Length.inLightYears cy - Theme.planetRadius

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

        bottomView : List (Svg PlayingMsg)
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
                                        ( -quantity, farm.ingredient )
                                            |> Just

                                    FactoryPlanet _ ->
                                        Nothing

                                    DepositPlanet _ ->
                                        Nothing
                            )
                        |> List.sortBy Tuple.first
                        |> List.map Tuple.second
                        |> List.Extra.greedyGroupsOf 3
                        |> svgThreeColumnsGrid
                            { x = cx
                            , y = cy
                            }
                        |> List.singleton

                ColonyPlanet colony ->
                    [ Svg.rect
                        [ SvgAttributes.x
                            (Length.inLightYears cx - Theme.planetRadius * 1.6)
                        , SvgAttributes.y
                            (Length.inLightYears cy + Theme.planetRadius * 1.25)
                        , SvgAttributes.width (Theme.planetRadius * 3.2)
                        , SvgAttributes.height (Theme.planetRadius * 5.0)
                        , Svg.Attributes.fill "#c44"
                        ]
                        []
                    , Svg.text_
                        [ SvgAttributes.x
                            (Length.inLightYears cx - Theme.planetRadius * 0.7)
                        , SvgAttributes.y
                            (Length.inLightYears cy + Theme.planetRadius * 4.6)
                        , SvgAttributes.fontSize 0.1
                        , Svg.Attributes.fill "white"
                        ]
                        [ Svg.text "then" ]
                    , svgTwoColumnsGrid []
                        { x = cx
                        , y = cy
                        }
                        [ Food colony.quantity colony.product
                        , Countdown colony.countdown
                        ]
                    , svgTwoColumnsGrid []
                        { x = cx
                        , y =
                            Length.lightYears (Theme.planetRadius * 3.4)
                                |> Quantity.plus cy
                        }
                        [ Food colony.nextQuantity colony.nextProduct
                        ]
                    ]

                OccupiedPlanet (FarmPlanet farm) ->
                    if farm.countdown > 0 then
                        [ svgTwoColumnsGrid []
                            { x = cx
                            , y = cy
                            }
                            [ Ingredient farm.perTurn farm.ingredient
                            , Countdown farm.countdown
                            ]
                        ]

                    else
                        []

                OccupiedPlanet (FactoryPlanet factory) ->
                    [ svgTwoColumnsGrid []
                        { x = cx
                        , y = cy
                        }
                        (case factory.product of
                            Just product ->
                                [ Product factory.efficiency product
                                ]

                            Nothing ->
                                [ Efficiency factory.efficiency
                                ]
                        )
                    ]

                OccupiedPlanet (DepositPlanet deposit) ->
                    let
                        foodLines =
                            List.map
                                (\( product, quantity ) -> Food quantity product)
                                (Food.Dict.toList deposit.content)
                    in
                    [ svgTwoColumnsGrid []
                        { x = cx
                        , y = cy
                        }
                        (if Maybe.Extra.isNothing deposit.capacity then
                            foodLines

                         else
                            Capacity deposit.capacity
                                :: foodLines
                        )
                    ]
    in
    ( Svg.g
        [ Svg.Attributes.id (Id.toString id)
        , Svg.Events.onClick (SelectPlanet id)
        , Svg.Events.onMouseOver (HighlightPlanet id)
        , Svg.Events.onMouseOut HighlightNone
        , Svg.Attributes.cursor "pointer"
        ]
        (bottomView ++ [ img ])
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
                        [] ->
                            iacc

                        (_ :: _) as linkView ->
                            linkView ++ iacc
                )
                acc
                fromPlanet.links
        )
        []
        model.planets


viewLink : PlayingModel -> Id PlanetId -> Planet -> Id PlanetId -> Link -> List (Svg PlayingMsg)
viewLink model from fromPlanet to link =
    if Food.Dict.all (\_ v -> v <= 0) link then
        []

    else
        case IdDict.get to model.planets of
            Just toPlanet ->
                let
                    avg : Length -> Length -> Length
                    avg l r =
                        Quantity.plus l (Quantity.multiplyBy 0.5 (Quantity.difference r l))

                    ( x1, y1 ) =
                        Point2d.coordinates fromPlanet.position

                    ( x2, y2 ) =
                        Point2d.coordinates toPlanet.position
                in
                [ Svg.line
                    [ SvgAttributes.x1 (Length.inLightYears x1)
                    , SvgAttributes.y1 (Length.inLightYears y1)
                    , SvgAttributes.x2 (Length.inLightYears x2)
                    , SvgAttributes.y2 (Length.inLightYears y2)
                    , Svg.Events.onClick (SelectPlanet from)
                    , SvgAttributes.strokeWidth 0.01
                    , Svg.Attributes.stroke linkGradient.ref
                    ]
                    []
                , link
                    |> Food.Dict.toList
                    |> List.filterMap
                        (\( food, quantity ) ->
                            if quantity == 0 then
                                Nothing

                            else
                                Just (Food quantity food)
                        )
                    |> svgTwoColumnsGrid [ Svg.Attributes.transform "scale(0.5, 0.5)" ]
                        { x = avg x1 x2
                        , y = avg y1 y2 |> Quantity.minus (Length.lightYears 0.2)
                        }
                ]

            Nothing ->
                []
