module Update exposing (init, update)

import Audio exposing (AudioCmd, AudioData)
import Browser.Dom
import Food
import Food.Dict
import IdDict
import Length
import Pixels
import Point2d
import Process
import Quantity
import Random
import Task
import Theme
import Time
import Turn
import Types exposing (GameMode, GamePhase(..), Highlighted(..), Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))
import View


init : () -> ( Model, Cmd Msg, AudioCmd Msg )
init () =
    ( { page = Menu "" False
      , sound = Nothing
      }
    , Cmd.none
    , Audio.cmdNone
    )


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update _ msg model =
    case {- Debug.log "msg" -} msg of
        SetSeed seed ->
            case model.page of
                Menu _ vegetarian ->
                    ( { model | page = Menu seed vegetarian }, Cmd.none, Audio.cmdNone )

                _ ->
                    ( model, Cmd.none, Audio.cmdNone )

        SetVegetarian vegetarian ->
            case model.page of
                Menu seed _ ->
                    ( { model | page = Menu seed vegetarian }, Cmd.none, Audio.cmdNone )

                _ ->
                    ( model, Cmd.none, Audio.cmdNone )

        PlaySound ->
            case model.sound of
                Nothing ->
                    ( model
                    , Cmd.none
                    , Audio.loadAudio AudioLoadResult Theme.bleep
                    )

                Just ( sound, _ ) ->
                    ( model
                    , Time.now |> Task.perform (TimeResult sound)
                    , Audio.cmdNone
                    )

        Play gameMode vegetarian ->
            ( model
            , let
                setSeed : Maybe Int
                setSeed =
                    case model.page of
                        Menu seed _ ->
                            String.toInt seed

                        _ ->
                            Nothing
              in
              case setSeed of
                Nothing ->
                    Random.int 0 Random.maxInt
                        |> Random.generate (InitialSeed gameMode vegetarian)

                Just seed ->
                    Random.constant seed
                        |> Random.generate (InitialSeed gameMode vegetarian)
            , Audio.cmdNone
            )

        InitialSeed gameMode vegetarian initialSeed ->
            ( { model
                | page =
                    initPlayingModel gameMode vegetarian initialSeed
                        |> Turn.addNewPlanets
                        |> Playing
              }
            , getSvgContainerSize
            , Audio.cmdNone
            )

        AudioLoadResult (Err e) ->
            -- let
            --     _ =
            --         Debug.log "AudioLoadResult (Err e)" e
            -- in
            ( model, Cmd.none, Audio.cmdNone )

        AudioLoadResult (Ok sound) ->
            ( model, Time.now |> Task.perform (TimeResult sound), Audio.cmdNone )

        TimeResult sound now ->
            ( { model | sound = Just ( sound, now ) }, Cmd.none, Audio.cmdNone )

        PlayingMsg playingMsg ->
            case model.page of
                Menu _ _ ->
                    ( model, Cmd.none, Audio.cmdNone )

                Playing playing ->
                    let
                        ( newPage, cmd ) =
                            updatePlaying playingMsg playing
                    in
                    ( { model | page = newPage }
                    , Cmd.map PlayingMsg cmd
                    , Audio.cmdNone
                    )

                Lost _ ->
                    ( model, Cmd.none, Audio.cmdNone )

        Resized _ _ ->
            ( model, getSvgContainerSize, Audio.cmdNone )

        GotSvgContainerSize (Err e) ->
            -- let
            --     _ =
            --         Debug.log "GotSvgContainerSize (Err e)" e
            -- in
            ( model, Cmd.none, Audio.cmdNone )

        GotSvgContainerSize (Ok { viewport }) ->
            case model.page of
                Menu _ _ ->
                    ( model, Cmd.none, Audio.cmdNone )

                Playing playingModel ->
                    ( { model
                        | page =
                            Playing
                                { playingModel
                                    | svgContainerSize =
                                        ( Pixels.pixels viewport.width
                                        , Pixels.pixels viewport.height
                                        )
                                }
                      }
                    , Cmd.none
                    , Audio.cmdNone
                    )

                Lost _ ->
                    ( model, Cmd.none, Audio.cmdNone )


getSvgContainerSize : Cmd Msg
getSvgContainerSize =
    Process.sleep 0
        |> Task.andThen (\_ -> Browser.Dom.getViewportOf View.svgContainerId)
        |> Task.attempt GotSvgContainerSize


initPlayingModel : GameMode -> Bool -> Int -> PlayingModel
initPlayingModel gameMode vegetarian initialSeed =
    let
        ( initialEarth, seed ) =
            Random.step (initialEarthGenerator vegetarian) (Random.initialSeed initialSeed)
    in
    { initialSeed = initialSeed
    , currentSeed = seed
    , vegetarian = vegetarian
    , planets = IdDict.empty |> IdDict.insertNew initialEarth
    , selected = SelectedNone
    , highlighted = HighlightedNone
    , score = 0
    , turns = 1
    , gameMode = gameMode
    , rings = 1
    , svgContainerSize = ( Pixels.pixels 200, Pixels.pixels 200 )
    , center = Point2d.origin
    , zoom = Quantity.rate Length.lightYear (Pixels.pixels 200)
    }


initialEarthGenerator : Bool -> Random.Generator Planet
initialEarthGenerator vegetarian =
    Random.map
        (\product ->
            { name = "Terra"
            , position = Point2d.origin
            , links = IdDict.empty
            , kind =
                ColonyPlanet
                    { product = Food.Ingredient Food.Water
                    , quantity = 2
                    , countdown = 10
                    , nextCountdown = 8
                    , nextQuantity = 1
                    , nextProduct = product
                    }
            }
        )
        (Turn.randomColonyRequest EarlyGame vegetarian)


updatePlaying : PlayingMsg -> PlayingModel -> ( Page, Cmd PlayingMsg )
updatePlaying msg model =
    case msg of
        SelectPlanet id ->
            let
                new : Selected
                new =
                    SelectedPlanet id
            in
            if new == model.selected then
                ( Playing { model | selected = SelectedNone }, Cmd.none )

            else
                ( Playing { model | selected = new }, Cmd.none )

        HighlightPlanet id ->
            ( Playing { model | highlighted = HighlightedPlanet id }, Cmd.none )

        HighlightNone ->
            ( Playing { model | highlighted = HighlightedNone }, Cmd.none )

        OccupyPlanet id kind ->
            ( Playing
                { model
                    | planets =
                        IdDict.updateIfExists
                            id
                            (\planet -> { planet | kind = OccupiedPlanet kind })
                            model.planets
                }
            , Cmd.none
            )

        EndTurn ->
            ( case Turn.run model of
                Ok newModel ->
                    Playing newModel

                Err newModel ->
                    Lost newModel
            , Cmd.none
            )

        SetFactoryProduction id order ->
            ( { model
                | planets =
                    IdDict.updateIfExists
                        id
                        (\planet ->
                            case planet.kind of
                                OccupiedPlanet (FactoryPlanet factory) ->
                                    { planet
                                        | kind =
                                            OccupiedPlanet (FactoryPlanet { factory | product = order })
                                    }

                                _ ->
                                    planet
                        )
                        model.planets
              }
                |> Playing
            , Cmd.none
            )

        SetLink from to product quantity ->
            ( { model
                | planets =
                    IdDict.updateIfExists
                        from
                        (\planet ->
                            { planet
                                | links =
                                    IdDict.update to
                                        (\links ->
                                            links
                                                |> Maybe.withDefault Food.Dict.empty
                                                |> Food.Dict.insert product quantity
                                                |> Just
                                        )
                                        planet.links
                            }
                        )
                        model.planets
              }
                |> Playing
            , Cmd.none
            )

        MouseWheel newZoom newCenter ->
            ( { model
                | zoom = newZoom
                , center = newCenter
              }
                |> Playing
            , Cmd.none
            )
