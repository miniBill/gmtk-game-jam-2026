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
import Types exposing (GameMode, Highlighted(..), Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))
import View


init : () -> ( Model, Cmd Msg, AudioCmd Msg )
init () =
    ( { page = Menu
      , sound = Nothing
      }
    , Cmd.none
    , Audio.cmdNone
    )


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update _ msg model =
    case {- Debug.log "msg" -} msg of
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

        Play gameMode ->
            ( model
            , Random.int 0 Random.maxInt |> Random.generate (InitialSeed gameMode)
            , Audio.cmdNone
            )

        InitialSeed gameMode initialSeed ->
            ( { model
                | page =
                    initPlayingModel gameMode initialSeed
                        |> Turn.addNewPlanets
                        |> Playing
              }
            , getSvgContainerSize
            , Audio.cmdNone
            )

        AudioLoadResult (Err e) ->
            let
                _ =
                    Debug.log "AudioLoadResult (Err e)" e
            in
            ( model, Cmd.none, Audio.cmdNone )

        AudioLoadResult (Ok sound) ->
            ( model, Time.now |> Task.perform (TimeResult sound), Audio.cmdNone )

        TimeResult sound now ->
            ( { model | sound = Just ( sound, now ) }, Cmd.none, Audio.cmdNone )

        PlayingMsg playingMsg ->
            case model.page of
                Menu ->
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
            let
                _ =
                    Debug.log "GotSvgContainerSize (Err e)" e
            in
            ( model, Cmd.none, Audio.cmdNone )

        GotSvgContainerSize (Ok { viewport }) ->
            case model.page of
                Menu ->
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


initPlayingModel : GameMode -> Int -> PlayingModel
initPlayingModel gameMode initialSeed =
    { initialSeed = initialSeed
    , currentSeed = Random.initialSeed initialSeed
    , planets = IdDict.empty |> IdDict.insertNew initialEarth
    , selected = SelectedNone
    , highlighted = HighlightedNone
    , score = 0
    , gameMode = gameMode
    , rings = 1
    , svgContainerSize = ( Pixels.pixels 200, Pixels.pixels 200 )
    , center = Point2d.origin
    , zoom = Quantity.rate Length.lightYear (Pixels.pixels 200)
    , mousePosition = Point2d.origin
    }


initialEarth : Planet
initialEarth =
    { name = "Terra"
    , position = Point2d.origin
    , links = IdDict.empty
    , kind =
        ColonyPlanet
            { product = Food.Ingredient Food.Water
            , quantity = 1
            , countdown = 10
            }
    }


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

        MouseMove position ->
            ( { model | mousePosition = position }
                |> Playing
            , Cmd.none
            )
