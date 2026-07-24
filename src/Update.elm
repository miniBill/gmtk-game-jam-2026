module Update exposing (init, update)

import Angle
import Audio exposing (AudioCmd, AudioData)
import Id exposing (Id(..), PlanetId, ProductId)
import IdDict
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity
import Random
import Task
import Time
import Types exposing (Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))


maximumLinkLength : Length
maximumLinkLength =
    Length.lightYears 1


ringWidth : Length
ringWidth =
    Length.lightYears 1


init : () -> ( Model, Cmd Msg, AudioCmd Msg )
init () =
    ( { page = Menu
      , sound = Nothing
      }
    , Cmd.none
    , Audio.cmdNone
    )


update : AudioData -> Msg -> Model -> ( Model, Cmd Msg, AudioCmd Msg )
update audioData msg model =
    case {- Debug.log "msg" -} msg of
        PlaySound ->
            case model.sound of
                Nothing ->
                    ( model
                    , Cmd.none
                    , Audio.loadAudio AudioLoadResult "/media/Interface_Bleeps_OGG/Bleep_05.ogg"
                    )

                Just ( sound, _ ) ->
                    ( model
                    , Time.now |> Task.perform (TimeResult sound)
                    , Audio.cmdNone
                    )

        Play ->
            ( model
            , Random.int 0 Random.maxInt |> Random.generate InitialSeed
            , Audio.cmdNone
            )

        InitialSeed initialSeed ->
            ( { model
                | page =
                    { initialSeed = initialSeed
                    , currentSeed = Random.initialSeed initialSeed
                    , links = IdDict.empty
                    , maximumDistanceReched = Quantity.zero
                    , planets = IdDict.empty
                    , selected = SelectedNone
                    }
                        |> updatePlanets
                        |> Playing
              }
            , Cmd.none
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
                        ( newPlaying, cmd ) =
                            updatePlaying playingMsg playing
                    in
                    ( { model | page = Playing newPlaying }
                    , Cmd.map PlayingMsg cmd
                    , Audio.cmdNone
                    )


updatePlaying : PlayingMsg -> PlayingModel -> ( PlayingModel, Cmd PlayingMsg )
updatePlaying msg model =
    case msg of
        TryLink from to ->
            ( model, Cmd.none )


updatePlanets : PlayingModel -> PlayingModel
updatePlanets model =
    let
        ( maximumDistanceOccupied, maximumDistanceSeen ) =
            IdDict.fold
                (\_ planet ( maxOccupied, maxSeen ) ->
                    let
                        distance : Length
                        distance =
                            Point2d.distanceFrom Point2d.origin planet.position
                    in
                    case planet.kind of
                        VirginPlanet _ ->
                            ( maxOccupied
                            , Quantity.max distance maxSeen
                            )

                        OccupiedPlanet _ ->
                            ( Quantity.max distance maxOccupied
                            , Quantity.max distance maxSeen
                            )
                )
                ( Quantity.zero, Quantity.zero )
                model.planets
    in
    if
        Quantity.difference maximumDistanceSeen maximumDistanceOccupied
            |> Quantity.lessThan ringWidth
    then
        let
            toAdd : Int
            toAdd =
                maximumDistanceSeen
                    |> Length.inLightYears
                    |> (\n -> n / 2)
                    |> ceiling
        in
        List.foldl
            (\_ m ->
                addPlanet
                    { fromDistance = maximumDistanceSeen
                    , toDistance = maximumDistanceSeen |> Quantity.plus ringWidth
                    }
                    m
            )
            model
            (List.range 1 toAdd)

    else
        model


addPlanet : { fromDistance : Length, toDistance : Length } -> PlayingModel -> PlayingModel
addPlanet ({ fromDistance, toDistance } as distances) model =
    let
        ( planet, nextSeed ) =
            Random.step planetGenerator model.currentSeed

        planetGenerator : Random.Generator PlanetGenerationResult
        planetGenerator =
            Random.map3 Planet
                nameGenerator
                positionGenerator
                kindGenerator
                |> Random.map PlanetGenerated

        positionGenerator : Random.Generator (Point2d Meters ())
        positionGenerator =
            Random.map2 Point2d.rTheta
                (Random.map Length.lightYears
                    (Random.float
                        (Length.inLightYears fromDistance)
                        (Length.inLightYears toDistance)
                    )
                )
                (Random.map Angle.radians (Random.float 0 (2 * pi)))

        kindGenerator : Random.Generator PlanetKind
        kindGenerator =
            Random.map VirginPlanet (Random.list 5 occupiedPlanetGenerator)

        occupiedPlanetGenerator : Random.Generator OccupiedPlanet
        occupiedPlanetGenerator =
            Random.weighted ( 1, FarmKind ) [ ( 1, FactoryKind ), ( 1, DepositKind ) ]
                |> Random.andThen
                    (\k ->
                        case k of
                            FarmKind ->
                                Random.map3
                                    (\product maxTurns perTurn ->
                                        FarmPlanet
                                            { product = product
                                            , turnsLeft = maxTurns
                                            , perTurn = perTurn
                                            }
                                    )
                                    randomProduct
                                    (Random.int 2 10)
                                    (Random.int 1 3)

                            FactoryKind ->
                                Random.map
                                    (\efficiency ->
                                        FactoryPlanet
                                            { efficiency = efficiency
                                            , incoming = IdDict.empty
                                            , order = Nothing
                                            }
                                    )
                                    (Random.weighted ( 3, 2 )
                                        [ ( 1, 1 )
                                        , ( 0.5, 3 )
                                        ]
                                    )

                            DepositKind ->
                                Random.map
                                    (\capacity ->
                                        DepositPlanet
                                            { capacity = capacity
                                            , content = IdDict.empty
                                            }
                                    )
                                    (Random.int 5 10)
                    )
    in
    case planet of
        GiveUpGeneration ->
            { model | currentSeed = nextSeed }

        PlanetGenerated generated ->
            { model
                | currentSeed = nextSeed
                , planets = IdDict.insert generated model.planets
            }

        RetryGeneration ->
            addPlanet distances { model | currentSeed = nextSeed }


type PlanetKindWithoutData
    = FarmKind
    | FactoryKind
    | DepositKind


randomProduct : Random.Generator (Id ProductId)
randomProduct =
    Random.map Id (Random.int 1 3)


nameGenerator : Random.Generator String
nameGenerator =
    -- TODO: unsteal this
    let
        pairGenerator : Random.Generator String
        pairGenerator =
            Random.uniform ""
                [ "LE", "XE", "GE", "ZA", "CE", "BI", "SO", "US", "ES", "AR", "MA", "IN", "DI", "RE", "A", "ER", "AT", "EN", "BE", "RA", "LA", "VE", "TI", "ED", "OR", "QU", "AN", "TE", "IS", "RI", "ON" ]
    in
    Random.weighted ( 3, 4 ) [ ( 1, 5 ) ]
        |> Random.andThen (\length -> Random.list length pairGenerator)
        |> Random.map String.concat


type PlanetGenerationResult
    = RetryGeneration
    | PlanetGenerated Planet
    | GiveUpGeneration
