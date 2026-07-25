module Types exposing (ColonyData, DepositData, FactoryData, FarmData, Highlighted(..), Link, Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Recipe, Selected(..))

import Audio
import Id exposing (Id, PlanetId)
import IdDict exposing (IdDict)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Product exposing (Product)
import Random
import Time


type alias Model =
    { page : Page
    , sound : Maybe ( Audio.Source, Time.Posix )
    }


type Page
    = Menu
    | Playing PlayingModel


type alias PlayingModel =
    { initialSeed : Int
    , currentSeed : Random.Seed
    , maximumDistanceReched : Length
    , planets : IdDict PlanetId Planet
    , selected : Selected
    , highlighted : Highlighted
    , score : Int
    }


type Selected
    = SelectedPlanet (Id PlanetId)
    | SelectedNone


type Highlighted
    = HighlightedPlanet (Id PlanetId)
    | HighlightedLink (Id PlanetId) (Id PlanetId)
    | HighlightedNone


type alias Planet =
    { links : IdDict PlanetId Link
    , name : String
    , position : Point2d Meters ()
    , kind : PlanetKind
    }


type alias Link =
    List { product : Product, quantity : Int }


type PlanetKind
    = VirginPlanet (List OccupiedPlanet)
    | ColonyPlanet ColonyData
    | OccupiedPlanet OccupiedPlanet


type OccupiedPlanet
    = FarmPlanet FarmData
    | FactoryPlanet FactoryData
    | DepositPlanet DepositData


type alias FarmData =
    { product : Product
    , timeout : Int
    , perTurn : Int
    }


type alias FactoryData =
    { efficiency : Int
    , order : Maybe Product
    , deposit : List { product : Product, quantity : Int }
    }


type alias DepositData =
    { capacity : Int
    , content : List { product : Product, quantity : Int }
    }


type alias ColonyData =
    { product : Product
    , quantity : Int
    , timeout : Int
    }


type alias Recipe =
    List
        { product : Product
        , quantity : Int
        }


type Msg
    = Play
    | TimeResult Audio.Source Time.Posix
    | InitialSeed Int
    | PlaySound
    | AudioLoadResult (Result Audio.LoadError Audio.Source)
    | PlayingMsg PlayingMsg


type PlayingMsg
    = SelectPlanet (Id PlanetId)
    | OccupyPlanet (Id PlanetId) OccupiedPlanet
    | EndTurn
    | SetFactoryProduction (Id PlanetId) (Maybe Product)
    | HighlightLink (Id PlanetId) (Id PlanetId)
    | HighlightPlanet (Id PlanetId)
    | HighlightNone
