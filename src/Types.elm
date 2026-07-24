module Types exposing (Link, Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Recipe, Selected(..))

import Audio
import Data exposing (Product)
import Id exposing (Id, LinkId, PlanetId)
import IdDict exposing (IdDict)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
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
    , links : IdDict LinkId Link
    , selected : Selected
    , earthNeed : { product : Product, quantity : Int, timeout : Int }
    }


type Selected
    = SelectedPlanet (Id PlanetId)
    | SelectedLink (Id LinkId)
    | SelectedEarth
    | SelectedNone


type alias Link =
    { from : Id PlanetId
    , to : Id PlanetId
    , transport : List { product : Product, quantity : Int }
    }


type alias Planet =
    { name : String
    , position : Point2d Meters ()
    , kind : PlanetKind
    }


type PlanetKind
    = VirginPlanet (List OccupiedPlanet)
    | OccupiedPlanet OccupiedPlanet


type OccupiedPlanet
    = FarmPlanet
        { product : Product
        , timeout : Int
        , perTurn : Int
        }
    | FactoryPlanet
        { efficiency : Int
        , order : Maybe Product
        , deposit : List { product : Product, quantity : Int }
        }
    | DepositPlanet
        { capacity : Int
        , content : List { product : Product, quantity : Int }
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
    = TryLink (Id PlanetId) (Id PlanetId)
    | SelectPlanet (Id PlanetId)
    | SelectEarth
    | OccupyPlanet (Id PlanetId) OccupiedPlanet
    | EndTurn
