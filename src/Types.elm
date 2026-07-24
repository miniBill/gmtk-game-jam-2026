module Types exposing (..)

import Audio
import Id exposing (Id, LinkId, PlanetId, ProductId)
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
    }


type Selected
    = SelectedPlanet (Id PlanetId)
    | SelectedLink (Id LinkId)
    | SelectedEarth
    | SelectedNone


type alias Link =
    { from : Id PlanetId
    , to : Id PlanetId
    , transport :
        List
            { product : Id ProductId
            , quantity : Int
            }
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
        { product : Id ProductId
        , turnsLeft : Int
        , perTurn : Int
        }
    | FactoryPlanet
        { efficiency : Int
        , order : Maybe FactoryOrder
        , incoming : IdDict ProductId Int -- quantity
        }
    | DepositPlanet
        { capacity : Int
        , content : IdDict ProductId Int -- quantity
        }


type alias FactoryOrder =
    Id ProductId


type alias Product =
    { name : String
    , icon : String
    , recipe : Maybe Recipe
    }


type alias Recipe =
    List
        { product : Id ProductId
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
