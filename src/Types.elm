module Types exposing (ColonyData, DepositData, FactoryData, FarmData, GameMode, Highlighted(..), Link, LostModel, Model, Msg(..), OccupiedPlanet(..), Page(..), Planet, PlanetKind(..), PlayingModel, PlayingMsg(..), Selected(..))

import Audio
import Browser.Dom
import Id exposing (Id, PlanetId)
import IdDict exposing (IdDict)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Product exposing (Product)
import Product.Dict exposing (ProductDict)
import Quantity exposing (Quantity)
import Random
import Time


type alias Model =
    { page : Page
    , sound : Maybe ( Audio.Source, Time.Posix )
    }


type Page
    = Menu
    | Playing PlayingModel
    | Lost LostModel


type alias PlayingModel =
    { initialSeed : Int
    , currentSeed : Random.Seed
    , planets : IdDict PlanetId Planet
    , selected : Selected
    , highlighted : Highlighted
    , score : Int
    , gameMode : GameMode
    , rings : Int
    , svgContainerSize : ( Quantity Int Pixels, Quantity Int Pixels )
    , center : Point2d Meters ()
    , zoom : Quantity Float (Quantity.Rate Meters Pixels)
    }


type alias LostModel =
    { initialSeed : Int
    , planets : IdDict PlanetId Planet
    , selected : Selected
    , highlighted : Highlighted
    , score : Int
    , gameMode : GameMode
    }


type alias GameMode =
    { hard : Bool }


type Selected
    = SelectedPlanet (Id PlanetId)
    | SelectedNone


type Highlighted
    = HighlightedPlanet (Id PlanetId)
    | HighlightedNone


type alias Planet =
    { links : IdDict PlanetId Link
    , name : String
    , position : Point2d Meters ()
    , kind : PlanetKind
    }


type alias Link =
    ProductDict Int


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
    , countdown : Int
    , perTurn : Int
    }


type alias FactoryData =
    { efficiency : Int
    , order : Maybe Product
    , deposit : ProductDict Int
    }


type alias DepositData =
    { capacity : Maybe Int
    , content : ProductDict Int
    }


type alias ColonyData =
    { product : Product
    , quantity : Int
    , countdown : Int
    }


type Msg
    = Play GameMode
    | TimeResult Audio.Source Time.Posix
    | InitialSeed GameMode Int
    | PlaySound
    | AudioLoadResult (Result Audio.LoadError Audio.Source)
    | PlayingMsg PlayingMsg
    | Resized Int Int
    | GotSvgContainerSize (Result Browser.Dom.Error Browser.Dom.Viewport)


type PlayingMsg
    = SelectPlanet (Id PlanetId)
    | OccupyPlanet (Id PlanetId) OccupiedPlanet
    | EndTurn
    | SetFactoryProduction (Id PlanetId) (Maybe Product)
    | HighlightPlanet (Id PlanetId)
    | HighlightNone
    | SetLink (Id PlanetId) (Id PlanetId) Product Int
