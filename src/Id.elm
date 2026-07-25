module Id exposing (Id(..), PlanetId, toString)


type PlanetId
    = PlanetId Never


type Id a
    = Id Int


toString : Id kind -> String
toString (Id id) =
    String.fromInt id
