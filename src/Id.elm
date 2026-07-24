module Id exposing (Id(..), LinkId, PlanetId, toString)


type PlanetId
    = PlanetId Never


type LinkId
    = LinkId Never


type Id a
    = Id Int


toString : Id kind -> String
toString (Id id) =
    String.fromInt id
