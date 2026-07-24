module Id exposing (Id(..), LinkId, PlanetId, ProductId, toString)


type PlanetId
    = PlanetId Never


type ProductId
    = ProductId Never


type LinkId
    = LinkId Never


type Id a
    = Id Int


toString : Id kind -> String
toString (Id id) =
    String.fromInt id
