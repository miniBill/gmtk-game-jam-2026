module Id exposing (Id(..), LinkId, PlanetId, ProductId)


type PlanetId
    = PlanetId Never


type ProductId
    = ProductId Never


type LinkId
    = LinkId Never


type Id a
    = Id Int
