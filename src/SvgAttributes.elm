module SvgAttributes exposing (cx, cy, height, r, rx, ry, strokeWidth, viewBox, viewBoxWithPadding, width, x, y)

import Length exposing (Length)
import Quantity
import Round
import Svg exposing (Attribute)
import Svg.Attributes


viewBox : Length -> Length -> Length -> Length -> String
viewBox minx_ miny_ width_ height_ =
    [ minx_, miny_, width_, height_ ]
        |> List.map (\l -> Round.round 2 (Length.inLightYears l))
        |> String.join " "


viewBoxWithPadding : Length -> Length -> Length -> Length -> Length -> String
viewBoxWithPadding padding minx_ miny_ width_ height_ =
    viewBox
        (minx_ |> Quantity.minus padding)
        (miny_ |> Quantity.minus padding)
        (width_ |> Quantity.plus (Quantity.multiplyBy 2 padding))
        (height_ |> Quantity.plus (Quantity.multiplyBy 2 padding))


strokeWidth : Float -> Attribute msg
strokeWidth v =
    Svg.Attributes.strokeWidth (Round.round 3 v)


x : Float -> Attribute msg
x v =
    Svg.Attributes.x (Round.round 2 v)


y : Float -> Attribute msg
y v =
    Svg.Attributes.y (Round.round 2 v)


width : Float -> Attribute msg
width v =
    Svg.Attributes.width (Round.round 2 v)


height : Float -> Attribute msg
height v =
    Svg.Attributes.height (Round.round 2 v)


cx : Float -> Attribute msg
cx v =
    Svg.Attributes.cx (Round.round 2 v)


cy : Float -> Attribute msg
cy v =
    Svg.Attributes.cy (Round.round 2 v)


r : Float -> Attribute msg
r v =
    Svg.Attributes.r (Round.round 2 v)


rx : Float -> Svg.Attribute msg
rx v =
    Svg.Attributes.rx (Round.round 2 v)


ry : Float -> Svg.Attribute msg
ry v =
    Svg.Attributes.ry (Round.round 2 v)
