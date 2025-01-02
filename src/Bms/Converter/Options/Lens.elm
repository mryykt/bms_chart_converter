module Bms.Converter.Options.Lens exposing (..)


type alias Getter a b =
    a -> b


type alias Setter a b =
    b -> a -> a


type alias Lens a b =
    { getter : Getter a b, setter : Setter a b }


compose : Lens b c -> Lens a b -> Lens a c
compose l1 l2 =
    { getter = l1.getter << l2.getter, setter = \x y -> l2.setter (l1.setter x <| l2.getter y) y }


get : Lens a b -> a -> b
get { getter } v =
    getter v


set : Lens a b -> b -> a -> a
set { setter } x y =
    setter x y


update : Lens a b -> (b -> b) -> a -> a
update { getter, setter } f x =
    setter (f <| getter x) x


value : Lens { a | value : b, invalid : Bool } b
value =
    { getter = .value, setter = \x y -> { y | value = x, invalid = False } }


chartType : Lens { a | chartType : b } b
chartType =
    { getter = .chartType, setter = \x y -> { y | chartType = x } }


bandWidth : Lens { a | bandWidth : b } b
bandWidth =
    { getter = .bandWidth, setter = \x y -> { y | bandWidth = x } }


kernelFunction : Lens { a | kernelFunction : b } b
kernelFunction =
    { getter = .kernelFunction, setter = \x y -> { y | kernelFunction = x } }


minDuration : Lens { a | minDuration : b } b
minDuration =
    { getter = .minDuration, setter = \x y -> { y | minDuration = x } }


isIncludeLn : Lens { a | isIncludeLn : b } b
isIncludeLn =
    { getter = .isIncludeLn, setter = \x y -> { y | isIncludeLn = x } }
