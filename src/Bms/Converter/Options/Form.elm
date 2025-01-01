module Bms.Converter.Options.Form exposing (Field, Form, from, init)

import Bms.Converter.Clustering.KernelFunction as Kernel exposing (KernelFunction)
import Bms.Converter.Options exposing (IncreaseScratchOptions, Options)
import Bms.Converter.Options.Lens as Lens exposing (..)
import Form.Decoder as D exposing (Decoder)


type alias Field a =
    { value : a, invalid : Bool }


type alias Form =
    { bandWidth : Field String
    , kernelFunction : Field KernelFunction

    --
    , increaseScratchOptions : Field Bool
    , minDuration : Field String
    , isIncludeLn : Field Bool
    }


initField : a -> Field a
initField x =
    Field x False


init : Form
init =
    { bandWidth = initField "480"
    , kernelFunction = initField Kernel.Gauss
    , increaseScratchOptions = initField False
    , minDuration = initField "8"
    , isIncludeLn = initField True
    }


from : Form -> ( Form, Maybe Options )
from form =
    case D.run decoder form of
        Ok options ->
            ( form, Just options )

        Err errs ->
            ( List.foldl (\errField form_ -> Lens.update errField (\x -> { x | invalid = True }) form_) form errs, Nothing )


decoder : Decoder Form (Lens Form (Field String)) Options
decoder =
    let
        increaseScratchOptions : Decoder Form (Lens Form (Field String)) (Maybe IncreaseScratchOptions)
        increaseScratchOptions =
            D.custom <|
                \form ->
                    if form.increaseScratchOptions.value then
                        D.run
                            (D.top IncreaseScratchOptions
                                |> intField minDuration
                                |> identityField isIncludeLn
                                |> D.map Just
                            )
                            form

                    else
                        Ok Nothing
    in
    D.top Options |> floatField bandWidth |> identityField kernelFunction |> D.field increaseScratchOptions


intField : Lens a (Field String) -> Decoder a (Lens a (Field String)) (Int -> b) -> Decoder a (Lens a (Field String)) b
intField lens =
    D.field (D.lift (.value << get lens) (D.int lens))


floatField : Lens a (Field String) -> Decoder a (Lens a (Field String)) (Float -> b) -> Decoder a (Lens a (Field String)) b
floatField lens =
    D.field (D.lift (.value << get lens) (D.float lens))


identityField : Lens a (Field b) -> Decoder a c (b -> d) -> Decoder a c d
identityField lens =
    D.field (D.lift (.value << get lens) D.identity)
