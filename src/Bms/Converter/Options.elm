module Bms.Converter.Options exposing (..)

import Bms.Converter.Clustering.KernelFunction exposing (KernelFunction)


type alias Options =
    { bandWidth : Float, kernelFunction : KernelFunction, inscreaseScratchOptions : Maybe IncreaseScratchOptions }


type alias IncreaseScratchOptions =
    { minDuration : Int, isIncludingLn : Bool }
