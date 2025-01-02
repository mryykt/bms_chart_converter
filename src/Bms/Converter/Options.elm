module Bms.Converter.Options exposing (..)

import Bms.Converter.Clustering.KernelFunction exposing (KernelFunction)
import Bms.Types exposing (ChartType)


type alias Options =
    { chartType : ChartType, bandWidth : Float, kernelFunction : KernelFunction, inscreaseScratchOptions : Maybe IncreaseScratchOptions }


type alias IncreaseScratchOptions =
    { minDuration : Int, isIncludingLn : Bool }
