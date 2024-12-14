module Bms.Converter.Options exposing (..)

import Bms.TimeObject as TimeObject
import Clustering.KernelFunction as Kernel


type alias Options =
    { bandWidth : Float, kernelFunction : Float -> Float, inscreaseScratchOptions : Maybe IncreaseScratchOptions }


type alias IncreaseScratchOptions =
    { minDuration : Int }


defOptions : Options
defOptions =
    { bandWidth = TimeObject.resolution / 2, kernelFunction = Kernel.gauss, inscreaseScratchOptions = Nothing }


defIncreaseScratchOptions : IncreaseScratchOptions
defIncreaseScratchOptions =
    { minDuration = 16 }
