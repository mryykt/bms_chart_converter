module Bms.Converter.Options exposing (..)

import Bms.Converter.Clustering.KernelFunction as Kernel
import Bms.TimeObject as TimeObject


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
