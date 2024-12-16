module Bms.Converter.Options exposing (..)

import Bms.Converter.Clustering.KernelFunction as Kernel
import Bms.TimeObject as TimeObject
import Ghost exposing (Ghost(..))


type alias Options =
    { bandWidth : Float, kernelFunction : Float -> Float, inscreaseScratchOptions : Ghost IncreaseScratchOptions }


type alias IncreaseScratchOptions =
    { minDuration : Int }


defOptions : Options
defOptions =
    { bandWidth = TimeObject.resolution / 2, kernelFunction = Kernel.gauss, inscreaseScratchOptions = Ghost defIncreaseScratchOptions }


defIncreaseScratchOptions : IncreaseScratchOptions
defIncreaseScratchOptions =
    { minDuration = 16 }
