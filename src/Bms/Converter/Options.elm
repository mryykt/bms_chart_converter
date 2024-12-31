module Bms.Converter.Options exposing (..)

import Bms.Converter.Clustering.KernelFunction as Kernel exposing (KernelFunction)
import Bms.TimeObject as TimeObject


type alias Options =
    { bandWidth : Float, kernelFunction : KernelFunction, inscreaseScratchOptions : Maybe IncreaseScratchOptions }


type alias Optional a =
    { enabled : Bool, value : a }


type alias IncreaseScratchOptions =
    { minDuration : Int, isIncludingLn : Bool }


defOptions : Options
defOptions =
    { bandWidth = TimeObject.resolution / 2, kernelFunction = Kernel.Gauss, inscreaseScratchOptions = Nothing }


defIncreaseScratchOptions : IncreaseScratchOptions
defIncreaseScratchOptions =
    { minDuration = 16, isIncludingLn = True }
