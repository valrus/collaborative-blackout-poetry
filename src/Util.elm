module Util exposing (..)


type alias IndexRange =
    { start : Int
    , end : Int
    }


indexRangeIncludes : IndexRange -> Int -> Bool
indexRangeIncludes indexRange target =
    if indexRange.start <= indexRange.end then
        indexRange.start <= target && indexRange.end >= target

    else
        indexRange.end <= target && indexRange.start >= target
