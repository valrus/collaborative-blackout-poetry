module Util exposing (..)


type alias IndexRange =
    { start : Int
    , end : Int
    }


type alias IndexSlice =
    { start : Int
    , end : Int
    }


toSlice : IndexRange -> IndexSlice
toSlice range =
    { start = range.start, end = range.end + 1 }


indexRangeIncludes : IndexRange -> Int -> Bool
indexRangeIncludes indexRange target =
    if indexRange.start <= indexRange.end then
        indexRange.start <= target && indexRange.end >= target

    else
        indexRange.end <= target && indexRange.start >= target


indexRangeEncompasses : IndexRange -> IndexRange -> Bool
indexRangeEncompasses outerRange innerRange =
    indexRangeIncludes outerRange innerRange.start
        && indexRangeIncludes outerRange innerRange.end
