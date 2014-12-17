module Streamer.SlidingWindow where


import qualified Data.IntSet as IS


data SlidingWindow = SlidingWindow
    { slLowBound   :: Int           -- Lower window limit.
    , slUpBound    :: Int           -- Upper window limit.
    , slFreePos    :: IS.IntSet     -- Set containing the "free" (unoccupied)
                                    -- positions.
    } deriving (Eq, Show)


createSlidingWindow :: Int -> SlidingWindow
createSlidingWindow up
    | up > 1 =
        SlidingWindow {slLowBound = 0, slUpBound = up-1, slFreePos = freeP}
    | otherwise =
        SlidingWindow {slLowBound = 0, slUpBound = 0, slFreePos = IS.empty}
    where
        freeP = IS.fromAscList [0..up-1]


-- Reserves the next available position.
reserve :: SlidingWindow -> (SlidingWindow, Maybe Int)
reserve win =
    if IS.null freePos == True
        -- If the set of free positions is empty, return the same window and
        -- Nothing.
        then (win, Nothing)
        -- otherwise, update the set of free positions and return the last one
        else (nWin, Just pos)
    where
        freePos = slFreePos win
        (pos, nFreePos) = IS.deleteFindMin freePos
        nWin = SlidingWindow { slLowBound = pos+1
                             , slUpBound = slUpBound win
                             , slFreePos = nFreePos}


-- Marks the position as free.
release :: SlidingWindow -> Int -> SlidingWindow
release win val
    | (val <= slUpBound win)
      && (IS.notMember val $ slFreePos win) =
    SlidingWindow { slLowBound = min val $ slLowBound win
                  , slUpBound = max val $ slUpBound win
                  , slFreePos = IS.insert val $ slFreePos win}
    | otherwise = win


-- Grows the window to a new upperbound.
grow :: SlidingWindow -> Int -> SlidingWindow
grow win newUpperBound
    | newUpperBound > slUpBound win =
        SlidingWindow { slLowBound = slLowBound win
                      , slUpBound = newUpperBound-1
                      , slFreePos = IS.union (slFreePos win) upperFreePos}
    | otherwise = win
    where
        upperFreePos = IS.fromAscList [(slUpBound win)..(newUpperBound-1)]