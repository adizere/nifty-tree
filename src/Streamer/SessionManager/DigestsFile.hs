module Streamer.SessionManager.DigestsFile
( consumeDgstFile
, collectDigestFileEntries
) where


import Data.Maybe
import Streamer.Util                            ( maybeRead )
import Control.Concurrent                       ( threadDelay )
import System.IO                                ( IOMode(ReadMode)
                                                , openFile
                                                , hSeek
                                                , SeekMode(AbsoluteSeek)
                                                , hGetContents )
import System.Posix.Files                       ( getFileStatus, fileSize )
import Control.Monad.STM                        ( atomically )
import qualified Control.Concurrent.STM.TQueue  as STQ



-- How much to wait if the digests file EOF found, in microseconds
consumeDigestsFileDelay :: Int
consumeDigestsFileDelay = 400000


-- | Consumes lines from a digest file and appends the lines to an TQueue. Takes
-- as arguments a FilePath, a TQueue, and a number representing the position in
-- file where it should start consuming, usually 0. The third parameter
-- indicates how many bytes to initially skip before starting to append to the
-- TQueue.
--
-- This function never returns (it will continue to call itself recursively),
-- therefore it should execute in its own thread.
--
-- Example:
-- >    queue   <- STQ.newTQueueIO :: IO (STQ.TQueue String)
-- >    forkIO (consumeDgstFile "/tmp/a" 0 queue)
--
-- NB: Rewritten using example from: https://gist.github.com/ijt/1055731.
consumeDgstFile ::
    FilePath                    -- ^ Path to the file which contains digests
    -> Integer                  -- ^ Last known position in the file
    -> STQ.TQueue (String)      -- ^ Queue where we redirect the digests
    -> IO ()
consumeDgstFile p s q = do
    stat <- getFileStatus p
    if (newS stat) <= s
        then threadDelay consumeDigestsFileDelay >> consumeDgstFile p s q
        else do
            h <- openFile p ReadMode
            hSeek h AbsoluteSeek s
            lz <- hGetContents h
            putStrLn $ "Consumed from " ++ (show s) ++ " until "
                    ++ (show $ newS stat) ++ "; total: "
                    ++ (show $ length (lines lz))
            mapM_ (\l -> atomically $ STQ.writeTQueue q l) $ lines lz
            consumeDgstFile p (newS stat) q
    where
        newS stt = fromIntegral $ fileSize stt :: Integer


-- | Collects entries from the digest file until either K are found or there is
-- no available entry. Optionally it takes as parameter a list of sequence
-- numbers to be skipped over.
--
-- Uses: 'getDigestFileEntry'
collectDigestFileEntries ::
    STQ.TQueue (String)         -- queue with digest lines
    -> Int                      -- limit on the number of returned entries
    -> [(Int,String)]           -- accumulator
    -> IO [(Int, String)]
collectDigestFileEntries _ 0 ac = return ac
collectDigestFileEntries q l ac = do
    mEntry <- getDigestFileEntry q
    case mEntry of
        Just entry  -> collectDigestFileEntries q (l-1) (ac++[entry])
        Nothing     -> return ac


--------------------------------------------------------------------------------
-- | Reads the next entry from the digest file. An entry has the form of a tuple
-- (sequence number, digest).
-- The digest file is represented as a fifo queue (STM.TQueue).
-- Non-blockin function: if there is no entry available, it returns Nothing.
getDigestFileEntry ::
    STQ.TQueue (String)          -- queue with digest lines
    -> IO (Maybe (Int, String))
getDigestFileEntry queue = do
    mLine <- atomically $ STQ.tryReadTQueue queue
    case mLine of
        Just digestLine -> do
            let maybeFmTuple = digestLineToFrameMetadata digestLine
            case maybeFmTuple of
                Just (seqNr, digest) -> return $ Just (seqNr, digest)
                Nothing -> putStrLn ("Couldn't parse line: " ++ show digestLine)
                           >> return Nothing
        Nothing -> return Nothing


--------------------------------------------------------------------------------
-- | Takes a string (as read from the digests file) and parses it, yielding a
-- sequence number and the associated digest.
--
-- For example,
-- >    digestLineToFrameMetadata
--          "2 076a27c79e5ace2a3d47f9dd2e83e4ff6ea8872b3c2218f66c92b89b55f36560"
-- will output the following tuple:
-- > ("2", "076a27c79e5ace2a3d47f9dd2e83e4ff6ea8872b3c2218f66c92b89b55f36560")
digestLineToFrameMetadata ::
    String
    -> Maybe (Int, String)
digestLineToFrameMetadata "" = Nothing
digestLineToFrameMetadata line
    | (length items == 2) && (isJust seqNr) && (length digest == 64) =
        Just (fromJust seqNr, digest)
    | otherwise = Nothing
    where
        items = words line
        seqNr = maybeRead $ items!!0 :: Maybe Int
        digest = items!!1