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
import qualified Control.Concurrent.BoundedChan as BCH
import qualified Data.List.Ordered              as R



-- How much to wait if the digests file EOF found, in microseconds
consumeDigestsFileDelay :: Int
consumeDigestsFileDelay = 400000


-- | Consumes lines from a digest file and appends the lines to a BoundedChan.
-- Takes as arguments a file handle and a BoundedChan and a number. The third
-- parameter indicates how many lines to initially skip before starting to
-- append to the BoundedChan.
--
-- This function never returns (it will continue to call itself recursively),
-- therefore it should execute in its own thread.
--
-- Example:
-- >    handle  <- openFile "/tmp/a" ReadMode
-- >    chan    <- newBoundedChan 10
-- >    forkIO (consumeDgstFile handle chan)
--
-- NB: Apparently, BoundedChan is not exception safe (i.e. we can't do
-- killThread on the thread executing consumeDgstFile). For an exception safe
-- chan: Control.Concurrent.Chan (http://stackoverflow.com/a/9250200/919383)
--
-- NB2: Rewritten using example from: https://gist.github.com/ijt/1055731.
consumeDgstFile ::
    FilePath                    -- ^ Path to the file which contains digests
    -> Integer                  -- ^ Last known position in the file
    -> BCH.BoundedChan (String) -- ^ Channel where we queue the digests
    -> IO ()
consumeDgstFile p s c = do
    stat <- getFileStatus p
    if (newS stat) <= s
        then threadDelay consumeDigestsFileDelay >> consumeDgstFile p s c
        else do
            h <- openFile p ReadMode
            hSeek h AbsoluteSeek s
            lz <- hGetContents h
            mapM_ (\l -> BCH.writeChan c l) $ lines lz
            consumeDgstFile p (newS stat) c
    where
        newS stt = fromIntegral $ fileSize stt :: Integer


-- | Collects entries from the digest file until either K are found or there is
-- no available entry. Optionally it takes as parameter a list of sequence
-- numbers to be skipped over.
--
-- Uses: 'getDigestFileEntry'
collectDigestFileEntries ::
    BCH.BoundedChan (String)    -- channel with digest lines
    -> [Int]                    -- list of sequence numbers to be ignored
    -> Int                      -- limit on the number of returned entries
    -> [(Int,String)]           -- accumulator
    -> IO [(Int, String)]
collectDigestFileEntries _    _             0 ac = return ac
collectDigestFileEntries c skipSeq l ac = do
    mEntry <- getDigestFileEntry c skipSeq
    case mEntry of
        Just entry ->
            collectDigestFileEntries c skipSeq (l-1) (ac++[entry])
        Nothing ->
            return ac


--------------------------------------------------------------------------------
-- | Reads the next entry from the digest file. An entry has the form of a tuple
-- (sequence number, digest).
-- The digest file is represented as a fifo queue (Bounded Chan).
-- The second parameter is a list of Int representing sequence numbers that this
-- function shall skip (if encountered in the fifo queue).
-- Non-blockin function: if there is no entry available, it returns Nothing.
getDigestFileEntry ::
    BCH.BoundedChan (String)    -- channel with digest lines
    -> [Int]                    -- list of sequence numbers to be ignored
    -> IO (Maybe (Int, String))
getDigestFileEntry chan skipSeqNrList = do
    mLine <- BCH.tryReadChan chan
    case mLine of
        Just digestLine -> do
            let maybeFmTuple = digestLineToFrameMetadata digestLine
            case maybeFmTuple of
                Just (seqNr, digest) -> do
                    if R.member seqNr skipSeqNrList
                        -- if we should skip this entry, call the function again
                        then getDigestFileEntry chan skipSeqNrList
                        else return $ Just (seqNr, digest)
                Nothing -> getDigestFileEntry chan skipSeqNrList
        Nothing -> return Nothing


--------------------------------------------------------------------------------
-- | Takes a line from the digests file and parses it, yielding a sequence
-- number and the associated digest.
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