import Data.Digest.SHA2
import qualified Data.ByteString as B
import System.IO


doManyHashes :: Int -> IO Bool
doManyHashes 0 = do
    return True

doManyHashes counter = do
    contents <- B.readFile "/tmp/data.0512"
    -- let contentsWord8 = B.unpack contents
    let hash = sha256 $ B.unpack contents
    -- let oct = toOctets hash
    -- putStr "."
    -- hFlush stdout
    -- putStrLn (show oct)
    doManyHashes $ counter-1


main = do
    doManyHashes 500000
    -- putStrLn ""